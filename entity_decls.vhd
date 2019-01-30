----------------------------------------------------------------------------
--  Block Entity Declarations Atmel AVR CPU Design
--
--	This file contains the entity declarations in VHDL for the following blocks
--	part of the overall design for the Atmel AVR CPU:
--	1. Control Unit
--	2. Program Memory Access Unit
--	3. Data Memory Access Unit
--	4. Registers Unit (Also used for I/O Space)
--	5. ALU Unit (including Status Register)
--
--  Revision History:
--	23 Jan 19	Kavya Sreedhar & Dan Xu 	Initial Revision
--  23 Jan 19	Kavya Sreedhar & Dan Xu		Modified registers unit, added IO
--  24 Jan 19	Kavya Sreedhar & Dan Xu 	Updated Comments
----------------------------------------------------------------------------

package CPU_CONSTANTS is
	-- number of general purpose registers
	constant NUM_REGISTERS: 		integer := 32;
	-- number of bits in data busses
	constant NUM_DATA_BITS: 		integer := 8;
	-- number of bits in address busses
	constant NUM_ADDRESS_BITS: 		integer := 16;
	-- number of bits in each general-purpose register
	constant NUM_BITS_PER_REGISTER: integer := 8;
end package;

package INSTRUCTION_CONSTANTS is
	constant NUM_INSTRUCTION_BITS: integer := 16;
	constant ADC_instruction: 	unsigned(NUM_INSTRUCTION_BITS - 1 downto 0)
		:= "000111----------";
	constant ADD_instruction: 	unsigned(NUM_INSTRUCTION_BITS - 1 downto 0)
		:= "000011----------";
	-- 2 clocks
	constant ADIW_instruction: 	unsigned(NUM_INSTRUCTION_BITS - 1 downto 0)
		:= "10010110--------";
	constant AND_instruction: 	unsigned(NUM_INSTRUCTION_BITS - 1 downto 0)
		:= "10010110--------";
	constant ANDI_instruction: 	unsigned(NUM_INSTRUCTION_BITS - 1 downto 0)
		:= "0111------------";
	constant ASR_instruction: 	unsigned(NUM_INSTRUCTION_BITS - 1 downto 0)
		:= "1001010-----0101";
	constant BCLR_instruction: 	unsigned(NUM_INSTRUCTION_BITS - 1 downto 0)
		:= "100101001---1000";
	constant BLD_instruction: 	unsigned(NUM_INSTRUCTION_BITS - 1 downto 0)
		:= "1111100-----0---";
	constant BSET_instruction: 	unsigned(NUM_INSTRUCTION_BITS - 1 downto 0)
		:= "100101000---1000";
	constant BST_instruction: 	unsigned(NUM_INSTRUCTION_BITS - 1 downto 0)
		:= "1001010-----0101";
	constant COM_instruction: 	unsigned(NUM_INSTRUCTION_BITS - 1 downto 0)
		:= "1001010-----0000";
	constant CP_instruction: 	unsigned(NUM_INSTRUCTION_BITS - 1 downto 0)
		:= "000101----------";
	constant CPC_instruction: 	unsigned(NUM_INSTRUCTION_BITS - 1 downto 0)
		:= "000001----------";
	constant CPI_instruction: 	unsigned(NUM_INSTRUCTION_BITS - 1 downto 0)
		:= "0011------------";
	constant DEC_instruction: 	unsigned(NUM_INSTRUCTION_BITS - 1 downto 0)
		:= "1001010-----1010";
	
end package;
	
package ALU_constants is 
	constant F_Block_Operation: std_logic_vector(1 downto 0) := "00";
	constant Adder_Subtractor_Operation: std_logic_vector(1 downto 0) := "01";
	constant Shifter_Rotater_Operation: std_logic_vector(1 downto 0) := "10";
	
	constant Shifter_low_bit_highest_bit: std_logic_vector(1 downto 0) := "00";
	constant Shifter_low_bit_0: std_logic_vector(1 downto 0) := "01";
	constant Shifter_low_bit_bit_1: std_logic_vector(1 downto 0) := "10";
	constant Shifter_low_bit_carry: std_logic_vector(1 downto 0) := "11";
	
	constant Shifter_middle_bits_select_immediate_right: std_logic := '0';
	constant Shifter_middle_bits_select_immediate_left: std_logic := '0';
	
	constant Shifter_high_bit_select_second_highest_bit: std_logic_vector := "000";
	constant Shifter_high_bit_select_highest_bit: std_logic_vector := "001";
	constant Shifter_high_bit_select_lowest_bit: std_logic_vector := "010";
	constant Shifter_high_bit_select_0: std_logic_vector := "011";
	constant Shifter_high_bit_select_carry: std_logic_vector := "100";
		
	constant F_Block_Select_0: std_logic_vector(3 downto 0) := "0000";
	constant F_Block_Select_nor: std_logic_vector(3 downto 0) := "0001";
	constant F_Block_Select_not_A: std_logic_vector(3 downto 0) := "0011";
	constant F_Block_Select_not_B: std_logic_vector(3 downto 0) := "0101";
	constant F_Block_Select_xor: std_logic_vector(3 downto 0) := "0110";
	constant F_Block_Select_nand: std_logic_vector(3 downto 0) := "0111";
	constant F_Block_Select_and: std_logic_vector(3 downto 0) := "1000";
	constant F_Block_Select_xnor: std_logic_vector(3 downto 0) := "1001";
	constant F_Block_Select_or: std_logic_vector(3 downto 0) := "1110";
	constant F_Block_Select_1: std_logic_vector(3 downto 0) := "1111";
	
	constant Subtraction: std_logic := '1';
	constant Addition: std_logic := '0';
	
	constant AddSub_Op_Select_0: std_logic_vector(1 downto 0) := "00";
	constant AddSub_Op_Select_1: std_logic_vector(1 downto 0) := "01";
	constant AddSub_Op_Select_OperandB: std_logic_vector(1 downto 0) := "10";
end package;
--
-- declaration of libraries used
--
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;

library opcodes;
use opcodes.opcodes.all;

use CPU_CONSTANTS.all;
use INSTRUCTION_CONSTANTS.all;

--
-- ALU entity declaration
-- contains all standard logic and arithmetic operations including Boolean
--  operations, shifts and rotates, bit functions, addition, subtraction, and
--  comparison
-- operands may be registers or immediate values from the instruction
--
entity ALU is
	port(
	
		-- system clk
		clk: in std_logic;
		
		-- control signal inputs
		-- selects ALU operation to perform
		-- 	00 F Block operation
		-- 	01 Adder/Subtractor operation
		-- 	10 Shifter/Rotater operation
		ALU_result_select: in std_logic_vector(1 downto 0);
		-- selects what to update lowest bit of shifter / rotater with
		--	00 select current highest bit
		-- 	01 select 0
		-- 	10 select current bit 1
		-- 	11 select current carry flag bit
		Shifter_low_bit_select: in std_logic_vector(1 downto 0);
		-- when ALU_result_select indicates Adder/Subtractor operation
		--	lowest bit of this vector is the Subtract control signal (all others
		-- 	bits are ignored)
		-- when ALU_result_select indicates Shifter/Rotater operation
		--	bit 0 selects value for middle bits:
		--		0 select bit to the immediate right
		--		1 select bit to the immediate left
		Shifter_middle_bits_select: in std_logic;
		-- 	bits 3 downto 1 selects high bit value:
		--		000 select current second highest bit
		--		001	select current highest bit
		--		010	select current lowest bit
		--		011	select 0
		--		100	select current carry flag bit
		Shifter_high_bit_select: in std_logic_vector(2 downto 0);
		-- when ALU_result_select indicates F Block operation
		--	F Block inputs to mux for F Block operations
		F_Block_Select: in std_logic_vector(3 downto 0);
		-- indicates whether an addition or subtraction operation should occur
		Subtract: in std_logic;
		-- indicating whether performing ALU operation involving current carry / 
		-- borrow bit
		ALU_op_with_carry: in std_logic;
		-- chooses value of second operand for addition / subtraction
		-- 	00 select 0
		--  01 select 1
		--  10 select OperandB
		AddSub_Op_Select: in std_logic_vector(1 downto 0);
		-- flag mask indicating which flag values to update after ALU operation
		Status_Register_Mask: in std_logic_vector(7 downto 0);
		
		-- other inputs
		-- first operand
		OperandA: in std_logic_vector(NUM_DATA_BITS - 1 downto 0);
		-- second operand
		OperandB: in std_logic_vector(NUM_DATA_BITS - 1 downto 0);
		
		-- outputs
		-- ALU result (from F Block, Adder/Subtractor, or Shifter/Rotater)
		Result: out std_logic_vector(NUM_DATA_BITS - 1 downto 0);
		-- updated status register
		Status_Register: out std_logic_vector(NUM_DATA_BITS - 1 downto 0)
        );
end entity;

architecture ALU_arch of ALU is
	signal shift_rotate_result: 	std_logic_vector(NUM_DATA_BITS - 1 downto 0);
	signal F_block_result: 			std_logic_vector(NUM_DATA_BITS - 1 downto 0);
	signal adder_subtractor_result: std_logic_vector(NUM_DATA_BITS - 1 downto 0);
	signal carry_outs: 				std_logic_vector(NUM_DATA_BITS - 1 downto 0);
	signal Operand2: 				std_logic_vector(NUM_DATA_BITS - 1 downto 0);
	signal ALU_result:				std_logic_vector(NUM_DATA_BITS - 1 downto 0);
begin

	get_F_block_result: for i in range 0 to NUM_DATA_BITS - 1 generate
		F_block_result(i) <= 
			'0' 							when F_Block_Select = "0000" else
			OperandA(i) nor OperandB 		when F_Block_Select = "0001" else
			not OperandA(i) 				when F_Block_Select = "0011" else
			not OperandB(i) 				when F_Block_Select = "0101" else
			OperandA(i) xor OperandB(i) 	when F_Block_Select = "0110" else
			OperandA(i) nand OperandB(i) 	when F_Block_Select = "0111" else
			OperandA(i) and OperandB(i) 	when F_Block_Select = "1000" else
			OperandA(i) xnor OperandB(i) 	when F_Block_Select = "1001" else
			OperandA(i) or OperandB(i) 		when F_Block_Select = "1110" else
			'1' 							when F_Block_Select = "1111";
	end generate get_F_block_result;
	
	Operand2 <= "00000000" when AddSub_Op_Select = "00" else
				"00000001" when AddSub_Op_Select = "01" else
				OperandB;
				
	carry_borrow_for_ALU_op <= 	'0' when ALU_op_with_carry = '0' else
								current_status_register(0);
								
	adder_subtractor_result(0) <= 
		OperandA(0) xor (Operand2(0) xor Subtract) xor 
		(Subtract xor carry_borrow_for_ALU_op);
	carry_outs(0) <= Subtract;
	
	get_adder_subtractor_bits: for i in range 1 to NUM_DATA_BITS - 1 generate
		adder_subtractor_result(i) <=
			OperandA(i) xor (Operand2(i) xor Subtract) xor carry_outs(i - 1);
		carry_outs(i) <= 
			(OperandA(i) and (Operand2(i) xor Subtract)) or 
			(carry_outs(i - 1) and (OperandA(i) xor (Operand2(i) xor Subtract)));
	end generate get_adder_subtractor_bits;

	shift_rotate_result(0) <= 	
		OperandA(NUM_DATA_BITS - 1) when Shifter_low_bit_select = "00" else
		'0' 						when Shifter_low_bit_select = "01" else
		OperandA(1) 				when Shifter_low_bit_select = "10" else
		current_status_register(0) 	when Shifter_low_bit_select = "11";
	
	get_shift_rotate_middle_bits: for i in range 1 to NUM_DATA_BITS - 2 generate
		shift_rotate_result(i) <= 	
			OperandA(i - 1) when Shifter_middle_bits_select = '0' else
			OperandA(i + 1) when Shifter_middle_bits_select = '1';
	end generate get_shift_rotate_middle_bits;
	
	shift_rotate_result(NUM_DATA_BITS - 1) <=
		OperandA(NUM_DATA_BITS - 2) when Shifter_high_bit_select = "000" else
		OperandA(NUM_DATA_BITS - 1) when Shifter_high_bit_select = "001" else
		OperandA(0) 				when Shifter_high_bit_select = "010" else
		'0' 						when Shifter_high_bit_select = "011" else
		current_status_register(0);
		
	ALU_result <= 
		F_block_result 			when ALU_result_select = "00" else
		adder_subtractor_result when ALU_result_select = "01" else
		shift_rotate_result 	when ALU_result_select = "10";
		
	process(clk)
	begin
		if rising_edge(clk) then
			Result <= ALU_result;
		end if;
	end process;
	
end architecture;

--
-- Registers entity declaration: used for general purpose registers and IO Space
--
entity Registers is
	port(
		-- array containing contents of all bits of all NUM_REGISTERS registers
		-- individual registers can be accessed by indexing according to the
		-- register number * NUM_BITS_PER_REGISTER appropriately
		Register_contents: buffer std_logic_vector(NUM_REGISTERS * 
			NUM_BITS_PER_REGISTER - 1 downto 0);
		
		-- if general purpose registers:
		--	00 IO Register
		-- 	01 Data_Data_Bus
		--	10 Output from ALU
		-- if IO Space:
		--	00 Data Data Bus
		--	01 Status Register Output from ALU
		Register_val_select: in std_logic_vector(1 downto 0);
		-- enable writing to general purpose registers
		Register_Write_Enable: out std_logic;
		-- select which register
		Register_select: out std_logic_vector(log2(NUM_REGISTERS) - 1 downto 0);
		-- indicates nibbles of a register should be swapped
		Swap: in std_logic;
		-- bit in register to set transfer bit in status register to or vice versa
		T_bit: in std_logic_vector(2 downto 0);
		-- indicates that a bit in a register should be updated to transfer bit
		-- or vice versa
		Transfer: in std_logic
        );
end entity;

--
-- Data Memory Access entity declaration
--
entity Data_Memory_Access is
	port(
		-- control signal inputs
		-- selects address source
		-- 	000 selects instruction register
		-- 	001 selects program data bus
		-- 	010 selects register X
		-- 	011 selects register Y
		-- 	100 selects register Z
		-- 	101 selects register SP
		-- 	110 selects Y with a 6-bit unsigned offset
		-- 	111 selects Z with a 6-bit unsigned offset
		Address_Source_Select: in std_logic_vector(2 downto 0);
		-- selects offset source
		--  00 select 0
		--  01 select +1
		--  10 select -1
		--  11 select q (for Y and Z registers with q unsigned offset)
		Offset_Source_Select: in std_logic_vector(3 downto 0);
		-- indicates whether or not pre/post-increment/decrement was 
		-- part of instruction
		Pre_or_Post_Select: in std_logic;
		
		-- other inputs
		-- second word of instruction
		Program_Data_Bus: in std_logic_vector(NUM_ADDRESS_BITS - 1 downto 0);
		X_register: in std_logic_vector(15 downto 0);
		Y_register: in std_logic_vector(15 downto 0);
		Z_register: in std_logic_vector(15 downto 0);
		SP_register: in std_logic_vector(15 downto 0);
		
		-- outputs
		Data_Address_Bus: out std_logic_vector(15 downto 0);
		Data_Data_Bus: out std_logic_vector(7 downto 0);
		-- active low control line indicating data memory is being read
		-- active only during 2nd half of the clock in the 2nd cycle
		Data_Read: out std_logic;
		-- active low control line indicating data memory is being written
		-- active only during 2nd half of the clock in the 2nd cycle
		Data_Write: out std_logic
        );
end entity;

--
-- Program Memory Access entity declaration
--
entity Program_Memory_Access is
	port(
		-- control signal inputs
		-- whether or not to load the current program counter value
		Load: in std_logic;
		-- what to add to either the current program counter value or 0
		-- 000 select immediate
		-- 001 select 1 (advance to next instruction)
		-- 010 select 2 (skipping next instruction)
		-- 011 select 3 (skipping next two instructions)
		-- 100 select Z_register and PC <- z
		-- 101 select Data_Data_Bus
		-- 110 select 0
		Program_Address_Source_Select: out std_logic_vector(2 downto 0);
		
		-- program counter: contains address of current instruction, updated by entity
		Program_Counter: buffer std_logic_vector(NUM_ADDRESS_BITS downto 0);
		
		-- outputs
		-- program address bus
		Program_Address_Bus: out std_logic_vector(NUM_ADDRESS_BITS - 1 downto 0)
        );
end entity;

--
-- Control Unit entity declaration
--
entity Control_Unit is
	port(
		-- inputs
		-- program data bus
		Program_Data_Bus: in opcode_word;
		-- instruction register
		-- IR: in opcode_word;
		
		-- control signal outputs
		
		-- to ALU
		-- selects ALU operation to perform
		ALU_result_select: out std_logic_vector(1 downto 0);
		-- selects what to update lowest bit of shifter / rotater with
		Shifter_low_bit_select: out std_logic_vector(1 downto 0);
		-- when ALU_result_select indicates F Block operation
		--	F Block inputs to mux for F Block operations
		-- when ALU_result_select indicates Adder/Subtractor operation
		--	lowest bit of this vector is the Subtract control signal (all others
		-- 	bits are ignored)
		-- when ALU_result_select indicates Shifter/Rotater operation
		-- 	bits 3 downto 1 selects high bit value
		--	bit 0 selects value for middle bits
		Shift_mid_high_bits_FBlock_Subtract: out std_logic_vector(3 downto 0);
		-- flag mask indicating which flag values to update after ALU operation
		Status_Register_Mask: out std_logic_vector(7 downto 0);
		-- bit in register to set transfer bit in status register to or vice versa
		T_bit: out std_logic_vector(2 downto 0);
		-- indicates that a bit in a register should be updated to transfer bit
		-- or vice versa
		Transfer: out std_logic;
		-- indicating whether performing ALU operation involving current carry / 
		-- borrow bit
		ALU_op_with_carry: out std_logic;
		-- chooses value of second operand for addition / subtraction
		AddSub_Op_Select: out std_logic_vector(1 downto 0);
		-- first operand
		OperandA: in std_logic_vector(NUM_DATA_BITS - 1 downto 0);
		-- second operand
		OperandB: in std_logic_vector(NUM_DATA_BITS - 1 downto 0);
		
		-- to Data_Memory_Access
		-- selects address source
		Address_Source_Select: out std_logic_vector(2 downto 0);
		-- selects offset source
		-- selects offset source
		--  00 select 0
		--  01 select +1
		--  10 select -1
		--  11 select q (for Y and Z registers with q unsigned offset)
		Offset_Source_Select: out std_logic_vector(3 downto 0);
		-- indicates whether or not pre/post-increment/decrement was 
		-- part of instruction
		Pre_or_Post_Select: out std_logic;
		
		-- to Program_Memory_Access
		-- whether or not to load the current program counter value
		Load: out std_logic;
		-- what to add to either the current program counter value or 0
		-- 000 select immediate
		-- 001 select 1 (advance to next instruction)
		-- 010 select 2 (skipping next instruction)
		-- 011 select 3 (skipping next two instructions)
		-- 100 select Z_register and PC <- z
		-- 101 select Data_Data_Bus
		-- 110 select 0
		Program_Address_Source_Select: out std_logic_vector(2 downto 0);
		
		-- to Registers
		-- selects what value to load into general-purpose registers or IO space
		Register_IO_val_select: out std_logic_vector(1 downto 0)
		-- enable writing to general purpose registers
		Register_Write_Enable: out std_logic;
		-- enable writing to IO registers
		IO_Write_Enable: out std_logic
		-- select register
		Register_select: out std_logic_vector(log2(NUM_REGISTERS) - 1 downto 0);
		-- indicates nibbles of a register should be swapped
		Swap: out std_logic
    );
end entity;

architecture control_arch of Control_Unit
	
	component ALU port (
		-- system clk
		clk: in std_logic;
		
		-- control signal inputs
		-- selects ALU operation to perform
		-- 	00 F Block operation
		-- 	01 Adder/Subtractor operation
		-- 	10 Shifter/Rotater operation
		ALU_result_select: in std_logic_vector(1 downto 0);
		-- selects what to update lowest bit of shifter / rotater with
		--	00 select current highest bit
		-- 	01 select 0
		-- 	10 select current bit 1
		-- 	11 select current carry flag bit
		Shifter_low_bit_select: in std_logic_vector(1 downto 0);
		-- when ALU_result_select indicates Adder/Subtractor operation
		--	lowest bit of this vector is the Subtract control signal (all others
		-- 	bits are ignored)
		-- when ALU_result_select indicates Shifter/Rotater operation
		--	bit 0 selects value for middle bits:
		--		0 select bit to the immediate right
		--		1 select bit to the immediate left
		Shifter_middle_bits_select: in std_logic;
		-- 	bits 3 downto 1 selects high bit value:
		--		000 select current second highest bit
		--		001	select current highest bit
		--		010	select current lowest bit
		--		011	select 0
		--		100	select current carry flag bit
		Shifter_high_bit_select: in std_logic_vector(2 downto 0);
		-- when ALU_result_select indicates F Block operation
		--	F Block inputs to mux for F Block operations
		F_Block_Select: in std_logic_vector(3 downto 0);
		-- indicates whether an addition or subtraction operation should occur
		Subtract: in std_logic;
		-- indicating whether performing ALU operation involving current carry / 
		-- borrow bit
		ALU_op_with_carry: in std_logic;
		-- chooses value of second operand for addition / subtraction
		-- 	00 select 0
		--  01 select 1
		--  10 select OperandB
		AddSub_Op_Select: in std_logic_vector(1 downto 0);
		-- flag mask indicating which flag values to update after ALU operation
		Status_Register_Mask: in std_logic_vector(7 downto 0);
		
		-- other inputs
		-- first operand
		OperandA: in std_logic_vector(NUM_DATA_BITS - 1 downto 0);
		-- second operand
		OperandB: in std_logic_vector(NUM_DATA_BITS - 1 downto 0);
		
		-- outputs
		-- ALU result (from F Block, Adder/Subtractor, or Shifter/Rotater)
		Result: out std_logic_vector(NUM_DATA_BITS - 1 downto 0);
		-- updated status register
		Status_Register: out std_logic_vector(NUM_DATA_BITS - 1 downto 0)
		);
	end component;
	
begin
	process (clk)
	begin
		if rising_edge(clk) then
			if std_match(Program_Data_Bus, ADC_instruction) then
				ALU_operation: ALU port map (
					clk => clk,
					ALU_result_select => Adder_Subtractor_Operation,
					Shifter_low_bit_select => ,
					Shifter_middle_bits_select => ,
					Shifter_high_bit_select => ,
					F_Block_Select => ,
					Subtract => Subtraction,
					ALU_op_with_carry => '1',
					AddSub_Op_Select => AddSub_Op_Select_OperandB,
					Status_Register_Mask => ,
					OperandA => ,
					OperandB => ,
					Result => Result,
					Status_Register =>
				);
			end if;
			
			if std_match(Program_Data_Bus, ADD_instruction) then
			
			end if;
			
			if std_match(Program_Data_Bus, ADC_instruction) then
			
			end if;
			
			if std_match(Program_Data_Bus, ADIW_instruction) then
			
			end if;

			if std_match(Program_Data_Bus, AND_instruction) then
			
			end if;
			
			if std_match(Program_Data_Bus, ANDI_instruction) then
			
			end if;
			
			if std_match(Program_Data_Bus, ASR_instruction) then
			
			end if;

			if std_match(Program_Data_Bus, BCLR_instruction) then
			
			end if;
			
			if std_match(Program_Data_Bus, BSET_instruction) then
			
			end if;
			
			if std_match(Program_Data_Bus, BST_instruction) then
			
			end if;
			
			if std_match(Program_Data_Bus, COM_instruction) then
			
			end if;
			
			if std_match(Program_Data_Bus, CP_instruction) then
			
			end if;
			
			if std_match(Program_Data_Bus, CPC_instruction) then
			
			end if;
			
			if std_match(Program_Data_Bus, CPI_instruction) then
			
			end if;
			
			if std_match(Program_Data_Bus, DEC_instruction) then
			
			end if;
		end if;
	end process;
end architecture;