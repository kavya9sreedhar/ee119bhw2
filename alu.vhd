--
-- declaration of libraries used
--
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;

library CPU_CONSTANTS;
use CPU_CONSTANTS.all;

library ALU_CONSTANTS;
use ALU_CONSTANTS.all;

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
		AddSub_Op_1_Select: in std_logic;
		-- chooses value of second operand for addition / subtraction
		-- 	00 select 0
		--  01 select 1
		--  10 select OperandB
		AddSub_Op_2_Select: in std_logic_vector(1 downto 0);
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
	
	Operand1 <= OperandA 	when AddSub_Op_1_Select = '0' else
				"11111111" when AddSub_Op_1_Select = '1';
				
	Operand2 <= "00000000" 	when AddSub_Op_2_Select = "00" else
				"00000001" 	when AddSub_Op_2_Select = "01" else
				OperandB;
				
	carry_borrow_for_ALU_op <= 	'0' when ALU_op_with_carry = '0' else
								current_status_register(0);
								
	adder_subtractor_result(0) <= 
		Operand1(0) xor (Operand2(0) xor Subtract) xor 
		(Subtract xor carry_borrow_for_ALU_op);
	carry_outs(0) <= Subtract;
	
	get_adder_subtractor_bits: for i in range 1 to NUM_DATA_BITS - 1 generate
		adder_subtractor_result(i) <=
			Operand1(i) xor (Operand2(i) xor Subtract) xor carry_outs(i - 1);
		carry_outs(i) <= 
			(Operand1(i) and (Operand2(i) xor Subtract)) or 
			(carry_outs(i - 1) and (Operand1(i) xor (Operand2(i) xor Subtract)));
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