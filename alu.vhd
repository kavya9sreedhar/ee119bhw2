----------------------------------------------------------------------------
--  Arithmetic Logic Unit (ALU)
--
--	This file contains an implementation of the ALU for an 8-bit
--  AVR architecture. The ALU consists of an 8-bit adder/subtractor, 
-- 	F-Block, shifter, and rotater.
--
--  Revision History:
--	28 Jan 19	Kavya Sreedhar & Dan Xu 	Initial revision
--	30 Jan 19	Kavya Sreedhar & Dan Xu		Updated comments
--	1  Feb 19	Kavya Sreedhar & Dan Xu		Added file header
--	2  Feb 19	Kavya Sreedhar & Dan Xu		Hooked up flag generator
----------------------------------------------------------------------------

--
-- declaration of libraries used
--
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;

-- contains constants used for all CPU units
library CPU_CONSTANTS;
use CPU_CONSTANTS.all;

-- contains ALU specific constants
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
		-- flag source controls indicating what and how to update after ALU operation
		-- Descriptions of the selection line meanings ca be found in the constants.vhd file.
        TBit_Select             : in std_logic_vector(DATA_BITS_LOG-1 downto 0);
        Interrupt_Flag_Sel      : in std_logic_vector(NUM_I_FLAG_BITS-1 downto 0);
        Transfer_Flag_Sel       : in std_logic_vector(NUM_T_FLAG_BITS-1 downto 0);
        Half_Carry_Flag_Sel     : in std_logic_vector(NUM_H_FLAG_BITS-1 downto 0);
        Corrected_Sign_Flag_Sel : in std_logic_vector(NUM_S_FLAG_BITS-1 downto 0);
        Signed_OF_Flag_Sel      : in std_logic_vector(NUM_V_FLAG_BITS-1 downto 0);
        Neg_Flag_Sel            : in std_logic_vector(NUM_N_FLAG_BITS-1 downto 0);
        Zero_Flag_Sel           : in std_logic_vector(NUM_Z_FLAG_BITS-1 downto 0);
        Carry_Flag_Sel          : in std_logic_vector(NUM_C_FLAG_BITS-1 downto 0);
		
		-- other inputs
		-- first operand
		OperandA: in std_logic_vector(NUM_DATA_BITS - 1 downto 0);
		-- second operand
		OperandB: in std_logic_vector(NUM_DATA_BITS - 1 downto 0);
		-- The old flags
		old_flags               : in std_logic_vector(N_FLAGS-1 downto 0);
		
		-- outputs
		-- ALU result (from F Block, Adder/Subtractor, or Shifter/Rotater)
		Result: out std_logic_vector(NUM_DATA_BITS - 1 downto 0);
		-- updated status register
		new_flags               : out std_logic_vector(N_FLAGS-1 downto 0)
        );
end entity;

architecture ALU_arch of ALU is
	-- stores result from shift and rotate operations in ALU
	signal shift_rotate_result: 	std_logic_vector(NUM_DATA_BITS - 1 downto 0);
	-- stores result from F block operations in ALU
	signal F_block_result: 			std_logic_vector(NUM_DATA_BITS - 1 downto 0);
	-- stores result from addition and subtraction operations in ALU
	signal adder_subtractor_result: std_logic_vector(NUM_DATA_BITS - 1 downto 0);
	-- stores intermediate carry's from intermediate bit calculations
	signal carry_outs: 				std_logic_vector(NUM_DATA_BITS - 1 downto 0);
	-- selects operand1 for addition / subtraction calculation
	signal Operand1: 				std_logic_vector(NUM_DATA_BITS - 1 downto 0);
	-- selects operand2 for addition / subtraction calculation
	signal Operand2: 				std_logic_vector(NUM_DATA_BITS - 1 downto 0);
	-- indicates whether shift/rotate, F_block, or adder/subtracter calculation
	-- should be taken for output from ALU
	signal ALU_result:				std_logic_vector(NUM_DATA_BITS - 1 downto 0);
	-- The signed OF flag
	signal Signed_OF:               std_logic;
begin

	-- F_BLOCK CALCULATION
	
	-- F Block calculation with input control signals
	get_F_block_result: for i in range 0 to NUM_DATA_BITS - 1 generate
		F_block_result(i) <= 
			-- when F Block output should be 0
			'0' 							
				when F_Block_Select = F_Block_Select_0 else
			-- when F Block output should be the nor of the two operands
			OperandA(i) nor OperandB 		
				when F_Block_Select = F_Block_Select_nor else
			-- when F Block output should be the not of the first operand
			not OperandA(i) 				
				when F_Block_Select = F_Block_Select_not_A else
			-- when F Block output should be the not of the second operand
			not OperandB(i) 				
				when F_Block_Select = F_Block_Select_not_B else
			-- when F Block output should be the xor of the two operands
			OperandA(i) xor OperandB(i) 	
				when F_Block_Select = F_Block_Select_xor else
			-- when F Block output should be the nand of the two operands
			OperandA(i) nand OperandB(i) 	
				when F_Block_Select = F_Block_Select_nand else
			-- when F Block output should be the and of the two operands
			OperandA(i) and OperandB(i) 	
				when F_Block_Select = F_Block_Select_and else
			-- when F Block output should be the xnor of the two operands
			OperandA(i) xnor OperandB(i) 	
				when F_Block_Select = F_Block_Select_xnor else
			-- when F Block output should be the or of the two operands
			OperandA(i) or OperandB(i) 		
				when F_Block_Select = F_Block_Select_or else
			-- when F Block output should be 1
			'1' 							
				when F_Block_Select = F_Block_Select_1;
	end generate get_F_block_result;
	
	-- ADDITION / SUBTRACTION CALCULATION
	
	-- select first operand for addition / subtraction calculation
	Operand1 <= OperandA 	when AddSub_Op_1_Select = AddSub_Op_1_Select_OperandA else
				"11111111" 	when AddSub_Op_1_Select = AddSub_Op_1_Select_FF else
				"00000000" 	when AddSub_Op_1_Select = AddSub_Op_1_Select_0;
				
	-- select second operand for addition / subtraction calculation
	Operand2 <= "00000000" 	when AddSub_Op_2_Select = AddSub_Op_2_Select_0 else
				"00000001" 	when AddSub_Op_2_Select = AddSub_Op_2_Select_1 else
				OperandB;
	-- use carry / borrow if this is an addition / subtraction operation
	-- with carry / borrow
	carry_borrow_for_ALU_op <= 	'0' when ALU_op_with_carry = '0' else
								current_status_register(0);
							
	-- bit 0 for addition / subtraction result
	adder_subtractor_result(0) <= 
		Operand1(0) xor (Operand2(0) xor Subtract) xor 
		(Subtract xor carry_borrow_for_ALU_op);
	-- carry out from bit 0 addition / subtraction
	carry_outs(0) <= 	(Operand1(0) and (Operand2(0) xor Subtract)) or 
						(Subtract and (Operand1(0) xor (Operand2(0) xor Subtract)));
	
	-- calculate bits 1 through n - 1 bits for addition / subtraction result
	get_adder_subtractor_bits: for i in 1 to NUM_DATA_BITS - 1 generate
		adder_subtractor_result(i) <=
			Operand1(i) xor (Operand2(i) xor Subtract) xor carry_outs(i - 1);
		-- calculate carry out from bits addition / subtraction for bits 1
		-- through n - 1
		carry_outs(i) <= 
			(Operand1(i) and (Operand2(i) xor Subtract)) or 
			(carry_outs(i - 1) and (Operand1(i) xor (Operand2(i) xor Subtract)));
	end generate get_adder_subtractor_bits;

	-- SHIFT / ROTATE CALCULATION
	
	-- bit 0 for shift / rotate calculation depending on which instruction
	shift_rotate_result(0) <= 	
		OperandA(NUM_DATA_BITS - 1) 
			when Shifter_low_bit_select = Shifter_low_bit_highest_bit else
		'0' 						
			when Shifter_low_bit_select = Shifter_low_bit_0 else
		OperandA(1) 				
			when Shifter_low_bit_select = Shifter_low_bit_bit_1 else
		current_status_register(0) 	
			when Shifter_low_bit_select = Shifter_low_bit_carry;
	
	-- bits 1 through n - 1 for shift / rotate calculation depending on which
	-- instruction
	get_shift_rotate_middle_bits: for i in range 1 to NUM_DATA_BITS - 2 generate
		shift_rotate_result(i) <= 	
			OperandA(i - 1) when Shifter_middle_bits_select = 
				Shifter_middle_bits_select_immediate_right else
			OperandA(i + 1) when Shifter_middle_bits_select = 
				Shifter_middle_bits_select_immediate_left;
	end generate get_shift_rotate_middle_bits;
	
	-- highest bit for shift / rotate calculation depending on which
	-- instruction
	shift_rotate_result(NUM_DATA_BITS - 1) <=
		OperandA(NUM_DATA_BITS - 2) when Shifter_high_bit_select =
			Shifter_high_bit_select_second_highest_bit else
		OperandA(NUM_DATA_BITS - 1) when Shifter_high_bit_select = 
			Shifter_high_bit_select_highest_bit else
		OperandA(0) 				when Shifter_high_bit_select =
			Shifter_high_bit_select_lowest_bit else
		'0' 						when Shifter_high_bit_select =
			Shifter_high_bit_select_0 else
		current_status_register(0);
	
	-- select whether result from ALU should be the F Block result, the addition / 
	-- subtraction result, or the shift / rotate result depending on which
	-- instruction
	ALU_result <= 
		F_block_result 			when ALU_result_select = "00" else
		adder_subtractor_result when ALU_result_select = "01" else
		shift_rotate_result 	when ALU_result_select = "10";
		
	Result <= ALU_result;

	-- Calculate signed overflow
	Signed_OF <= carry_outs(NUM_DATA_BITS-1) xor carry_outs(NUM_DATA_BITS-2);

	-- Flag generation
	UUT: entity work.FlagGenerator(standard)
	port map (
		-- Connect up the old flags
		old_flags               => old_flags,
		-- Connect the new generated flags
		new_flags               => new_flags,

		-- Connect up ALU flags
		ALU_Input_LSB           => OperandA(0),
		ALU_OF                  => Signed_OF,
		ALU_CF                  => carry_outs(NUM_DATA_BITS-1),
		ALU_HF                  => carry_outs(NUM_DATA_BITS/2 - 1),

		-- Connect up the ALU output
		ALU_Output              => ALU_result,

		-- Connect up all of the control signals for what to use for each flag.
		TBit_Select             => TBit_Select,
		Interrupt_Flag_Sel      => Interrupt_Flag_Sel,
		Transfer_Flag_Sel       => Transfer_Flag_Sel,
		Half_Carry_Flag_Sel     => Half_Carry_Flag_Sel,
		Corrected_Sign_Flag_Sel => Corrected_Sign_Flag_Sel,
		Signed_OF_Flag_Sel      => Signed_OF_Flag_Sel,
		Neg_Flag_Sel            => Neg_Flag_Sel,
		Zero_Flag_Sel           => Zero_Flag_Sel,
		Carry_Flag_Sel          => Carry_Flag_Sel

	);
	
end architecture;