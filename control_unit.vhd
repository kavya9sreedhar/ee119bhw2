----------------------------------------------------------------------------
--  AVR Control Unit
--
--	This file contains an implementation of the control for an 8-bit
--  AVR architecture. This implementation consists of a FSM and instruction
--  decoder to process instructions from the program data bus.
--
--  Revision History:
--	28 Jan 19	Kavya Sreedhar & Dan Xu 	Initial Revision
--  1  Feb 19	Kavya Sreedhar & Dan Xu		Updated revision history
----------------------------------------------------------------------------

-- declaration of libraries used
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;

-- import library containing all instructions supported with this AVR implementation
library opcodes;
use opcodes.opcodes.all;

-- import library containing all general AVR CPU constants
library CPU_CONSTANTS;
use CPU_CONSTANTS.all;

-- import library containing constants specific to the ALU
library ALU_CONSTANTS;
use ALU_CONSTANTS.all;

--
-- Control Unit entity declaration
--
entity Control_Unit is
	port(
		-- inputs
		-- program data bus
		Program_Data_Bus: in opcode_word
		-- instruction register
		-- IR: in opcode_word;
		
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
	end component;
	
	signal Result: std_logic_vector(NUM_DATA_BITS - 1 downto 0);
	signal second_clock_flag: std_logic;
	
begin
	process (clk)
	begin
		
		if rising_edge(clk) then
			if std_match(Program_Data_Bus, OpADC) then
				ALU_operation: ALU port map (
					clk => clk,
					ALU_result_select => Adder_Subtractor_Operation,
					-- values do not matter
					Shifter_low_bit_select => ,
					Shifter_middle_bits_select => ,
					Shifter_high_bit_select => ,
					F_Block_Select => ,
					Subtract => Addition,
					ALU_op_with_carry => '1',
					AddSub_Op_1_Select => AddSub_Op_1_Select_OperandA,
					AddSub_Op_2_Select => AddSub_Op_2_Select_OperandB,
					Status_Register_Mask => ,
					OperandA => ,
					OperandB => ,
					Result => Result,
					Status_Register =>
				);
			end if;
			
			if std_match(Program_Data_Bus, OpADD) then
				ALU_operation: ALU port map (
					clk => clk,
					ALU_result_select => Adder_Subtractor_Operation,
					-- values do not matter
					Shifter_low_bit_select => ,
					Shifter_middle_bits_select => ,
					Shifter_high_bit_select => ,
					F_Block_Select => ,
					Subtract => Addition,
					ALU_op_with_carry => '0',
					AddSub_Op_1_Select => AddSub_Op_1_Select_OperandA,
					AddSub_Op_2_Select => AddSub_Op_2_Select_OperandB,
					Status_Register_Mask => ,
					OperandA => ,
					OperandB => ,
					Result => Result,
					Status_Register =>
				);
			end if;
			
			-- 2 clocks
			if std_match(Program_Data_Bus, OpADIW) then
				if second_clock_flag = '0' then
					ALU_operation: ALU port map (
					clk => clk,
					ALU_result_select => Adder_Subtractor_Operation,
					-- values do not matter
					Shifter_low_bit_select => ,
					Shifter_middle_bits_select => ,
					Shifter_high_bit_select => ,
					F_Block_Select => ,
					Subtract => Addition,
					ALU_op_with_carry => '0',
					AddSub_Op_1_Select => AddSub_Op_1_Select_OperandA,
					AddSub_Op_2_Select => AddSub_Op_2_Select_OperandB,
					Status_Register_Mask => ,
					-- Rd
					OperandA => ,
					-- K
					OperandB => ,
					Result => Result,
					Status_Register =>
					);
				
					second_clock_flag = '1';
					
				else
					ALU_operation: ALU port map (
					clk => clk,
					ALU_result_select => Adder_Subtractor_Operation,
					-- values do not matter
					Shifter_low_bit_select => ,
					Shifter_middle_bits_select => ,
					Shifter_high_bit_select => ,
					F_Block_Select => ,
					Subtract => Addition,
					ALU_op_with_carry => '0',
					AddSub_Op_1_Select => AddSub_Op_1_Select_OperandA,
					AddSub_Op_2_Select => AddSub_Op_2_Select_0,
					Status_Register_Mask => ,
					--Rd + 1
					OperandA => ,
					OperandB => ,
					Result => Result,
					Status_Register =>
					);
					
					second_clock_flag = '0';
				end if;
			end if;

			if std_match(Program_Data_Bus, OpAND) then
				ALU_operation: ALU port map (
					clk => clk,
					ALU_result_select => F_Block_Operation,
					-- values do not matter
					Shifter_low_bit_select => ,
					Shifter_middle_bits_select => ,
					Shifter_high_bit_select => ,
					F_Block_Select => F_Block_Select_and,
					Subtract => ,
					ALU_op_with_carry => ,
					AddSub_Op_1_Select => ,
					AddSub_Op_2_Select => ,
					Status_Register_Mask => ,
					OperandA => ,
					OperandB => ,
					Result => Result,
					Status_Register =>
				);
			end if;
			
			if std_match(Program_Data_Bus, OpANDI) then
				ALU_operation: ALU port map (
					clk => clk,
					ALU_result_select => F_Block_Operation,
					-- values do not matter
					Shifter_low_bit_select => ,
					Shifter_middle_bits_select => ,
					Shifter_high_bit_select => ,
					F_Block_Select => F_Block_Select_and,
					Subtract => ,
					ALU_op_with_carry => ,
					AddSub_Op_1_Select => ,
					AddSub_Op_2_Select => ,
					Status_Register_Mask => ,
					OperandA => ,
					OperandB => ,
					Result => Result,
					Status_Register =>
				);
			end if;
			
			if std_match(Program_Data_Bus, OpASR) then
				ALU_operation: ALU port map (
					clk => clk,
					ALU_result_select => Shifter_Rotater_Operation,
					Shifter_low_bit_select => Shifter_low_bit_bit_1,
					Shifter_middle_bits_select => 
						Shifter_middle_bits_select_immediate_left,
					Shifter_high_bit_select => Shifter_high_bit_select_highest_bit,
					F_Block_Select => ,
					Subtract => ,
					ALU_op_with_carry => ,
					AddSub_Op_1_Select => ,
					AddSub_Op_2_Select => ,
					Status_Register_Mask => ,
					OperandA => ,
					OperandB => ,
					Result => Result,
					Status_Register =>
				);
			end if;

			if std_match(Program_Data_Bus, OpBCLR) then
				ALU_operation: ALU port map (
					clk => clk,
					ALU_result_select => ,
					Shifter_low_bit_select => ,
					Shifter_middle_bits_select => ,
					Shifter_high_bit_select => ,
					F_Block_Select => ,
					Subtract => ,
					ALU_op_with_carry => ,
					AddSub_Op_1_Select => ,
					AddSub_Op_2_Select => ,
					Status_Register_Mask => ,
					OperandA => ,
					OperandB => ,
					Result => Result,
					Status_Register =>
				);
			end if;
			
			if std_match(Program_Data_Bus, OpBLD) then
				ALU_operation: ALU port map (
					clk => clk,
					ALU_result_select => ,
					Shifter_low_bit_select => ,
					Shifter_middle_bits_select => ,
					Shifter_high_bit_select => ,
					F_Block_Select => ,
					Subtract => ,
					ALU_op_with_carry => ,
					AddSub_Op_1_Select => ,
					AddSub_Op_2_Select => ,
					Status_Register_Mask => ,
					OperandA => ,
					OperandB => ,
					Result => Result,
					Status_Register =>
				);
			end if;
			
			if std_match(Program_Data_Bus, OpBSET) then
				ALU_operation: ALU port map (
					clk => clk,
					ALU_result_select => ,
					Shifter_low_bit_select => ,
					Shifter_middle_bits_select => ,
					Shifter_high_bit_select => ,
					F_Block_Select => ,
					Subtract => ,
					ALU_op_with_carry => ,
					AddSub_Op_1_Select => ,
					AddSub_Op_2_Select => ,
					Status_Register_Mask => ,
					OperandA => ,
					OperandB => ,
					Result => Result,
					Status_Register =>
				);
			end if;
			
			if std_match(Program_Data_Bus, OpBST) then
				ALU_operation: ALU port map (
					clk => clk,
					ALU_result_select => ,
					Shifter_low_bit_select => ,
					Shifter_middle_bits_select => ,
					Shifter_high_bit_select => ,
					F_Block_Select => ,
					Subtract => ,
					ALU_op_with_carry => ,
					AddSub_Op_1_Select => ,
					AddSub_Op_2_Select => ,
					Status_Register_Mask => ,
					OperandA => ,
					OperandB => ,
					Result => Result,
					Status_Register =>
				);
			end if;
			
			if std_match(Program_Data_Bus, OpCOM) then
				ALU_operation: ALU port map (
					clk => clk,
					ALU_result_select => Adder_Subtractor_Operation,
					Shifter_low_bit_select => ,
					Shifter_middle_bits_select => ,
					Shifter_high_bit_select => ,
					F_Block_Select => ,
					Subtract => Subtraction,
					ALU_op_with_carry => '0',
					AddSub_Op_1_Select => AddSub_Op_1_Select_FF,
					AddSub_Op_2_Select => AddSub_Op_2_Select_OperandB,
					Status_Register_Mask => ,
					OperandA => ,
					OperandB => ,
					Result => Result,
					Status_Register =>
				);
			end if;
			
			if std_match(Program_Data_Bus, OpCP) then
				ALU_operation: ALU port map (
					clk => clk,
					ALU_result_select => Adder_Subtractor_Operation,
					Shifter_low_bit_select => ,
					Shifter_middle_bits_select => ,
					Shifter_high_bit_select => ,
					F_Block_Select => ,
					Subtract => Subtraction,
					ALU_op_with_carry => '0',
					AddSub_Op_1_Select => AddSub_Op_1_Select_OperandA,
					AddSub_Op_2_Select => AddSub_Op_2_Select_OperandB,
					Status_Register_Mask => ,
					OperandA => ,
					OperandB => ,
					Result => Result,
					Status_Register =>
				);
				-- do not store result back in registers
			end if;
			
			if std_match(Program_Data_Bus, OpCPC) then
				ALU_operation: ALU port map (
					clk => clk,
					ALU_result_select => Adder_Subtractor_Operation,
					Shifter_low_bit_select => ,
					Shifter_middle_bits_select => ,
					Shifter_high_bit_select => ,
					F_Block_Select => ,
					Subtract => Subtraction,
					ALU_op_with_carry => '1',
					AddSub_Op_1_Select => AddSub_Op_1_Select_OperandA,
					AddSub_Op_2_Select => AddSub_Op_2_Select_OperandB,
					Status_Register_Mask => ,
					OperandA => ,
					OperandB => ,
					Result => Result,
					Status_Register =>
				);
				-- do not store result back in registers
			end if;
			
			if std_match(Program_Data_Bus, OpCPI) then
				ALU_operation: ALU port map (
					clk => clk,
					ALU_result_select => Adder_Subtractor_Operation,
					Shifter_low_bit_select => ,
					Shifter_middle_bits_select => ,
					Shifter_high_bit_select => ,
					F_Block_Select => ,
					Subtract => Subtraction,
					ALU_op_with_carry => '0',
					AddSub_Op_1_Select => AddSub_Op_1_Select_OperandA,
					AddSub_Op_2_Select => AddSub_Op_2_Select_OperandB,
					Status_Register_Mask => ,
					OperandA => ,
					OperandB => ,
					Result => Result,
					Status_Register =>
				);
				-- do not store result back in registers
			end if;
			
			if std_match(Program_Data_Bus, OpDEC) then
				ALU_operation: ALU port map (
					clk => clk,
					ALU_result_select => Adder_Subtractor_Operation,
					Shifter_low_bit_select => ,
					Shifter_middle_bits_select => ,
					Shifter_high_bit_select => ,
					F_Block_Select => ,
					Subtract => Subtraction,
					ALU_op_with_carry => '0',
					AddSub_Op_1_Select => AddSub_Op_1_Select_OperandA,
					AddSub_Op_2_Select => AddSub_Op_2_Select_1,
					Status_Register_Mask => ,
					OperandA => ,
					OperandB => ,
					Result => Result,
					Status_Register =>
				);
			end if;
			
			if std_match(Program_Data_Bus, OpEOR) then
				ALU_operation: ALU port map (
					clk => clk,
					ALU_result_select => F_Block_Operation,
					Shifter_low_bit_select => ,
					Shifter_middle_bits_select => ,
					Shifter_high_bit_select => ,
					F_Block_Select => F_Block_Select_xor,
					Subtract => ,
					ALU_op_with_carry => ,
					AddSub_Op_1_Select => ,
					AddSub_Op_2_Select => ,
					Status_Register_Mask => ,
					OperandA => ,
					OperandB => ,
					Result => Result,
					Status_Register =>
				);
			end if;
			
			if std_match(Program_Data_Bus, OpINC) then
				ALU_operation: ALU port map (
					clk => clk,
					ALU_result_select => Adder_Subtractor_Operation,
					Shifter_low_bit_select => ,
					Shifter_middle_bits_select => ,
					Shifter_high_bit_select => ,
					F_Block_Select => ,
					Subtract => Addition,
					ALU_op_with_carry => '0',
					AddSub_Op_1_Select => AddSub_Op_1_Select_OperandA,
					AddSub_Op_2_Select => AddSub_Op_2_Select_1,
					Status_Register_Mask => ,
					OperandA => ,
					OperandB => ,
					Result => Result,
					Status_Register =>
				);
			end if;
			
			if std_match(Program_Data_Bus, OpLSR) then
				ALU_operation: ALU port map (
					clk => clk,
					ALU_result_select => Shifter_Rotater_Operation,
					Shifter_low_bit_select => Shifter_low_bit_bit_1,
					Shifter_middle_bits_select => 
						Shifter_middle_bits_select_immediate_left,
					Shifter_high_bit_select => Shifter_high_bit_select_0,
					F_Block_Select => ,
					Subtract => ,
					ALU_op_with_carry => ,
					AddSub_Op_1_Select => ,
					AddSub_Op_2_Select => ,
					Status_Register_Mask => ,
					OperandA => ,
					OperandB => ,
					Result => Result,
					Status_Register =>
				);
			end if;
			
			if std_match(Program_Data_Bus, OpNEG) then
				ALU_operation: ALU port map (
					clk => clk,
					ALU_result_select => Adder_Subtractor_Operation,
					Shifter_low_bit_select => ,
					Shifter_middle_bits_select => ,
					Shifter_high_bit_select => ,
					F_Block_Select => ,
					Subtract => Subtraction,
					ALU_op_with_carry => '0',
					AddSub_Op_1_Select => AddSub_Op_1_Select_0,
					AddSub_Op_2_Select => AddSub_Op_2_Select_OperandB,
					Status_Register_Mask => ,
					OperandA => ,
					OperandB => ,
					Result => Result,
					Status_Register =>
				);
			end if;
			
			if std_match(Program_Data_Bus, OpOR) then
				ALU_operation: ALU port map (
					clk => clk,
					ALU_result_select => F_Block_Select,
					Shifter_low_bit_select => ,
					Shifter_middle_bits_select => ,
					Shifter_high_bit_select => ,
					F_Block_Select => F_Block_Select_or,
					Subtract => ,
					ALU_op_with_carry => ,
					AddSub_Op_1_Select => ,
					AddSub_Op_2_Select => ,
					Status_Register_Mask => ,
					OperandA => ,
					OperandB => ,
					Result => Result,
					Status_Register =>
				);
			end if;
			
			if std_match(Program_Data_Bus, OpOR) then
				ALU_operation: ALU port map (
					clk => clk,
					ALU_result_select => F_Block_Select,
					Shifter_low_bit_select => ,
					Shifter_middle_bits_select => ,
					Shifter_high_bit_select => ,
					F_Block_Select => F_Block_Select_or,
					Subtract => ,
					ALU_op_with_carry => ,
					AddSub_Op_1_Select => ,
					AddSub_Op_2_Select => ,
					Status_Register_Mask => ,
					OperandA => ,
					OperandB => ,
					Result => Result,
					Status_Register =>
				);
			end if;
			
			if std_match(Program_Data_Bus, OpROR) then
				ALU_operation: ALU port map (
					clk => clk,
					ALU_result_select => Shifter_Rotater_Operation,
					Shifter_low_bit_select => Shifter_low_bit_bit_1,
					Shifter_middle_bits_select => 	
						Shifter_middle_bits_select_immediate_left,
					Shifter_high_bit_select => Shifter_high_bit_select_lowest_bit,
					F_Block_Select => ,
					Subtract => ,
					ALU_op_with_carry => ,
					AddSub_Op_1_Select => ,
					AddSub_Op_2_Select => ,
					Status_Register_Mask => ,
					OperandA => ,
					OperandB => ,
					Result => Result,
					Status_Register =>
				);
			end if;
			
			if std_match(Program_Data_Bus, OpSBC) then
				ALU_operation: ALU port map (
					clk => clk,
					ALU_result_select => Adder_Subtractor_Operation,
					Shifter_low_bit_select => ,
					Shifter_middle_bits_select => ,
					Shifter_high_bit_select => ,
					F_Block_Select => ,
					Subtract => Subtraction,
					ALU_op_with_carry => '1',
					AddSub_Op_1_Select => AddSub_Op_1_Select_OperandA,
					AddSub_Op_2_Select => AddSub_Op_2_Select_OperandB,
					Status_Register_Mask => ,
					OperandA => ,
					OperandB => ,
					Result => Result,
					Status_Register =>
				);
			end if;
			
			if std_match(Program_Data_Bus, OpSBCI) then
				ALU_operation: ALU port map (
					clk => clk,
					ALU_result_select => Adder_Subtractor_Operation,
					Shifter_low_bit_select => ,
					Shifter_middle_bits_select => ,
					Shifter_high_bit_select => ,
					F_Block_Select => ,
					Subtract => Subtraction,
					ALU_op_with_carry => '1',
					AddSub_Op_1_Select => AddSub_Op_1_Select_OperandA,
					AddSub_Op_2_Select => AddSub_Op_2_Select_OperandB,
					Status_Register_Mask => ,
					OperandA => ,
					OperandB => ,
					Result => Result,
					Status_Register =>
				);
			end if;
			
			-- 2 clocks
			if std_match(Program_Data_Bus, OpSBIW) then
				if second_clock_flag = '0' then
					ALU_operation: ALU port map (
					clk => clk,
					ALU_result_select => Adder_Subtractor_Operation,
					-- values do not matter
					Shifter_low_bit_select => ,
					Shifter_middle_bits_select => ,
					Shifter_high_bit_select => ,
					F_Block_Select => ,
					Subtract => Subtraction,
					ALU_op_with_carry => '0',
					AddSub_Op_1_Select => AddSub_Op_1_Select_OperandA,
					AddSub_Op_2_Select => AddSub_Op_2_Select_OperandB,
					Status_Register_Mask => ,
					-- Rd
					OperandA => ,
					-- K
					OperandB => ,
					Result => Result,
					Status_Register =>
					);
				
					second_clock_flag = '1';
					
				else
					ALU_operation: ALU port map (
					clk => clk,
					ALU_result_select => Adder_Subtractor_Operation,
					-- values do not matter
					Shifter_low_bit_select => ,
					Shifter_middle_bits_select => ,
					Shifter_high_bit_select => ,
					F_Block_Select => ,
					Subtract => Addition,
					ALU_op_with_carry => '0',
					AddSub_Op_1_Select => AddSub_Op_1_Select_OperandA,
					AddSub_Op_2_Select => AddSub_Op_2_Select_0,
					Status_Register_Mask => ,
					--Rd + 1
					OperandA => ,
					OperandB => ,
					Result => Result,
					Status_Register =>
					);
					
					second_clock_flag = '0';
				end if;
			end if;

			if std_match(Program_Data_Bus, OpAND) then
				ALU_operation: ALU port map (
					clk => clk,
					ALU_result_select => F_Block_Operation,
					-- values do not matter
					Shifter_low_bit_select => ,
					Shifter_middle_bits_select => ,
					Shifter_high_bit_select => ,
					F_Block_Select => F_Block_Select_and,
					Subtract => ,
					ALU_op_with_carry => ,
					AddSub_Op_1_Select => ,
					AddSub_Op_2_Select => ,
					Status_Register_Mask => ,
					OperandA => ,
					OperandB => ,
					Result => Result,
					Status_Register =>
				);
			end if;
			
			if std_match(Program_Data_Bus, OpSUB) then
				ALU_operation: ALU port map (
					clk => clk,
					ALU_result_select => Adder_Subtractor_Operation,
					Shifter_low_bit_select => ,
					Shifter_middle_bits_select => ,
					Shifter_high_bit_select => ,
					F_Block_Select => ,
					Subtract => Subtraction,
					ALU_op_with_carry => '0',
					AddSub_Op_1_Select => AddSub_Op_1_Select_OperandA,
					AddSub_Op_2_Select => AddSub_Op_2_Select_OperandB,
					Status_Register_Mask => ,
					OperandA => ,
					OperandB => ,
					Result => Result,
					Status_Register =>
				);
			end if;
			
			if std_match(Program_Data_Bus, OpSUBI) then
				ALU_operation: ALU port map (
					clk => clk,
					ALU_result_select => Adder_Subtractor_Operation,
					Shifter_low_bit_select => ,
					Shifter_middle_bits_select => ,
					Shifter_high_bit_select => ,
					F_Block_Select => ,
					Subtract => Subtraction,
					ALU_op_with_carry => '0',
					AddSub_Op_1_Select => AddSub_Op_1_Select_OperandA,
					AddSub_Op_2_Select => AddSub_Op_2_Select_OperandB,
					Status_Register_Mask => ,
					OperandA => ,
					OperandB => ,
					Result => Result,
					Status_Register =>
				);
			end if;
			
			if std_match(Program_Data_Bus, OpSWAP) then
				ALU_operation: ALU port map (
					clk => clk,
					ALU_result_select => Adder_Subtractor_Operation,
					Shifter_low_bit_select => ,
					Shifter_middle_bits_select => ,
					Shifter_high_bit_select => ,
					F_Block_Select => ,
					Subtract => Subtraction,
					ALU_op_with_carry => '0',
					AddSub_Op_1_Select => AddSub_Op_1_Select_OperandA,
					AddSub_Op_2_Select => AddSub_Op_2_Select_OperandB,
					Status_Register_Mask => ,
					OperandA => ,
					OperandB => ,
					Result => Result,
					Status_Register =>
				);
			end if;
			
		end if;
	end process;
end architecture;