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
	
	-- Control Unit Signals
	signal Result: 						std_logic_vector(NUM_DATA_BITS - 1 downto 0);
	signal second_clock_flag: 			std_logic;
	
	-- ALU control signals
	signal ALU_result_select: 			out std_logic_vector(1 downto 0);
	signal Shifter_low_bit_select: 		out std_logic_vector(1 downto 0);
	signal Shifter_middle_bits_select: 	out std_logic;
	signal Shifter_high_bit_select: 	out std_logic_vector(2 downto 0);
	signal F_Block_Select: 				out std_logic_vector(3 downto 0);
	signal Subtract: 					out std_logic;
	signal ALU_op_with_carry: 			out std_logic;
	signal AddSub_Op_1_Select: 			out std_logic;
	signal AddSub_Op_2_Select: 			out std_logic_vector(1 downto 0);
	-- flag mask indicating which flag values to update after ALU operation
	signal Status_Register_Mask: 		out std_logic_vector(7 downto 0);
	
	-- Register control signals
	signal GP_Src_SelectA: 				out std_logic_vector(NUM_REG_LOG-1 downto 0);
	signal GP_Src_SelectB:				out std_logic_vector(NUM_REG_LOG-1 downto 0);
	
	signal GP_outA: 					in std_logic_vector(NUM_DATA_BITS-1 downto 0);
	signal GP_outB: 					in std_logic_vector(NUM_DATA_BITS-1 downto 0);
	
begin
	process (clk)
	begin
		
		if rising_edge(clk) then
			if std_match(Program_Data_Bus, OpADC) then
				
				-- Control signals to register
				GP_Src_SelectA <= Program_Data_Bus(9) & Program_Data_Bus(3 downto 0);
				GP_Src_SelectB <= Program_Data_Bus(8 downto 4);
				
				-- Control signals to ALU
				-- addition operation
				ALU_result_select <= Adder_Subtractor_Operation,
				-- value does not matter
				Shifter_low_bit_select <= "00" ,
				-- value does not matter
				Shifter_middle_bits_select <= '0',
				-- value does not matter
				Shifter_high_bit_select <= "000",
				-- value does not matter
				F_Block_Select <= "0000",
				-- addition operation
				Subtract <= Addition,
				-- addition with carry
				ALU_op_with_carry <= '1',
				-- use operands passed in as arguments from Control Unit
				AddSub_Op_1_Select <= AddSub_Op_1_Select_OperandA,
				AddSub_Op_2_Select <= AddSub_Op_2_Select_OperandB,
				
				Status_Register_Mask <= ,
				OperandA <= GP_outA,
				OperandB <= GP_outB,
			end if;
			
			if std_match(Program_Data_Bus, OpADD) then
				-- Control signals to register
				GP_Src_SelectA <= Program_Data_Bus(9) & Program_Data_Bus(3 downto 0);
				GP_Src_SelectB <= Program_Data_Bus(8 downto 4);
				
				-- Control signals to ALU
				-- addition operation
				ALU_result_select <= Adder_Subtractor_Operation,
				-- values do not matter
				Shifter_low_bit_select <= "00",
				-- values do not matter
				Shifter_middle_bits_select <= '0',
				-- values do not matter
				Shifter_high_bit_select <= ,
				F_Block_Select <= ,
				Subtract <= Addition,
				ALU_op_with_carry <= '0',
				AddSub_Op_1_Select <= AddSub_Op_1_Select_OperandA,
				AddSub_Op_2_Select <= AddSub_Op_2_Select_OperandB,
				Status_Register_Mask <= ,
				OperandA <= ,
				OperandB <= ,
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