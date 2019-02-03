----------------------------------------------------------------------------
--  AVR Control Unit
--
--	This file contains an implementation of the control for an 8-bit
--  AVR architecture. This implementation consists of a FSM and instruction
--  decoder to process instructions from the program data bus.
--
--  Revision History:
--	28 Jan 19	Kavya Sreedhar & Dan Xu 	Initial Revision
--  1  Feb 19	Kavya Sreedhar & Dan Xu	Updated revision history
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
	
	-- ALU control signals
	ALU_result_select: 			out std_logic_vector(1 downto 0);
	Shifter_low_bit_select: 	out std_logic_vector(1 downto 0);
	Shifter_middle_bits_select: out std_logic;
	Shifter_high_bit_select: 	out std_logic_vector(2 downto 0);
	F_Block_Select: 			out std_logic_vector(3 downto 0);
	Subtract: 					out std_logic;
	ALU_op_with_carry: 			out std_logic;
	AddSub_Op_1_Select: 		out std_logic;
	AddSub_Op_2_Select: 		out std_logic_vector(1 downto 0);

	OperandA:                   out std_logic_vector(NUM_DATA_BITS - 1 downto 0);
	-- second operand
	OperandB:                   out std_logic_vector(NUM_DATA_BITS - 1 downto 0);

	-- Flag update control
	TBit_Select             : out std_logic_vector(DATA_BITS_LOG-1 downto 0);
	Interrupt_Flag_Sel      : out std_logic_vector(NUM_I_FLAG_BITS-1 downto 0);
	Transfer_Flag_Sel       : out std_logic_vector(NUM_T_FLAG_BITS-1 downto 0);
	Half_Carry_Flag_Sel     : out std_logic_vector(NUM_H_FLAG_BITS-1 downto 0);
	Corrected_Sign_Flag_Sel : out std_logic_vector(NUM_S_FLAG_BITS-1 downto 0);
	Signed_OF_Flag_Sel      : out std_logic_vector(NUM_V_FLAG_BITS-1 downto 0);
	Neg_Flag_Sel            : out std_logic_vector(NUM_N_FLAG_BITS-1 downto 0);
	Zero_Flag_Sel           : out std_logic_vector(NUM_Z_FLAG_BITS-1 downto 0);
	Carry_Flag_Sel          : out std_logic_vector(NUM_C_FLAG_BITS-1 downto 0);
	
	-- Register control signals
	GP_Input_Select         : out std_logic_vector(NUM_GP_INP_SELECT_BITS-1 downto 0);
	GP_Write_Enable         : out std_logic;
	GP_Swap_Nibbles         : out std_logic;
	GP_Dst_Select           : out std_logic_vector(NUM_REG_LOG-1 downto 0);
	GP_Src_SelectA          : out std_logic_vector(NUM_REG_LOG-1 downto 0);
	GP_Src_SelectB          : out std_logic_vector(NUM_REG_LOG-1 downto 0);

	GP_outA                 : in std_logic_vector(NUM_DATA_BITS-1 downto 0);
	GP_outB                 : in std_logic_vector(NUM_DATA_BITS-1 downto 0);	

	IO_Input_Select         : out std_logic;
	IO_Write_Enable         : out std_logic;
	IO_Dst_Select           : out std_logic_vector(NUM_IO_LOG-1 downto 0);
	IO_Src_SelectA          : out std_logic_vector(NUM_IO_LOG-1 downto 0);
	IO_Src_SelectB          : out std_logic_vector(NUM_IO_LOG-1 downto 0)
	
    );
end entity;

architecture control_arch of Control_Unit
	
	-- Control Unit Signals
	signal second_clock_flag: 	std_logic;
	
begin
	process (clk)
	begin
	
	if rising_edge(clk) then
	
		-- add with carry
		if std_match(Program_Data_Bus, OpADC) then
		
			-- Control signals to register
			GP_Src_SelectA <= Program_Data_Bus(9) & Program_Data_Bus(3 downto 0);
			GP_Src_SelectB <= Program_Data_Bus(8 downto 4);
			
			-- Control signals to ALU
			-- addition operation
			ALU_result_select <= Adder_Subtractor_Operation;
			-- value does not matter
			Shifter_low_bit_select <= Shifter_low_bit_highest_bit;
			-- value does not matter
			Shifter_middle_bits_select <= Shifter_middle_bits_select_immediate_right;
			-- value does not matter
			Shifter_high_bit_select <= Shifter_high_bit_select_second_highest_bit;
			-- value does not matter
			F_Block_Select <= F_Block_Select_0;
			-- addition operation
			Subtract <= Addition;
			-- addition with carry
			ALU_op_with_carry <= '1';
			-- use operands passed in as arguments from Control Unit
			AddSub_Op_1_Select <= AddSub_Op_1_Select_OperandA;
			AddSub_Op_2_Select <= AddSub_Op_2_Select_OperandB;

			-- Register d contents
			OperandA <= GP_outA;
			-- Register r contents
			OperandB <= GP_outB;			
			
			-- Control signals to Register to store result
			-- get result from ALU
			ALU_in <= ALU_Result;
			-- indicate register input is from ALU
			GP_Input_Select <= GP_IN_SEL_ALU;
			-- enable write to register
			GP_Write_Enable <= '1';
			-- indicate nibbles of register should not be swapped
			GP_Swap_Nibbles <= '0';
			-- store result in Register d
			GP_Dst_Select <= GP_Src_SelectA;


			-- IO Register Control
			-- Update from the ALU
			IO_Input_Select         <= IO_IN_SEL_SREG_ALU;
			-- Allow Write
			IO_Write_Enable         <= '1';
			-- Read/Write from status regs
			IO_Dst_Select           <= IO_REG_LOC;
			IO_Src_SelectA          <= IO_REG_LOC;

			-- Flag updates
			-- Unused
			TBit_Select             <= (DATA_BITS_LOG-1 downto 0 => '-');
			-- Hold current values
			Interrupt_Flag_Sel      <= I_HOLD_VALUE;
			Transfer_Flag_Sel       <= T_HOLD_VALUE;
			-- Update arithmetic flags since adding
			Half_Carry_Flag_Sel     <= H_FROM_ALU;
			Corrected_Sign_Flag_Sel <= S_FROM_ALU;
			Signed_OF_Flag_Sel      <= V_FROM_ALU;
			Neg_Flag_Sel            <= N_FROM_ALU;
			Zero_Flag_Sel           <= Z_FROM_ALU;
			Carry_Flag_Sel          <= C_FROM_ALU;

		end if;
		
		-- addition
		if std_match(Program_Data_Bus, OpADD) then
			-- Control signals to register
			GP_Src_SelectA <= Program_Data_Bus(9) & Program_Data_Bus(3 downto 0);
			GP_Src_SelectB <= Program_Data_Bus(8 downto 4);
			
			-- Control signals to ALU
			-- addition operation
			ALU_result_select <= Adder_Subtractor_Operation;
			-- values do not matter
			Shifter_low_bit_select <= Shifter_low_bit_highest_bit;
			-- values do not matter
			Shifter_middle_bits_select <= Shifter_middle_bits_select_immediate_right;
			-- values do not matter
			Shifter_high_bit_select <= Shifter_high_bit_select_second_highest_bit;
			-- values do not matter
			F_Block_Select <= F_Block_Select_0;
			-- addition operation
			Subtract <= Addition;
			-- no add with carry
			ALU_op_with_carry <= '0';
			-- use operands passed in as arguments from Control Unit
			AddSub_Op_1_Select <= AddSub_Op_1_Select_OperandA;
			AddSub_Op_2_Select <= AddSub_Op_2_Select_OperandB;
			
			-- Control signals to Register to store result
			-- get result from ALU
			ALU_in <= ALU_Result;
			-- indicate register input is from ALU
			GP_Input_Select <= GP_IN_SEL_ALU;
			-- enable write to register
			GP_Write_Enable <= '1';
			-- indicate nibbles of register should not be swapped
			GP_Swap_Nibbles <= '0';
			-- store result in Register d
			GP_Dst_Select <= GP_Src_SelectA;

			-- Register d contents
			OperandA <= GP_outA;
			-- Register r contents
			OperandB <= GP_outB;

			-- IO Register Control
			-- Update from the ALU
			IO_Input_Select         <= IO_IN_SEL_SREG_ALU;
			-- Allow Write
			IO_Write_Enable         <= '1';
			-- Read/Write from status regs
			IO_Dst_Select           <= IO_REG_LOC;
			IO_Src_SelectA          <= IO_REG_LOC;

			-- Flag updates
			-- Unused
			TBit_Select             <= (DATA_BITS_LOG-1 downto 0 => '-');
			-- Hold current values
			Interrupt_Flag_Sel      <= I_HOLD_VALUE;
			Transfer_Flag_Sel       <= T_HOLD_VALUE;
			-- Update arithmetic flags since adding
			Half_Carry_Flag_Sel     <= H_FROM_ALU;
			Corrected_Sign_Flag_Sel <= S_FROM_ALU;
			Signed_OF_Flag_Sel      <= V_FROM_ALU;
			Neg_Flag_Sel            <= N_FROM_ALU;
			Zero_Flag_Sel           <= Z_FROM_ALU;
			Carry_Flag_Sel          <= C_FROM_ALU;


		end if;
		
		-- 2 clocks
		-- add immediate to word
		if std_match(Program_Data_Bus, OpADIW) then
			if second_clock_flag = '0' then
				-- choose which register to use depending on instruction decoding
				if Program_Data_Bus(5 downto 4) = "00" then
					GP_Src_SelectA <= "11000";
				end if;
				
				if Program_Data_Bus(5 downto 4) = "01" then
					GP_Src_SelectA <= "11010";
				end if;
				
				if Program_Data_Bus(5 downto 4) = "10" then
					GP_Src_SelectA <= "11100";
				end if;
				
				if Program_Data_Bus(5 downto 4) = "11" then
					GP_Src_SelectA <= "11110";
				end if;
				
				-- value does not matter
				GP_Src_SelectB <= Program_Data_Bus(8 downto 4);
			
				-- addition operation
				ALU_result_select <= Adder_Subtractor_Operation;
				-- values do not matter
				Shifter_low_bit_select <= Shifter_low_bit_highest_bit;
				-- values do not matter
				Shifter_middle_bits_select <= Shifter_middle_bits_select_immediate_right;
				-- values do not matter
				Shifter_high_bit_select <= Shifter_high_bit_select_second_highest_bit;
				-- values do not matter
				F_Block_Select <= F_Block_Select_0;
				-- addition
				Subtract <= Addition;
				-- no add with carry
				ALU_op_with_carry <= '0';
				-- use operands passed in as arguments from Control Unit
				AddSub_Op_1_Select <= AddSub_Op_1_Select_OperandA;
				AddSub_Op_2_Select <= AddSub_Op_2_Select_OperandB;
				-- Rd contents
				OperandA <= GP_outA;
				-- K, immediate value from Program_Data_Bus
				OperandB <= Program_Data_Bus(7 downto 6) & Program_Data_Bus(3 downto 0);
				
				-- Control signals to Register to store result
				-- get result from ALU
				ALU_in <= ALU_Result;
				-- indicate register input is from ALU
				GP_Input_Select <= GP_IN_SEL_ALU;
				-- enable write to register
				GP_Write_Enable <= '1';
				-- indicate nibbles of register should not be swapped
				GP_Swap_Nibbles <= '0';
				-- store result in Register d
				GP_Dst_Select <= GP_Src_SelectA;
			);
			
			-- indicates 1 clock of instruction has occurred
			second_clock_flag <= '1';
			
			else
				-- choose which register to use depending on instruction decoding, use next
				-- register for second clock of operation
				if Program_Data_Bus(5 downto 4) = "00" then
					GP_Src_SelectA <= "11001";
				end if;
				
				if Program_Data_Bus(5 downto 4) = "01" then
					GP_Src_SelectA <= "11011";
				end if;
				
				if Program_Data_Bus(5 downto 4) = "10" then
					GP_Src_SelectA <= "11101";
				end if;
				
				if Program_Data_Bus(5 downto 4) = "11" then
					GP_Src_SelectA <= "11111";
				end if;
				
				-- value does not matter
				GP_Src_SelectB <= Program_Data_Bus(8 downto 4);
				
				-- addition operation
				ALU_result_select <= Adder_Subtractor_Operation;
				-- values do not matter
				Shifter_low_bit_select <= Shifter_low_bit_highest_bit;
				-- values do not matter
				Shifter_middle_bits_select <= Shifter_middle_bits_select_immediate_right;
				-- values do not matter
				Shifter_high_bit_select <= Shifter_high_bit_select_second_highest_bit;
				-- values do not matter
				F_Block_Select <= F_Block_Select_0;
				-- addition operation
				Subtract <= Addition;
				-- no add with carry
				ALU_op_with_carry <= '0';
				-- add nothing to the next register
				AddSub_Op_1_Select <= AddSub_Op_1_Select_OperandA;
				AddSub_Op_2_Select <= AddSub_Op_2_Select_0;
				-- Rd + 1 contents
				OperandA <= GP_outA;
				-- does not matter
				OperandB <= GP_outA;
				
				-- Control signals to Register to store result
				-- get result from ALU
				ALU_in <= ALU_Result;
				-- indicate register input is from ALU
				GP_Input_Select <= GP_IN_SEL_ALU;
				-- enable write to register
				GP_Write_Enable <= '1';
				-- indicate nibbles of register should not be swapped
				GP_Swap_Nibbles <= '0';
				-- store result in Register d
				GP_Dst_Select <= GP_Src_SelectA;			
			);

			-- IO Register Control
			-- Update from the ALU
			IO_Input_Select         <= IO_IN_SEL_SREG_ALU;
			-- Allow Write
			IO_Write_Enable         <= '1';
			-- Read/Write from status regs
			IO_Dst_Select           <= IO_REG_LOC;
			IO_Src_SelectA          <= IO_REG_LOC;
			
			-- Flag updates (need to do both times to do properly)
			-- Unused
			TBit_Select             <= (DATA_BITS_LOG-1 downto 0 => '-');
			-- Hold current values
			Interrupt_Flag_Sel      <= I_HOLD_VALUE;
			Transfer_Flag_Sel       <= T_HOLD_VALUE;
			Half_Carry_Flag_Sel     <= H_HOLD_VALUE;
			-- Update arithmetic flags since adding
			Corrected_Sign_Flag_Sel <= S_FROM_ALU;
			Signed_OF_Flag_Sel      <= V_FROM_ALU;
			Neg_Flag_Sel            <= N_FROM_ALU;
			Zero_Flag_Sel           <= Z_FROM_ALU;
			Carry_Flag_Sel          <= C_FROM_ALU;				
			
			-- reset clock flag for next instruction
			second_clock_flag <= '0';
			end if;
		end if;

		-- and instruction
		if std_match(Program_Data_Bus, OpAND) then
		
			-- Control signals to register
			GP_Src_SelectA <= Program_Data_Bus(9) & Program_Data_Bus(3 downto 0);
			GP_Src_SelectB <= Program_Data_Bus(8 downto 4);
			
			-- F Block operation
			ALU_result_select <= F_Block_Operation;
			-- values do not matter
			Shifter_low_bit_select <= Shifter_low_bit_highest_bit;
			-- values do not matter
			Shifter_middle_bits_select <= Shifter_middle_bits_select_immediate_right;
			-- values do not matter
			Shifter_high_bit_select <= Shifter_high_bit_select_second_highest_bit;
			-- and operation on F-block
			F_Block_Select <= F_Block_Select_and;
			-- values do not matter
			Subtract <= Addition;
			-- values do not matter
			ALU_op_with_carry <= '0';
			-- values do not matter
			AddSub_Op_1_Select <= AddSub_Op_1_Select_OperandA;
			-- values do not matter
			AddSub_Op_2_Select <= AddSub_Op_1_Select_OperandB;
			Status_Register_Mask <= ;
			Status_Register <= ;
			
			-- Control signals to Register to store result
			-- get result from ALU
			ALU_in <= ALU_Result;
			-- indicate register input is from ALU
			GP_Input_Select <= GP_IN_SEL_ALU;
			-- enable write to register
			GP_Write_Enable <= '1';
			-- indicate nibbles of register should not be swapped
			GP_Swap_Nibbles <= '0';
			-- store result in Register d
			GP_Dst_Select <= GP_Src_SelectA;

			-- IO Register Control
			-- Update from the ALU
			IO_Input_Select         <= IO_IN_SEL_SREG_ALU;
			-- Allow Write
			IO_Write_Enable         <= '1';
			-- Read/Write from status regs
			IO_Dst_Select           <= IO_REG_LOC;
			IO_Src_SelectA          <= IO_REG_LOC;
			
			-- Flag updates (need to do both times to do properly)
			-- Unused
			TBit_Select             <= (DATA_BITS_LOG-1 downto 0 => '-');
			-- Hold current values
			Interrupt_Flag_Sel      <= I_HOLD_VALUE;
			Transfer_Flag_Sel       <= T_HOLD_VALUE;
			Half_Carry_Flag_Sel     <= H_HOLD_VALUE;
			Carry_Flag_Sel          <= C_HOLD_VALUE;	
			-- Update the flags needed from ANDing
			Corrected_Sign_Flag_Sel <= S_FROM_ALU;
			Signed_OF_Flag_Sel      <= V_CLEAR_VALUE;
			Neg_Flag_Sel            <= N_FROM_ALU;
			Zero_Flag_Sel           <= Z_FROM_ALU;	
		
		end if;
		
		if std_match(Program_Data_Bus, OpANDI) then
			-- Control signals to register
			GP_Src_SelectA <= '1' & Program_Data_Bus(7 downto 4);
			-- value does not matter
			GP_Src_SelectB <= Program_Data_Bus(8 downto 4);
			
			-- F Block operation
			ALU_result_select <= F_Block_Operation;
			-- values do not matter
			Shifter_low_bit_select <= Shifter_low_bit_highest_bit;
			-- values do not matter
			Shifter_middle_bits_select <= Shifter_middle_bits_select_immediate_right;
			-- values do not matter
			Shifter_high_bit_select <= Shifter_high_bit_select_second_highest_bit;
			-- and operation on F-block
			F_Block_Select <= F_Block_Select_and;
			-- values do not matter
			Subtract <= Addition;
			-- values do not matter
			ALU_op_with_carry <= '0';
			-- values do not matter
			AddSub_Op_1_Select <= AddSub_Op_1_Select_OperandA;
			-- values do not matter
			AddSub_Op_2_Select <= AddSub_Op_1_Select_OperandB;
			Status_Register_Mask <= ;
			Status_Register <= ;
			
			-- Register d contents
			OperandA <= GP_outA;
			-- K, immediate value
			OperandB <= Program_Data_Bus(11 downto 8) & Program_Data_Bus(3 downto 0);
			
			-- Control signals to Register to store result
			-- get result from ALU
			ALU_in <= ALU_Result;
			-- indicate register input is from ALU
			GP_Input_Select <= GP_IN_SEL_ALU;
			-- enable write to register
			GP_Write_Enable <= '1';
			-- indicate nibbles of register should not be swapped
			GP_Swap_Nibbles <= '0';
			-- store result in Register d
			GP_Dst_Select <= GP_Src_SelectA;

			-- IO Register Control
			-- Update from the ALU
			IO_Input_Select         <= IO_IN_SEL_SREG_ALU;
			-- Allow Write
			IO_Write_Enable         <= '1';
			-- Read/Write from status regs
			IO_Dst_Select           <= IO_REG_LOC;
			IO_Src_SelectA          <= IO_REG_LOC;
			
			-- Flag updates
			-- Unused
			TBit_Select             <= (DATA_BITS_LOG-1 downto 0 => '-');
			-- Hold current values
			Interrupt_Flag_Sel      <= I_HOLD_VALUE;
			Transfer_Flag_Sel       <= T_HOLD_VALUE;
			Half_Carry_Flag_Sel     <= H_HOLD_VALUE;
			Carry_Flag_Sel          <= C_HOLD_VALUE;	
			-- Update the flags needed from ANDing
			Corrected_Sign_Flag_Sel <= S_FROM_ALU;
			Signed_OF_Flag_Sel      <= V_CLEAR_VALUE;
			Neg_Flag_Sel            <= N_FROM_ALU;
			Zero_Flag_Sel           <= Z_FROM_ALU;	
			
		end if;
		
		if std_match(Program_Data_Bus, OpASR) then
			-- Control signals to register
			GP_Src_SelectA <= Program_Data_Bus(8 downto 4);
			-- value does not matter
			GP_Src_SelectB <= Program_Data_Bus(8 downto 4);
			
			-- doing a shift operation
			ALU_result_select <= Shifter_Rotater_Operation;
			-- shifting to the right
			Shifter_low_bit_select <= Shifter_low_bit_bit_1;
			Shifter_middle_bits_select <=
				Shifter_middle_bits_select_immediate_left;
			-- arithmetic shift, preserve high bit
			Shifter_high_bit_select <= Shifter_high_bit_select_highest_bit;
			-- values do not matter
			F_Block_Select <= F_Block_Select_0;
			-- values do not matter
			Subtract <= Addition;
			-- values do not matter
			ALU_op_with_carry <= ;
			-- values do not matter
			AddSub_Op_1_Select <= AddSub_Op_1_Select_OperandA;
			-- values do not matter
			AddSub_Op_2_Select <= AddSub_Op_1_Select_OperandB;
			
			-- Register d contents
			OperandA <= GP_outA;
			-- value does not matter
			OperandB <= Program_Data_Bus(7 downto 0);
			
			-- Control signals to Register to store result
			-- get result from ALU
			ALU_in <= ALU_Result;
			-- indicate register input is from ALU
			GP_Input_Select <= GP_IN_SEL_ALU;
			-- enable write to register
			GP_Write_Enable <= '1';
			-- indicate nibbles of register should not be swapped
			GP_Swap_Nibbles <= '0';
			-- store result in Register d
			GP_Dst_Select <= GP_Src_SelectA;

			-- IO Register Control
			-- Update from the ALU
			IO_Input_Select         <= IO_IN_SEL_SREG_ALU;
			-- Allow Write
			IO_Write_Enable         <= '1';
			-- Read/Write from status regs
			IO_Dst_Select           <= IO_REG_LOC;
			IO_Src_SelectA          <= IO_REG_LOC;
			
			-- Flag updates
			-- Unused
			TBit_Select             <= (DATA_BITS_LOG-1 downto 0 => '-');
			-- Hold current values
			Interrupt_Flag_Sel      <= I_HOLD_VALUE;
			Transfer_Flag_Sel       <= T_HOLD_VALUE;
			Half_Carry_Flag_Sel     <= H_HOLD_VALUE;
			-- Update the flags needed from shifting right
			Corrected_Sign_Flag_Sel <= S_FROM_ALU;
			Signed_OF_Flag_Sel      <= V_C_XOR_N;
			Neg_Flag_Sel            <= N_FROM_ALU;
			Zero_Flag_Sel           <= Z_FROM_ALU;
			Carry_Flag_Sel          <= C_FROM_LSB;			

		end if;

		if std_match(Program_Data_Bus, OpBCLR) then
		
		end if;
		
		if std_match(Program_Data_Bus, OpBLD) then
		
		end if;
		
		if std_match(Program_Data_Bus, OpBSET) then

		end if;
		
		if std_match(Program_Data_Bus, OpBST) then

		end if;
		
		if std_match(Program_Data_Bus, OpCOM) then
			-- value does not matter
			GP_Src_SelectA <= Program_Data_Bus(8 downto 4);
			-- Control signals to register
			GP_Src_SelectB <= Program_Data_Bus(8 downto 4);
			
			-- subtraction operation
			ALU_result_select <= Adder_Subtractor_Operation;
			-- values does not matter
			Shifter_low_bit_select <= Shifter_low_bit_highest_bit;
			-- values does not matter
			Shifter_middle_bits_select <= Shifter_middle_bits_select_immediate_right;
			-- values does not matter
			Shifter_high_bit_select <= Shifter_high_bit_select_second_highest_bit;
			-- values does not matter
			F_Block_Select <= F_Block_Select_0;
			-- subtraction operation
			Subtract <= Subtraction;
			-- not subtract with carry
			ALU_op_with_carry <= '0';
			-- to not the register, do FF - register value
			AddSub_Op_1_Select <= AddSub_Op_1_Select_FF;
			AddSub_Op_2_Select <= AddSub_Op_2_Select_OperandB;

			-- value does not matter
			OperandA <= Program_Data_Bus(7 downto 0);
			-- Register d contents
			OperandB <= GP_outB;
			
			-- Control signals to Register to store result
			-- get result from ALU
			ALU_in <= ALU_Result;
			-- indicate register input is from ALU
			GP_Input_Select <= GP_IN_SEL_ALU;
			-- enable write to register
			GP_Write_Enable <= '1';
			-- indicate nibbles of register should not be swapped
			GP_Swap_Nibbles <= '0';
			-- store result in Register d
			GP_Dst_Select <= GP_Src_SelectB;

			-- IO Register Control
			-- Update from the ALU
			IO_Input_Select         <= IO_IN_SEL_SREG_ALU;
			-- Allow Write
			IO_Write_Enable         <= '1';
			-- Read/Write from status regs
			IO_Dst_Select           <= IO_REG_LOC;
			IO_Src_SelectA          <= IO_REG_LOC;
			
			-- Flag updates
			-- Unused
			TBit_Select             <= (DATA_BITS_LOG-1 downto 0 => '-');
			-- Hold current values
			Interrupt_Flag_Sel      <= I_HOLD_VALUE;
			Transfer_Flag_Sel       <= T_HOLD_VALUE;
			Half_Carry_Flag_Sel     <= H_HOLD_VALUE;
			-- Update the flags needed for complement
			Corrected_Sign_Flag_Sel <= S_FROM_ALU;
			Signed_OF_Flag_Sel      <= V_CLEAR_VALUE;
			Neg_Flag_Sel            <= N_FROM_ALU;
			Zero_Flag_Sel           <= Z_FROM_ALU;
			Carry_Flag_Sel          <= C_SET_VALUE;

		end if;
		
		if std_match(Program_Data_Bus, OpCP) then
		
			-- Control signals to register
			GP_Src_SelectA <= Program_Data_Bus(8 downto 4);
			GP_Src_SelectB <= Program_Data_Bus(8 downto 4);
			
			-- subtraction operation
			ALU_result_select <= Adder_Subtractor_Operation;
			-- values do not matter
			Shifter_low_bit_select <= Shifter_low_bit_highest_bit;
			-- values do not matter
			Shifter_middle_bits_select <= Shifter_middle_bits_select_immediate_right;
			-- values do not matter
			Shifter_high_bit_select <= Shifter_high_bit_select_second_highest_bit;
			-- values do not matter
			F_Block_Select <= F_Block_Select_0;
			-- subtraction operation
			Subtract <= Subtraction;
			-- no subtract with carry
			ALU_op_with_carry <= '0';
			-- use operands passed in as arguments from Control Unit
			AddSub_Op_1_Select <= AddSub_Op_1_Select_OperandA;
			AddSub_Op_2_Select <= AddSub_Op_2_Select_OperandB;
			Status_Register_Mask <= ;
			Status_Register <= ;
			
			-- Register d contents
			OperandA <= GP_outA;
			-- Register r contents
			OperandB <= GP_outB;
			
			-- do not store result back in registers

			-- IO Register Control
			-- Update from the ALU
			IO_Input_Select         <= IO_IN_SEL_SREG_ALU;
			-- Allow Write
			IO_Write_Enable         <= '1';
			-- Read/Write from status regs
			IO_Dst_Select           <= IO_REG_LOC;
			IO_Src_SelectA          <= IO_REG_LOC;
			
			-- Flag updates
			-- Unused
			TBit_Select             <= (DATA_BITS_LOG-1 downto 0 => '-');
			-- Hold current values
			Interrupt_Flag_Sel      <= I_HOLD_VALUE;
			Transfer_Flag_Sel       <= T_HOLD_VALUE;
			-- Update the flags needed for subtraction
			Half_Carry_Flag_Sel     <= H_FROM_ALU;
			Corrected_Sign_Flag_Sel <= S_FROM_ALU;
			Signed_OF_Flag_Sel      <= V_FROM_ALU;
			Neg_Flag_Sel            <= N_FROM_ALU;
			Zero_Flag_Sel           <= Z_FROM_ALU;
			Carry_Flag_Sel          <= C_FROM_ALU;			
		end if;
		
		if std_match(Program_Data_Bus, OpCPC) then
			-- Control signals to register
			GP_Src_SelectA <= Program_Data_Bus(9) & Program_Data_Bus(3 downto 0);
			GP_Src_SelectB <= Program_Data_Bus(8 downto 4);
			
			-- subtraction operation
			ALU_result_select <= Adder_Subtractor_Operation;
			-- values do not matter
			Shifter_low_bit_select <= Shifter_low_bit_highest_bit;
			-- values do not matter
			Shifter_middle_bits_select <= Shifter_middle_bits_select_immediate_right;
			-- values do not matter
			Shifter_high_bit_select <= Shifter_high_bit_select_second_highest_bit;
			-- values do not matter
			F_Block_Select <= F_Block_Select_0;
			-- subtraction operation
			Subtract <= Subtraction;
			-- subtract with carry
			ALU_op_with_carry <= '1';
			-- use operands passed in as arguments from Control Unit
			AddSub_Op_1_Select <= AddSub_Op_1_Select_OperandA;
			AddSub_Op_2_Select <= AddSub_Op_2_Select_OperandB;
			Status_Register_Mask <= ;
			Status_Register <= ;
			
			-- Register d contents
			OperandA <= GP_outA;
			-- Register r contents
			OperandB <= GP_outB;
			
			-- do not store result back in registers

			-- IO Register Control
			-- Update from the ALU
			IO_Input_Select         <= IO_IN_SEL_SREG_ALU;
			-- Allow Write
			IO_Write_Enable         <= '1';
			-- Read/Write from status regs
			IO_Dst_Select           <= IO_REG_LOC;
			IO_Src_SelectA          <= IO_REG_LOC;
			
			-- Flag updates
			-- Unused
			TBit_Select             <= (DATA_BITS_LOG-1 downto 0 => '-');
			-- Hold current values
			Interrupt_Flag_Sel      <= I_HOLD_VALUE;
			Transfer_Flag_Sel       <= T_HOLD_VALUE;
			-- Update the flags needed for subtraction
			Half_Carry_Flag_Sel     <= H_FROM_ALU;
			Corrected_Sign_Flag_Sel <= S_FROM_ALU;
			Signed_OF_Flag_Sel      <= V_FROM_ALU;
			Neg_Flag_Sel            <= N_FROM_ALU;
			Zero_Flag_Sel           <= Z_FROM_ALU;
			Carry_Flag_Sel          <= C_FROM_ALU;		
		end if;
		
		if std_match(Program_Data_Bus, OpCPI) then
			-- Control signals to register
			GP_Src_SelectA <= '1' & Program_Data_Bus(7 downto 4);
			-- value does not matter
			GP_Src_SelectB <= Program_Data_Bus(8 downto 4);
			
			-- subtraction operation
			ALU_result_select <= Adder_Subtractor_Operation;
			-- 
			Shifter_low_bit_select <= ;
			Shifter_middle_bits_select <= ;
			Shifter_high_bit_select <= ;
			F_Block_Select <= ;
			Subtract <= Subtraction;
			ALU_op_with_carry <= '0';
			AddSub_Op_1_Select <= AddSub_Op_1_Select_OperandA;
			AddSub_Op_2_Select <= AddSub_Op_2_Select_OperandB;
			Status_Register_Mask <= ;
			Status_Register <= ;
		
		-- do not store result back in registers

			-- IO Register Control
			-- Update from the ALU
			IO_Input_Select         <= IO_IN_SEL_SREG_ALU;
			-- Allow Write
			IO_Write_Enable         <= '1';
			-- Read/Write from status regs
			IO_Dst_Select           <= IO_REG_LOC;
			IO_Src_SelectA          <= IO_REG_LOC;
			
			-- Flag updates
			-- Unused
			TBit_Select             <= (DATA_BITS_LOG-1 downto 0 => '-');
			-- Hold current values
			Interrupt_Flag_Sel      <= I_HOLD_VALUE;
			Transfer_Flag_Sel       <= T_HOLD_VALUE;
			-- Update the flags needed for subtraction
			Half_Carry_Flag_Sel     <= H_FROM_ALU;
			Corrected_Sign_Flag_Sel <= S_FROM_ALU;
			Signed_OF_Flag_Sel      <= V_FROM_ALU;
			Neg_Flag_Sel            <= N_FROM_ALU;
			Zero_Flag_Sel           <= Z_FROM_ALU;
			Carry_Flag_Sel          <= C_FROM_ALU;

		end if;
		
		if std_match(Program_Data_Bus, OpDEC) then

			-- IO Register Control
			-- Update from the ALU
			IO_Input_Select         <= IO_IN_SEL_SREG_ALU;
			-- Allow Write
			IO_Write_Enable         <= '1';
			-- Read/Write from status regs
			IO_Dst_Select           <= IO_REG_LOC;
			IO_Src_SelectA          <= IO_REG_LOC;
			
			-- Flag updates
			-- Unused
			TBit_Select             <= (DATA_BITS_LOG-1 downto 0 => '-');
			-- Hold current values
			Interrupt_Flag_Sel      <= I_HOLD_VALUE;
			Transfer_Flag_Sel       <= T_HOLD_VALUE;
			Half_Carry_Flag_Sel     <= H_HOLD_VALUE;
			Carry_Flag_Sel          <= C_HOLD_VALUE;
			-- Update the flags needed for decrementing
			Corrected_Sign_Flag_Sel <= S_FROM_ALU;
			Signed_OF_Flag_Sel      <= V_FROM_ALU;
			Neg_Flag_Sel            <= N_FROM_ALU;
			Zero_Flag_Sel           <= Z_FROM_ALU;				
		end if;
		
		if std_match(Program_Data_Bus, OpEOR) then

			-- IO Register Control
			-- Update from the ALU
			IO_Input_Select         <= IO_IN_SEL_SREG_ALU;
			-- Allow Write
			IO_Write_Enable         <= '1';
			-- Read/Write from status regs
			IO_Dst_Select           <= IO_REG_LOC;
			IO_Src_SelectA          <= IO_REG_LOC;
			
			-- Flag updates
			-- Unused
			TBit_Select             <= (DATA_BITS_LOG-1 downto 0 => '-');
			-- Hold current values
			Interrupt_Flag_Sel      <= I_HOLD_VALUE;
			Transfer_Flag_Sel       <= T_HOLD_VALUE;
			Half_Carry_Flag_Sel     <= H_HOLD_VALUE;
			Carry_Flag_Sel          <= C_HOLD_VALUE;
			-- Update the flags needed for doing XOR
			Corrected_Sign_Flag_Sel <= S_FROM_ALU;
			Signed_OF_Flag_Sel      <= V_CLEAR_VALUE;
			Neg_Flag_Sel            <= N_FROM_ALU;
			Zero_Flag_Sel           <= Z_FROM_ALU;

		end if;
		
		if std_match(Program_Data_Bus, OpINC) then

			-- IO Register Control
			-- Update from the ALU
			IO_Input_Select         <= IO_IN_SEL_SREG_ALU;
			-- Allow Write
			IO_Write_Enable         <= '1';
			-- Read/Write from status regs
			IO_Dst_Select           <= IO_REG_LOC;
			IO_Src_SelectA          <= IO_REG_LOC;
			
			-- Flag updates
			-- Unused
			TBit_Select             <= (DATA_BITS_LOG-1 downto 0 => '-');
			-- Hold current values
			Interrupt_Flag_Sel      <= I_HOLD_VALUE;
			Transfer_Flag_Sel       <= T_HOLD_VALUE;
			Half_Carry_Flag_Sel     <= H_HOLD_VALUE;
			Carry_Flag_Sel          <= C_HOLD_VALUE;
			-- Update the flags needed for incrementing
			Corrected_Sign_Flag_Sel <= S_FROM_ALU;
			Signed_OF_Flag_Sel      <= V_FROM_ALU;
			Neg_Flag_Sel            <= N_FROM_ALU;
			Zero_Flag_Sel           <= Z_FROM_ALU;

		end if;
		
		if std_match(Program_Data_Bus, OpLSR) then

			-- IO Register Control
			-- Update from the ALU
			IO_Input_Select         <= IO_IN_SEL_SREG_ALU;
			-- Allow Write
			IO_Write_Enable         <= '1';
			-- Read/Write from status regs
			IO_Dst_Select           <= IO_REG_LOC;
			IO_Src_SelectA          <= IO_REG_LOC;
			
			-- Flag updates
			-- Unused
			TBit_Select             <= (DATA_BITS_LOG-1 downto 0 => '-');
			-- Hold current values
			Interrupt_Flag_Sel      <= I_HOLD_VALUE;
			Transfer_Flag_Sel       <= T_HOLD_VALUE;
			Half_Carry_Flag_Sel     <= H_FROM_ALU;
			-- Update the flags needed for rotating
			Corrected_Sign_Flag_Sel <= S_FROM_ALU;
			Signed_OF_Flag_Sel      <= V_C_XOR_N;
			Neg_Flag_Sel            <= N_CLEAR_VALUE;
			Zero_Flag_Sel           <= Z_FROM_ALU;
			Carry_Flag_Sel          <= C_FROM_LSB;
		end if;
		
		if std_match(Program_Data_Bus, OpNEG) then

			-- IO Register Control
			-- Update from the ALU
			IO_Input_Select         <= IO_IN_SEL_SREG_ALU;
			-- Allow Write
			IO_Write_Enable         <= '1';
			-- Read/Write from status regs
			IO_Dst_Select           <= IO_REG_LOC;
			IO_Src_SelectA          <= IO_REG_LOC;
			
			-- Flag updates
			-- Unused
			TBit_Select             <= (DATA_BITS_LOG-1 downto 0 => '-');
			-- Hold current values
			Interrupt_Flag_Sel      <= I_HOLD_VALUE;
			Transfer_Flag_Sel       <= T_HOLD_VALUE;
			-- Update the flags needed for negation
			Half_Carry_Flag_Sel     <= H_FROM_ALU;
			Corrected_Sign_Flag_Sel <= S_FROM_ALU;
			Signed_OF_Flag_Sel      <= V_FROM_ALU;
			Neg_Flag_Sel            <= N_FROM_ALU;
			Zero_Flag_Sel           <= Z_FROM_ALU;
			Carry_Flag_Sel          <= C_FROM_ALU;
		end if;
		
		if std_match(Program_Data_Bus, OpOR) then
			-- IO Register Control
			-- Update from the ALU
			IO_Input_Select         <= IO_IN_SEL_SREG_ALU;
			-- Allow Write
			IO_Write_Enable         <= '1';
			-- Read/Write from status regs
			IO_Dst_Select           <= IO_REG_LOC;
			IO_Src_SelectA          <= IO_REG_LOC;
			
			-- Flag updates
			-- Unused
			TBit_Select             <= (DATA_BITS_LOG-1 downto 0 => '-');
			-- Hold current values
			Interrupt_Flag_Sel      <= I_HOLD_VALUE;
			Transfer_Flag_Sel       <= T_HOLD_VALUE;
			Half_Carry_Flag_Sel     <= H_HOLD_VALUE;
			Carry_Flag_Sel          <= C_HOLD_VALUE;
			-- Update the flags needed for doing OR
			Corrected_Sign_Flag_Sel <= S_FROM_ALU;
			Signed_OF_Flag_Sel      <= V_CLEAR_VALUE;
			Neg_Flag_Sel            <= N_FROM_ALU;
			Zero_Flag_Sel           <= Z_FROM_ALU;
		end if;
		
		if std_match(Program_Data_Bus, OpORI) then
		
			-- IO Register Control
			-- Update from the ALU
			IO_Input_Select         <= IO_IN_SEL_SREG_ALU;
			-- Allow Write
			IO_Write_Enable         <= '1';
			-- Read/Write from status regs
			IO_Dst_Select           <= IO_REG_LOC;
			IO_Src_SelectA          <= IO_REG_LOC;
			
			-- Flag updates
			-- Unused
			TBit_Select             <= (DATA_BITS_LOG-1 downto 0 => '-');
			-- Hold current values
			Interrupt_Flag_Sel      <= I_HOLD_VALUE;
			Transfer_Flag_Sel       <= T_HOLD_VALUE;
			Half_Carry_Flag_Sel     <= H_HOLD_VALUE;
			Carry_Flag_Sel          <= C_HOLD_VALUE;
			-- Update the flags needed for doing OR
			Corrected_Sign_Flag_Sel <= S_FROM_ALU;
			Signed_OF_Flag_Sel      <= V_CLEAR_VALUE;
			Neg_Flag_Sel            <= N_FROM_ALU;
			Zero_Flag_Sel           <= Z_FROM_ALU;
		end if;
		
		if std_match(Program_Data_Bus, OpROR) then
			-- IO Register Control
			-- Update from the ALU
			IO_Input_Select         <= IO_IN_SEL_SREG_ALU;
			-- Allow Write
			IO_Write_Enable         <= '1';
			-- Read/Write from status regs
			IO_Dst_Select           <= IO_REG_LOC;
			IO_Src_SelectA          <= IO_REG_LOC;
			
			-- Flag updates
			-- Unused
			TBit_Select             <= (DATA_BITS_LOG-1 downto 0 => '-');
			-- Hold current values
			Interrupt_Flag_Sel      <= I_HOLD_VALUE;
			Transfer_Flag_Sel       <= T_HOLD_VALUE;
			Half_Carry_Flag_Sel     <= H_FROM_ALU;
			-- Update the flags needed for rotating
			Corrected_Sign_Flag_Sel <= S_FROM_ALU;
			Signed_OF_Flag_Sel      <= V_C_XOR_N;
			Neg_Flag_Sel            <= N_FROM_ALU;
			Zero_Flag_Sel           <= Z_FROM_ALU;
			Carry_Flag_Sel          <= C_FROM_LSB;
		end if;
		
		if std_match(Program_Data_Bus, OpSBC) then

			-- IO Register Control
			-- Update from the ALU
			IO_Input_Select         <= IO_IN_SEL_SREG_ALU;
			-- Allow Write
			IO_Write_Enable         <= '1';
			-- Read/Write from status regs
			IO_Dst_Select           <= IO_REG_LOC;
			IO_Src_SelectA          <= IO_REG_LOC;
			
			-- Flag updates
			-- Unused
			TBit_Select             <= (DATA_BITS_LOG-1 downto 0 => '-');
			-- Hold current values
			Interrupt_Flag_Sel      <= I_HOLD_VALUE;
			Transfer_Flag_Sel       <= T_HOLD_VALUE;
			-- Update the flags needed for subtraction
			Half_Carry_Flag_Sel     <= H_FROM_ALU;
			Corrected_Sign_Flag_Sel <= S_FROM_ALU;
			Signed_OF_Flag_Sel      <= V_FROM_ALU;
			Neg_Flag_Sel            <= N_FROM_ALU;
			Zero_Flag_Sel           <= Z_FROM_ALU;
			Carry_Flag_Sel          <= C_FROM_ALU;
		end if;
		
		if std_match(Program_Data_Bus, OpSBCI) then

			-- IO Register Control
			-- Update from the ALU
			IO_Input_Select         <= IO_IN_SEL_SREG_ALU;
			-- Allow Write
			IO_Write_Enable         <= '1';
			-- Read/Write from status regs
			IO_Dst_Select           <= IO_REG_LOC;
			IO_Src_SelectA          <= IO_REG_LOC;
			
			-- Flag updates
			-- Unused
			TBit_Select             <= (DATA_BITS_LOG-1 downto 0 => '-');
			-- Hold current values
			Interrupt_Flag_Sel      <= I_HOLD_VALUE;
			Transfer_Flag_Sel       <= T_HOLD_VALUE;
			-- Update the flags needed for subtraction
			Half_Carry_Flag_Sel     <= H_FROM_ALU;
			Corrected_Sign_Flag_Sel <= S_FROM_ALU;
			Signed_OF_Flag_Sel      <= V_FROM_ALU;
			Neg_Flag_Sel            <= N_FROM_ALU;
			Zero_Flag_Sel           <= Z_FROM_ALU;
			Carry_Flag_Sel          <= C_FROM_ALU;
		end if;
		
		-- 2 clocks
		if std_match(Program_Data_Bus, OpSBIW) then
			
			-- IO Register Control
			-- Update from the ALU
			IO_Input_Select         <= IO_IN_SEL_SREG_ALU;
			-- Allow Write
			IO_Write_Enable         <= '1';
			-- Read/Write from status regs
			IO_Dst_Select           <= IO_REG_LOC;
			IO_Src_SelectA          <= IO_REG_LOC;
			
			-- Flag updates (Need to do beween calculation finishing)
			-- Unused
			TBit_Select             <= (DATA_BITS_LOG-1 downto 0 => '-');
			-- Hold current values
			Interrupt_Flag_Sel      <= I_HOLD_VALUE;
			Transfer_Flag_Sel       <= T_HOLD_VALUE;
			-- Update the flags needed for subtraction
			Half_Carry_Flag_Sel     <= H_FROM_ALU;
			Corrected_Sign_Flag_Sel <= S_FROM_ALU;
			Signed_OF_Flag_Sel      <= V_FROM_ALU;
			Neg_Flag_Sel            <= N_FROM_ALU;
			Zero_Flag_Sel           <= Z_FROM_ALU;
			Carry_Flag_Sel          <= C_FROM_ALU;

		end if;
		
		if std_match(Program_Data_Bus, OpSUB) then

			-- IO Register Control
			-- Update from the ALU
			IO_Input_Select         <= IO_IN_SEL_SREG_ALU;
			-- Allow Write
			IO_Write_Enable         <= '1';
			-- Read/Write from status regs
			IO_Dst_Select           <= IO_REG_LOC;
			IO_Src_SelectA          <= IO_REG_LOC;
			
			-- Flag updates
			-- Unused
			TBit_Select             <= (DATA_BITS_LOG-1 downto 0 => '-');
			-- Hold current values
			Interrupt_Flag_Sel      <= I_HOLD_VALUE;
			Transfer_Flag_Sel       <= T_HOLD_VALUE;
			-- Update the flags needed for subtraction
			Half_Carry_Flag_Sel     <= H_FROM_ALU;
			Corrected_Sign_Flag_Sel <= S_FROM_ALU;
			Signed_OF_Flag_Sel      <= V_FROM_ALU;
			Neg_Flag_Sel            <= N_FROM_ALU;
			Zero_Flag_Sel           <= Z_FROM_ALU;
			Carry_Flag_Sel          <= C_FROM_ALU;
		end if;
		
		if std_match(Program_Data_Bus, OpSUBI) then
			-- IO Register Control
			-- Update from the ALU
			IO_Input_Select         <= IO_IN_SEL_SREG_ALU;
			-- Allow Write
			IO_Write_Enable         <= '1';
			-- Read/Write from status regs
			IO_Dst_Select           <= IO_REG_LOC;
			IO_Src_SelectA          <= IO_REG_LOC;
			
			-- Flag updates
			-- Unused
			TBit_Select             <= (DATA_BITS_LOG-1 downto 0 => '-');
			-- Hold current values
			Interrupt_Flag_Sel      <= I_HOLD_VALUE;
			Transfer_Flag_Sel       <= T_HOLD_VALUE;
			-- Update the flags needed for subtraction
			Half_Carry_Flag_Sel     <= H_FROM_ALU;
			Corrected_Sign_Flag_Sel <= S_FROM_ALU;
			Signed_OF_Flag_Sel      <= V_FROM_ALU;
			Neg_Flag_Sel            <= N_FROM_ALU;
			Zero_Flag_Sel           <= Z_FROM_ALU;
			Carry_Flag_Sel          <= C_FROM_ALU;
		end if;
		
		if std_match(Program_Data_Bus, OpSWAP) then
			-- IO Register Control
			-- Update from the ALU
			IO_Input_Select         <= IO_IN_SEL_SREG_ALU;
			-- Allow Write
			IO_Write_Enable         <= '1';
			-- Read/Write from status regs
			IO_Dst_Select           <= IO_REG_LOC;
			IO_Src_SelectA          <= IO_REG_LOC;
			
			-- Flag updates
			-- Unused
			TBit_Select             <= (DATA_BITS_LOG-1 downto 0 => '-');
			-- Hold current values
			Interrupt_Flag_Sel      <= I_HOLD_VALUE;
			Transfer_Flag_Sel       <= T_HOLD_VALUE;
			Half_Carry_Flag_Sel     <= H_HOLD_VALUE;
			Corrected_Sign_Flag_Sel <= S_HOLD_VALUE;
			Signed_OF_Flag_Sel      <= V_HOLD_VALUE;
			Neg_Flag_Sel            <= N_HOLD_VALUE;
			Zero_Flag_Sel           <= Z_HOLD_VALUE;
			Carry_Flag_Sel          <= C_HOLD_VALUE;
		end if;
	
	end if;
	end process;
end architecture;