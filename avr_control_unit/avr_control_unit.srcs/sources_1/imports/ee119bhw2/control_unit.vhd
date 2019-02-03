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
	-- program data bus, contains instruction to decode
	Program_Data_Bus: in opcode_word;
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
	
	-- flag mask indicating which flag values to update after ALU operation
	Status_Register_Mask: 		out std_logic_vector(7 downto 0);
	
	-- Register control signals
	-- indicates which register contents to get
	GP_Src_SelectA: 			out std_logic_vector(NUM_REG_LOG-1 downto 0);
	GP_Src_SelectB:				out std_logic_vector(NUM_REG_LOG-1 downto 0);
	-- register contents from registers to use to send to other units in control unit
	GP_outA: 					in std_logic_vector(NUM_DATA_BITS-1 downto 0);
	GP_outB: 					in std_logic_vector(NUM_DATA_BITS-1 downto 0)
	
    );
end entity;

architecture control_arch of Control_Unit is
	
	-- indicates whether or not the second clock has occurred for 2 clock instructions
	-- will be changed to a finite state machine or counter in future weeks
	signal second_clock_flag: 	std_logic;
	
begin

	-- instruction decoder process
	-- depending on the instruction in the program data bus, appropriate control
	-- signals are output and appropriate CPU operation performed with results 
	-- returned and stored appropriately
	process (clk)
	begin
	
	if rising_edge(clk) then
	
		-- ADC: add with carry
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
		
		-- ADD addition with registers
		if std_match(Program_Data_Bus, OpADD) then
		
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
			-- no add with carry
			ALU_op_with_carry <= '0';
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
		
		-- ADIW add immediate to word
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
				-- value does not matter
				Shifter_low_bit_select <= Shifter_low_bit_highest_bit;
				-- value does not matter
				Shifter_middle_bits_select <= Shifter_middle_bits_select_immediate_right;
				-- value does not matter
				Shifter_high_bit_select <= Shifter_high_bit_select_second_highest_bit;
				-- value does not matter
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
				-- add with carry from first part lower 8 bit addition
				ALU_op_with_carry <= '1';
				-- add nothing to the next register
				AddSub_Op_1_Select <= AddSub_Op_1_Select_OperandA;
				AddSub_Op_2_Select <= AddSub_Op_2_Select_0;
				
				-- Rd + 1 contents
				OperandA <= GP_outA;
				-- does not matter
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
			
				-- reset clock flag for next instruction
				second_clock_flag <= '0';
			end if;
			
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
			
		end if;

		-- AND with registers
		if std_match(Program_Data_Bus, OpAND) then
		
			-- Control signals to register
			GP_Src_SelectA <= Program_Data_Bus(9) & Program_Data_Bus(3 downto 0);
			GP_Src_SelectB <= Program_Data_Bus(8 downto 4);
			
			-- F Block operation
			ALU_result_select <= F_Block_Operation;
			-- value does not matter
			Shifter_low_bit_select <= Shifter_low_bit_highest_bit;
			-- value does not matter
			Shifter_middle_bits_select <= Shifter_middle_bits_select_immediate_right;
			-- value does not matter
			Shifter_high_bit_select <= Shifter_high_bit_select_second_highest_bit;
			-- and operation on F-block
			F_Block_Select <= F_Block_Select_and;
			-- value does not matter
			Subtract <= Addition;
			-- value does not matter
			ALU_op_with_carry <= '0';
			-- value does not matter
			AddSub_Op_1_Select <= AddSub_Op_1_Select_OperandA;
			-- value does not matter
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
		
		-- ANDI add immediate
		if std_match(Program_Data_Bus, OpANDI) then
			-- Control signals to register
			GP_Src_SelectA <= '1' & Program_Data_Bus(7 downto 4);
			-- value does not matter
			GP_Src_SelectB <= Program_Data_Bus(8 downto 4);
			
			-- F Block operation
			ALU_result_select <= F_Block_Operation;
			-- value does not matter
			Shifter_low_bit_select <= Shifter_low_bit_highest_bit;
			-- value does not matter
			Shifter_middle_bits_select <= Shifter_middle_bits_select_immediate_right;
			-- value does not matter
			Shifter_high_bit_select <= Shifter_high_bit_select_second_highest_bit;
			-- and operation on F-block
			F_Block_Select <= F_Block_Select_and;
			-- value does not matter
			Subtract <= Addition;
			-- value does not matter
			ALU_op_with_carry <= '0';
			-- value does not matter
			AddSub_Op_1_Select <= AddSub_Op_1_Select_OperandA;
			-- value does not matter
			AddSub_Op_2_Select <= AddSub_Op_2_Select_OperandB;
			
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
		
		-- ASR arithmetic shift right a register
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
			-- value does not matter
			F_Block_Select <= F_Block_Select_0;
			-- value does not matter
			Subtract <= Addition;
			-- value does not matter
			ALU_op_with_carry <= ;
			-- value does not matter
			AddSub_Op_1_Select <= AddSub_Op_1_Select_OperandA;
			-- value does not matter
			AddSub_Op_2_Select <= AddSub_Op_2_Select_OperandB;
			
			-- Register d contents
			OperandA <= GP_outA;
			-- value does not matter
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
			Half_Carry_Flag_Sel     <= H_HOLD_VALUE;
			-- Update the flags needed from shifting right
			Corrected_Sign_Flag_Sel <= S_FROM_ALU;
			Signed_OF_Flag_Sel      <= V_C_XOR_N;
			Neg_Flag_Sel            <= N_FROM_ALU;
			Zero_Flag_Sel           <= Z_FROM_ALU;
			Carry_Flag_Sel          <= C_FROM_LSB;	
			
		end if;

		-- BCLR bit clear in the status register
		if std_match(Program_Data_Bus, OpBCLR) then
		
		end if;
		
		-- BLD bit load in a general purpose register
		if std_match(Program_Data_Bus, OpBLD) then
		
		end if;
		
		-- BSET bit set in the status register
		if std_match(Program_Data_Bus, OpBSET) then

		end if;
		
		-- BST bit set in a general purpose register
		if std_match(Program_Data_Bus, OpBST) then

		end if;
		
		-- COM complement of a register (register <- not register)
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
		
		-- CP compare registers
		if std_match(Program_Data_Bus, OpCP) then
		
			-- Control signals to register
			GP_Src_SelectA <= Program_Data_Bus(8 downto 4);
			GP_Src_SelectB <= Program_Data_Bus(8 downto 4);
			
			-- subtraction operation
			ALU_result_select <= Adder_Subtractor_Operation;
			-- value does not matter
			Shifter_low_bit_select <= Shifter_low_bit_highest_bit;
			-- value does not matter
			Shifter_middle_bits_select <= Shifter_middle_bits_select_immediate_right;
			-- value does not matter
			Shifter_high_bit_select <= Shifter_high_bit_select_second_highest_bit;
			-- value does not matter
			F_Block_Select <= F_Block_Select_0;
			-- subtraction operation
			Subtract <= Subtraction;
			-- no subtract with carry
			ALU_op_with_carry <= '0';
			-- use operands passed in as arguments from Control Unit
			AddSub_Op_1_Select <= AddSub_Op_1_Select_OperandA;
			AddSub_Op_2_Select <= AddSub_Op_2_Select_OperandB;
			
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
		
		-- CPC compare registers with carry
		if std_match(Program_Data_Bus, OpCPC) then
			
			-- Control signals to register
			GP_Src_SelectA <= Program_Data_Bus(9) & Program_Data_Bus(3 downto 0);
			GP_Src_SelectB <= Program_Data_Bus(8 downto 4);
			
			-- subtraction operation
			ALU_result_select <= Adder_Subtractor_Operation;
			-- value does not matter
			Shifter_low_bit_select <= Shifter_low_bit_highest_bit;
			-- value does not matter
			Shifter_middle_bits_select <= Shifter_middle_bits_select_immediate_right;
			-- value does not matter
			Shifter_high_bit_select <= Shifter_high_bit_select_second_highest_bit;
			-- value does not matter
			F_Block_Select <= F_Block_Select_0;
			-- subtraction operation
			Subtract <= Subtraction;
			-- subtract with carry
			ALU_op_with_carry <= '1';
			-- use operands passed in as arguments from Control Unit
			AddSub_Op_1_Select <= AddSub_Op_1_Select_OperandA;
			AddSub_Op_2_Select <= AddSub_Op_2_Select_OperandB;
			
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
		
		-- CPI compare register with immediate value
		if std_match(Program_Data_Bus, OpCPI) then
			
			-- Control signals to register
			GP_Src_SelectA <= '1' & Program_Data_Bus(7 downto 4);
			-- value does not matter
			GP_Src_SelectB <= Program_Data_Bus(8 downto 4);
			
			-- subtraction operation
			ALU_result_select <= Adder_Subtractor_Operation;
			-- value does not matter
			Shifter_low_bit_select <= Shifter_low_bit_highest_bit;
			-- value does not matter
			Shifter_middle_bits_select <= Shifter_middle_bits_select_immediate_right;
			-- value does not matter
			Shifter_high_bit_select <= Shifter_high_bit_select_second_highest_bit;
			-- value does not matter
			F_Block_Select <= F_Block_Select_0;
			-- subtraction operation
			Subtract <= Subtraction;
			-- no subtract with carry
			ALU_op_with_carry <= '0';
			-- use operands passed in as arguments from Control Unit
			AddSub_Op_1_Select <= AddSub_Op_1_Select_OperandA;
			AddSub_Op_2_Select <= AddSub_Op_2_Select_OperandB;
			
			-- Register d contents
			OperandA <= GP_outA;
			-- Register r contents
			OperandB <= Program_Data_Bus(11 downto 8) & Program_Data_Bus(3 downto 0);
			
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
		
		-- DEC decrement a register
		if std_match(Program_Data_Bus, OpDEC) then
			
			-- Control signals to register
			GP_Src_SelectA <= '1' & Program_Data_Bus(7 downto 4);
			-- value does not matter
			GP_Src_SelectB <= Program_Data_Bus(8 downto 4);
			
			-- subtraction operation
			ALU_result_select <= Adder_Subtractor_Operation;
			-- value does not matter
			Shifter_low_bit_select <= Shifter_low_bit_highest_bit;
			-- value does not matter
			Shifter_middle_bits_select <= Shifter_middle_bits_select_immediate_right;
			-- value does not matter
			Shifter_high_bit_select <= Shifter_high_bit_select_second_highest_bit;
			-- value does not matter
			F_Block_Select <= F_Block_Select_0;
			-- subtraction operation
			Subtract <= Subtraction;
			-- no subtract with carry
			ALU_op_with_carry <= '0';
			-- use Register d and 1 as the operands
			AddSub_Op_1_Select <= AddSub_Op_1_Select_OperandA;
			AddSub_Op_2_Select <= AddSub_Op_2_Select_1;
			
			-- Register d contents
			OperandA <= GP_outA;
			-- value does not matter
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
			Half_Carry_Flag_Sel     <= H_HOLD_VALUE;
			Carry_Flag_Sel          <= C_HOLD_VALUE;
			-- Update the flags needed for decrementing
			Corrected_Sign_Flag_Sel <= S_FROM_ALU;
			Signed_OF_Flag_Sel      <= V_FROM_ALU;
			Neg_Flag_Sel            <= N_FROM_ALU;
			Zero_Flag_Sel           <= Z_FROM_ALU;		
			
		end if;
		
		-- EOR xor registers
		if std_match(Program_Data_Bus, OpEOR) then
			
			-- Control signals to register
			GP_Src_SelectA <= Program_Data_Bus(9) & Program_Data_Bus(3 downto 0);
			GP_Src_SelectB <= Program_Data_Bus(8 downto 4);
			
			-- F Block operation
			ALU_result_select <= F_Block_Operation;
			-- value does not matter
			Shifter_low_bit_select <= Shifter_low_bit_highest_bit;
			-- value does not matter
			Shifter_middle_bits_select <= Shifter_middle_bits_select_immediate_right;
			-- value does not matter
			Shifter_high_bit_select <= Shifter_high_bit_select_second_highest_bit;
			-- F Block xor operation
			F_Block_Select <= F_Block_Select_xor;
			-- value does not matter
			Subtract <= Addition;
			-- value does not matter
			ALU_op_with_carry <= '0';
			-- value does not matter
			AddSub_Op_1_Select <= AddSub_Op_1_Select_OperandA;
			-- value does not matter
			AddSub_Op_2_Select <= AddSub_Op_2_Select_OperandB;
			
			-- Register d contents
			OperandA <= GP_outA;
			-- Register g contents
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
			Half_Carry_Flag_Sel     <= H_HOLD_VALUE;
			Carry_Flag_Sel          <= C_HOLD_VALUE;
			-- Update the flags needed for doing XOR
			Corrected_Sign_Flag_Sel <= S_FROM_ALU;
			Signed_OF_Flag_Sel      <= V_CLEAR_VALUE;
			Neg_Flag_Sel            <= N_FROM_ALU;
			Zero_Flag_Sel           <= Z_FROM_ALU;
		
		end if;
		
		-- INC increment a register
		if std_match(Program_Data_Bus, OpINC) then
			
			-- Control signals to register
			GP_Src_SelectA <= Program_Data_Bus(9) & Program_Data_Bus(3 downto 0);
			GP_Src_SelectB <= Program_Data_Bus(8 downto 4);
			
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
			-- no add with carry
			ALU_op_with_carry <= '0';
			-- Register d and 1 are operands for increment
			AddSub_Op_1_Select <= AddSub_Op_1_Select_OperandA;
			AddSub_Op_2_Select <= AddSub_Op_2_Select_1;
		
			-- Register d contents
			OperandA <= GP_outA;
			-- Register g contents
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
			Half_Carry_Flag_Sel     <= H_HOLD_VALUE;
			Carry_Flag_Sel          <= C_HOLD_VALUE;
			-- Update the flags needed for incrementing
			Corrected_Sign_Flag_Sel <= S_FROM_ALU;
			Signed_OF_Flag_Sel      <= V_FROM_ALU;
			Neg_Flag_Sel            <= N_FROM_ALU;
			Zero_Flag_Sel           <= Z_FROM_ALU;
			
		end if;
		
		-- LSR logical shift right a register
		if std_match(Program_Data_Bus, OpLSR) then

			-- Control signals to register
			GP_Src_SelectA <= Program_Data_Bus(8 downto 4);
			-- value does not matter
			GP_Src_SelectB <= Program_Data_Bus(8 downto 4);
			
			-- shifter operation
			ALU_result_select <= Shifter_Rotater_Operation;
			-- logical shift right
			Shifter_low_bit_select <= Shifter_low_bit_bit_1;
			Shifter_middle_bits_select <= ;
				Shifter_middle_bits_select_immediate_left;
			-- logical shift does not preserve high bit
			Shifter_high_bit_select <= Shifter_high_bit_select_0;
			-- value does not matter
			F_Block_Select <= F_Block_Select_0;
			-- value does not matter
			Subtract <= Addition;
			-- value does not matter
			ALU_op_with_carry <= '0';
			-- value does not matter
			AddSub_Op_1_Select <= AddSub_Op_1_Select_OperandA;
			-- value does not matter
			AddSub_Op_2_Select <= AddSub_Op_2_Select_OperandB;
			-- value does not matter
		
			-- Register d contents
			OperandA <= GP_outA;
			-- value does not matter
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
			Half_Carry_Flag_Sel     <= H_FROM_ALU;
			-- Update the flags needed for rotating
			Corrected_Sign_Flag_Sel <= S_FROM_ALU;
			Signed_OF_Flag_Sel      <= V_C_XOR_N;
			Neg_Flag_Sel            <= N_CLEAR_VALUE;
			Zero_Flag_Sel           <= Z_FROM_ALU;
			Carry_Flag_Sel          <= C_FROM_LSB;
			
		end if;
		
		-- NEG negate a register
		if std_match(Program_Data_Bus, OpNEG) then
			-- value does not matter
			GP_Src_SelectA <= Program_Data_Bus(8 downto 4);
			-- Control signals to register
			GP_Src_SelectB <= Program_Data_Bus(8 downto 4);
			
			-- subtraction operation
			ALU_result_select <= Adder_Subtractor_Operation;
			-- value does not matter
			Shifter_low_bit_select <= Shifter_low_bit_highest_bit;
			-- value does not matter
			Shifter_middle_bits_select <= Shifter_middle_bits_select_immediate_right;
			-- value does not matter
			Shifter_high_bit_select <= Shifter_high_bit_select_second_highest_bit;
			-- value does not matter
			F_Block_Select <= F_Block_Select_0;
			-- subtraction operation
			Subtract <= Subtraction;
			-- no subtract with carry
			ALU_op_with_carry <= '0';
			-- subtract Register d from 0 for negate
			AddSub_Op_1_Select <= AddSub_Op_1_Select_0;
			AddSub_Op_2_Select <= AddSub_Op_2_Select_OperandB;
			
			-- value does not matter
			OperandA <= GP_outA;
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
			-- Update the flags needed for negation
			Half_Carry_Flag_Sel     <= H_FROM_ALU;
			Corrected_Sign_Flag_Sel <= S_FROM_ALU;
			Signed_OF_Flag_Sel      <= V_FROM_ALU;
			Neg_Flag_Sel            <= N_FROM_ALU;
			Zero_Flag_Sel           <= Z_FROM_ALU;
			Carry_Flag_Sel          <= C_FROM_ALU;
			
		end if;
		
		-- OR or registers
		if std_match(Program_Data_Bus, OpOR) then
			
			-- Control signals to register
			GP_Src_SelectA <= Program_Data_Bus(9) & Program_Data_Bus(3 downto 0);
			GP_Src_SelectB <= Program_Data_Bus(8 downto 4);
			
			-- F Block operation
			ALU_result_select <= F_Block_Select;
			-- value does not matter
			Shifter_low_bit_select <= Shifter_low_bit_highest_bit;
			-- value does not matter
			Shifter_middle_bits_select <= Shifter_middle_bits_select_immediate_right;
			-- value does not matter
			Shifter_high_bit_select <= Shifter_high_bit_select_second_highest_bit;
			-- F Block or operation
			F_Block_Select <= F_Block_Select_or;
			-- value does not matter
			Subtract <= Addition;
			-- value does not matter
			ALU_op_with_carry <= '0';
			-- value does not matter
			AddSub_Op_1_Select <= AddSub_Op_1_Select_OperandA;
			-- value does not matter
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
			Half_Carry_Flag_Sel     <= H_HOLD_VALUE;
			Carry_Flag_Sel          <= C_HOLD_VALUE;
			-- Update the flags needed for doing OR
			Corrected_Sign_Flag_Sel <= S_FROM_ALU;
			Signed_OF_Flag_Sel      <= V_CLEAR_VALUE;
			Neg_Flag_Sel            <= N_FROM_ALU;
			Zero_Flag_Sel           <= Z_FROM_ALU;
			
		end if;
		
		-- ORI or register with an immediate value
		if std_match(Program_Data_Bus, OpORI) then
			
			-- Control signals to register
			GP_Src_SelectA <= '1' & Program_Data_Bus(7 downto 4);
			GP_Src_SelectB <= Program_Data_Bus(4 downto 0);
			
			-- F Block operation
			ALU_result_select <= F_Block_Select;
			-- value does not matter
			Shifter_low_bit_select <= Shifter_low_bit_highest_bit;
			-- value does not matter
			Shifter_middle_bits_select <= Shifter_middle_bits_select_immediate_right;
			-- value does not matter
			Shifter_high_bit_select <= Shifter_high_bit_select_second_highest_bit;
			-- F Block or operation
			F_Block_Select <= F_Block_Select_or;
			-- value does not matter
			Subtract <= Addition;
			-- value does not matter
			ALU_op_with_carry <= '0';
			-- value does not matter
			AddSub_Op_1_Select <= AddSub_Op_1_Select_OperandA;
			-- value does not matter
			AddSub_Op_2_Select <= AddSub_Op_2_Select_OperandB;
			
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
			-- Update the flags needed for doing OR
			Corrected_Sign_Flag_Sel <= S_FROM_ALU;
			Signed_OF_Flag_Sel      <= V_CLEAR_VALUE;
			Neg_Flag_Sel            <= N_FROM_ALU;
			Zero_Flag_Sel           <= Z_FROM_ALU;
			
		end if;
		
		-- ROR rotate right a register
		if std_match(Program_Data_Bus, OpROR) then
			
			-- Control signals to register
			GP_Src_SelectA <= Program_Data_Bus(8 downto 4);
			-- value does not matter
			GP_Src_SelectB <= Program_Data_Bus(4 downto 0);
			
			-- rotate operation
			ALU_result_select => Shifter_Rotater_Operation;
			-- rotate right bit updates
			Shifter_low_bit_select => Shifter_low_bit_bit_1;
			Shifter_middle_bits_select =>
				Shifter_middle_bits_select_immediate_left;
			Shifter_high_bit_select => Shifter_high_bit_select_lowest_bit;
			-- value does not matter
			F_Block_Select => F_Block_Select_0;
			-- value does not matter
			Subtract => Addition;
			-- value does not matter
			ALU_op_with_carry => '0';
			-- value does not matter
			AddSub_Op_1_Select => AddSub_Op_1_Select_OperandA;
			-- value does not matter
			AddSub_Op_2_Select => AddSub_Op_2_Select_OperandB;
			Status_Register_Mask => ;
			Status_Register => ;
			
			-- Register d contents
			OperandA <= GP_outA;
			-- value does not matter
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
			Half_Carry_Flag_Sel     <= H_FROM_ALU;
			-- Update the flags needed for rotating
			Corrected_Sign_Flag_Sel <= S_FROM_ALU;
			Signed_OF_Flag_Sel      <= V_C_XOR_N;
			Neg_Flag_Sel            <= N_FROM_ALU;
			Zero_Flag_Sel           <= Z_FROM_ALU;
			Carry_Flag_Sel          <= C_FROM_LSB;
			
		end if;
		
		-- SBC subtract registers with carry
		if std_match(Program_Data_Bus, OpSBC) then
			
			-- Control signals to register
			GP_Src_SelectA <= Program_Data_Bus(9) & Program_Data_Bus(3 downto 0);
			GP_Src_SelectB <= Program_Data_Bus(8 downto 4);
			
			-- subtraction operation
			ALU_result_select <= Adder_Subtractor_Operation;
			-- value does not matter
			Shifter_low_bit_select <= Shifter_low_bit_highest_bit;
			-- value does not matter
			Shifter_middle_bits_select <= Shifter_middle_bits_select_immediate_right;
			-- value does not matter
			Shifter_high_bit_select <= Shifter_high_bit_select_second_highest_bit;
			-- value does not matter
			F_Block_Select <= F_Block_Select_0;
			-- subtraction operation
			Subtract <= Subtraction;
			-- subtract with carry
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
			-- Update the flags needed for subtraction
			Half_Carry_Flag_Sel     <= H_FROM_ALU;
			Corrected_Sign_Flag_Sel <= S_FROM_ALU;
			Signed_OF_Flag_Sel      <= V_FROM_ALU;
			Neg_Flag_Sel            <= N_FROM_ALU;
			Zero_Flag_Sel           <= Z_FROM_ALU;
			Carry_Flag_Sel          <= C_FROM_ALU;
			
		end if;
		
		-- SBCI subtract register and immediate value with carry
		if std_match(Program_Data_Bus, OpSBCI) then
			
			-- Control signals to register
			GP_Src_SelectA <= '1' & Program_Data_Bus(7 downto 4);
			-- value does not matter
			GP_Src_SelectB <= Program_Data_Bus(4 downto 0);
			
			-- subtraction operation
			ALU_result_select <= Adder_Subtractor_Operation;
			-- value does not matter
			Shifter_low_bit_select <= Shifter_low_bit_highest_bit;
			-- value does not matter
			Shifter_middle_bits_select <= Shifter_middle_bits_select_immediate_right;
			-- value does not matter
			Shifter_high_bit_select <= Shifter_high_bit_select_second_highest_bit;
			-- value does not matter
			F_Block_Select <= F_Block_Select_0;
			-- subtraction operation
			Subtract <= Subtraction;
			-- subtract with carry
			ALU_op_with_carry <= '1';
			-- use operands passed in as arguments from Control Unit
			AddSub_Op_1_Select <= AddSub_Op_1_Select_OperandA;
			AddSub_Op_2_Select <= AddSub_Op_2_Select_OperandB;
			
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
			-- Update the flags needed for subtraction
			Half_Carry_Flag_Sel     <= H_FROM_ALU;
			Corrected_Sign_Flag_Sel <= S_FROM_ALU;
			Signed_OF_Flag_Sel      <= V_FROM_ALU;
			Neg_Flag_Sel            <= N_FROM_ALU;
			Zero_Flag_Sel           <= Z_FROM_ALU;
			Carry_Flag_Sel          <= C_FROM_ALU;
			
		end if;
		
		-- SBIW subtract immediate to word
		if std_match(Program_Data_Bus, OpSBIW) then
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
				
				-- subtract operation
				ALU_result_select <= Adder_Subtractor_Operation;
				-- value does not matter
				Shifter_low_bit_select <= Shifter_low_bit_highest_bit;
				-- value does not matter
				Shifter_middle_bits_select <=
					Shifter_middle_bits_select_immediate_right;
				-- value does not matter
				Shifter_high_bit_select <=
					Shifter_high_bit_select_second_highest_bit;
				-- value does not matter
				F_Block_Select <= F_Block_Select_0;
				-- subtract operation
				Subtract <= Subtraction;
				-- no subtract with carry
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
				
				-- subtraction operation
				ALU_result_select <= Adder_Subtractor_Operation;
				-- value does not matter
				Shifter_low_bit_select <= Shifter_low_bit_highest_bit;
				-- value does not matter
				Shifter_middle_bits_select <= Shifter_middle_bits_select_immediate_right;
				-- value does not matter
				Shifter_high_bit_select <= 
					Shifter_high_bit_select_second_highest_bit;
				-- value does not matter
				F_Block_Select <= F_Block_Select_0;
				-- subtraction operation
				Subtract <= Subtraction;
				-- subtract with carry from first part lower 8 bit addition
				ALU_op_with_carry <= '1';
				-- add nothing to the next register
				AddSub_Op_1_Select <= AddSub_Op_1_Select_OperandA;
				AddSub_Op_2_Select <= AddSub_Op_2_Select_0;
				
				--Rd + 1 contents
				OperandA <= GP_outA;
				-- value does not matter
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
				
				-- reset clock flag for next instruction
				second_clock_flag <= '0';
			end if;
			
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
		
		-- SUB subtract registers
		if std_match(Program_Data_Bus, OpSUB) then
			
			-- Control signals to register
			GP_Src_SelectA <= Program_Data_Bus(9) & Program_Data_Bus(3 downto 0);
			GP_Src_SelectB <= Program_Data_Bus(8 downto 4);
			
			-- subtraction operation
			ALU_result_select => Adder_Subtractor_Operation;
			-- value does not matter
			Shifter_low_bit_select => Shifter_low_bit_highest_bit;
			-- value does not matter
			Shifter_middle_bits_select => Shifter_middle_bits_select_immediate_right;
			-- value does not matter
			Shifter_high_bit_select => Shifter_high_bit_select_second_highest_bit;
			-- value does not matter
			F_Block_Select => F_Block_Select_0;
			-- subtraction operation
			Subtract => Subtraction;
			-- no subtract with carry
			ALU_op_with_carry => '0';
			-- use operands passed in as arguments from Control Unit
			AddSub_Op_1_Select => AddSub_Op_1_Select_OperandA;
			AddSub_Op_2_Select => AddSub_Op_2_Select_OperandB;
			Status_Register_Mask => ;
			Status_Register => ;
			
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
			-- Update the flags needed for subtraction
			Half_Carry_Flag_Sel     <= H_FROM_ALU;
			Corrected_Sign_Flag_Sel <= S_FROM_ALU;
			Signed_OF_Flag_Sel      <= V_FROM_ALU;
			Neg_Flag_Sel            <= N_FROM_ALU;
			Zero_Flag_Sel           <= Z_FROM_ALU;
			Carry_Flag_Sel          <= C_FROM_ALU;
			
		end if;
		
		-- SUBI subtract immediate value from register
		if std_match(Program_Data_Bus, OpSUBI) then

			-- Control signals to register
			GP_Src_SelectA <= '1' & Program_Data_Bus(7 downto 4);
			-- value does not matter
			GP_Src_SelectB <= Program_Data_Bus(4 downto 0);
			
			-- subtraction operation
			ALU_result_select <= Adder_Subtractor_Operation;
			-- value does not matter
			Shifter_low_bit_select <= Shifter_low_bit_highest_bit;
			-- value does not matter
			Shifter_middle_bits_select <= Shifter_middle_bits_select_immediate_right;
			-- value does not matter
			Shifter_high_bit_select <= Shifter_high_bit_select_second_highest_bit;
			-- value does not matter
			F_Block_Select <= F_Block_Select_0;
			-- subtraction operation
			Subtract <= Subtraction;
			-- no subtract with carry
			ALU_op_with_carry <= '0';
			-- use operands passed in as arguments from Control Unit
			AddSub_Op_1_Select <= AddSub_Op_1_Select_OperandA;
			AddSub_Op_2_Select <= AddSub_Op_2_Select_OperandB;
			
			-- Register d contents
			OperandA <= GP_outA;
			-- value does not matter
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
			-- Update the flags needed for subtraction
			Half_Carry_Flag_Sel     <= H_FROM_ALU;
			Corrected_Sign_Flag_Sel <= S_FROM_ALU;
			Signed_OF_Flag_Sel      <= V_FROM_ALU;
			Neg_Flag_Sel            <= N_FROM_ALU;
			Zero_Flag_Sel           <= Z_FROM_ALU;
			Carry_Flag_Sel          <= C_FROM_ALU;
			
		end if;
		
		-- SWAP swap nibbles of a register
		if std_match(Program_Data_Bus, OpSWAP) then
		
			-- Control signals to register
			GP_Src_SelectA <= Program_Data_Bus(8 downto 4);
			-- value does not matter
			GP_Src_SelectB <= Program_Data_Bus(4 downto 0);
			
			-- Register d contents
			OperandA <= GP_outA;
			-- value does not matter
			OperandB <= GP_outB;
			
			-- indicate nibbles of register should be swapped
			GP_Swap_Nibbles <= '1';
			
			-- the following ALU signal values do not matter
			ALU_result_select <= Adder_Subtractor_Operation;
			Shifter_low_bit_select <= Shifter_low_bit_highest_bit;
			Shifter_middle_bits_select <= Shifter_middle_bits_select_immediate_right;
			Shifter_high_bit_select <= Shifter_high_bit_select_second_highest_bit;
			F_Block_Select <= F_Block_Select_0;
			Subtract <= Subtraction;
			ALU_op_with_carry <= '0';
			AddSub_Op_1_Select <= AddSub_Op_1_Select_OperandA;
			AddSub_Op_2_Select <= AddSub_Op_2_Select_OperandB;
			
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