----------------------------------------------------------------------------
--  AVR Control Unit
--
--    This file contains an implementation of the control for an 8-bit
--  AVR architecture. This implementation consists of a FSM and instruction
--  decoder to process instructions from the program data bus as well as the
--  16-bit instruction register which holds the instruction to be executed.
--	The second word of an instruction is available on the bus Program_Data_Bus
--  and the stack pointer in implemented in the control unit as well. An 
--  active low reset is used to initialize the stack pointer to all 1's (note
--  this is inconsistent with the standard AVR processor which does not initialize
--  its stack pointer on reset). 
-- 
--	Only instructions part of the data memory access unit are currently supported
--  and the control unit generates appropriate signals for that unit to generate
--  addresses and read and write data for the data memory. While the data memory
--  access unit updates the data address bus, appropriate control signals are
--  sent to the registers to correctly update the data data bus with the value
--  at that address. There are also 2 active-low control lines for accessing the
--  memory: DataRd indicates that the data memory is being read and DataWr indicates
--  it is being written. These lines are only active during the second half of the
--  clock (while the clock is low) in the last clock cycle for the insturction.
--
--  Revision History:
--	28 Jan 19   Kavya Sreedhar & Dan Xu    	Initial revision
-- 	30 Jan 19   Kavya Sreedhar & Dan Xu    	Updated more control signals 
--  01 Feb 19   Kavya Sreedhar & Dan Xu    	Updated revision history
--  02 Feb 19   Kavya Sreedhar & Dan Xu    	Fixed syntax errors, updated comments
--	10 Feb 19	Kavya Sreedhar & Dan Xu	   	Added data memory access unit control
--											signals, deleted ALU
--  11 Feb 19	Kavya Sreedhar & Dan Xu		Updated comments
--  11 Feb 19	Kavya Sreedhar & Dan Xu		Added the stack instructions and the
--                                          direct memory instructions
----------------------------------------------------------------------------

-- declaration of libraries used
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;

-- contains constants used for all CPU units
library work;
use work.CPU_CONSTANTS.all;
use work.DMAU_constants.all;
use work.opcodes.all;
use work.RegConstants.all;

--
-- Control Unit entity declaration
-- includes control signals to other units such as data memory access unit and
-- accesses registers for values to be sent to units such as data memory access 
-- unit
--
entity Control_Unit is
    port(
    
	-------------------------------------------------------------------
	-- GENERAL
	-------------------------------------------------------------------
	
    -- system clock.
    clk: in std_logic;
    
    -- inputs
    -- program data bus
    Program_Data_Bus        : in std_logic_vector(15 downto 0);
    -- instruction register
    IR                      : in opcode_word;
 
	-------------------------------------------------------------------
	-- REGISTERS
	-------------------------------------------------------------------
	
	-- CONTROL SIGNALS TO REGISTERS UNIT
	
	-- GP Reg Ctrl Signals
	-- Ctrl signal to swap the nibbles for a general purpose register
    GP_Swap_Nibbles         : out std_logic;
	-- Ctrl signal for which register to write to for
	--   a standard register write
    GP_Dst_SelectA          : out std_logic_vector(NUM_REG_LOG-1 downto 0);
	-- Select for which GP register to output on A
    GP_Src_SelectA          : out std_logic_vector(NUM_REG_LOG-1 downto 0);
    -- Select for which GP register to output on B
	GP_Src_SelectB          : out std_logic_vector(NUM_REG_LOG-1 downto 0);
	-- Destination for a wide bus write to registers
	GP_Dst_SelectB          : out std_logic_vector(NUM_GP_WIDE_LOAD_BITS-1 downto 0);
	-- Enable standard writes to the GP registers
	GP_Write_EnableA		: out std_logic;
	-- Enables wide bus writes to certain registers.
	GP_Write_EnableB		: out std_logic;
	-- Select which input source to use (Data bus or ALU)
	GP_Input_Select			: out std_logic_vector(
									NUM_GP_INP_SELECT_BITS - 1 downto 0);

	-- IO Reg Ctrl Signals
	-- Select the 
	IO_Input_Select         : out std_logic;
	-- Enable for regular IO
	IO_Write_EnableA        : out std_logic;
	-- Enable for wide IO
	IO_Write_EnableB        : out std_logic;
	-- Destination for regular writing
	IO_Dst_SelectA          : out std_logic_vector(NUM_IO_LOG-1 downto 0);
	-- Select for which IO register to output on A
	IO_Src_SelectA          : out std_logic_vector(NUM_IO_LOG-1 downto 0);
	-- Select for which IO register to output on B
	IO_Src_SelectB          : out std_logic_vector(NUM_IO_LOG-1 downto 0);

	-- REGISTER VALUES FROM THE REGISTERS UNIT
	-- Output A from the GP registers
    GP_outA                 : in std_logic_vector(NUM_DATA_BITS-1 downto 0);
	-- Output B from the GP registers
	GP_outB                 : in std_logic_vector(NUM_DATA_BITS-1 downto 0);

	-- Output A from the IO registers
	IO_outA                 : in std_logic_vector(NUM_DATA_BITS-1 downto 0);
	-- Output B from the IO registers
	IO_outB                 : in std_logic_vector(NUM_DATA_BITS-1 downto 0);
	
	-------------------------------------------------------------------
	-- ALU
	-------------------------------------------------------------------
	
	-- ALU control signals
    ALU_result_select			: 
		out std_logic_vector(num_bits_ALU_result_select - 1 downto 0);
    Shifter_low_bit_select		: 
		out std_logic_vector(num_bits_Shifter_low_bit_select - 1 downto 0);
    Shifter_middle_bits_select	: out std_logic;
    Shifter_high_bit_select		: 
		out std_logic_vector(num_bits_Shifter_high_bit_select - 1 downto 0);
    F_Block_Select				: 
		out std_logic_vector(num_bits_F_Block_Select - 1 downto 0);
    Subtract					: out std_logic;
    ALU_op_with_carry			: out std_logic;
    AddSub_Op_1_Select			: 
		out std_logic_vector(num_bits_AddSub_Op_1_Select - 1 downto 0);
    AddSub_Op_2_Select			: 
		out std_logic_vector(num_bits_AddSub_Op_2_Select - 1 downto 0);
	
	OperandA:                   out std_logic_vector(NUM_DATA_BITS - 1 downto 0);
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
	
	-------------------------------------------------------------------
	-- DATA MEMORY ACCESS UNIT
	-------------------------------------------------------------------
	
	-- Data Memory Access Unit Control Signals and values
	-- selects address source
	Data_Addr_Src_Sel		: out std_logic_vector(
								num_bits_Data_Addr_Src_Sel - 1 downto 0);
	-- selects offset source
	Offset_Src_Sel			: out std_logic_vector(
								num_bits_Offset_Src_Sel - 1 downto 0);
	-- contains value of unsigned displacement for Y or Z registers for 
	-- particular instructions which can be selected to be used as the offset
	-- source
	unsigned_displacement	: out std_logic_vector(NUM_ADDRESS_BITS - 1 downto 0);
	-- indicates whether or not pre/post-increment/decrement was 
	-- part of instruction
	Pre_Post_Sel			: out std_logic;
	
	-- 8 bits of data, zero padded upper bits
	Immediate_Data_Address	: out std_logic_vector(NUM_ADDRESS_BITS - 1 downto 0);
	-- current X register contents
	X_register				: out std_logic_vector(NUM_ADDRESS_BITS - 1 downto 0);
	-- current Y register contents
	Y_register				: out std_logic_vector(NUM_ADDRESS_BITS - 1 downto 0);
	-- current Z register contents
	Z_register				: out std_logic_vector(NUM_ADDRESS_BITS - 1 downto 0);
	-- current SP register contents
	SP_register				: out std_logic_vector(NUM_ADDRESS_BITS - 1 downto 0);
	
	-- load immediate signals
	-- indicates whether an LDI operation is happening
	LDI_op					: out std_logic;
	-- immediate value for LDI operation to move into a register
	immed_val				: out std_logic_vector(NUM_DATA_BITS - 1 downto 0);
	
	-- indicates whether or not instruction indicates a store operation
	Store					: out std_logic;
	
	-- active low control line indicating data memory is being read
	-- active only during 2nd half of the clock in the 2nd cycle
	DataRd					: out std_logic;
	DataRdOut				: out std_logic;
	-- active low control line indicating data memory is being written
	-- active only during 2nd half of the clock in the 2nd cycle
	DataWr					: out std_logic;
	DataWrOut				: out std_logic
	
	
    
    );
end entity;

architecture standard of Control_Unit is
	type state_type is (Clock1, Clock2, Clock3);
	signal state: state_type;
	signal next_state: state_type;

begin

	actions: process(IR, GP_outA, GP_outB, IO_outA, IO_outB, state)
	
		variable Regd : std_logic_vector(NUM_DATA_BITS - 1 downto 0);
		
	begin

		-- By default do not enable any of the IO register inputs
		IO_Input_Select  <= '0';
		IO_Write_EnableA <= '0';
		IO_Write_EnableB <= '0';
		IO_Dst_SelectA <= (others => '0');
		IO_Src_SelectA <= (others => '0');
		IO_Src_SelectB <= (others => '0');

		Store <= '0';
	
		-- default values so control signals do not need to be set when
		-- value does not matter 
		ALU_result_select <= F_Block_Operation;
		Shifter_low_bit_select <= Shifter_low_bit_highest_bit;
		Shifter_middle_bits_select <= Shifter_middle_bits_select_immediate_right;
		Shifter_high_bit_select <= Shifter_high_bit_select_second_highest_bit;
		F_Block_Select <= F_Block_Select_0;
		Subtract <= Addition;
		ALU_op_with_carry <= '0';
		AddSub_Op_1_Select <= AddSub_Op_1_Select_OperandA;
		AddSub_Op_2_Select <= AddSub_Op_2_Select_OperandB;
		
		OperandA <= (others => '0');
		OperandB <= (others => '0');

		case state is

			--
			-- CLOCK 1
			--

			when Clock1 =>

				--------------------------------------------------------------
				--------------------------------------------------------------
				-- ALU --
				--------------------------------------------------------------
				--------------------------------------------------------------
				
				-- ADC: add with carry
				if std_match(IR, OpADC) then
				
					Regd := IR(8 downto 4);
					GP_Src_SelectA <= Regd;
					GP_Src_SelectB <= IR(9) & IR(3 downto 0);
					
					-- Control signals to ALU
					-- addition operation
					ALU_result_select <= Adder_Subtractor_Operation;
					Subtract <= Addition;
					-- no add with carry
					ALU_op_with_carry <= '1';
					-- use operands passed in as arguments from Control Unit
					AddSub_Op_1_Select <= AddSub_Op_1_Select_OperandA;
					AddSub_Op_2_Select <= AddSub_Op_2_Select_OperandB;
					
					-- Register d contents
					OperandA <= GP_outA;
					-- Register r contents
					OperandB <= GP_outB;
					
					-- Control signals to Register to store result
					-- indicate register input is from ALU
					GP_Input_Select <= GP_IN_SEL_ALU;
					GP_Write_EnableA <= '1';
					GP_Write_EnableB <= '0';
					-- indicate nibbles of register should not be swapped
					GP_Swap_Nibbles <= '0';
					-- store result in Register d
					GP_Dst_SelectA <= Regd;
					
				end if;
				
				-- ADD addition with registers
				if std_match(IR, OpADD) then
				
					Regd := IR(8 downto 4);
					GP_Src_SelectA <= Regd;
					GP_Src_SelectB <= IR(9) & IR(3 downto 0);
					
					-- Control signals to ALU
					-- addition operation
					ALU_result_select <= Adder_Subtractor_Operation;
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
					-- indicate register input is from ALU
					GP_Input_Select <= GP_IN_SEL_ALU;
					GP_Write_EnableA <= '1';
					GP_Write_EnableB <= '0';
					-- indicate nibbles of register should not be swapped
					GP_Swap_Nibbles <= '0';
					-- store result in Register d
					GP_Dst_SelectA <= Regd;
					
				end if;
				
				-- AND with registers
				if std_match(IR, OpAND) then
					
					Regd := IR(8 downto 4);
					GP_Src_SelectA <= Regd;
					GP_Src_SelectB <= IR(9) & IR(3 downto 0);
					
					-- Control signals to ALU
					-- and operation on F-block
					ALU_result_select <= F_Block_Operation;
					F_Block_Select <= F_Block_Select_and;
					
					-- Register d contents
					OperandA <= GP_outA;
					-- Register r contents
					OperandB <= GP_outB;
					
					-- Control signals to Register to store result
					-- indicate register input is from ALU
					GP_Input_Select <= GP_IN_SEL_ALU;
					GP_Write_EnableA <= '1';
					GP_Write_EnableB <= '0';
					-- indicate nibbles of register should not be swapped
					GP_Swap_Nibbles <= '0';
					-- store result in Register d
					GP_Dst_SelectA <= Regd;
					
				end if;
				
				-- ANDI add immediate
				if std_match(IR, OpANDI) then
				
					Regd := '1' & IR(7 downto 4);;
					GP_Src_SelectA <= Regd;
					
					-- Control signals to ALU
					-- and operation on F-block
					ALU_result_select <= F_Block_Operation;
					F_Block_Select <= F_Block_Select_and;
					
					-- Register d contents
					OperandA <= GP_outA;
					-- K, immediate value
					OperandB <= IR(11 downto 8) & IR(3 downto 0);
					
					-- Control signals to Register to store result
					-- indicate register input is from ALU
					GP_Input_Select <= GP_IN_SEL_ALU;
					GP_Write_EnableA <= '1';
					GP_Write_EnableB <= '0';
					-- indicate nibbles of register should not be swapped
					GP_Swap_Nibbles <= '0';
					-- store result in Register d
					GP_Dst_SelectA <= Regd;
					
				end if;
				
				-- ASR arithmetic shift right a register
				if std_match(IR, OpASR) then
				
					Regd := '1' & IR(7 downto 4);;
					GP_Src_SelectA <= Regd;
					
					-- Control signals to ALU
					-- doing a shift operation
					ALU_result_select <= Shifter_Rotater_Operation;
					-- shifting to the right
					Shifter_low_bit_select <= Shifter_low_bit_bit_1;
					Shifter_middle_bits_select <=
						Shifter_middle_bits_select_immediate_left;
					-- arithmetic shift, preserve high bit
					Shifter_high_bit_select <= Shifter_high_bit_select_highest_bit;
					
					-- Register d contents
					OperandA <= GP_outA;
					
					-- Control signals to Register to store result
					-- indicate register input is from ALU
					GP_Input_Select <= GP_IN_SEL_ALU;
					GP_Write_EnableA <= '1';
					GP_Write_EnableB <= '0';
					-- indicate nibbles of register should not be swapped
					GP_Swap_Nibbles <= '0';
					-- store result in Register d
					GP_Dst_SelectA <= Regd;
					
				end if;
				
				-- BCLR bit clear in the status register
				if std_match(IR, OpBCLR) then
				
				end if;
				
				-- BLD bit load in a general purpose register
				if std_match(IR, OpBLD) then
					
				end if;
				
				-- BSET bit set in the status register
				if std_match(IR, OpBSET) then

				end if;
				
				-- BST bit set in a general purpose register
				if std_match(IR, OpBST) then

				end if;
				
				-- COM complement of a register (register <- not register)
				if std_match(IR, OpCOM) then
					
					Regd := '1' & IR(7 downto 4);;
					GP_Src_SelectB <= Regd;
					
					-- Control signals to ALU
					-- subtraction operation
					ALU_result_select <= Adder_Subtractor_Operation;
					-- subtraction operation
					Subtract <= Subtraction;
					-- not subtract with carry
					ALU_op_with_carry <= '0';
					-- to not the register, do FF - register value
					AddSub_Op_1_Select <= AddSub_Op_1_Select_FF;
					AddSub_Op_2_Select <= AddSub_Op_2_Select_OperandB;
					
					-- Register d contents
					OperandB <= GP_outB;
					
					-- Control signals to Register to store result
					-- indicate register input is from ALU
					GP_Input_Select <= GP_IN_SEL_ALU;
					GP_Write_EnableA <= '1';
					GP_Write_EnableB <= '0';
					-- indicate nibbles of register should not be swapped
					GP_Swap_Nibbles <= '0';
					-- store result in Register d
					GP_Dst_SelectA <= Regd;
					
				end if;
				
				-- CP compare registers
				if std_match(IR, OpCP) then
					
					Regd := IR(8 downto 4);
					GP_Src_SelectA <= Regd;
					GP_Src_SelectB <= IR(9) & IR(3 downto 0);
					
					-- Control signals to ALU
					-- subtraction operation
					ALU_result_select <= Adder_Subtractor_Operation;
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
					
					-- Control signals to Register to store result
					-- do not store result back in registers
					GP_Write_EnableA <= '0';
					GP_Write_EnableB <= '0';
					-- indicate nibbles of register should not be swapped
					GP_Swap_Nibbles <= '0';
					-- store result in Register d
					
				end if;
				
				-- CPC compare registers with carry
				if std_match(IR, OpCPC) then
					
					Regd := IR(8 downto 4);
					GP_Src_SelectA <= Regd;
					GP_Src_SelectB <= IR(9) & IR(3 downto 0);
					
					-- Control signals to ALU
					-- subtraction operation
					ALU_result_select <= Adder_Subtractor_Operation;
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
					-- do not store result back in registers
					GP_Write_EnableA <= '0';
					GP_Write_EnableB <= '0';
					-- indicate nibbles of register should not be swapped
					GP_Swap_Nibbles <= '0';
					-- store result in Register d
					
				end if;
				
				-- CPI compare register with immediate value
				if std_match(IR, OpCPI) then
					
					Regd := '1' & IR(7 downto 4);
					GP_Src_SelectA <= Regd;
					
					-- Control signals to ALU
					-- subtraction operation
					ALU_result_select <= Adder_Subtractor_Operation;
					Subtract <= Subtraction;
					-- no subtract with carry
					ALU_op_with_carry <= '0';
					-- use operands passed in as arguments from Control Unit
					AddSub_Op_1_Select <= AddSub_Op_1_Select_OperandA;
					AddSub_Op_2_Select <= AddSub_Op_2_Select_OperandB;
					
					-- Register d contents
					OperandA <= GP_outA;
					OperandB <= IR(11 downto 8) & IR(3 downto 0);
					
					-- Control signals to Register to store result
					-- do not store result back in registers
					GP_Write_EnableA <= '0';
					GP_Write_EnableB <= '0';
					-- indicate nibbles of register should not be swapped
					GP_Swap_Nibbles <= '0';
					-- store result in Register d
					
				end if;
				
				-- DEC decrement a register
				if std_match(IR, OpDEC) then
				
					Regd := IR(8 downto 4);;
					GP_Src_SelectA <= Regd;
					
					-- Control signals to ALU
					-- subtraction operation
					ALU_result_select <= Adder_Subtractor_Operation;
					-- subtraction operation
					Subtract <= Subtraction;
					-- not subtract with carry
					ALU_op_with_carry <= '0';
					-- use Register d and 1 as the operands
					AddSub_Op_1_Select <= AddSub_Op_1_Select_OperandA;
					AddSub_Op_2_Select <= AddSub_Op_2_Select_1;
					
					-- Register d contents
					OperandA <= GP_outA;
					
					-- Control signals to Register to store result
					-- indicate register input is from ALU
					GP_Input_Select <= GP_IN_SEL_ALU;
					GP_Write_EnableA <= '1';
					GP_Write_EnableB <= '0';
					-- indicate nibbles of register should not be swapped
					GP_Swap_Nibbles <= '0';
					-- store result in Register d
					GP_Dst_SelectA <= Regd;
				
				end if;
				
				-- EOR xor registers
				if std_match(IR, OpEOR) then
					
					Regd := IR(8 downto 4);
					GP_Src_SelectA <= Regd;
					GP_Src_SelectB <= IR(9) & IR(3 downto 0);
					
					-- Control signals to ALU
					-- F Block operation
					ALU_result_select <= F_Block_Operation;
					-- F Block xor operation
					F_Block_Select <= F_Block_Select_xor;
					
					-- Register d contents
					OperandA <= GP_outA;
					-- Register g contents
					OperandB <= GP_outB;
					
					-- Control signals to Register to store result
					-- indicate register input is from ALU
					GP_Input_Select <= GP_IN_SEL_ALU;
					GP_Write_EnableA <= '1';
					GP_Write_EnableB <= '0';
					-- indicate nibbles of register should not be swapped
					GP_Swap_Nibbles <= '0';
					-- store result in Register d
					GP_Dst_SelectA <= Regd;
					
				end if;
				
				-- INC increment a register
				if std_match(IR, OpINC) then
				
					Regd := IR(8 downto 4);
					GP_Src_SelectA <= Regd;
					
					-- Control signals to ALU
					-- addition operation
					ALU_result_select <= Adder_Subtractor_Operation;
					-- addition operation
					Subtract <= Addition;
					-- no add with carry
					ALU_op_with_carry <= '0';
					-- Register d and 1 are operands for increment
					AddSub_Op_1_Select <= AddSub_Op_1_Select_OperandA;
					AddSub_Op_2_Select <= AddSub_Op_2_Select_1;
					
					-- Register d contents
					OperandA <= GP_outA;
					
					-- Control signals to Register to store result
					-- indicate register input is from ALU
					GP_Input_Select <= GP_IN_SEL_ALU;
					GP_Write_EnableA <= '1';
					GP_Write_EnableB <= '0';
					-- indicate nibbles of register should not be swapped
					GP_Swap_Nibbles <= '0';
					-- store result in Register d
					GP_Dst_SelectA <= Regd;
				
				end if;
				
				-- LSR logical shift right a register
				if std_match(IR, OpLSR) then
					
					Regd := IR(8 downto 4);
					GP_Src_SelectA <= Regd;
					
					-- Control signals to ALU
					-- shifter operation
					ALU_result_select <= Shifter_Rotater_Operation;
					-- logical shift right
					Shifter_low_bit_select <= Shifter_low_bit_bit_1;
					Shifter_middle_bits_select <= 
						Shifter_middle_bits_select_immediate_left;
					-- logical shift does not preserve high bit
					Shifter_high_bit_select <= Shifter_high_bit_select_0;
					
					-- Register d contents
					OperandA <= GP_outA;
					
					-- Control signals to Register to store result
					-- indicate register input is from ALU
					GP_Input_Select <= GP_IN_SEL_ALU;
					GP_Write_EnableA <= '1';
					GP_Write_EnableB <= '0';
					-- indicate nibbles of register should not be swapped
					GP_Swap_Nibbles <= '0';
					-- store result in Register d
					GP_Dst_SelectA <= Regd;
					
				end if;
				
				-- NEG negate a register
				if std_match(IR, OpNEG) then
					
					Regd := IR(8 downto 4);
					GP_Src_SelectB <= Regd;
					
					-- Control signals to ALU
					-- subtraction operation
					ALU_result_select <= Adder_Subtractor_Operation;
					-- subtraction operation
					Subtract <= Subtraction;
					-- no subtract with carry
					ALU_op_with_carry <= '0';
					-- subtract Register d from 0 for negate
					AddSub_Op_1_Select <= AddSub_Op_1_Select_0;
					AddSub_Op_2_Select <= AddSub_Op_2_Select_OperandB;
				
					-- Register d contents
					OperandB <= GP_outB;
					
					-- Control signals to Register to store result
					-- indicate register input is from ALU
					GP_Input_Select <= GP_IN_SEL_ALU;
					GP_Write_EnableA <= '1';
					GP_Write_EnableB <= '0';
					-- indicate nibbles of register should not be swapped
					GP_Swap_Nibbles <= '0';
					-- store result in Register d
					GP_Dst_SelectA <= Regd;
					
				end if;
				
				-- OR or registers
				if std_match(IR, OpOR) then
					
					Regd := IR(8 downto 4);
					GP_Src_SelectA <= Regd;
					GP_Src_SelectB <= IR(9) & IR(3 downto 0);
					
					-- Control signals to ALU
					-- F Block operation
					ALU_result_select <= F_Block_Operation;
					-- F Block or operation
					F_Block_Select <= F_Block_Select_or;

					-- Register d contents
					OperandA <= GP_outA;
					-- Register r contents
					OperandB <= GP_outB;
					
					-- Control signals to Register to store result
					-- indicate register input is from ALU
					GP_Input_Select <= GP_IN_SEL_ALU;
					GP_Write_EnableA <= '1';
					GP_Write_EnableB <= '0';
					-- indicate nibbles of register should not be swapped
					GP_Swap_Nibbles <= '0';
					-- store result in Register d
					GP_Dst_SelectA <= Regd;
					
				end if;
			
				-- ORI or register with an immediate value
				if std_match(IR, OpORI) then
					
					Regd := '1' & IR(7 downto 4);
					GP_Src_SelectA <= Regd;
					
					-- Control signals to ALU
					-- F Block operation
					ALU_result_select <= F_Block_Operation;
					-- F Block or operation
					F_Block_Select <= F_Block_Select_or;

					-- Register d contents
					OperandA <= GP_outA;
					-- Register r contents
					OperandB <= IR(11 downto 8) & IR(3 downto 0);
					
					-- Control signals to Register to store result
					-- indicate register input is from ALU
					GP_Input_Select <= GP_IN_SEL_ALU;
					GP_Write_EnableA <= '1';
					GP_Write_EnableB <= '0';
					-- indicate nibbles of register should not be swapped
					GP_Swap_Nibbles <= '0';
					-- store result in Register d
					GP_Dst_SelectA <= Regd;
					
				end if;
			
				-- ROR rotate right a register
				if std_match(IR, OpROR) then
				
					Regd := IR(8 downto 4);
					GP_Src_SelectA <= Regd;
					
					-- Control signals to ALU
					-- rotate operation
					ALU_result_select <= Shifter_Rotater_Operation;
					-- rotate right bit updates
					Shifter_low_bit_select <= Shifter_low_bit_bit_1;
					Shifter_middle_bits_select <=
						Shifter_middle_bits_select_immediate_left;
					Shifter_high_bit_select <= Shifter_high_bit_select_lowest_bit;

					-- Register d contents
					OperandA <= GP_outA;
					
					-- Control signals to Register to store result
					-- indicate register input is from ALU
					GP_Input_Select <= GP_IN_SEL_ALU;
					GP_Write_EnableA <= '1';
					GP_Write_EnableB <= '0';
					-- indicate nibbles of register should not be swapped
					GP_Swap_Nibbles <= '0';
					-- store result in Register d
					GP_Dst_SelectA <= Regd;
					
				end if;
				
				-- SBC subtract registers with carry
				if std_match(IR, OpSBC) then
				
					Regd := IR(8 downto 4);
					GP_Src_SelectA <= Regd;
					GP_Src_SelectB <= IR(9) & IR(3 downto 0);
					
					-- Control signals to ALU
					-- subtraction operation
					ALU_result_select <= Adder_Subtractor_Operation;
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
					-- indicate register input is from ALU
					GP_Input_Select <= GP_IN_SEL_ALU;
					GP_Write_EnableA <= '1';
					GP_Write_EnableB <= '0';
					-- indicate nibbles of register should not be swapped
					GP_Swap_Nibbles <= '0';
					-- store result in Register d
					GP_Dst_SelectA <= Regd;
				
				end if;
				
				-- SBCI subtract register and immediate value with carry
				if std_match(IR, OpSBCI) then
					
					Regd := IR(8 downto 4);
					GP_Src_SelectA <= Regd;
					
					-- Control signals to ALU
					-- subtraction operation
					ALU_result_select <= Adder_Subtractor_Operation;
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
					OperandB <= IR(11 downto 8) & IR(3 downto 0);
					
					-- Control signals to Register to store result
					-- indicate register input is from ALU
					GP_Input_Select <= GP_IN_SEL_ALU;
					GP_Write_EnableA <= '1';
					GP_Write_EnableB <= '0';
					-- indicate nibbles of register should not be swapped
					GP_Swap_Nibbles <= '0';
					-- store result in Register d
					GP_Dst_SelectA <= Regd;
					
				end if;
				
				-- SUB subtract registers
				if std_match(IR, OpSUB) then
					
					Regd := IR(8 downto 4);
					GP_Src_SelectA <= Regd;
					GP_Src_SelectB <= IR(9) & IR(3 downto 0);
					
					-- Control signals to ALU
					-- subtraction operation
					ALU_result_select <= Adder_Subtractor_Operation;
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
					
					-- Control signals to Register to store result
					-- indicate register input is from ALU
					GP_Input_Select <= GP_IN_SEL_ALU;
					GP_Write_EnableA <= '1';
					GP_Write_EnableB <= '0';
					-- indicate nibbles of register should not be swapped
					GP_Swap_Nibbles <= '0';
					-- store result in Register d
					GP_Dst_SelectA <= Regd;
					
				end if;
			
				-- SUBI subtract immediate value from register
				if std_match(IR, OpSUBI) then
					
					Regd := '1' & IR(7 downto 4);
					GP_Src_SelectA <= Regd;
					
					-- Control signals to ALU
					-- subtraction operation
					ALU_result_select <= Adder_Subtractor_Operation;
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
					OperandB <= IR(11 downto 8) & IR(3 downto 0);
					
					-- Control signals to Register to store result
					-- indicate register input is from ALU
					GP_Input_Select <= GP_IN_SEL_ALU;
					GP_Write_EnableA <= '1';
					GP_Write_EnableB <= '0';
					-- indicate nibbles of register should not be swapped
					GP_Swap_Nibbles <= '0';
					-- store result in Register d
					GP_Dst_SelectA <= Regd;
					
				end if;
				
				-- SWAP swap nibbles of a register
				if std_match(IR, OpSWAP) then
					
					Regd := '1' & IR(7 downto 4);
					GP_Src_SelectA <= Regd;
				
					-- Register d contents
					OperandA <= GP_outA;
					
					-- indicate nibbles of register should not be swapped
					GP_Swap_Nibbles <= SWAP_EN;
					
				end if;
				
				-- ADIW add immediate to word
				if std_match(IR, OpADIW) then
					-- choose which register to use depending on instruction decoding
					if IR(5 downto 4) = "00" then
						Regd := "11000";
					end if;
					
					if IR(5 downto 4) = "01" then
						Regd := "11010";
					end if;
					
					if IR(5 downto 4) = "10" then
						Regd := "11100";
					end if;
					
					if IR(5 downto 4) = "11" then
						Regd := "11110";
					end if;
					
					GP_Src_SelectA <= Regd;
					
					-- Control signals to ALU
					-- addition operation
					ALU_result_select <= Adder_Subtractor_Operation;
					-- addition
					Subtract <= Addition;
					-- no add with carry
					ALU_op_with_carry <= '0';
					-- use operands passed in as arguments from Control Unit
					AddSub_Op_1_Select <= AddSub_Op_1_Select_OperandA;
					AddSub_Op_2_Select <= AddSub_Op_2_Select_OperandB;
				
					-- Register d contents
					OperandA <= GP_outA;
					-- K, immediate value from IR
					OperandB <= "00" & IR(7 downto 6) & IR(3 downto 0);
					
					-- Control signals to Register to store result
					-- indicate register input is from ALU
					GP_Input_Select <= GP_IN_SEL_ALU;
					GP_Write_EnableA <= '1';
					GP_Write_EnableB <= '0';
					-- indicate nibbles of register should not be swapped
					GP_Swap_Nibbles <= '0';
					-- store result in Register d
					GP_Dst_SelectA <= Regd;
					
				end if;
			
				-- SBIW subtract immediate to word
				if std_match(IR, OpSBIW) then
					-- choose which register to use depending on instruction decoding
					if IR(5 downto 4) = "00" then
						Regd := "11000";
					end if;
					
					if IR(5 downto 4) = "01" then
						Regd := "11010";
					end if;
					
					if IR(5 downto 4) = "10" then
						Regd := "11100";
					end if;
					
					if IR(5 downto 4) = "11" then
						Regd := "11110";
					end if;
					
					GP_Src_SelectA <= Regd;
					
					-- Control signals to ALU
					-- addition operation
					ALU_result_select <= Adder_Subtractor_Operation;
					-- subtract operation
					Subtract <= Subtraction;
					-- no add with carry
					ALU_op_with_carry <= '0';
					-- use operands passed in as arguments from Control Unit
					AddSub_Op_1_Select <= AddSub_Op_1_Select_OperandA;
					AddSub_Op_2_Select <= AddSub_Op_2_Select_OperandB;
				
					-- Register d contents
					OperandA <= GP_outA;
					-- K, immediate value from IR
					OperandB <= "00" & IR(7 downto 6) & IR(3 downto 0);
					
					-- Control signals to Register to store result
					-- indicate register input is from ALU
					GP_Input_Select <= GP_IN_SEL_ALU;
					GP_Write_EnableA <= '1';
					GP_Write_EnableB <= '0';
					-- indicate nibbles of register should not be swapped
					GP_Swap_Nibbles <= '0';
					-- store result in Register d
					GP_Dst_SelectA <= Regd;
					
				end if;
				
				--------------------------------------------------------------
				--------------------------------------------------------------
				-- DATA MEMORY ACCESS UNIT --
				--------------------------------------------------------------
				--------------------------------------------------------------
				
				if std_match(IR, OpLDI) then
					-- LDI operation
					LDI_op <= '1';
					-- get register to load immediate value into
					GP_Dst_SelectA <= 
						'1' & IR(DMAU_Reg_high_bit - 1 downto DMAU_Reg_low_bit);
					-- get immediate value to load into register
					immed_val <= IR(IMMED_VAL_HIGH_BYTE1 downto IMMED_VAL_LOW_BYTE1)
								& IR(IMMED_VAL_HIGH_BYTE2 downto IMMED_VAL_LOW_BYTE2);
					-- proceed to next instruction (instruction over)
					GP_Write_EnableA <= '1';
					GP_Write_EnableB <= '0';
					DataRdOut <= '0';
					DataWrOut <= '1';
					GP_Input_Select <= GP_IN_SEL_DATA_DATABUS;

					-- Not Storing
					Store <= '0';

				end if;

				if std_match(IR, OpLDX) then
					-- not a LDI operation
					LDI_op <= '0';
					-- not reading or writing data right now
					DataRdOut <= '1';
					DataWrOut <= '1';
					-- X register operation
					Data_Addr_Src_Sel <= Data_Addr_Src_Sel_X;
					-- not modifying X
					Offset_Src_Sel <= Offset_Src_Sel_0;
					-- value does not matter
					unsigned_displacement <= (others => '0');
					-- address is not changed
					Pre_Post_Sel <= Pre_Post_Sel_Pre;
					-- value does not matter
					Immediate_Data_Address <= (others => '0');
					-- get current X register value
					GP_Src_SelectA <= X_REG_HIGH_BYTE;
					GP_Src_SelectB <= X_REG_LOW_BYTE;
					X_register <= GP_outA & GP_outB;
					-- value does not matter
					Y_register <= (others => '0');
					-- value does not matter
					Z_register <= (others => '0');
					-- value does not matter
					SP_register <= (others => '0');
					GP_Input_Select <= GP_IN_SEL_DATA_DATABUS;

					-- No writing
					GP_Write_EnableA <= '0';
					GP_Write_EnableB <= '0';

				end if;

				if std_match(IR, OpLDXI) then
					-- not a LDI operation
					LDI_op <= '0';
					-- not reading or writing data right now
					DataRdOut <= '1';
					DataWrOut <= '1';
					-- X register operation
					Data_Addr_Src_Sel <= Data_Addr_Src_Sel_X;
					-- incrementing X
					Offset_Src_Sel <= Offset_Src_Sel_pos_1;
					-- value does not matter
					unsigned_displacement <= (others => '0');
					-- post-increment
					Pre_Post_Sel <= Pre_Post_Sel_Post;
					-- value does not matter
					Immediate_Data_Address <= (others => '0');
					-- get current X register value
					GP_Src_SelectA <= X_REG_HIGH_BYTE;
					GP_Src_SelectB <= X_REG_LOW_BYTE;
					X_register <= GP_outA & GP_outB;
					-- value does not matter
					Y_register <= (others => '0');
					-- value does not matter
					Z_register <= (others => '0');
					-- value does not matter
					SP_register <= (others => '0');
					GP_Input_Select <= GP_IN_SEL_DATA_DATABUS;

					-- No writing
					GP_Write_EnableA <= '0';
					GP_Write_EnableB <= '0';
				end if;
				
				if std_match(IR, OpLDXD) then
					-- not a LDI operation
					LDI_op <= '0';
					-- not reading or writing data right now
					DataRdOut <= '1';
					DataWrOut <= '1';
					-- X register operation
					Data_Addr_Src_Sel <= Data_Addr_Src_Sel_X;
					-- decrementing X
					Offset_Src_Sel <= Offset_Src_Sel_neg_1;
					-- value does not matter
					unsigned_displacement <= (others => '0');
					Pre_Post_Sel <= Pre_Post_Sel_Pre;
					-- value does not matter
					Immediate_Data_Address <= (others => '0');
					GP_Src_SelectA <= X_REG_HIGH_BYTE;
					GP_Src_SelectB <= X_REG_LOW_BYTE;
					X_register <= GP_outA & GP_outB;
					-- value does not matter
					Y_register <= (others => '0');
					-- value does not matter
					Z_register <= (others => '0');
					-- value does not matter
					SP_register <= (others => '0');
					GP_Input_Select <= GP_IN_SEL_DATA_DATABUS;
					
					-- No writing
					GP_Write_EnableA <= '0';
					GP_Write_EnableB <= '0';
				end if;

				-- Store X
				if std_match(IR, OpSTX) then
					-- not a LDI operation
					LDI_op <= '0';
					-- not reading or writing data right now
					DataRdOut <= '1';
					DataWrOut <= '1';
					-- X register operation
					Data_Addr_Src_Sel <= Data_Addr_Src_Sel_X;
					-- not changing X
					Offset_Src_Sel <= Offset_Src_Sel_0;
					-- value does not matter
					unsigned_displacement <= (others => '0');
					-- save current value of X, no change
					Pre_Post_Sel <= Pre_Post_Sel_Pre;
					-- value does not matter
					Immediate_Data_Address <= (others => '0');
					-- get current X register value
					GP_Src_SelectA <= X_REG_HIGH_BYTE;
					GP_Src_SelectB <= X_REG_LOW_BYTE;
					X_register <= GP_outA & GP_outB;
					-- value does not matter
					Y_register <= (others => '0');
					-- value does not matter
					Z_register <= (others => '0');
					-- value does not matter
					SP_register <= (others => '0');
					-- access data data bus
					GP_Input_Select <= GP_IN_SEL_DATA_DATABUS;

					-- No writing
					GP_Write_EnableA <= '0';
					GP_Write_EnableB <= '0';
				end if;
				
				if std_match(IR, OpSTXI) then
					-- not a LDI operation
					LDI_op <= '0';
					-- not reading or writing data right now
					DataRdOut <= '1';
					DataWrOut <= '1';
					-- X register operation
					Data_Addr_Src_Sel <= Data_Addr_Src_Sel_X;
					-- incrementing X
					Offset_Src_Sel <= Offset_Src_Sel_pos_1;
					-- value does not matter
					unsigned_displacement <= (others => '0');
					-- post increment
					Pre_Post_Sel <= Pre_Post_Sel_Post;
					-- value does not matter
					Immediate_Data_Address <= (others => '0');
					-- get current X register value
					GP_Src_SelectA <= X_REG_HIGH_BYTE;
					GP_Src_SelectB <= X_REG_LOW_BYTE;
					X_register <= GP_outA & GP_outB;
					-- value does not matter
					Y_register <= (others => '0');
					-- value does not matter
					Z_register <= (others => '0');
					-- value does not matter
					SP_register <= (others => '0');
					-- access data data bus
					GP_Input_Select <= GP_IN_SEL_DATA_DATABUS;

					-- No writing
					GP_Write_EnableA <= '0';
					GP_Write_EnableB <= '0';					
				end if;
				
				if std_match(IR, OpSTXD) then
					-- not a LDI operation
					LDI_op <= '0';
					-- not reading or writing data right now
					DataRdOut <= '1';
					DataWrOut <= '1';
					Data_Addr_Src_Sel <= Data_Addr_Src_Sel_X;
					-- decremnting X by 1
					Offset_Src_Sel <= Offset_Src_Sel_neg_1;
					-- value does not matter
					unsigned_displacement <= (others => '0');
					-- pre decrement
					Pre_Post_Sel <= Pre_Post_Sel_Pre;
					-- value does not matter
					Immediate_Data_Address <= (others => '0');
					-- get current X register value
					GP_Src_SelectA <= X_REG_HIGH_BYTE;
					GP_Src_SelectB <= X_REG_LOW_BYTE;
					X_register <= GP_outA & GP_outB;
					-- value does not matter
					Y_register <= (others => '0');
					-- value does not matter
					Z_register <= (others => '0');
					-- value does not matter
					SP_register <= (others => '0');
					-- access data data bus
					GP_Input_Select <= GP_IN_SEL_DATA_DATABUS;

					-- No writing
					GP_Write_EnableA <= '0';
					GP_Write_EnableB <= '0';					
				end if;

				-- Load Y

				if std_match(IR, OpLDYI) then
					-- not a LDI operation
					LDI_op <= '0';
					-- not reading or writing data right now
					DataRdOut <= '1';
					DataWrOut <= '1';
					-- Y register operation
					Data_Addr_Src_Sel <= Data_Addr_Src_Sel_Y;
					-- incrementing Y
					Offset_Src_Sel <= Offset_Src_Sel_pos_1;
					-- value does not matter
					unsigned_displacement <= (others => '0');
					-- post increment
					Pre_Post_Sel <= Pre_Post_Sel_Post;
					-- value does not matter
					Immediate_Data_Address <= (others => '0');
					-- value does not matter
					X_register <= (others => '0');
					-- get current Y register value
					GP_Src_SelectA <= Y_REG_HIGH_BYTE;
					GP_Src_SelectB <= Y_REG_LOW_BYTE;
					Y_register <= GP_outA & GP_outB;
					-- value does not matter
					Z_register <= (others => '0');
					-- value does not matter
					SP_register <= (others => '0');
					-- access data data bus
					GP_Input_Select <= GP_IN_SEL_DATA_DATABUS;

					-- No writing
					GP_Write_EnableA <= '0';
					GP_Write_EnableB <= '0';
				end if;
				
				if std_match(IR, OpLDYD) then
					-- not a LDI operation
					LDI_op <= '0';
					-- not reading or writing data right now
					DataRdOut <= '1';
					DataWrOut <= '1';
					-- Y register operation
					Data_Addr_Src_Sel <= Data_Addr_Src_Sel_Y;
					-- decrementing Y
					Offset_Src_Sel <= Offset_Src_Sel_neg_1;
					-- value does not matter
					unsigned_displacement <= (others => '0');
					-- pre decrement
					Pre_Post_Sel <= Pre_Post_Sel_Pre;
					-- value does not matter
					Immediate_Data_Address <= (others => '0');
					-- value does not matter
					X_register <= (others => '0');
					-- get current Y register value
					GP_Src_SelectA <= Y_REG_HIGH_BYTE;
					GP_Src_SelectB <= Y_REG_LOW_BYTE;
					Y_register <= GP_outA & GP_outB;
					-- value does not matter
					Z_register <= (others => '0');
					-- value does not matter
					SP_register <= (others => '0');
					-- access data data bus
					GP_Input_Select <= GP_IN_SEL_DATA_DATABUS;

					-- No writing
					GP_Write_EnableA <= '0';
					GP_Write_EnableB <= '0';
				end if;
				
				if std_match(IR, OpLDDY) then
					-- not a LDI operation
					LDI_op <= '0';
					-- not reading or writing data right now
					DataRdOut <= '1';
					DataWrOut <= '1';
					-- Y register operation
					Data_Addr_Src_Sel <= Data_Addr_Src_Sel_Y;
					-- adding unsiged offset to Y
					Offset_Src_Sel <= Offset_Src_Sel_unsigned_q;
					unsigned_displacement(5 downto 0) <= (IR(13) & IR(11 downto 10) & IR(2 downto 0));
					unsigned_displacement(7 downto 6) <= "00";
					Pre_Post_Sel <= Pre_Post_Sel_Pre;
					-- value does not matter
					Immediate_Data_Address <= (others => '0');
					-- value does not matter
					X_register <= (others => '0');
					-- get current Y register value
					GP_Src_SelectA <= Y_REG_HIGH_BYTE;
					GP_Src_SelectB <= Y_REG_LOW_BYTE;
					Y_register <= GP_outA & GP_outB;
					-- value does not matter
					Z_register <= (others => '0');
					-- value does not matter
					SP_register <= (others => '0');
					-- access data data bus
					GP_Input_Select <= GP_IN_SEL_DATA_DATABUS;

					-- No writing
					GP_Write_EnableA <= '0';
					GP_Write_EnableB <= '0';
				end if;

				-- Store Y
				if std_match(IR, OpSTYI) then
					-- not a LDI operation
					LDI_op <= '0';
					-- not reading or writing data right now
					DataRdOut <= '1';
					DataWrOut <= '1';
					-- Y register operation
					Data_Addr_Src_Sel <= Data_Addr_Src_Sel_Y;
					-- incrementing Y
					Offset_Src_Sel <= Offset_Src_Sel_pos_1;
					-- value does not matter
					unsigned_displacement <= (others => '0');
					-- post increment
					Pre_Post_Sel <= Pre_Post_Sel_Post;
					-- value does not matter
					Immediate_Data_Address <= (others => '0');
					-- value does not matter
					X_register <= (others => '0');
					-- get current Y register value
					GP_Src_SelectA <= Y_REG_HIGH_BYTE;
					GP_Src_SelectB <= Y_REG_LOW_BYTE;
					Y_register <= GP_outA & GP_outB;
					-- value does not matter
					Z_register <= (others => '0');
					-- value does not matter
					SP_register <= (others => '0');
					-- access data data bus
					GP_Input_Select <= GP_IN_SEL_DATA_DATABUS;

					-- No writing
					GP_Write_EnableA <= '0';
					GP_Write_EnableB <= '0';
				end if;
				
				if std_match(IR, OpSTYD) then
					-- not a LDI operation
					LDI_op <= '0';
					-- not reading or writing data right now
					DataRdOut <= '1';
					DataWrOut <= '1';
					-- Y register operation
					Data_Addr_Src_Sel <= Data_Addr_Src_Sel_Y;
					-- decrementing Y
					Offset_Src_Sel <= Offset_Src_Sel_neg_1;
					-- value does not matter
					unsigned_displacement <= (others => '0');
					-- pre decrement
					Pre_Post_Sel <= Pre_Post_Sel_Pre;
					-- value does not matter
					Immediate_Data_Address <= (others => '0');
					-- value does not matter
					X_register <= (others => '0');
					-- get current Y register value
					GP_Src_SelectA <= Y_REG_HIGH_BYTE;
					GP_Src_SelectB <= Y_REG_LOW_BYTE;
					Y_register <= GP_outA & GP_outB;
					-- value does not matter
					Z_register <= (others => '0');
					-- value does not matter
					SP_register <= (others => '0');
					-- access data data bus
					GP_Input_Select <= GP_IN_SEL_DATA_DATABUS;

					-- No writing
					GP_Write_EnableA <= '0';
					GP_Write_EnableB <= '0';
				end if;
				
				if std_match(IR, OpSTDY) then
					-- not a LDI operation
					LDI_op <= '0';
					-- not a LDI operation
					DataRdOut <= '1';
					DataWrOut <= '1';
					-- not a LDI operation
					Data_Addr_Src_Sel <= Data_Addr_Src_Sel_Y;
					-- adding unsiged offset to Y
					Offset_Src_Sel <= Offset_Src_Sel_unsigned_q;
					unsigned_displacement(5 downto 0) <= (IR(13) & IR(11 downto 10) & IR(2 downto 0));
					unsigned_displacement(7 downto 6) <= "00";
					Pre_Post_Sel <= Pre_Post_Sel_Pre;
					-- value does not matter
					Immediate_Data_Address <= (others => '0');
					-- value does not matter
					X_register <= (others => '0');
					-- get current Y register value
					GP_Src_SelectA <= Y_REG_HIGH_BYTE;
					GP_Src_SelectB <= Y_REG_LOW_BYTE;
					Y_register <= GP_outA & GP_outB;
					-- value does not matter
					Z_register <= (others => '0');
					-- value does not matter
					SP_register <= (others => '0');
					-- access data data bus
					GP_Input_Select <= GP_IN_SEL_DATA_DATABUS;

					-- No writing
					GP_Write_EnableA <= '0';
					GP_Write_EnableB <= '0';
				end if;				

				-- Load Z
				if std_match(IR, OpLDZI) then
					-- not a LDI operation
					LDI_op <= '0';
					-- not reading or writing data right now
					DataRdOut <= '1';
					DataWrOut <= '1';
					-- Z register operation
					Data_Addr_Src_Sel <= Data_Addr_Src_Sel_Z;
					-- incrementing Z
					Offset_Src_Sel <= Offset_Src_Sel_pos_1;
					-- value does not matter
					unsigned_displacement <= (others => '0');
					-- post increment
					Pre_Post_Sel <= Pre_Post_Sel_Post;
					-- value does not matter
					Immediate_Data_Address <= (others => '0');
					-- value does not matter
					X_register <= (others => '0');
					-- value does not matter
					Y_register <= (others => '0');
					-- get current Z register value
					GP_Src_SelectA <= Z_REG_HIGH_BYTE;
					GP_Src_SelectB <= Z_REG_LOW_BYTE;
					Z_register <= GP_outA & GP_outB;
					-- value does not matter
					SP_register <= (others => '0');
					-- access data data bus
					GP_Input_Select <= GP_IN_SEL_DATA_DATABUS;

					-- No writing
					GP_Write_EnableA <= '0';
					GP_Write_EnableB <= '0';
				end if;
				
				if std_match(IR, OpLDZD) then
					-- not a LDI operation
					LDI_op <= '0';
					-- not reading or writing data right now
					DataRdOut <= '1';
					DataWrOut <= '1';
					-- Z register operation
					Data_Addr_Src_Sel <= Data_Addr_Src_Sel_Z;
					-- decrementing Z
					Offset_Src_Sel <= Offset_Src_Sel_neg_1;
					-- value does not matter
					unsigned_displacement <= (others => '0');
					-- pre decrement
					Pre_Post_Sel <= Pre_Post_Sel_Pre;
					-- value does not matter
					Immediate_Data_Address <= (others => '0');
					-- value does not matter
					X_register <= (others => '0');
					-- value does not matter
					Y_register <= (others => '0');
					-- get current Z register value
					GP_Src_SelectA <= Z_REG_HIGH_BYTE;
					GP_Src_SelectB <= Z_REG_LOW_BYTE;
					Z_register <= GP_outA & GP_outB;
					SP_register <= (others => '0');
					GP_Input_Select <= GP_IN_SEL_DATA_DATABUS;

					-- No writing
					GP_Write_EnableA <= '0';
					GP_Write_EnableB <= '0';
				end if;
				
				if std_match(IR, OpLDDZ) then
					-- not a LDI operation
					LDI_op <= '0';
					-- not reading or writing data right now
					DataRdOut <= '1';
					DataWrOut <= '1';
					-- Z register operation
					Data_Addr_Src_Sel <= Data_Addr_Src_Sel_Z;
					-- adding unsiged offset to Z
					Offset_Src_Sel <= Offset_Src_Sel_unsigned_q;
					unsigned_displacement(5 downto 0) <= (IR(13) & IR(11 downto 10) & IR(2 downto 0));
					unsigned_displacement(7 downto 6) <= "00";
					Pre_Post_Sel <= Pre_Post_Sel_Pre;
					-- value does not matter
					Immediate_Data_Address <= (others => '0');
					-- value does not matter
					X_register <= (others => '0');
					-- value does not matter
					Y_register <= (others => '0');
					-- get current Z register value
					GP_Src_SelectA <= Z_REG_HIGH_BYTE;
					GP_Src_SelectB <= Z_REG_LOW_BYTE;
					Z_register <= GP_outA & GP_outB;
					-- value does not matter
					SP_register <= (others => '0');
					-- access data data bus
					GP_Input_Select <= GP_IN_SEL_DATA_DATABUS;

					-- No writing
					GP_Write_EnableA <= '0';
					GP_Write_EnableB <= '0';
				end if;

				-- Store Z
				if std_match(IR, OpSTZI) then
					-- not a LDI operation
					LDI_op <= '0';
					-- not reading or writing data right now
					DataRdOut <= '1';
					DataWrOut <= '1';
					-- Z register operation
					Data_Addr_Src_Sel <= Data_Addr_Src_Sel_Z;
					-- incrementing Z
					Offset_Src_Sel <= Offset_Src_Sel_pos_1;
					-- value does not matter
					unsigned_displacement <= (others => '0');
					-- post increment
					Pre_Post_Sel <= Pre_Post_Sel_Post;
					-- value does not matter
					Immediate_Data_Address <= (others => '0');
					-- value does not matter
					X_register <= (others => '0');
					-- value does not matter
					Y_register <= (others => '0');
					-- get current Y register value
					GP_Src_SelectA <= Z_REG_HIGH_BYTE;
					GP_Src_SelectB <= Z_REG_LOW_BYTE;
					Z_register <= GP_outA & GP_outB;
					-- value does not matter
					SP_register <= (others => '0');
					-- access data data bus
					GP_Input_Select <= GP_IN_SEL_DATA_DATABUS;

					-- No writing
					GP_Write_EnableA <= '0';
					GP_Write_EnableB <= '0';
				end if;
				
				if std_match(IR, OpSTZD) then
					-- not a LDI operation
					LDI_op <= '0';
					-- not reading or writing data right now
					DataRdOut <= '1';
					DataWrOut <= '1';
					-- Z register operation
					Data_Addr_Src_Sel <= Data_Addr_Src_Sel_Z;
					-- decrementing Z
					Offset_Src_Sel <= Offset_Src_Sel_neg_1;
					-- value does not matter
					unsigned_displacement <= (others => '0');
					-- pre decrement
					Pre_Post_Sel <= Pre_Post_Sel_Pre;
					-- value does not matter
					Immediate_Data_Address <= (others => '0');
					-- value does not matter
					X_register <= (others => '0');
					-- value does not matter
					Y_register <= (others => '0');
					-- get current Z register value
					GP_Src_SelectA <= Z_REG_HIGH_BYTE;
					GP_Src_SelectB <= Z_REG_LOW_BYTE;
					Z_register <= GP_outA & GP_outB;
					-- value does not matter
					SP_register <= (others => '0');
					-- access data data bus
					GP_Input_Select <= GP_IN_SEL_DATA_DATABUS;

					-- No writing
					GP_Write_EnableA <= '0';
					GP_Write_EnableB <= '0';
				end if;
				
				if std_match(IR, OpSTDZ) then
					-- not a LDI operation
					LDI_op <= '0';
					-- not reading or writing data right now
					DataRdOut <= '1';
					DataWrOut <= '1';
					-- Z register operation
					Data_Addr_Src_Sel <= Data_Addr_Src_Sel_Z;
					-- adding unsiged offset to Z
					Offset_Src_Sel <= Offset_Src_Sel_unsigned_q;
					unsigned_displacement(5 downto 0) <= (IR(13) & IR(11 downto 10) & IR(2 downto 0));
					unsigned_displacement(7 downto 6) <= "00";
					Pre_Post_Sel <= Pre_Post_Sel_Pre;
					-- value does not matter
					Immediate_Data_Address <= (others => '0');
					-- value does not matter
					X_register <= (others => '0');
					-- value does not matter
					Y_register <= (others => '0');
					-- get current Z register value
					GP_Src_SelectA <= Z_REG_HIGH_BYTE;
					GP_Src_SelectB <= Z_REG_LOW_BYTE;
					Z_register <= GP_outA & GP_outB;
					-- value does not matter
					SP_register <= (others => '0');
					-- access data data bus
					GP_Input_Select <= GP_IN_SEL_DATA_DATABUS;

					-- No writing
					GP_Write_EnableA <= '0';
					GP_Write_EnableB <= '0';
				end if;

				-- MOV instruction
				if std_match(IR, OpMOV) then
					-- not a LDI operation
					LDI_op <= '0';
					-- not reading or writing data right now
					DataRdOut <= '1';
					DataWrOut <= '1';
					
					-- Data address unit doesn't matter
					unsigned_displacement(7 downto 0) <= "00000000";
					Offset_Src_Sel <= Offset_Src_Sel_unsigned_q;
					-- Z register operation (value does not matter)
					Data_Addr_Src_Sel <= Data_Addr_Src_Sel_Z;
					Pre_Post_Sel <= Pre_Post_Sel_Pre;
					-- value does not matter
					Immediate_Data_Address <= (others => '0');
					-- value does not matter
					X_register <= (others => '0');
					-- value does not matter
					Y_register <= (others => '0');
					-- value does not matter
					Z_register <= (others => '0');
					-- value does not matter
					SP_register <= (others => '0');

					-- get register value we want using A, B doesn't matter
					GP_Src_SelectA <= IR(9) & IR(3 downto 0);
					GP_Src_SelectB <= (others => '0');

					-- Use the GP reg output
					GP_Input_Select <= GP_IN_SEL_GP_A;

					-- Move to correct other GP reg.
					GP_Dst_SelectA <= IR(8 downto 4);

					-- Write to register
					GP_Write_EnableA <= '1';
					GP_Write_EnableB <= '0';
					
					-- Not Storing
					Store <= '0';
				end if;

				-- Push/Pop Instructions
				if std_match(IR, OpPUSH) then
					-- not a LDI operation
					LDI_op <= '0';
					-- not reading or writing data right now
					DataRdOut <= '1';
					DataWrOut <= '1';
					-- value does not matter
					unsigned_displacement <= (others => '0');
					-- value does not matter
					Immediate_Data_Address <= (others => '0');
					-- value does not matter
					X_register <= (others => '0');
					-- value does not matter
					Y_register <= (others => '0');
					-- value does not matter
					Z_register <= (others => '0');
					-- value does not matter
					GP_Src_SelectA <= Z_REG_HIGH_BYTE;
					GP_Src_SelectB <= Z_REG_LOW_BYTE;
					-- access data data bus
					GP_Input_Select <= GP_IN_SEL_DATA_DATABUS;
					-- No writing to GP regs
					GP_Write_EnableA <= '0';
					GP_Write_EnableB <= '0';	

					-- SP register operation
					Data_Addr_Src_Sel <= Data_Addr_Src_Sel_SP;
					-- post decrement
					Pre_Post_Sel <= Pre_Post_Sel_Post;
					-- decrementing SP
					Offset_Src_Sel <= Offset_Src_Sel_neg_1;

					-- Select stack pointer
					IO_Src_SelectA <= std_logic_vector(to_unsigned(SPH, NUM_IO_LOG));
					IO_Src_SelectB <= std_logic_vector(to_unsigned(SPL, NUM_IO_LOG));

					-- Create the input source
					SP_register <= IO_outA & IO_outB;

					-- Not Storing this clock
					Store <= '0';			
				end if;

				if std_match(IR, OpPOP) then
					-- not a LDI operation
					LDI_op <= '0';
					-- not reading or writing data right now
					DataRdOut <= '1';
					DataWrOut <= '1';
					-- value does not matter
					unsigned_displacement <= (others => '0');
					-- value does not matter
					Immediate_Data_Address <= (others => '0');
					-- value does not matter
					X_register <= (others => '0');
					-- value does not matter
					Y_register <= (others => '0');
					-- value does not matter
					Z_register <= (others => '0');
					-- value does not matter
					GP_Src_SelectA <= Z_REG_HIGH_BYTE;
					GP_Src_SelectB <= Z_REG_LOW_BYTE;

					-- access data data bus
					GP_Input_Select <= GP_IN_SEL_DATA_DATABUS;
					-- No writing to GP regs yet
					GP_Write_EnableA <= '0';
					GP_Write_EnableB <= '0';

					-- SP register operation
					Data_Addr_Src_Sel <= Data_Addr_Src_Sel_SP;
					-- post decrement
					Pre_Post_Sel <= Pre_Post_Sel_Pre;
					-- decrementing SP
					Offset_Src_Sel <= Offset_Src_Sel_pos_1;

					-- Select stack pointer
					IO_Src_SelectA <= std_logic_vector(to_unsigned(SPH, NUM_IO_LOG));
					IO_Src_SelectB <= std_logic_vector(to_unsigned(SPL, NUM_IO_LOG));

					-- Create the input source
					SP_register <= IO_outA & IO_outB;

					-- Not Storing
					Store <= '0';	

				end if;

				-- IN/OUT instructions

				if std_match(IR, OpIN) then
					-- not a LDI operation
					LDI_op <= '0';
					-- not reading or writing data right now
					DataRdOut <= '1';
					DataWrOut <= '1';
					
					-- Data address unit doesn't matter
					unsigned_displacement(7 downto 0) <= "00000000";
					Offset_Src_Sel <= Offset_Src_Sel_unsigned_q;
					-- Z register operation (value does not matter)
					Data_Addr_Src_Sel <= Data_Addr_Src_Sel_Z;
					Pre_Post_Sel <= Pre_Post_Sel_Pre;
					-- value does not matter
					Immediate_Data_Address <= (others => '0');
					-- value does not matter
					X_register <= (others => '0');
					-- value does not matter
					Y_register <= (others => '0');
					-- value does not matter
					Z_register <= (others => '0');
					-- value does not matter
					SP_register <= (others => '0');

					-- Neither output matters
					GP_Src_SelectA <= (others => '0');
					GP_Src_SelectB <= (others => '0');

					-- Use the IO reg output
					GP_Input_Select <= GP_IN_SEL_IO_A;

					-- Move to correct other GP reg.
					GP_Dst_SelectA <= IR(8 downto 4);

					-- Write to register
					GP_Write_EnableA <= '1';
					GP_Write_EnableB <= '0';
					
					-- Not Storing
					Store <= '0';

					-- Do not need to input anything into IO Regs
					IO_Input_Select  <= '0';
					IO_Write_EnableA <= '0';
					IO_Write_EnableB <= '0';
					IO_Dst_SelectA <= (others => '0');

					-- Need to output the proper register
					IO_Src_SelectA <= IR(10 downto 9) & IR(3 downto 0);
					IO_Src_SelectB <= (others => '0');
				end if;

				if std_match(IR, OpOUT) then
					-- not a LDI operation
					LDI_op <= '0';
					-- not reading or writing data right now
					DataRdOut <= '1';
					DataWrOut <= '1';
					
					-- Data address unit doesn't matter
					unsigned_displacement(7 downto 0) <= "00000000";
					Offset_Src_Sel <= Offset_Src_Sel_unsigned_q;
					-- Z register operation (value does not matter)
					Data_Addr_Src_Sel <= Data_Addr_Src_Sel_Z;
					Pre_Post_Sel <= Pre_Post_Sel_Pre;
					-- value does not matter
					Immediate_Data_Address <= (others => '0');
					-- value does not matter
					X_register <= (others => '0');
					-- value does not matter
					Y_register <= (others => '0');
					-- value does not matter
					Z_register <= (others => '0');
					-- value does not matter
					SP_register <= (others => '0');
					-- value does not matter
					GP_Input_Select <= GP_IN_SEL_DATA_DATABUS;
					-- Input doesn't matter
					GP_Input_Select <= GP_IN_SEL_GP_A;
					-- Destination doesn't matter
					GP_Dst_SelectA <= (others => '0');

					-- get register value we want using A, B doesn't matter
					GP_Src_SelectA <= IR(8 downto 4);
					GP_Src_SelectB <= (others => '0');

					-- Do not write to registers
					GP_Write_EnableA <= '0';
					GP_Write_EnableB <= '0';
					
					-- Not Storing
					Store <= '0';

					-- Do not need to input anything into IO Regs
					IO_Input_Select  <= IO_IN_SEL_GP_A;
					IO_Write_EnableA <= '1';
					IO_Write_EnableB <= '0';
					IO_Dst_SelectA <= IR(10 downto 9) & IR(3 downto 0);

					-- Output doesn't matter
					IO_Src_SelectA <= (others => '0');
					IO_Src_SelectB <= (others => '0');
				end if;

				-- Direct Memory
				if std_match(IR, OpLDS) then
					-- not a LDI operation
					LDI_op <= '0';
					-- not reading or writing data right now
					DataRdOut <= '1';
					DataWrOut <= '1';
					-- Imm register operation
					Data_Addr_Src_Sel <= Data_Addr_Src_Sel_Imm_Addr;
					-- not modifying X
					Offset_Src_Sel <= Offset_Src_Sel_0;
					-- value does not matter
					unsigned_displacement <= (others => '0');
					-- address is not changed
					Pre_Post_Sel <= Pre_Post_Sel_Pre;
					-- Need to wait
					Immediate_Data_Address <= (others => '0');
					-- value does not matter
					GP_Src_SelectA <= X_REG_HIGH_BYTE;
					GP_Src_SelectB <= X_REG_LOW_BYTE;
					X_register <= (others => '0');
					-- value does not matter
					Y_register <= (others => '0');
					-- value does not matter
					Z_register <= (others => '0');
					-- value does not matter
					SP_register <= (others => '0');
					GP_Input_Select <= GP_IN_SEL_DATA_DATABUS;

					-- No writing
					GP_Write_EnableA <= '0';
					GP_Write_EnableB <= '0';
				end if;
		
				if std_match(IR, OpSTS) then
					-- not a LDI operation
					LDI_op <= '0';
					-- not reading or writing data right now
					DataRdOut <= '1';
					DataWrOut <= '1';
					-- Imm operation
					Data_Addr_Src_Sel <= Data_Addr_Src_Sel_Imm_Addr;
					-- not changing X
					Offset_Src_Sel <= Offset_Src_Sel_0;
					-- value does not matter
					unsigned_displacement <= (others => '0');
					-- save current value of X, no change
					Pre_Post_Sel <= Pre_Post_Sel_Pre;
					-- Need to wait
					Immediate_Data_Address <= (others => '0');
					-- value does not matter
					GP_Src_SelectA <= X_REG_HIGH_BYTE;
					GP_Src_SelectB <= X_REG_LOW_BYTE;
					X_register <= (others => '0');
					-- value does not matter
					Y_register <= (others => '0');
					-- value does not matter
					Z_register <= (others => '0');
					-- value does not matter
					SP_register <= (others => '0');
					-- access data data bus
					GP_Input_Select <= GP_IN_SEL_DATA_DATABUS;

					-- No writing
					GP_Write_EnableA <= '0';
					GP_Write_EnableB <= '0';
				end if;			

			--
			-- CLOCK 2
			--

			when Clock2 =>
			
				--------------------------------------------------------------
				--------------------------------------------------------------
				-- ALU --
				--------------------------------------------------------------
				--------------------------------------------------------------
				
				-- ADIW add immediate to word
				if std_match(IR, OpADIW) then
					-- choose which register to use depending on instruction decoding
					if IR(5 downto 4) = "00" then
						Regd := "11001";
					end if;
					
					if IR(5 downto 4) = "01" then
						Regd := "11011";
					end if;
					
					if IR(5 downto 4) = "10" then
						Regd := "11101";
					end if;
					
					if IR(5 downto 4) = "11" then
						Regd := "11111";
					end if;
					
					GP_Src_SelectA <= Regd;
					
					-- Control signals to ALU
					-- addition operation
					ALU_result_select <= Adder_Subtractor_Operation;
					-- addition
					Subtract <= Addition;
					-- add with carry
					ALU_op_with_carry <= '1';
					-- use operands passed in as arguments from Control Unit
					AddSub_Op_1_Select <= AddSub_Op_1_Select_OperandA;
					AddSub_Op_2_Select <= AddSub_Op_2_Select_0;
				
					-- Register d + 1 contents
					OperandA <= GP_outA;
					
					-- Control signals to Register to store result
					-- indicate register input is from ALU
					GP_Input_Select <= GP_IN_SEL_ALU;
					GP_Write_EnableA <= '1';
					GP_Write_EnableB <= '0';
					-- indicate nibbles of register should not be swapped
					GP_Swap_Nibbles <= '0';
					-- store result in Register d + 1
					GP_Dst_SelectA <= Regd;
					
				end if;
			
				-- SBIW subtract immediate to word
				if std_match(IR, OpSBIW) then
					-- choose which register to use depending on instruction decoding
					if IR(5 downto 4) = "00" then
						Regd := "11001";
					end if;
					
					if IR(5 downto 4) = "01" then
						Regd := "11011";
					end if;
					
					if IR(5 downto 4) = "10" then
						Regd := "11101";
					end if;
					
					if IR(5 downto 4) = "11" then
						Regd := "11111";
					end if;
					
					GP_Src_SelectA <= Regd;
					
					-- Control signals to ALU
					-- addition operation
					ALU_result_select <= Adder_Subtractor_Operation;
					-- subtract operation
					Subtract <= Subtraction;
					-- no add with carry
					ALU_op_with_carry <= '1';
					-- use operands passed in as arguments from Control Unit
					AddSub_Op_1_Select <= AddSub_Op_1_Select_OperandA;
					AddSub_Op_2_Select <= AddSub_Op_2_Select_Operand0;
				
					-- Register d contents
					OperandA <= GP_outA;
					
					-- Control signals to Register to store result
					-- indicate register input is from ALU
					GP_Input_Select <= GP_IN_SEL_ALU;
					GP_Write_EnableA <= '1';
					GP_Write_EnableB <= '0';
					-- indicate nibbles of register should not be swapped
					GP_Swap_Nibbles <= '0';
					-- store result in Register d
					GP_Dst_SelectA <= Regd;
					
				end if;
				
				--------------------------------------------------------------
				--------------------------------------------------------------
				-- DATA MEMORY ACCESS UNIT --
				--------------------------------------------------------------
				--------------------------------------------------------------
				
				-- Load X
				if std_match(IR, OpLDX) then
					DataRdOut <= '0';
					DataWrOut <= '1';
					GP_Dst_SelectB <= GP_Dst_SelectB_X;
					Store <= '0';
					GP_Write_EnableA <= '1';
					GP_Write_EnableB <= '0';
					GP_Dst_SelectA <= 
						IR(DMAU_Reg_high_bit downto DMAU_Reg_low_bit);
					GP_Input_Select <= GP_IN_SEL_DATA_DATABUS;
				end if;

				if std_match(IR, OpLDXI) then
					DataRdOut <= '0';
					DataWrOut <= '1';
					GP_Dst_SelectB <= GP_Dst_SelectB_X;
					Store <= '0';
					GP_Write_EnableA <= '1';
					GP_Write_EnableB <= '1';
					GP_Dst_SelectA <= 
						IR(DMAU_Reg_high_bit downto DMAU_Reg_low_bit);
					GP_Input_Select <= GP_IN_SEL_DATA_DATABUS;
				end if;
				
				if std_match(IR, OpLDXD) then
					DataRdOut <= '0';
					DataWrOut <= '1';
					GP_Dst_SelectB <= GP_Dst_SelectB_X;
					Store <= '0';
					GP_Write_EnableA <= '1';
					GP_Write_EnableB <= '1';
					GP_Dst_SelectA <= 
						IR(DMAU_Reg_high_bit downto DMAU_Reg_low_bit);
					GP_Input_Select <= GP_IN_SEL_DATA_DATABUS;
				end if;

				-- Store X
				if std_match(IR, OpSTX) then
					DataRdOut <= '1';
					DataWrOut <= '0';
					GP_Dst_SelectB <= GP_Dst_SelectB_X;
					Store <= '1';
					GP_Write_EnableB <= '1';
					GP_Write_EnableA <= '0';
					GP_Src_SelectA <= 
						IR(DMAU_Reg_high_bit downto DMAU_Reg_low_bit);
					GP_Input_Select <= GP_IN_SEL_DATA_DATABUS;
				end if;
				
				if std_match(IR, OpSTXI) then
					DataRdOut <= '1';
					DataWrOut <= '0';
					GP_Dst_SelectB <= GP_Dst_SelectB_X;
					Store <= '1';
					GP_Write_EnableB <= '1';
					GP_Write_EnableA <= '0';
					GP_Src_SelectA <= 
						IR(DMAU_Reg_high_bit downto DMAU_Reg_low_bit);
					GP_Input_Select <= GP_IN_SEL_DATA_DATABUS;
				end if;
				
				if std_match(IR, OpSTXD) then
					DataRdOut <= '1';
					DataWrOut <= '0';
					GP_Dst_SelectB <= GP_Dst_SelectB_X;
					Store <= '1';
					GP_Write_EnableB <= '1';
					GP_Write_EnableA <= '0';
					GP_Src_SelectA <= 
						IR(DMAU_Reg_high_bit downto DMAU_Reg_low_bit);
					GP_Input_Select <= GP_IN_SEL_DATA_DATABUS;
				end if;

				-- Y Loads
				if std_match(IR, OpLDYI) then
					DataRdOut <= '0';
					DataWrOut <= '1';
					GP_Dst_SelectB <= GP_Dst_SelectB_Y;
					Store <= '0';
					GP_Write_EnableA <= '1';
					GP_Write_EnableB <= '1';
					GP_Dst_SelectA <= 
						IR(DMAU_Reg_high_bit downto DMAU_Reg_low_bit);
					GP_Input_Select <= GP_IN_SEL_DATA_DATABUS;
				end if;
				
				if std_match(IR, OpLDYD) then
					DataRdOut <= '0';
					DataWrOut <= '1';
					GP_Dst_SelectB <= GP_Dst_SelectB_Y;
					Store <= '0';
					GP_Write_EnableA <= '1';
					GP_Write_EnableB <= '1';
					GP_Dst_SelectA <= 
						IR(DMAU_Reg_high_bit downto DMAU_Reg_low_bit);
					GP_Input_Select <= GP_IN_SEL_DATA_DATABUS;
				end if;
				
				if std_match(IR, OpLDDY) then
					DataRdOut <= '0';
					DataWrOut <= '1';
					GP_Dst_SelectB <= GP_Dst_SelectB_Y;
					Store <= '0';
					GP_Write_EnableA <= '1';
					GP_Write_EnableB <= '0';
					GP_Dst_SelectA <= 
						IR(DMAU_Reg_high_bit downto DMAU_Reg_low_bit);
					GP_Input_Select <= GP_IN_SEL_DATA_DATABUS;
				end if;

				-- Y Stores
				if std_match(IR, OpSTYI) then
					DataRdOut <= '1';
					DataWrOut <= '0';
					GP_Dst_SelectB <= GP_Dst_SelectB_Y;
					Store <= '1';
					GP_Write_EnableB <= '1';
					GP_Write_EnableA <= '0';
					GP_Src_SelectA <= 
						IR(DMAU_Reg_high_bit downto DMAU_Reg_low_bit);
					GP_Input_Select <= GP_IN_SEL_DATA_DATABUS;
				end if;
				
				if std_match(IR, OpSTYD) then
					DataRdOut <= '1';
					DataWrOut <= '0';
					GP_Dst_SelectB <= GP_Dst_SelectB_Y;
					Store <= '1';
					GP_Write_EnableB <= '1';
					GP_Write_EnableA <= '0';
					GP_Src_SelectA <= 
						IR(DMAU_Reg_high_bit downto DMAU_Reg_low_bit);
					GP_Input_Select <= GP_IN_SEL_DATA_DATABUS;
				end if;
				
				if std_match(IR, OpSTDY) then
					DataRdOut <= '1';
					DataWrOut <= '0';
					GP_Dst_SelectB <= GP_Dst_SelectB_Y;
					Store <= '1';
					GP_Write_EnableB <= '0';
					GP_Write_EnableA <= '0';
					GP_Src_SelectA <= 
						IR(DMAU_Reg_high_bit downto DMAU_Reg_low_bit);
					GP_Input_Select <= GP_IN_SEL_DATA_DATABUS;
				end if;

				-- Load Z
				if std_match(IR, OpLDZI) then
					DataRdOut <= '0';
					DataWrOut <= '1';
					GP_Dst_SelectB <= GP_Dst_SelectB_Z;
					Store <= '0';
					GP_Write_EnableA <= '1';
					GP_Write_EnableB <= '1';
					GP_Dst_SelectA <= 
						IR(DMAU_Reg_high_bit downto DMAU_Reg_low_bit);
					GP_Input_Select <= GP_IN_SEL_DATA_DATABUS;
				end if;
				
				if std_match(IR, OpLDZD) then
					DataRdOut <= '0';
					DataWrOut <= '1';
					GP_Dst_SelectB <= GP_Dst_SelectB_Z;
					Store <= '0';
					GP_Write_EnableA <= '1';
					GP_Write_EnableB <= '1';
					GP_Dst_SelectA <= 
						IR(DMAU_Reg_high_bit downto DMAU_Reg_low_bit);
					GP_Input_Select <= GP_IN_SEL_DATA_DATABUS;
				end if;
				
				if std_match(IR, OpLDDZ) then
					DataRdOut <= '0';
					DataWrOut <= '1';
					GP_Dst_SelectB <= GP_Dst_SelectB_Z;
					Store <= '0';
					GP_Write_EnableA <= '1';
					GP_Write_EnableB <= '0';
					GP_Dst_SelectA <= 
						IR(DMAU_Reg_high_bit downto DMAU_Reg_low_bit);
					GP_Input_Select <= GP_IN_SEL_DATA_DATABUS;
				end if;

				-- Store Z
				if std_match(IR, OpSTZI) then
					DataRdOut <= '1';
					DataWrOut <= '0';
					GP_Dst_SelectB <= GP_Dst_SelectB_Z;
					Store <= '1';
					GP_Write_EnableB <= '1';
					GP_Write_EnableA <= '0';
					GP_Src_SelectA <= 
						IR(DMAU_Reg_high_bit downto DMAU_Reg_low_bit);
					GP_Input_Select <= GP_IN_SEL_DATA_DATABUS;
				end if;
				
				if std_match(IR, OpSTZD) then
					DataRdOut <= '1';
					DataWrOut <= '0';
					GP_Dst_SelectB <= GP_Dst_SelectB_Z;
					Store <= '1';
					GP_Write_EnableB <= '1';
					GP_Write_EnableA <= '0';
					GP_Src_SelectA <= 
						IR(DMAU_Reg_high_bit downto DMAU_Reg_low_bit);
					GP_Input_Select <= GP_IN_SEL_DATA_DATABUS;
				end if;
				
				if std_match(IR, OpSTDZ) then
					DataRdOut <= '1';
					DataWrOut <= '0';
					GP_Dst_SelectB <= GP_Dst_SelectB_Z;
					Store <= '1';
					GP_Write_EnableB <= '0';
					GP_Write_EnableA <= '0';
					GP_Src_SelectA <= 
						IR(DMAU_Reg_high_bit downto DMAU_Reg_low_bit);
					GP_Input_Select <= GP_IN_SEL_DATA_DATABUS;
				end if;

				-- Push/Pop
				if std_match(IR, OpPUSH) then
					-- Write
					DataRdOut <= '1';
					DataWrOut <= '0';
					-- Storing
					Store <= '1';
					-- Doesn't matter
					GP_Dst_SelectB <= GP_Dst_SelectB_Z;
					GP_Write_EnableB <= '0';
					GP_Write_EnableA <= '0';
					GP_Input_Select <= GP_IN_SEL_DATA_DATABUS;
					GP_Src_SelectA <= 
						IR(DMAU_Reg_high_bit downto DMAU_Reg_low_bit);

					-- Load in
					IO_Write_EnableB <= '1';
					
				end if;

				if std_match(IR, OpPOP) then
					-- Read
					DataRdOut <= '0';
					DataWrOut <= '1';
					-- Not storing
					Store <= '0';
					-- Doesn't matter
					GP_Dst_SelectB <= GP_Dst_SelectB_Z;
					GP_Write_EnableB <= '0';

					-- Write into register
					GP_Write_EnableA <= '1';
					GP_Input_Select <= GP_IN_SEL_DATA_DATABUS;
					GP_Dst_SelectA <= IR(DMAU_Reg_high_bit downto DMAU_Reg_low_bit);

					-- Load in
					IO_Write_EnableB <= '1';
					
				end if;

				-- Direct Memory
				if std_match(IR, OpLDS) then
					-- Load proper address
					Immediate_Data_Address <= Program_Data_Bus;
				end if;
	
				if std_match(IR, OpSTS) then
					-- Load proper address
					Immediate_Data_Address <= Program_Data_Bus;
				end if;			

			when Clock3 =>

				--------------------------------------------------------------
				--------------------------------------------------------------
				-- DATA MEMORY ACCESS UNIT --
				--------------------------------------------------------------
				--------------------------------------------------------------
				
				-- Direct Memory
				if std_match(IR, OpLDS) then

					DataRdOut <= '0';
					DataWrOut <= '1';

					GP_Write_EnableA <= '1';
					GP_Write_EnableB <= '0';
					GP_Dst_SelectA <= 
						IR(DMAU_Reg_high_bit downto DMAU_Reg_low_bit);
					GP_Input_Select <= GP_IN_SEL_DATA_DATABUS;

					-- Not storing
					Store <= '0';

				end if;
	
				if std_match(IR, OpSTS) then
					
					DataRdOut <= '1';
					DataWrOut <= '0';

					GP_Write_EnableA <= '0';
					GP_Write_EnableB <= '0';

					GP_Src_SelectA <= 
						IR(DMAU_Reg_high_bit downto DMAU_Reg_low_bit);
					GP_Input_Select <= GP_IN_SEL_DATA_DATABUS;

					-- Storing
					Store <= '1';

				end if;					
		end case;
	end process actions;

	state_transition: process (IR, state)
	begin
		case state is
			when Clock1 =>
			
				--------------------------------------------------------------
				--------------------------------------------------------------
				-- ALU --
				--------------------------------------------------------------
				--------------------------------------------------------------
				
				if 	std_match(IR, OpADC) or
					std_match(IR, OpADD) or
					std_match(IR, OpAND) or
					std_match(IR, OpANDI) or
					std_match(IR, OpASR) or
					std_match(IR, OpBCLR) or
					std_match(IR, OpBLD) or
					std_match(IR, OpBSET) or
					std_match(IR, OpCOM) or
					std_match(IR, OpCP) or
					std_match(IR, OpCPC) or
					std_match(IR, OpCPI) or
					std_match(IR, OpDEC) or
					std_match(IR, OpEOR) or
					std_match(IR, OpINC) or
					std_match(IR, OpLSR) or
					std_match(IR, OpMUL) or
					std_match(IR, OpNEG) or
					std_match(IR, OpOR) or
					std_match(IR, OpORI) or
					std_match(IR, OpROR) or
					std_match(IR, OpSBC) or
					std_match(IR, OpSBCI) or
					std_match(IR, OpSBIW) or
					std_match(IR, OpSUB) or
					std_match(IR, OpSWAP) or
					then
					
					next_state <= Clock1;
				
				end if;
				
				if 	std_match(IR, OpADIW) or
					std_match(IR, OpSUBI) or
					then
					
					next_state <= Clock2;
				
				end if;
				
				--------------------------------------------------------------
				--------------------------------------------------------------
				-- DATA MEMORY ACCESS UNIT --
				--------------------------------------------------------------
				--------------------------------------------------------------
				
				-- Immediate Load
				if std_match(IR, OpLDI) then
					next_state <= Clock1;
				end if;
				-- X Reg. Loads
				if std_match(IR, OpLDX) then
					next_state <= Clock2;
				end if;
				if std_match(IR, OpLDXI) then
					next_state <= Clock2;
				end if;
				if std_match(IR, OpLDXD) then
					next_state <= Clock2;
				end if;
				-- X Reg. Stores
				if std_match(IR, OpSTX) then
					next_state <= Clock2;
				end if;
				if std_match(IR, OpSTXI) then
					next_state <= Clock2;
				end if;
				if std_match(IR, OpSTXD) then
					next_state <= Clock2;
				end if;
				-- Y Reg. Loads
				if std_match(IR, OpLDYI) then
					next_state <= Clock2;
				end if;
				if std_match(IR, OpLDYD) then
					next_state <= Clock2;					
				end if;	
				if std_match(IR, OpLDDY) then
					next_state <= Clock2;
				end if;
				-- Y Reg Stores
				if std_match(IR, OpSTYI) then
					next_state <= Clock2;
				end if;
				if std_match(IR, OpSTYD) then
					next_state <= Clock2;
				end if;
				if std_match(IR, OpSTDY) then
					next_state <= Clock2;
				end if;
				-- Z Reg. Loads
				if std_match(IR, OpLDZI) then
					next_state <= Clock2;
				end if;
				if std_match(IR, OpLDZD) then
					next_state <= Clock2;					
				end if;	
				if std_match(IR, OpLDDZ) then
					next_state <= Clock2;
				end if;
				-- Z Reg Stores
				if std_match(IR, OpSTZI) then
					next_state <= Clock2;
				end if;
				if std_match(IR, OpSTZD) then
					next_state <= Clock2;
				end if;
				if std_match(IR, OpSTDZ) then
					next_state <= Clock2;
				end if;
				-- MOV
				if std_match(IR, OpMOV) then
					next_state <= Clock1;
				end if;
				-- Push Pop
				if std_match(IR, OpPUSH) then
					next_state <= Clock2;
				end if;
				if std_match(IR, OpPOP) then
					next_state <= Clock2;
				end if;
				-- Direct Memory
				if std_match(IR, OpLDS) then
					next_state <= Clock2;
				end if;
				if std_match(IR, OpSTS) then
					next_state <= Clock2;
				end if;
			when Clock2 =>
				
				--------------------------------------------------------------
				--------------------------------------------------------------
				-- ALU --
				--------------------------------------------------------------
				--------------------------------------------------------------
				
				if 	std_match(IR, OpADIW) or
					std_match(IR, OpSUBI) or
					then
					
					next_state <= Clock1;
				
				end if;
				
				--------------------------------------------------------------
				--------------------------------------------------------------
				-- DATA MEMORY ACCESS UNIT --
				--------------------------------------------------------------
				--------------------------------------------------------------
				
				-- X Reg. Loads
				if std_match(IR, OpLDX) then
					next_state <= Clock1;
				end if;
				if std_match(IR, OpLDXI) then
					next_state <= Clock1;
				end if;
				if std_match(IR, OpLDXD) then
					next_state <= Clock1;
				end if;
				-- X Reg. Stores
				if std_match(IR, OpSTX) then
					next_state <= Clock1;
				end if;
				if std_match(IR, OpSTXI) then
					next_state <= Clock1;
				end if;
				if std_match(IR, OpSTXD) then
					next_state <= Clock1;
				end if;
				-- Y Reg. Loads
				if std_match(IR, OpLDYI) then
					next_state <= Clock1;
				end if;
				if std_match(IR, OpLDYD) then
					next_state <= Clock1;					
				end if;	
				if std_match(IR, OpLDDY) then
					next_state <= Clock1;
				end if;
				-- Y Reg Stores
				if std_match(IR, OpSTYI) then
					next_state <= Clock1;
				end if;
				if std_match(IR, OpSTYD) then
					next_state <= Clock1;
				end if;
				if std_match(IR, OpSTDY) then
					next_state <= Clock1;
				end if;	
				-- Z Reg. Loads
				if std_match(IR, OpLDZI) then
					next_state <= Clock1;
				end if;
				if std_match(IR, OpLDZD) then
					next_state <= Clock1;					
				end if;	
				if std_match(IR, OpLDDZ) then
					next_state <= Clock1;
				end if;
				-- Z Reg Stores
				if std_match(IR, OpSTZI) then
					next_state <= Clock1;
				end if;
				if std_match(IR, OpSTZD) then
					next_state <= Clock1;
				end if;
				if std_match(IR, OpSTDZ) then
					next_state <= Clock1;
				end if;
				-- Push Pop
				if std_match(IR, OpPUSH) then
					next_state <= Clock1;
				end if;
				if std_match(IR, OpPOP) then
					next_state <= Clock1;
				end if;
				-- Direct Memory
				if std_match(IR, OpLDS) then
					next_state <= Clock3;
				end if;
				if std_match(IR, OpSTS) then
					next_state <= Clock3;
				end if;										
			when Clock3 =>
			
				--------------------------------------------------------------
				--------------------------------------------------------------
				-- DATA MEMORY ACCESS UNIT --
				--------------------------------------------------------------
				--------------------------------------------------------------
				
				-- Direct Memory
				if std_match(IR, OpLDS) then
					next_state <= Clock1;
				end if;
				if std_match(IR, OpSTS) then
					next_state <= Clock1;
				end if;			
		end case;
	end process state_transition;

	state_update : process (clk)
	begin
		if rising_edge(clk) then
			state <= next_state;
		end if;
	end process state_update;

end architecture;