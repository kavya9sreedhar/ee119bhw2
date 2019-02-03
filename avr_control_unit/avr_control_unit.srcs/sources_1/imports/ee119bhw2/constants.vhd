----------------------------------------------------------------------------
--  AVR Constants
--
--	This file contains contains general constants used for the 
--	implementation of an 8-bit AVR architecture.
--
--  Revision History:
--	28 Jan 19	Kavya Sreedhar & Dan Xu 	Initial revision
--	1  Feb 19	Kavya Sreedhar & Dan Xu		Updated revision history
----------------------------------------------------------------------------

-- declaration of libraries used
library ieee;
use ieee.std_logic_1164.all;

-- general constants shared across all files
package CPU_CONSTANTS is
	-- number of general purpose registers
	constant NUM_REGISTERS: 		integer := 32;
	-- number of IO Registers
	constant NUM_IO_REG:            integer := 64;

	-- number of bits in data busses
	constant NUM_DATA_BITS: 		integer := 8;
	-- number of bits in address busses
	constant NUM_ADDRESS_BITS: 		integer := 16;
	-- number of bits in each general-purpose register
	constant NUM_BITS_PER_REGISTER: integer := 8;

	-- Log of numbers
	constant NUM_REG_LOG           integer := 5;
	constant NUM_IO_LOG            integer := 6;
	constant DATA_BITS_LOG         integer := 3;
	constant ADDR_BITS_LOG         integer := 4;

end package;
	
-- constants for the ALU implementation only
package ALU_constants is 
	-- indicates various types of ALU operations
	constant F_Block_Operation: std_logic_vector(1 downto 0) := "00";
	constant Adder_Subtractor_Operation: std_logic_vector(1 downto 0) := "01";
	constant Shifter_Rotater_Operation: std_logic_vector(1 downto 0) := "10";
	
	-- SHIFT / ROTATE CONSTANTS
	
	-- indicates possibilities for update for lowest bit during a shift / rotate
	-- operation
	constant Shifter_low_bit_highest_bit: std_logic_vector(1 downto 0) := "00";
	constant Shifter_low_bit_0: std_logic_vector(1 downto 0) := "01";
	constant Shifter_low_bit_bit_1: std_logic_vector(1 downto 0) := "10";
	constant Shifter_low_bit_carry: std_logic_vector(1 downto 0) := "11";
	
	-- indicates possibilities for update for middle bits during a shift / rotate
	-- operation
	constant Shifter_middle_bits_select_immediate_right: std_logic := '0';
	constant Shifter_middle_bits_select_immediate_left: std_logic := '1';
	
	-- indicates possibilities for update for highest bit during a shift / rotate
	-- operation
	constant Shifter_high_bit_select_second_highest_bit: std_logic_vector := "000";
	constant Shifter_high_bit_select_highest_bit: std_logic_vector := "001";
	constant Shifter_high_bit_select_lowest_bit: std_logic_vector := "010";
	constant Shifter_high_bit_select_0: std_logic_vector := "011";
	constant Shifter_high_bit_select_carry: std_logic_vector := "100";
		
	-- F BLOCK CONSTANTS
	
	-- indicates possibilities for control signals for various F Block outputs
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
	
	-- ADDITION / SUBTRACTION CONSTANTS
	
	-- indicates subtraction versus addition for addition / subtraction operation
	constant Subtraction: std_logic := '1';
	constant Addition: std_logic := '0';
	
	-- indicates possibilities for first operand for addition / subtraction operation
	constant AddSub_Op_1_Select_OperandA: std_logic := "00";
	constant AddSub_Op_1_Select_FF: std_logic := "01";
	constant AddSub_Op_1_Select_0: std_logic := "10";
	
	-- indicates possibilities for second operand for addition / subtraction operation
	constant AddSub_Op_2_Select_0: std_logic_vector(1 downto 0) := "00";
	constant AddSub_Op_2_Select_1: std_logic_vector(1 downto 0) := "01";
	constant AddSub_Op_2_Select_OperandB: std_logic_vector(1 downto 0) := "10";
	
	constant AddSub_Op_1_Select_OperandA: std_logic := "00";
	constant AddSub_Op_1_Select_FF: std_logic := "01";
	constant AddSub_Op_1_Select_0: std_logic := "10";
end package;

-- constants for the Registers implementation only
package RegConstants is

	-- Swap control values
	constant SWAP_EN                : std_logic := '1';
	constant SWAP_DIS               : std_logic := '0';

	-- GP Reg input control values
	constant NUM_GP_INP_SELECT_BITS : integer := 2;
	-- Use one of GP register outputs.
	constant GP_IN_SEL_GP_A         : std_logic_vector(NUM_GP_INP_SELECT_BITS-1 downto 0) := "00";
	-- Use one of IO register outputs.
	constant GP_IN_SEL_IO_A         : std_logic_vector(NUM_GP_INP_SELECT_BITS-1 downto 0) := "01";
	-- Use the data bus
	constant GP_IN_SEL_DATA_DATABUS : std_logic_vector(NUM_GP_INP_SELECT_BITS-1 downto 0) := "10";
	-- Use the ALU
	constant GP_IN_SEL_ALU          : std_logic_vector(NUM_GP_INP_SELECT_BITS-1 downto 0) := "11";

	-- GP Reg input control values
	constant NUM_IO_INP_SELECT_BITS : integer := 1;
	-- Use a GP register
	constant IO_IN_SEL_GP_A         : std_logic := '1';
	-- Use the SREG update from ALU
	constant IO_IN_SEL_SREG_ALU     : std_logic := '0';

end package;

-- constants for the Status Register implementation only
package FlagConstants is

	-- Number of flags
	constant N_FLAGS                : integer := NUM_DATA_BITS;

	-- Flag locations
	--  I 7 Global Interrupt Enable/Disable
	constant IFLAG                  : integer := 7;
	--  T 6 Transfer bit used by BLD and BST instructions
	constant TFLAG                  : integer := 6;
	--  H 5 Half Carry (carry out of bit 3, into bit 4)
	constant HFLAG                  : integer := 5;
	--  S 4 Corrected Signed (N xor V)
	constant SFLAG                  : integer := 4;	
	--  V 3 Signed Overflow (carry out of bit 6 doesn't match carry out of bit 7),
	--      is zero (0) for logical operations and N xor C for shift operations
	constant VFLAG                  : integer := 3;
	--  N 2 Negative (sign bit, high bit of result)
	constant NFLAG                  : integer := 2;
	--  Z 1 Zero (result is zero)
	constant ZFLAG                  : integer := 1;
	--  C 0 Carry (carry out of bit 7), set for COM instruction
	constant CFLAG                  : integer := 0;

	-- Control signal declarations
	
	-- Interrupt flag
	constant NUM_I_FLAG_BITS        : integer := 2;
	constant I_HOLD_VALUE           : std_logic_vector(NUM_I_FLAG_BITS-1 downto 0) := "00";
	constant I_SET_VALUE            : std_logic_vector(NUM_I_FLAG_BITS-1 downto 0) := "01";
	constant I_CLEAR_VALUE          : std_logic_vector(NUM_I_FLAG_BITS-1 downto 0) := "10";

	-- Transfer flag
	constant NUM_T_FLAG_BITS        : integer := 2;
	constant T_HOLD_VALUE           : std_logic_vector(NUM_T_FLAG_BITS-1 downto 0) := "00";
	constant T_SET_VALUE            : std_logic_vector(NUM_T_FLAG_BITS-1 downto 0) := "01";
	constant T_CLEAR_VALUE          : std_logic_vector(NUM_T_FLAG_BITS-1 downto 0) := "10";
	constant T_GET_BIT              : std_logic_vector(NUM_T_FLAG_BITS-1 downto 0) := "11";

	-- Half Carry flag
	constant NUM_H_FLAG_BITS        : integer := 2;
	constant H_HOLD_VALUE           : std_logic_vector(NUM_H_FLAG_BITS-1 downto 0) := "00";
	constant H_SET_VALUE            : std_logic_vector(NUM_H_FLAG_BITS-1 downto 0) := "01";
	constant H_CLEAR_VALUE          : std_logic_vector(NUM_H_FLAG_BITS-1 downto 0) := "10";
	constant H_FROM_ALU             : std_logic_vector(NUM_H_FLAG_BITS-1 downto 0) := "11";

	-- Correct Sign
	constant NUM_S_FLAG_BITS        : integer := 2;
	constant S_HOLD_VALUE           : std_logic_vector(NUM_S_FLAG_BITS-1 downto 0) := "00";
	constant S_SET_VALUE            : std_logic_vector(NUM_S_FLAG_BITS-1 downto 0) := "01";
	constant S_CLEAR_VALUE          : std_logic_vector(NUM_S_FLAG_BITS-1 downto 0) := "10";
	constant S_FROM_ALU             : std_logic_vector(NUM_S_FLAG_BITS-1 downto 0) := "11";

	-- Signed Overflow
	constant NUM_V_FLAG_BITS        : integer := 3;
	constant V_HOLD_VALUE           : std_logic_vector(NUM_V_FLAG_BITS-1 downto 0) := "000";
	constant V_SET_VALUE            : std_logic_vector(NUM_V_FLAG_BITS-1 downto 0) := "001";
	constant V_CLEAR_VALUE          : std_logic_vector(NUM_V_FLAG_BITS-1 downto 0) := "010";
	constant V_FROM_ALU             : std_logic_vector(NUM_V_FLAG_BITS-1 downto 0) := "011";
	constant V_C_XOR_N              : std_logic_vector(NUM_V_FLAG_BITS-1 downto 0) := "100";

	-- Negative
	constant NUM_N_FLAG_BITS        : integer := 2;
	constant N_HOLD_VALUE           : std_logic_vector(NUM_N_FLAG_BITS-1 downto 0) := "00";
	constant N_SET_VALUE            : std_logic_vector(NUM_N_FLAG_BITS-1 downto 0) := "01";
	constant N_CLEAR_VALUE          : std_logic_vector(NUM_N_FLAG_BITS-1 downto 0) := "10";
	constant N_FROM_ALU             : std_logic_vector(NUM_Z_FLAG_BITS-1 downto 0) := "11"; 

	-- Zero
	constant NUM_Z_FLAG_BITS        : integer := 2;
	constant Z_HOLD_VALUE           : std_logic_vector(NUM_Z_FLAG_BITS-1 downto 0) := "00";
	constant Z_SET_VALUE            : std_logic_vector(NUM_Z_FLAG_BITS-1 downto 0) := "01";
	constant Z_CLEAR_VALUE          : std_logic_vector(NUM_Z_FLAG_BITS-1 downto 0) := "10";
	constant Z_FROM_ALU             : std_logic_vector(NUM_Z_FLAG_BITS-1 downto 0) := "11";

	-- Carry
	constant NUM_C_FLAG_BITS        : integer := 3;
	constant C_HOLD_VALUE           : std_logic_vector(NUM_C_FLAG_BITS-1 downto 0) := "000";
	constant C_SET_VALUE            : std_logic_vector(NUM_C_FLAG_BITS-1 downto 0) := "001";
	constant C_CLEAR_VALUE          : std_logic_vector(NUM_C_FLAG_BITS-1 downto 0) := "010";
	constant C_FROM_ALU             : std_logic_vector(NUM_Z_FLAG_BITS-1 downto 0) := "011";
	constant C_FROM_LSB             : std_logic_vector(NUM_Z_FLAG_BITS-1 downto 0) := "100";

end package;