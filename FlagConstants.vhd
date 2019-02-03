----------------------------------------------------------------------------
--  Flag Constants
--
--	This file contains contains constants used for the 
--	flags implementation specifically for an 8-bit AVR architecture.
--
--  Revision History:
--	28 Jan 19	Kavya Sreedhar & Dan Xu 	Initial revision
--	1  Feb 19	Kavya Sreedhar & Dan Xu		Updated revision history
----------------------------------------------------------------------------

-- declaration of libraries used
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

-- constants for the Status Register implementation only
package FlagConstants is

	-- Location in IO Regs
	constant IO_REG_LOC             : std_logic_vector(5 downto 0) := "111111";

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