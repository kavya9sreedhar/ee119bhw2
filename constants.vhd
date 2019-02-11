----------------------------------------------------------------------------
--  AVR Constants
--
--	This file contains contains constants used for the 
--	implementation of an 8-bit AVR architecture. The constants are split
--  into the following packages for use with different units part of 
--  the architecture:
--	
--	CPU_CONSTANTS 	- general CPU constants
--	ALU_constants 	- ALU specific constants
--	RegConstants 	- Register specific constants
--	FlagConstants 	- Flag specific constants
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
	constant NUM_REG_LOG:           integer := 5;
	constant NUM_IO_LOG:            integer := 6;
	constant DATA_BITS_LOG:         integer := 3;
	constant ADDR_BITS_LOG:         integer := 4;
	
	constant X_REG_LOW_BYTE:		std_logic_vector(NUM_DATA_BITS - 1 downto 0)
									:= "11010";
	constant X_REG_HIGH_BYTE:		std_logic_vector(NUM_DATA_BITS - 1 downto 0)
									:= "11011";
	constant Y_REG_LOW_BYTE:		std_logic_vector(NUM_DATA_BITS - 1 downto 0)
									:= "11100";
	constant Y_REG_HIGH_BYTE:		std_logic_vector(NUM_DATA_BITS - 1 downto 0)
									:= "11101";
	constant Z_REG_LOW_BYTE:		std_logic_vector(NUM_DATA_BITS - 1 downto 0)
									:= "11110";
	constant Z_REG_HIGH_BYTE:		std_logic_vector(NUM_DATA_BITS - 1 downto 0)
									:= "11111";
									
	constant GP_Dst_SelectB_Reg		std_logic_vector(1 downto 0) := "00";
	constant GP_Dst_SelectB_X		std_logic_vector(1 downto 0) := "01";
	constant GP_Dst_SelectB_Y		std_logic_vector(1 downto 0) := "10";
	constant GP_Dst_SelectB_Z		std_logic_vector(1 downto 0) := "11";
	
end package;