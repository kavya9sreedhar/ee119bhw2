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

end package;