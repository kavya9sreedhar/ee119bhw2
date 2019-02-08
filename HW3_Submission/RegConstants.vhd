----------------------------------------------------------------------------
--  Register Constants
--
--	This file contains contains constants used for the 
--	Registers implementation specifically for an 8-bit AVR architecture.
--
--  Revision History:
--	28 Jan 19	Kavya Sreedhar & Dan Xu 	Initial revision
--	1  Feb 19	Kavya Sreedhar & Dan Xu		Updated revision history
----------------------------------------------------------------------------

-- declaration of libraries used
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

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