----------------------------------------------------------------------------
--
--  Test Bench for AVR Registers
--
--  This is a test bench for the REG_TEST entity.
--  Tests included are:
--    
--
--  The test bench entity is called Register_Tester.
--
--  Revision History:
--     02/01/19  Daniel Xu                Initial revision.
----------------------------------------------------------------------------

library ieee;
library work;
library opcodes;

use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;
use ieee.std_logic_arith.ALL;
use opcodes.opcodes.all;

entity Register_Tester is
    -- 1MHz Clock
    constant CLOCK_PERIOD  : time := 1 us;
    
end Register_Tester;