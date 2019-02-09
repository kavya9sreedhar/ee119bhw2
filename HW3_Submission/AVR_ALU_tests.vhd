----------------------------------------------------------------------------
--
--  Test Bench for AVR ALU
--
--  This is a test bench for the ALU_TEST entity.
--  Tests included are:
--    
--
--  The test bench entity is called ALU_Tester.
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

entity ALU_Tester is
    -- 1MHz Clock
    constant CLOCK_PERIOD  : time := 1 us;
    
end ALU_Tester;