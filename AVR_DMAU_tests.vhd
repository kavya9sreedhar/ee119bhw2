----------------------------------------------------------------------------
--
--  Test Bench for AVR Data Memory Address Units
--
--  This is a test bench for the MEM_TEST entity.
--  Tests that data is successfully moved in and out of the registers
--  using the Data Address Unit
--
--  The test bench entity is called DataMemory_Tester.
--
--  Revision History:
--     02/01/19  Daniel Xu and Kavya Sreedhar   Initial revision.
--     02/02/19  Daniel Xu and Kavya Sreedhar   Finished writing tests.
----------------------------------------------------------------------------

library ieee;
library work;

use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;
use ieee.std_logic_arith.ALL;

use work.opcodes.all;
use work.RegConstants.all;
use work.CPU_CONSTANTS.all;

entity Register_Tester is
    -- 1MHz Clock
    constant CLOCK_PERIOD  : time := 1 us;
    
end Register_Tester;

-- Architecture
architecture TB of Register_Tester is

    -- Clock signal
    signal clk       : std_logic;

    -- The instruction register input
    signal IR_input  : opcode_word;
    -- The second word of instruction
    signal ProgDB    : std_logic_vector(2*NUM_BITS-1 downto 0);

    -- System reset signal.
    signal Reset     : std_logic;

    -- Data Address bus
    signal DataAB    : std_logic_vector(2*NUM_BITS-1 downto 0);
    -- Data Data bus
    signal DataDB    : std_logic_vector(NUM_BITS-1 downto 0);

    -- Read/Write Signals
    signal DataRd  :      std_logic;
    signal DataWr  :      std_logic;

    -- Signal used to stop clock signal generators
    signal  END_SIM  :  BOOLEAN := FALSE;
    
end architecture;