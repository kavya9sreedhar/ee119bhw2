----------------------------------------------------------------------------
--
--  Test Bench for Register
--
--  This is a test bench for the Register entity. Tests proper loading
--  and displaying of registers.
--
--  The test bench entity is called RegisterTester.
--
--  Revision History:
--	30 Jan 19    Dan Xu    Initial Revision
----------------------------------------------------------------------------

library ieee;
library work;

-- library declarations
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.std_logic_arith.all;

entity RegisterTester is
    -- 1MHz Clock
    constant CLOCK_PERIOD  : time := 1 us;
    
end RegisterTester;

architecture TestBench of RegisterTester is

    -- Number of bits for the register unit to test.
    constant NUM_BITS            : positive := 8;
    -- Number of registers to test.
    constant LNUM_REGISTERS       : positive := 5;

    -- Clock
    signal clk                   : std_logic;   
    
    -- Input
    signal reg_in                : std_logic_vector(NUM_BITS-1 downto 0);

    -- Outputs (Comes as a buffer with all bits)
    signal reg_outA              : std_logic_vector(NUM_BITS-1 downto 0);
    signal reg_outB              : std_logic_vector(NUM_BITS-1 downto 0);

    -- Ctrl signals
    signal Register_Write_Enable : std_logic;
    signal Register_Dst_Select   : std_logic_vector(LNUM_REGISTERS-1 downto 0);
    signal Register_Src_SelectA  : std_logic_vector(LNUM_REGISTERS-1 downto 0);
    signal Register_Src_SelectB  : std_logic_vector(LNUM_REGISTERS-1 downto 0);
    
    -- Signal used to stop clock signal generators
    signal  END_SIM  :  BOOLEAN := FALSE;

    ---------------
    -- Functions --
    ---------------
    
    -- std_logic_vec_to_string
    -- Converts a std_logic_vector to a string
    --
    -- Params:
    --     vec
    --         The std_logic_vector to convert to a string
    --
    -- Return:
    --     A string representation of the std_logic_vector
    function std_logic_vec_to_string (vec: std_logic_vector) return string is
        variable representation : string (vec'length downto 1);
    begin
        for i in vec'length-1 downto 0 loop
            representation(i+1) := std_logic'image(vec((i)))(2);
        end loop;
    return representation;
    
end function;

begin

    -- Register Unit to be tested
    UUT : entity work.Registers(standard)
        port map(

            -- Hook up the clock
            clk                   => clk,

            -- Hook up the input
            reg_in                => reg_in,

            -- Hook up outputs
            reg_outA              => reg_outA,
            reg_outB              => reg_outB,

            -- Ctrl signals
            Register_Write_Enable => Register_Write_Enable,
            Register_Dst_Select   => Register_Dst_Select,
            Register_Src_SelectA  => Register_Src_SelectA,
            Register_Src_SelectB  => Register_Src_SelectB
        );

    -----------------------
    --   START TESTING   --
    -----------------------
    test_main: process
        variable current              : std_logic_vector(LNUM_REGISTERS-1 downto 0);
    begin
        report "-------- START TESTS --------";
        
        -- Initialize all input values to 'X'
        Register_Write_Enable <= 'X';
        Register_Dst_Select   <= (others => 'X');
        Register_Src_SelectA  <= (others => 'X');
        Register_Src_SelectB  <= (others => 'X');
        reg_in                <= (others => 'X');
        
        wait for 4*CLOCK_PERIOD;
        
        for i in 0 to 2 ** LNUM_REGISTERS - 1 loop

            current               := std_logic_vector(to_unsigned(i, LNUM_REGISTERS));

            -- Load a value corresponding to the register
            reg_in                <= std_logic_vector(to_unsigned(i, NUM_BITS));
            
            -- Signals to load it in.
            Register_Write_Enable <= '1';
            Register_Dst_Select   <= current;

            wait for CLOCK_PERIOD;

        end loop;

        for i in 0 to 2 ** LNUM_REGISTERS - 1 loop

            current               := std_logic_vector(to_unsigned(i, LNUM_REGISTERS));

            -- Load a value corresponding to the register
            reg_in                <= (NUM_BITS-1 downto 0 => '0');
            
            -- Signals to not load it in.
            Register_Write_Enable <= '0';
            Register_Dst_Select   <= current;
            Register_Src_SelectA  <= current;
            Register_Src_SelectB  <= current;

            wait for CLOCK_PERIOD/2;

            assert (std_match(reg_outA, std_logic_vector(to_unsigned(i, NUM_BITS))))
            report  "RegA Selection Failure"  & 
                    " Got: "                  & std_logic_vec_to_string(reg_outA) &
                    " Expected: "             & std_logic_vec_to_string(
                                                  std_logic_vector(to_unsigned(i, NUM_BITS)))
            severity  ERROR;

            assert (std_match(reg_outB, std_logic_vector(to_unsigned(i, NUM_BITS))))
            report  "RegB Selection Failure"  & 
                    " Got: "                  & std_logic_vec_to_string(reg_outB) &
                    " Expected: "             & std_logic_vec_to_string(
                                                  std_logic_vector(to_unsigned(i, NUM_BITS)))
            severity  ERROR;

            wait for CLOCK_PERIOD/2;

        end loop;

        END_SIM <= TRUE;        -- end of stimulus events
        
        -- Report that all tests are done.
        report "-------- ALL TESTS DONE --------";
        
        wait;                   -- wait for simulation to end

    end process test_main;

    -- Generate clock
    clk_gen : process
    begin
        -- this process generates a 50% duty cycle clock based on the CLOCK_PERIOD constant
        -- only generate clock if still simulating
        if END_SIM = FALSE then
            clk <= '1';
            wait for CLOCK_PERIOD/2;
        else
            wait;
        end if;

        if END_SIM = FALSE then
            clk <= '0';
            wait for CLOCK_PERIOD/2;
        else
            wait;
        end if;

    end process;

end architecture TestBench;