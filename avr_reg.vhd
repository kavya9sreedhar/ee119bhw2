----------------------------------------------------------------------------
--  AVR Registers
--
--	This file contains an implementation of the registers for an 8-bit
--  AVR architecture include the GP registers along with the IO space
--  registers.
--
--  Revision History:
--	30 Jan 19	Kavya Sreedhar & Dan Xu 	Initial Revision
--	31 Jan 19	Kavya Sreedhar & Dan Xu 	Bug Fixes
--	01 Feb 19	Kavya Sreedhar & Dan Xu 	Added more documentation
----------------------------------------------------------------------------

library ieee;
library RegConstants;
library CPU_CONSTANTS;

use CPU_CONSTANTS.all;
use RegConstants.all;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

--
-- AVR Registers entity declarations
-- Ports
--
-- Inputs:
-- clk
--   The clock signal into the system.
-- data_databus_in
--   The data bus going into the registers
-- ALU_in
--   The ALU output going into the registers
-- Updated_SREG [NUM_DATA_BITS-1..0]
--   The updated status register.
-- GP Reg Control lines.
-- GP_Input_Select [NUM_GP_INP_SELECT_BITS-1..0]
--   Select which input source to use (Data bus or ALU)
-- GP_Write_Enable
--   Enable write to the GP registers
-- GP_Swap_Nibbles
--   Ctrl signal to swap the nibbles for the GP register
-- GP_Dst_Select [NUM_REG_LOG-1..0]
--   Ctrl signal for which register to write to.
-- GP_Src_SelectA [NUM_REG_LOG-1..0]
--   Select for which GP register to output on A.
-- GP_Src_SelectB [NUM_REG_LOG-1..0]
--   Select for which GP register to output on B.
-- IO Reg Control lines.
-- IO_Input_Select
--   The select for what IO input source to use.
-- IO_Write_Enable
--   The enable for writing to the IO registers.
-- IO_Dst_Select [NUM_IO_LOG-1..0]
--   The select for which IO reg. to write to.
-- IO_Src_SelectA [NUM_IO_LOG-1..0]
--   The select for what IO register to output on A.
-- IO_Src_SelectB [NUM_IO_LOG-1..0]
--   The select for what IO register to output on B.
--

entity AVRRegisters is
	port(
		-- Clock
		clk                   : in std_logic;   
        
        -- GP Register control
        -- Input
        data_databus_in       : in std_logic_vector(NUM_DATA_BITS-1 downto 0);
        ALU_in                : in std_logic_vector(NUM_DATA_BITS-1 downto 0);
		-- Outputs (Comes as a buffer with all bits)
		GP_outA               : out std_logic_vector(NUM_DATA_BITS-1 downto 0);
		GP_outB               : out std_logic_vector(NUM_DATA_BITS-1 downto 0);
        -- Ctrl signals
        GP_Input_Select       : in std_logic_vector(NUM_GP_INP_SELECT_BITS-1 downto 0);
        GP_Write_Enable       : in std_logic;
        GP_Swap_Nibbles       : in std_logic;
		GP_Dst_Select         : in std_logic_vector(NUM_REG_LOG-1 downto 0);
		GP_Src_SelectA        : in std_logic_vector(NUM_REG_LOG-1 downto 0);
        GP_Src_SelectB        : in std_logic_vector(NUM_REG_LOG-1 downto 0);
        
        -- IO Register control
        -- Inputs
        Updated_SREG          : in std_logic_vector(NUM_DATA_BITS-1 downto 0);
		-- Outputs (Comes as a buffer with all bits)
		IO_outA               : out std_logic_vector(NUM_DATA_BITS-1 downto 0);
		IO_outB               : out std_logic_vector(NUM_DATA_BITS-1 downto 0);
        -- Ctrl signals
        IO_Input_Select       : in std_logic;
		IO_Write_Enable       : in std_logic;
		IO_Dst_Select         : in std_logic_vector(NUM_IO_LOG-1 downto 0);
		IO_Src_SelectA        : in std_logic_vector(NUM_IO_LOG-1 downto 0);
        IO_Src_SelectB        : in std_logic_vector(NUM_IO_LOG-1 downto 0)		
     );
end entity;

-- Architecture for the AVR registers
architecture standard of AVRRegisters is

    signal GPReg_in : out std_logic_vector(NUM_DATA_BITS-1 downto 0);
    signal IOReg_in : in std_logic_vector(NUM_DATA_BITS-1 downto 0);

begin

    -- GP registers
    GPReg : entity work.Registers(standard)
        port map(

            -- Hook up the clock
            clk                   => clk,

            -- Hook up the input
            reg_in                => GPReg_in,

            -- Hook up outputs
            reg_outA              => GP_outA,
            reg_outB              => GP_outB,

            -- Ctrl signals
            Register_Write_Enable => GP_Write_Enable,
            Register_Dst_Select   => GP_Dst_Select,
            Register_Src_SelectA  => GP_Src_SelectA,
            Register_Src_SelectB  => GP_Src_SelectB
    );

    -- IO Registers
    IOReg : entity work.Registers(standard)
        port map(

            -- Hook up the clock
            clk                   => clk,

            -- Hook up the input
            reg_in                => IOReg_in,

            -- Hook up outputs
            reg_outA              => IO_outA,
            reg_outB              => IO_outB,

            -- Ctrl signals
            Register_Write_Enable => IO_Write_Enable,
            Register_Dst_Select   => IO_Dst_Select,
            Register_Src_SelectA  => IO_Src_SelectA,
            Register_Src_SelectB  => IO_Src_SelectB
    );

    -- Process to determint the input to the GP registers
    GP_input_determine : process(GP_Input_Select, GP_Swap_Nibbles, GP_outA, IO_outA, data_databus_in, ALU_in)

        -- Selection before swapping
        signal intermediate_select : std_logic_vector(NUM_IO_LOG-1 downto 0);

    begin

        -- Figure out which input to use for IO registers
        case GP_Input_Select is
            -- Use one of GP register outputs.
            when GP_IN_SEL_GP_A => 
                intermediate_select := GP_outA;
            -- Use one of IO register outputs.
            when GP_IN_SEL_IO_A =>
                intermediate_select := IO_outA;
            -- Use the data bus
            when GP_IN_SEL_DATA_DATABUS =>
                intermediate_select := data_databus_in;
            -- Use the ALU
            when GP_IN_SEL_ALU =>
                intermediate_select := ALU_in;
            -- Error value -> should not occur
            when others =>
                intermediate_select := (NUM_IO_LOG-1 downto 0 => 'X');
        end case;
        
        -- Check for swap, and swap if so
        if (GP_Swap_Nibbles = SWAP_EN) then
            GPReg_in <= intermediate_select((NUM_IO_LOG/2)-1 downto 0) & 
              intermediate_select(NUM_IO_LOG-1 downto (NUM_IO_LOG/2));
        else
            GPReg_in <= intermediate_select;
        end if;

    end process GP_input_determine;

    -- Process to determint the input to the IO registers
    IO_input_determine : process(IO_Input_Select, GP_outA, Updated_SREG)
    begin

        -- Figure out which input to use for IO registers
        case IO_Input_Select is
            -- Use one of GP register outputs.
            when IO_IN_SEL_GP_A => 
                IOReg_in := GP_outA;
            -- Use one of IO register outputs.
            when IO_IN_SEL_SREG_ALU =>
                IOReg_in := Updated_SREG;
            -- Error value -> should not occur
            when others =>
                IOReg_in := 'X';
        end case;

    end process IO_input_determine;    

end architecture;