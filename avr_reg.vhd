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
--	08 Feb 19	Kavya Sreedhar & Dan Xu 	Added wide loading functionality
--	09 Feb 19	Kavya Sreedhar & Dan Xu 	Changed to use two separate reg.
--                                            modlues.
----------------------------------------------------------------------------

library ieee;
library work;

use work.CPU_CONSTANTS.all;
use work.RegConstants.all;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

--
-- AVR Registers entity declarations
-- Ports
--
-- Inputs:
-- clk
--   The clock signal into the system.
-- data_databus_in [NUM_DATA_BITS-1..0]
--   The data bus going into the registers
-- ALU_in [NUM_DATA_BITS-1..0]
--   The ALU output going into the registers
-- data_address_in [2*NUM_DATA_BITS-1..0]
--   The updated data address
-- Updated_SREG [NUM_DATA_BITS-1..0]
--   The updated status register.
-- GP Reg Control lines.
-- GP_Input_Select [NUM_GP_INP_SELECT_BITS-1..0]
--   Select which input source to use (Data bus or ALU)
-- GP_Write_EnableA
--   Enable standard writes to the GP registers
-- GP_Swap_Nibbles
--   Ctrl signal to swap the nibbles for the GP register
-- GP_Dst_SelectA [NUM_REG_LOG-1..0]
--   Ctrl signal for which register to write to for
--   a standard register write.
-- GP_Write_EnableB
--   Enables wide bus writes to certain registers.
-- GP_Dst_SelectB [NUM_GP_WIDE_LOAD_BITS-1..0]
--   Destination for a wide bus write to registers
--   00 - R16
--   01 - X
--   10 - Y
--   10 - Z
-- GP_Src_SelectA [NUM_REG_LOG-1..0]
--   Select for which GP register to output on A.
-- GP_Src_SelectB [NUM_REG_LOG-1..0]
--   Select for which GP register to output on B.
-- IO Reg Control lines.
-- IO_Input_Select
--   The select for what IO input source to use.
-- IO_Write_EnableA
--   The enable for standard writing to the IO registers.
-- IO_Write_EnableB
--   Enable a wide bus write to the stack pointer.
-- IO_Dst_Select [NUM_IO_LOG-1..0]
--   The select for which IO reg. to write to.
-- IO_Src_SelectA [NUM_IO_LOG-1..0]
--   The select for what IO register to output on A.
-- IO_Src_SelectB [NUM_IO_LOG-1..0]
--   The select for what IO register to output on B.
--
-- Outputs:
-- GP_outA
--   Output A from the GP registers
-- GP_outB
--   Output B from the GP registers
-- IO_outA
--   Output A from the IO registers
-- IO_outB
--   Output B from the IO registers

entity AVRRegisters is
	port(
		-- Clock
		clk                   : in std_logic;   
        
        -- GP Register control
        -- Input
        data_databus_in       : in std_logic_vector(NUM_DATA_BITS-1 downto 0);
        ALU_in                : in std_logic_vector(NUM_DATA_BITS-1 downto 0);
        data_address_in       : in std_logic_vector(2*NUM_DATA_BITS-1 downto 0);
		-- Outputs (Comes as a buffer with all bits)
		GP_outA               : out std_logic_vector(NUM_DATA_BITS-1 downto 0);
		GP_outB               : out std_logic_vector(NUM_DATA_BITS-1 downto 0);
        -- Ctrl signals
        GP_Input_Select       : in std_logic_vector(NUM_GP_INP_SELECT_BITS-1 downto 0);

        GP_Write_EnableA      : in std_logic;
        GP_Swap_Nibbles       : in std_logic;
        GP_Dst_SelectA        : in std_logic_vector(NUM_REG_LOG-1 downto 0);
        
        GP_Write_EnableB      : in std_logic;
		GP_Dst_SelectB        : in std_logic_vector(NUM_GP_WIDE_LOAD_BITS-1 downto 0);

		GP_Src_SelectA        : in std_logic_vector(NUM_REG_LOG-1 downto 0);
        GP_Src_SelectB        : in std_logic_vector(NUM_REG_LOG-1 downto 0);
        
        -- IO Register control
        -- Inputs
        Updated_SREG          : in std_logic_vector(NUM_DATA_BITS-1 downto 0);
        Updated_SP            : in std_logic_vector(2*NUM_DATA_BITS-1 downto 0);
		-- Outputs (Comes as a buffer with all bits)
		IO_outA               : out std_logic_vector(NUM_DATA_BITS-1 downto 0);
		IO_outB               : out std_logic_vector(NUM_DATA_BITS-1 downto 0);
        -- Ctrl signals
        IO_Input_Select       : in std_logic;
        IO_Write_EnableA      : in std_logic;
        IO_Write_EnableB      : in std_logic;
		IO_Dst_SelectA        : in std_logic_vector(NUM_IO_LOG-1 downto 0);
		IO_Src_SelectA        : in std_logic_vector(NUM_IO_LOG-1 downto 0);
        IO_Src_SelectB        : in std_logic_vector(NUM_IO_LOG-1 downto 0)		
     );
end entity;

-- Architecture for the AVR registers
architecture standard of AVRRegisters is

    signal GPReg_in       : std_logic_vector(NUM_DATA_BITS-1 downto 0);
    signal IOReg_in       : std_logic_vector(NUM_DATA_BITS-1 downto 0);
    
    signal GP_A_intermed  : std_logic_vector(NUM_DATA_BITS-1 downto 0);
    signal IO_A_intermed  : std_logic_vector(NUM_DATA_BITS-1 downto 0);

begin

    -- Connect up the signals
    GP_outA <= GP_A_intermed;
    IO_outA <= IO_A_intermed;

    -- GP registers
    GPReg : entity work.GPRegisters(standard)
        generic map (
            NUM_BITS               =>  NUM_DATA_BITS,
            LNUM_REGISTERS         =>  NUM_REG_LOG
        )
        port map(

            -- Hook up the clock
            clk                    => clk,

            -- Hook up the input
            reg_inA                => GPReg_in,
            reg_inB                => data_address_in,

            -- Hook up outputs
            reg_outA               => GP_A_intermed,
            reg_outB               => GP_outB,

            -- Ctrl signals
            Register_Write_EnableA => GP_Write_Enable,
            Register_Dst_SelectA   => GP_Dst_Select,

            Register_Write_EnableB => GP_Write_EnableB,
            Register_Dst_SelectB   => GP_Dst_SelectB,

            Register_Src_SelectA  => GP_Src_SelectA,
            Register_Src_SelectB  => GP_Src_SelectB
    );

    -- IO Registers
    IOReg : entity work.IORegisters(standard)
        generic map (
			NUM_BITS               =>  NUM_DATA_BITS,
            LNUM_REGISTERS         =>  NUM_IO_LOG
		)
		port map(

            -- Hook up the clock
            clk                    => clk,

            -- Hook up the input
            reg_inA                => IOReg_in,
            reg_inB                => data_address_in,

            -- Hook up outputs
            reg_outA               => IO_A_intermed,
            reg_outB               => IO_outB,

            -- Ctrl signals
            Register_Write_EnableA => IO_Write_EnableA,
            Register_Write_EnableB => IO_Write_EnableB,

            Register_Dst_SelectA   => IO_Dst_SelectA,

            Register_Src_SelectA   => IO_Src_SelectA,
            Register_Src_SelectB   => IO_Src_SelectB

    );

    -- Process to determint the input to the GP registers
    GP_input_determine : process(GP_Input_Select, GP_Swap_Nibbles, GP_A_intermed, IO_A_intermed, data_databus_in, ALU_in)

        -- Selection before swapping
        variable intermediate_select : std_logic_vector(NUM_DATA_BITS-1 downto 0);

    begin

        -- Figure out which input to use for IO registers
        case GP_Input_Select is
            -- Use one of GP register outputs.
            when GP_IN_SEL_GP_A => 
                intermediate_select := GP_A_intermed;
            -- Use one of IO register outputs.
            when GP_IN_SEL_IO_A =>
                intermediate_select := IO_A_intermed;
            -- Use the data bus
            when GP_IN_SEL_DATA_DATABUS =>
                intermediate_select := data_databus_in;
            -- Use the ALU
            when GP_IN_SEL_ALU =>
                intermediate_select := ALU_in;
            -- Error value -> should not occur
            when others =>
                intermediate_select := (NUM_DATA_BITS-1 downto 0 => 'X');
        end case;
        
        -- Check for swap, and swap if so
        if (GP_Swap_Nibbles = SWAP_EN) then
            GPReg_in <= intermediate_select((NUM_DATA_BITS/2)-1 downto 0) & 
              intermediate_select(NUM_DATA_BITS-1 downto (NUM_DATA_BITS/2));
        else
            GPReg_in <= intermediate_select;
        end if;

    end process GP_input_determine;

    -- Process to determint the input to the IO registers
    IO_input_determine : process(IO_Input_Select, GP_A_intermed, Updated_SREG)
    begin

        -- Figure out which input to use for IO registers
        case IO_Input_Select is
            -- Use one of GP register outputs.
            when IO_IN_SEL_GP_A => 
                IOReg_in <= GP_A_intermed;
            -- Use one of IO register outputs.
            when IO_IN_SEL_SREG_ALU =>
                IOReg_in <= Updated_SREG;
            -- Error value -> should not occur
            when others =>
                IOReg_in <= (NUM_DATA_BITS-1 downto 0 => 'X');
        end case;

    end process IO_input_determine;    

end architecture;