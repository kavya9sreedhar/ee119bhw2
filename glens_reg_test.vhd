----------------------------------------------------------------------------
--
--  Atmel AVR Register Array Test Entity Declaration
--
--  This is the entity declaration which must be used for building the
--  register array portion of the AVR design for testing.
--
--  Revision History:
--     17 Apr 98  Glen George       Initial revision.
--     20 Apr 98  Glen George       Fixed minor syntax bugs.
--     22 Apr 02  Glen George       Updated comments.
--     18 Apr 04  Glen George       Updated comments and formatting.
--     21 Jan 06  Glen George       Updated comments.
--
----------------------------------------------------------------------------


--
--  REG_TEST
--
--  This is the register array testing interface.  It just brings all the
--  important register array signals out for testing along with the
--  Instruction Register.
--
--  Inputs:
--    IR      - Instruction Register (16 bits)
--    RegIn   - input to the register array (8 bits)
--    clock   - the system clock
--
--  Outputs:
--    RegAOut - register bus A output (8 bits), eventually will connect to ALU
--    RegBOut - register bus B output (8 bits), eventually will connect to ALU
--

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.numeric_std.all;

library opcodes;
use opcodes.opcodes.all;

library work;


entity  REG_TEST  is

    port(
        IR       :  in  opcode_word;                        -- Instruction Register
        RegIn    :  in  std_logic_vector(7 downto 0);       -- input register bus
        clock    :  in  std_logic;                          -- system clock
        RegAOut  :  out std_logic_vector(7 downto 0);       -- register bus A out
        RegBOut  :  out std_logic_vector(7 downto 0)        -- register bus B out
    );

end  REG_TEST;

architecture structural of REG_TEST is 
        


        -- GP Register control
        signal GP_Input_Select : std_logic_vector(NUM_GP_INP_SELECT_BITS-1 downto 0);
        signal GP_Write_Enable : std_logic;
        signal GP_Swap_Nibbles : std_logic;
		signal GP_Dst_Select   : std_logic_vector(NUM_REG_LOG-1 downto 0);
		signal GP_Src_SelectA  : std_logic_vector(NUM_REG_LOG-1 downto 0);
        signal GP_Src_SelectB  : std_logic_vector(NUM_REG_LOG-1 downto 0);
        
        -- IO Register control
        signal IO_Input_Select : std_logic;
		signal IO_Write_Enable : std_logic;
		signal IO_Dst_Select   : std_logic_vector(NUM_IO_LOG-1 downto 0);
		signal IO_Src_SelectA  : std_logic_vector(NUM_IO_LOG-1 downto 0);
        signal IO_Src_SelectB  : std_logic_vector(NUM_IO_LOG-1 downto 0);

        -- Inputs (IOREG)
        signal Updated_SREG    : std_logic_vector(NUM_DATA_BITS-1 downto 0);
        -- Outputs (IOREG)
		signal IO_outA         : std_logic_vector(NUM_DATA_BITS-1 downto 0);
		signal IO_outB         : std_logic_vector(NUM_DATA_BITS-1 downto 0)
    
begin

    ControlUnit : entity work.control_unit(control_arch)
        port map (

        );

    Registers : entity work.AVRRegisters(standard) 
        port map (
            -- Connect up the clock
            clk             => clock;

            -- For now just connect both inputs to the Reg in
            data_databus_in => RegIn;
            ALU_in          => RegIn;

            GP_Input_Select =>
            GP_Write_Enable =>
            GP_Swap_Nibbles =>
            GP_Dst_Select   =>
            GP_Src_SelectA  =>
            GP_Src_SelectB  =>
            
            -- GP Outputs
            GP_outA         => RegAOut,
            GP_outB         => RegBOut,

            -- IO Input (NOT CONNECTED TO ANY INPUT)
            Updated_SREG    => Updated_SREG;

            -- IO Control Signals (NOT CONNECTED TO ANY OUTPUT)
            IO_Input_Select => IO_Input_Select,
            IO_Write_Enable => IO_Write_Enable,
            IO_Dst_Select   => IO_Dst_Select,
            IO_Src_SelectA  => IO_Src_SelectA,
            IO_Src_SelectB  => IO_Src_SelectB,

            -- IO Outputs (NOT CONNECTED TO ANY OUTPUT)
            IO_outA         => IO_outA,
            IO_outB         => IO_outB

        );

end architecture;
