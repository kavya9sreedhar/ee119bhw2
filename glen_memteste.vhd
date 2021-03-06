----------------------------------------------------------------------------
--
--  Atmel AVR Data Memory Test Entity Declaration
--
--  This is the high level entity connecting the Control Unit, Registers and
--  Data Memory Access unit. This entity is intended for use in testing the
--  Dana Memory Access unit. Note that there are some high level multiplexing
--  of signals in this file to handle the data bus and to handle the read
--  write signals.
--
--  Revision History:
--     24 Apr 98  Glen George       Initial revision.
--     25 Apr 00  Glen George       Fixed entity name and updated comments.
--      2 May 02  Glen George       Updated comments.
--      3 May 02  Glen George       Fixed Reset signal type.
--     23 Jan 06  Glen George       Updated comments.
--     21 Jan 08  Glen George       Updated comments.
--     11 Feb 09  Kavya & Dan		Updated to fit with our files
--									and test our signals + code
--	   11 Feb 09  Kavya & Dan		Updated comments
--
----------------------------------------------------------------------------


--
--  MEM_TEST
--
--  This is the data memory access testing interface.  It brings all
--  the important data memory access signals out for testing along with the
--  Instruction Register and Program Data Bus.
--
--  Needs the Control Unit, Registers and Data Memory Access Unit
--
--  Inputs:
--    IR     - Instruction Register (16 bits)
--    ProgDB - program memory data bus (16 bits)
--    Reset  - active low reset signal
--    clock  - the system clock
--
--  Outputs:
--    DataAB - data memory address bus (16 bits)
--    DataDB - data memory data bus (8 bits)
--    DataRd - data read (active low)
--    DataWr - data write (active low)
--

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.numeric_std.all;

library work;
use work.CPU_CONSTANTS.all;
use work.DMAU_constants.all;
use work.opcodes.all;
use work.RegConstants.all;


entity  MEM_TEST  is

    port (
        IR      :  in     opcode_word;                      -- Instruction Register
        ProgDB  :  in     std_logic_vector(15 downto 0);    -- second word of instruction
        Reset   :  in     std_logic;                        -- system reset signal (active low)
        clock   :  in     std_logic;                        -- system clock
        DataAB  :  out    std_logic_vector(15 downto 0);    -- data address bus
        DataDB  :  inout  std_logic_vector(7 downto 0);     -- data data bus
        DataRd  :  out    std_logic;                        -- data read (active low)
        DataWr  :  out    std_logic                         -- data write (active low)
    );

end  MEM_TEST;

architecture structural of MEM_TEST is

    signal databus_in_mux          : std_logic_vector(NUM_DATA_BITS-1 downto 0);
    signal Store                   : std_logic;
    
    -- Intermediate Rd/Wr signals
    signal inter_Rd                : std_logic;
    signal inter_Wr                : std_logic;

    -- Register inputs
    signal ALU_in                  : std_logic_vector(NUM_DATA_BITS-1 downto 0);

    -- Register Ctrl Sginals
    signal GP_Swap_Nibbles         : std_logic;

    signal GP_Write_EnableA	       : std_logic;
    signal GP_Dst_SelectA          : std_logic_vector(NUM_REG_LOG-1 downto 0);
    signal GP_Src_SelectA          : std_logic_vector(NUM_REG_LOG-1 downto 0);

    signal GP_Write_EnableB	       : std_logic;
    signal GP_Dst_SelectB          : std_logic_vector(NUM_GP_WIDE_LOAD_BITS-1 downto 0);
    signal GP_Src_SelectB          : std_logic_vector(NUM_REG_LOG-1 downto 0);

    signal GP_Input_Select         : std_logic_vector(NUM_GP_INP_SELECT_BITS - 1 downto 0);

    -- Register Outputs
    signal GP_outA                 : std_logic_vector(NUM_DATA_BITS-1 downto 0);
    signal GP_outB                 : std_logic_vector(NUM_DATA_BITS-1 downto 0);
    
    -- IO Reg Inputs
    signal Updated_SREG            : std_logic_vector(NUM_DATA_BITS-1 downto 0);
    -- IO Reg Outputs
    signal IO_outA                 : std_logic_vector(NUM_DATA_BITS-1 downto 0);
    signal IO_outB                 : std_logic_vector(NUM_DATA_BITS-1 downto 0);
    -- IO Reg Ctrl signals
    signal IO_Input_Select         : std_logic;
    signal IO_Write_EnableA        : std_logic;
    signal IO_Write_EnableB        : std_logic;
    signal IO_Dst_SelectA          : std_logic_vector(NUM_IO_LOG-1 downto 0);
    signal IO_Src_SelectA          : std_logic_vector(NUM_IO_LOG-1 downto 0);
    signal IO_Src_SelectB          : std_logic_vector(NUM_IO_LOG-1 downto 0);

    -- Data Address Unit Control Signals

	signal Data_Addr_Src_Sel       : std_logic_vector(num_bits_Data_Addr_Src_Sel - 1 downto 0);
	signal Offset_Src_Sel		   : std_logic_vector(num_bits_Offset_Src_Sel - 1 downto 0);
	signal unsigned_displacement   : std_logic_vector(NUM_ADDRESS_BITS - 1 downto 0);
	signal Pre_Post_Sel            : std_logic;

    signal Immediate_Data_Address  : std_logic_vector(NUM_ADDRESS_BITS - 1 downto 0);
    signal X_register              : std_logic_vector(NUM_ADDRESS_BITS - 1 downto 0);
    signal Y_register              : std_logic_vector(NUM_ADDRESS_BITS - 1 downto 0);
    signal Z_register              : std_logic_vector(NUM_ADDRESS_BITS - 1 downto 0);
    signal SP_register             : std_logic_vector(NUM_ADDRESS_BITS - 1 downto 0);

    -- Data Immediate Load signals
    signal LDI_op                  : std_logic;
	signal immed_val               : std_logic_vector(NUM_DATA_BITS - 1 downto 0);

    -- Data Address Unit return ptr value
    signal Summed_Signal           : std_logic_vector(NUM_ADDRESS_BITS - 1 downto 0);

begin

    DataRd <= inter_Rd or clock;
    DataWr <= inter_Wr or clock;

    databus_mux : process (DataDB, LDI_op, immed_val, GP_outA, Store)
    begin

        -- Check for immediate loading
        if (LDI_op = '1') then 
            databus_in_mux <= immed_val;
        else
            databus_in_mux <= DataDB;
        end if;

        -- Check if we are currently storing
        if (Store = '1') then
            DataDB <= GP_outA;
        else
            DataDB <= (others => 'Z');
        end if;
    end process databus_mux;

    ControlUnit : entity work.control_unit(standard)
    port map (
        -- Connect up the clock
        clk                    => clock,

        -- Hook up Program stuff
        Program_Data_Bus       => ProgDB,
        IR                     => IR,

        -- Hook up Register Control Signals
        GP_outA                => GP_outA,
        GP_outB                => GP_outB,

        IO_outA                => IO_outA,
        IO_outB                => IO_outB,

        GP_Swap_Nibbles        => GP_Swap_Nibbles,
        GP_Dst_SelectA         => GP_Dst_SelectA,
        GP_Src_SelectA         => GP_Src_SelectA,
        GP_Src_SelectB         => GP_Src_SelectB,
        GP_Dst_SelectB         => GP_Dst_SelectB,
        GP_Write_EnableA       => GP_Write_EnableA,
        GP_Write_EnableB       => GP_Write_EnableB,
        GP_Input_Select        => GP_Input_Select,

        IO_Input_Select        => IO_Input_Select,
        IO_Write_EnableA       => IO_Write_EnableA,
        IO_Write_EnableB       => IO_Write_EnableB,
        IO_Dst_SelectA         => IO_Dst_SelectA,
        IO_Src_SelectA         => IO_Src_SelectA,
        IO_Src_SelectB         => IO_Src_SelectB,

        -- Hook up Data Memory Control Signals
        Data_Addr_Src_Sel      => Data_Addr_Src_Sel,
        Offset_Src_Sel         => Offset_Src_Sel,
        unsigned_displacement  => unsigned_displacement,
        Pre_Post_Sel           => Pre_Post_Sel,
        Immediate_Data_Address => Immediate_Data_Address,
        X_register             => X_register,
        Y_register             => Y_register,
        Z_register             => Z_register,
        SP_register            => SP_register,
        LDI_op                 => LDI_op,
        immed_val              => immed_val,

        Store                  => Store,

        -- Read Write Signals
        DataRdOut              => inter_Rd,
        DataWrOut              => inter_Wr
    );

    Registers : entity work.AVRRegisters(standard) 
    port map (
        -- Connect up the clock
        clk                    => clock,
        -- Connect the Reset Line
        rst                    => Reset,

        -- GP Register inputs
        data_databus_in        => databus_in_mux,
        ALU_in                 => ALU_in,
        data_address_in        => Summed_Signal,

        -- GP Control signals
        GP_outA                => GP_outA,
        GP_outB                => GP_outB,

        GP_Input_Select        => GP_Input_Select,
        GP_Write_EnableA       => GP_Write_EnableA,
        GP_Swap_Nibbles        => GP_Swap_Nibbles,
        GP_Dst_SelectA         => GP_Dst_SelectA,

        GP_Write_EnableB       => GP_Write_EnableB,
        GP_Dst_SelectB         => GP_Dst_SelectB,

        GP_Src_SelectA         => GP_Src_SelectA,
        GP_Src_SelectB         => GP_Src_SelectB,
 
        -- IO Register Inputs
        Updated_SREG           => Updated_SREG,
        Updated_SP             => Summed_Signal,

        -- IO Control signals
        IO_outA                => IO_outA,
        IO_outB                => IO_outB,

        IO_Input_Select        => IO_Input_Select,
        IO_Write_EnableA       => IO_Write_EnableA, 
        IO_Dst_SelectA         => IO_Dst_SelectA,

        IO_Write_EnableB       => IO_Write_EnableB,

        IO_Src_SelectA         => IO_Src_SelectA,
        IO_Src_SelectB         => IO_Src_SelectB
    );

    DataAddressUnit : entity work.Data_Memory_Access(Data_arch)
    port map (

        Data_Addr_Src_Sel      => Data_Addr_Src_Sel,
        Offset_Src_Sel         => Offset_Src_Sel,
        unsigned_displacement  => unsigned_displacement,
        Pre_Post_Sel           => Pre_Post_Sel,

        Immediate_Data_Address => Immediate_Data_Address,
        X_register             => X_register,
        Y_register             => Y_register,
        Z_register             => Z_register,
        SP_register            => SP_register,

        Data_Address_Bus       => DataAB,
        Summed_Signal          => Summed_Signal

    );

end architecture;