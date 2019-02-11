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
use work.AVR_Opcode_Formation.all;

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
    signal ProgDB    : std_logic_vector(2*NUM_DATA_BITS-1 downto 0);

    -- System reset signal.
    signal Reset     : std_logic;

    -- Data Address bus
    signal DataAB    : std_logic_vector(2*NUM_DATA_BITS-1 downto 0);
    -- Data Data bus
    signal DataDB    : std_logic_vector(NUM_DATA_BITS-1 downto 0);

    -- Read/Write Signals
    signal DataRd  :      std_logic;
    signal DataWr  :      std_logic;

    -- Signal used to stop clock signal generators
    signal  END_SIM  :  BOOLEAN := FALSE;

    -- Array of operations
    type op_lst_t is array (integer range <>) of opcode_word;
    type addr_lst_t is array (integer range <>) of std_logic_vector(NUM_ADDRESS_BITS-1 downto 0);
    type data_lst_t is array (integer range <>) of std_logic_vector(NUM_DATA_BITS-1 downto 0);

begin
    -- Connect all signals to the entity being tested.
    UUT: entity work.MEM_TEST(structural)
    port map(

        -- Hook up the clock
        clock => clk,
        -- Hook up the IR in
        IR => IR_input,

        -- Hook up the busses
        ProgDB => ProgDB,
        DataAB => DataAB,
        DataDB => DataDB,

        -- Hook up the Rd/Wr
        DataRd => DataRd,
        DataWr => DataWr,

        -- Hook up reset
        Reset => Reset
    );

    test_main: process
        constant NUM_X_TESTS            : integer := 14;
        variable X_test_opcodes         :   op_lst_t(0 to NUM_X_TESTS-1);
        variable X_test_data_ld         : data_lst_t(0 to NUM_X_TESTS-1);
        variable X_test_corr_addr       : addr_lst_t(0 to NUM_X_TESTS-1);
        variable X_test_corr_data       : data_lst_t(0 to NUM_X_TESTS-1);
        variable X_test_coor_rd         : std_logic_vector(0 to NUM_X_TESTS-1);
        variable X_test_corr_wr         : std_logic_vector(0 to NUM_X_TESTS-1);

        constant NUM_Y_TESTS            : integer := 14;
        variable Y_test_opcodes         :   op_lst_t(0 to NUM_Y_TESTS-1);
        variable Y_test_data_ld         : data_lst_t(0 to NUM_Y_TESTS-1);
        variable Y_test_corr_addr       : addr_lst_t(0 to NUM_Y_TESTS-1);
        variable Y_test_corr_data       : data_lst_t(0 to NUM_Y_TESTS-1);
        variable Y_test_coor_rd         : std_logic_vector(0 to NUM_Y_TESTS-1);
        variable Y_test_corr_wr         : std_logic_vector(0 to NUM_Y_TESTS-1);

        constant NUM_Z_TESTS            : integer := 14;
        variable Z_test_opcodes         :   op_lst_t(0 to NUM_Z_TESTS-1);
        variable Z_test_data_ld         : data_lst_t(0 to NUM_Z_TESTS-1);
        variable Z_test_corr_addr       : addr_lst_t(0 to NUM_Z_TESTS-1);
        variable Z_test_corr_data       : data_lst_t(0 to NUM_Z_TESTS-1);
        variable Z_test_coor_rd         : std_logic_vector(0 to NUM_Z_TESTS-1);
        variable Z_test_corr_wr         : std_logic_vector(0 to NUM_Z_TESTS-1);

        constant NUM_MOV_TESTS          : integer := 15;
        variable MOV_test_opcodes       :   op_lst_t(0 to NUM_MOV_TESTS-1);
        variable MOV_test_data_ld       : data_lst_t(0 to NUM_MOV_TESTS-1);
        variable MOV_test_corr_addr     : addr_lst_t(0 to NUM_MOV_TESTS-1);
        variable MOV_test_corr_data     : data_lst_t(0 to NUM_MOV_TESTS-1);
        variable MOV_test_coor_rd       : std_logic_vector(0 to NUM_MOV_TESTS-1);
        variable MOV_test_corr_wr       : std_logic_vector(0 to NUM_MOV_TESTS-1);

        constant NUM_STACK_TESTS        : integer := 10;
        variable SP_test_opcodes        :   op_lst_t(0 to NUM_STACK_TESTS-1);
        variable SP_test_data_ld        : data_lst_t(0 to NUM_STACK_TESTS-1);
        variable SP_test_corr_addr      : addr_lst_t(0 to NUM_STACK_TESTS-1);
        variable SP_test_corr_data      : data_lst_t(0 to NUM_STACK_TESTS-1);
        variable SP_test_coor_rd        : std_logic_vector(0 to NUM_STACK_TESTS-1);
        variable SP_test_corr_wr        : std_logic_vector(0 to NUM_STACK_TESTS-1);

        constant NUM_MEM_TESTS          : integer := 6;
        variable MEM_test_opcodes       :   op_lst_t(0 to NUM_MEM_TESTS-1);
        variable MEM_test_progDB        : addr_lst_t(0 to NUM_MEM_TESTS-1);
        variable MEM_test_data_ld       : data_lst_t(0 to NUM_MEM_TESTS-1);
        variable MEM_test_corr_addr     : addr_lst_t(0 to NUM_MEM_TESTS-1);
        variable MEM_test_corr_data     : data_lst_t(0 to NUM_MEM_TESTS-1);
        variable MEM_test_coor_rd       : std_logic_vector(0 to NUM_MEM_TESTS-1);
        variable MEM_test_corr_wr       : std_logic_vector(0 to NUM_MEM_TESTS-1);

    begin

        -- X Tests
        X_test_opcodes := (
                        -- LOAD TESTS
                        form_dest_src_LDST(OpLDX , 0),
                        form_dest_src_LDST(OpLDX , 1),
                        form_dest_src_LDST(OpLDXI, 2),
                        form_dest_src_LDST(OpLDXD, 3),
                        form_dest_src_LDST(OpLDXD, 4),
                        form_dest_src_LDST(OpLDXI, 5),
                        form_dest_src_LDST(OpLDX , 6),
                        -- STORE TESTS
                        form_dest_src_LDST(OpSTX , 0),
                        form_dest_src_LDST(OpSTX , 1),
                        form_dest_src_LDST(OpSTXI, 2),
                        form_dest_src_LDST(OpSTXD, 3),
                        form_dest_src_LDST(OpSTXD, 4),
                        form_dest_src_LDST(OpSTXI, 5),
                        form_dest_src_LDST(OpSTX , 6)
                        );

        X_test_data_ld := (
                        -- LOAD TESTS
                        int_to_std_vector(0, NUM_DATA_BITS),
                        int_to_std_vector(1, NUM_DATA_BITS),
                        int_to_std_vector(2, NUM_DATA_BITS),
                        int_to_std_vector(3, NUM_DATA_BITS),
                        int_to_std_vector(4, NUM_DATA_BITS),
                        int_to_std_vector(5, NUM_DATA_BITS),
                        int_to_std_vector(6, NUM_DATA_BITS),
                        -- STORE TESTS
                        (NUM_DATA_BITS-1 downto 0 => 'Z'),
                        (NUM_DATA_BITS-1 downto 0 => 'Z'),
                        (NUM_DATA_BITS-1 downto 0 => 'Z'),
                        (NUM_DATA_BITS-1 downto 0 => 'Z'),
                        (NUM_DATA_BITS-1 downto 0 => 'Z'),
                        (NUM_DATA_BITS-1 downto 0 => 'Z'),
                        (NUM_DATA_BITS-1 downto 0 => 'Z')
                        );  

        X_test_corr_data := (
                        -- LOAD TESTS
                        (NUM_DATA_BITS-1 downto 0 => '-'),
                        (NUM_DATA_BITS-1 downto 0 => '-'),
                        (NUM_DATA_BITS-1 downto 0 => '-'),
                        (NUM_DATA_BITS-1 downto 0 => '-'),
                        (NUM_DATA_BITS-1 downto 0 => '-'),
                        (NUM_DATA_BITS-1 downto 0 => '-'),
                        (NUM_DATA_BITS-1 downto 0 => '-'),
                        -- STORE TESTS
                        int_to_std_vector(0, NUM_DATA_BITS),
                        int_to_std_vector(1, NUM_DATA_BITS),
                        int_to_std_vector(2, NUM_DATA_BITS),
                        int_to_std_vector(3, NUM_DATA_BITS),
                        int_to_std_vector(4, NUM_DATA_BITS),
                        int_to_std_vector(5, NUM_DATA_BITS)
                        );

        X_test_corr_addr := (
                        -- LOAD TESTS
                        int_to_std_vector(0    , NUM_ADDRESS_BITS),
                        int_to_std_vector(0    , NUM_ADDRESS_BITS),
                        int_to_std_vector(0    , NUM_ADDRESS_BITS),
                        int_to_std_vector(0    , NUM_ADDRESS_BITS),
                        int_to_std_vector(65535, NUM_ADDRESS_BITS),
                        int_to_std_vector(65535, NUM_ADDRESS_BITS),
                        int_to_std_vector(0    , NUM_ADDRESS_BITS),
                        -- STORE TESTS
                        int_to_std_vector(0    , NUM_ADDRESS_BITS),
                        int_to_std_vector(0    , NUM_ADDRESS_BITS),
                        int_to_std_vector(0    , NUM_ADDRESS_BITS),
                        int_to_std_vector(0    , NUM_ADDRESS_BITS),
                        int_to_std_vector(65535, NUM_ADDRESS_BITS),
                        int_to_std_vector(65535, NUM_ADDRESS_BITS),
                        int_to_std_vector(0    , NUM_ADDRESS_BITS)
        );

        X_test_coor_rd := "11111110000000"; 
        X_test_corr_wr := "00000001111111";

        -- Y Tests
        Y_test_opcodes := (
                        -- LOAD TESTS
                        form_dest_src_LDST_with_disp(OpLDDY , 0, 16),
                        form_dest_src_LDST_with_disp(OpLDDY , 1, 24),
                        form_dest_src_LDST(OpLDYI, 2),
                        form_dest_src_LDST(OpLDYD, 3),
                        form_dest_src_LDST(OpLDYD, 4),
                        form_dest_src_LDST(OpLDYI, 5),
                        form_dest_src_LDST_with_disp(OpLDDY , 6, 0),
                        -- STORE TESTS
                        form_dest_src_LDST_with_disp(OpSTDY , 0, 16),
                        form_dest_src_LDST_with_disp(OpSTDY , 1, 24),
                        form_dest_src_LDST(OpSTYI, 2),
                        form_dest_src_LDST(OpSTYD, 3),
                        form_dest_src_LDST(OpSTYD, 4),
                        form_dest_src_LDST(OpSTYI, 5),
                        form_dest_src_LDST_with_disp(OpSTDY , 6, 0)
                        );

        Y_test_data_ld := (
                        -- LOAD TESTS
                        int_to_std_vector(0, NUM_DATA_BITS),
                        int_to_std_vector(1, NUM_DATA_BITS),
                        int_to_std_vector(2, NUM_DATA_BITS),
                        int_to_std_vector(3, NUM_DATA_BITS),
                        int_to_std_vector(4, NUM_DATA_BITS),
                        int_to_std_vector(5, NUM_DATA_BITS),
                        int_to_std_vector(6, NUM_DATA_BITS),
                        -- STORE TESTS
                        (NUM_DATA_BITS-1 downto 0 => 'Z'),
                        (NUM_DATA_BITS-1 downto 0 => 'Z'),
                        (NUM_DATA_BITS-1 downto 0 => 'Z'),
                        (NUM_DATA_BITS-1 downto 0 => 'Z'),
                        (NUM_DATA_BITS-1 downto 0 => 'Z'),
                        (NUM_DATA_BITS-1 downto 0 => 'Z'),
                        (NUM_DATA_BITS-1 downto 0 => 'Z')
                        );  

        Y_test_corr_data := (
                        -- LOAD TESTS
                        (NUM_DATA_BITS-1 downto 0 => '-'),
                        (NUM_DATA_BITS-1 downto 0 => '-'),
                        (NUM_DATA_BITS-1 downto 0 => '-'),
                        (NUM_DATA_BITS-1 downto 0 => '-'),
                        (NUM_DATA_BITS-1 downto 0 => '-'),
                        (NUM_DATA_BITS-1 downto 0 => '-'),
                        (NUM_DATA_BITS-1 downto 0 => '-'),
                        -- STORE TESTS
                        int_to_std_vector(0, NUM_DATA_BITS),
                        int_to_std_vector(1, NUM_DATA_BITS),
                        int_to_std_vector(2, NUM_DATA_BITS),
                        int_to_std_vector(3, NUM_DATA_BITS),
                        int_to_std_vector(4, NUM_DATA_BITS),
                        int_to_std_vector(5, NUM_DATA_BITS)
                        );

        Y_test_corr_addr := (
                        -- LOAD TESTS
                        int_to_std_vector(16   , NUM_ADDRESS_BITS),
                        int_to_std_vector(24   , NUM_ADDRESS_BITS),
                        int_to_std_vector(0    , NUM_ADDRESS_BITS),
                        int_to_std_vector(0    , NUM_ADDRESS_BITS),
                        int_to_std_vector(65535, NUM_ADDRESS_BITS),
                        int_to_std_vector(65535, NUM_ADDRESS_BITS),
                        int_to_std_vector(0    , NUM_ADDRESS_BITS),
                        -- STORE TESTS
                        int_to_std_vector(16   , NUM_ADDRESS_BITS),
                        int_to_std_vector(24   , NUM_ADDRESS_BITS),
                        int_to_std_vector(0    , NUM_ADDRESS_BITS),
                        int_to_std_vector(0    , NUM_ADDRESS_BITS),
                        int_to_std_vector(65535, NUM_ADDRESS_BITS),
                        int_to_std_vector(65535, NUM_ADDRESS_BITS),
                        int_to_std_vector(0    , NUM_ADDRESS_BITS)
        );

        Y_test_coor_rd := "11111110000000"; 
        Y_test_corr_wr := "00000001111111";

        -- Z Tests
        Z_test_opcodes := (
                        -- LOAD TESTS
                        form_dest_src_LDST_with_disp(OpLDDZ , 0, 16),
                        form_dest_src_LDST_with_disp(OpLDDZ , 1, 24),
                        form_dest_src_LDST(OpLDZI, 2),
                        form_dest_src_LDST(OpLDZD, 3),
                        form_dest_src_LDST(OpLDZD, 4),
                        form_dest_src_LDST(OpLDZI, 5),
                        form_dest_src_LDST_with_disp(OpLDDZ , 6, 0),
                        -- STORE TESTS
                        form_dest_src_LDST_with_disp(OpSTDZ , 0, 16),
                        form_dest_src_LDST_with_disp(OpSTDZ , 1, 24),
                        form_dest_src_LDST(OpSTZI, 2),
                        form_dest_src_LDST(OpSTZD, 3),
                        form_dest_src_LDST(OpSTZD, 4),
                        form_dest_src_LDST(OpSTZI, 5),
                        form_dest_src_LDST_with_disp(OpSTDZ , 6, 0)
                        );

        Z_test_data_ld := (
                        -- LOAD TESTS
                        int_to_std_vector(0, NUM_DATA_BITS),
                        int_to_std_vector(1, NUM_DATA_BITS),
                        int_to_std_vector(2, NUM_DATA_BITS),
                        int_to_std_vector(3, NUM_DATA_BITS),
                        int_to_std_vector(4, NUM_DATA_BITS),
                        int_to_std_vector(5, NUM_DATA_BITS),
                        int_to_std_vector(6, NUM_DATA_BITS),
                        -- STORE TESTS
                        (NUM_DATA_BITS-1 downto 0 => 'Z'),
                        (NUM_DATA_BITS-1 downto 0 => 'Z'),
                        (NUM_DATA_BITS-1 downto 0 => 'Z'),
                        (NUM_DATA_BITS-1 downto 0 => 'Z'),
                        (NUM_DATA_BITS-1 downto 0 => 'Z'),
                        (NUM_DATA_BITS-1 downto 0 => 'Z'),
                        (NUM_DATA_BITS-1 downto 0 => 'Z')
                        );  

        Z_test_corr_data := (
                        -- LOAD TESTS
                        (NUM_DATA_BITS-1 downto 0 => '-'),
                        (NUM_DATA_BITS-1 downto 0 => '-'),
                        (NUM_DATA_BITS-1 downto 0 => '-'),
                        (NUM_DATA_BITS-1 downto 0 => '-'),
                        (NUM_DATA_BITS-1 downto 0 => '-'),
                        (NUM_DATA_BITS-1 downto 0 => '-'),
                        (NUM_DATA_BITS-1 downto 0 => '-'),
                        -- STORE TESTS
                        int_to_std_vector(0, NUM_DATA_BITS),
                        int_to_std_vector(1, NUM_DATA_BITS),
                        int_to_std_vector(2, NUM_DATA_BITS),
                        int_to_std_vector(3, NUM_DATA_BITS),
                        int_to_std_vector(4, NUM_DATA_BITS),
                        int_to_std_vector(5, NUM_DATA_BITS)
                        );

        Z_test_corr_addr := (
                        -- LOAD TESTS
                        int_to_std_vector(16   , NUM_ADDRESS_BITS),
                        int_to_std_vector(24   , NUM_ADDRESS_BITS),
                        int_to_std_vector(0    , NUM_ADDRESS_BITS),
                        int_to_std_vector(0    , NUM_ADDRESS_BITS),
                        int_to_std_vector(65535, NUM_ADDRESS_BITS),
                        int_to_std_vector(65535, NUM_ADDRESS_BITS),
                        int_to_std_vector(0    , NUM_ADDRESS_BITS),
                        -- STORE TESTS
                        int_to_std_vector(16   , NUM_ADDRESS_BITS),
                        int_to_std_vector(24   , NUM_ADDRESS_BITS),
                        int_to_std_vector(0    , NUM_ADDRESS_BITS),
                        int_to_std_vector(0    , NUM_ADDRESS_BITS),
                        int_to_std_vector(65535, NUM_ADDRESS_BITS),
                        int_to_std_vector(65535, NUM_ADDRESS_BITS),
                        int_to_std_vector(0    , NUM_ADDRESS_BITS)
        );

        Z_test_coor_rd := "11111110000000"; 
        Z_test_corr_wr := "00000001111111";

        -- MOV tests
        MOV_test_opcodes := (
                        -- LOAD IN VALUES
                        form_imm_load(OpLDI, 0, 0),
                        form_imm_load(OpLDI, 2, 1),
                        form_imm_load(OpLDI, 4, 2),
                        form_imm_load(OpLDI, 8, 3),
                        form_imm_load(OpLDI, 16, 4),
                        -- MOV Around
                        form_mov_operation(OpMOV, 0, 1),
                        form_mov_operation(OpMOV, 2, 3),
                        form_mov_operation(OpMOV, 4, 5),
                        form_mov_operation(OpMOV, 8, 9),
                        form_mov_operation(OpMOV, 16, 17),
                        -- Check by outputting
                        form_dest_src_LDST(OpSTX , 1),
                        form_dest_src_LDST(OpSTX , 2),
                        form_dest_src_LDST(OpSTX , 5),
                        form_dest_src_LDST(OpSTX , 6),
                        form_dest_src_LDST(OpSTX , 17)
                        );

        MOV_test_data_ld := (
                        -- LOAD IN VALUES
                        (NUM_DATA_BITS-1 downto 0 => 'Z'),
                        (NUM_DATA_BITS-1 downto 0 => 'Z'),
                        (NUM_DATA_BITS-1 downto 0 => 'Z'),
                        (NUM_DATA_BITS-1 downto 0 => 'Z'),
                        (NUM_DATA_BITS-1 downto 0 => 'Z'),
                        -- MOV Around
                        (NUM_DATA_BITS-1 downto 0 => 'Z'),
                        (NUM_DATA_BITS-1 downto 0 => 'Z'),
                        (NUM_DATA_BITS-1 downto 0 => 'Z'),
                        (NUM_DATA_BITS-1 downto 0 => 'Z'),
                        (NUM_DATA_BITS-1 downto 0 => 'Z'),
                        -- Check by outputting
                        (NUM_DATA_BITS-1 downto 0 => 'Z'),
                        (NUM_DATA_BITS-1 downto 0 => 'Z'),
                        (NUM_DATA_BITS-1 downto 0 => 'Z'),
                        (NUM_DATA_BITS-1 downto 0 => 'Z'),
                        (NUM_DATA_BITS-1 downto 0 => 'Z')
                        );  

        MOV_test_corr_data := (
                        -- LOAD IN VALUES
                        (NUM_DATA_BITS-1 downto 0 => '-'),
                        (NUM_DATA_BITS-1 downto 0 => '-'),
                        (NUM_DATA_BITS-1 downto 0 => '-'),
                        (NUM_DATA_BITS-1 downto 0 => '-'),
                        (NUM_DATA_BITS-1 downto 0 => '-'),
                        -- MOV Around
                        (NUM_DATA_BITS-1 downto 0 => '-'),
                        (NUM_DATA_BITS-1 downto 0 => '-'),
                        (NUM_DATA_BITS-1 downto 0 => '-'),
                        (NUM_DATA_BITS-1 downto 0 => '-'),
                        (NUM_DATA_BITS-1 downto 0 => '-'),
                        -- Check by outputting
                        int_to_std_vector(0 , NUM_DATA_BITS),
                        int_to_std_vector(1 , NUM_DATA_BITS),
                        int_to_std_vector(2 , NUM_DATA_BITS),
                        int_to_std_vector(3 , NUM_DATA_BITS),
                        int_to_std_vector(4 , NUM_DATA_BITS)
                        );

        MOV_test_corr_addr := (
                        -- LOAD IN VALUES
                        (NUM_ADDRESS_BITS-1 downto 0 => '-'),
                        (NUM_ADDRESS_BITS-1 downto 0 => '-'),
                        (NUM_ADDRESS_BITS-1 downto 0 => '-'),
                        (NUM_ADDRESS_BITS-1 downto 0 => '-'),
                        (NUM_ADDRESS_BITS-1 downto 0 => '-'),
                        -- MOV Around
                        (NUM_ADDRESS_BITS-1 downto 0 => '-'),
                        (NUM_ADDRESS_BITS-1 downto 0 => '-'),
                        (NUM_ADDRESS_BITS-1 downto 0 => '-'),
                        (NUM_ADDRESS_BITS-1 downto 0 => '-'),
                        (NUM_ADDRESS_BITS-1 downto 0 => '-'),
                        -- Check by outputting
                        (NUM_ADDRESS_BITS-1 downto 0 => '-'),
                        (NUM_ADDRESS_BITS-1 downto 0 => '-'),
                        (NUM_ADDRESS_BITS-1 downto 0 => '-'),
                        (NUM_ADDRESS_BITS-1 downto 0 => '-'),
                        (NUM_ADDRESS_BITS-1 downto 0 => '-')
                        );

        MOV_test_coor_rd := "000000000000000"; 
        MOV_test_corr_wr := "000000000011111";

        -- Stack Tests
        SP_test_opcodes := (
                        -- Reset the stack
                        form_imm_load(OpLDI, 0, 0),
                        form_imm_load(OpLDI, 1, 10),
                        form_inout_operation(OpIN, 0, SPH),
                        form_inout_operation(OpIN, 1, SPL),
                        -- PUSH
                        form_dest_src_LDST(OpPUSH, 0),
                        form_dest_src_LDST(OpPUSH, 1),
                        form_dest_src_LDST(OpPUSH, 2),
                        -- POP
                        form_dest_src_LDST(OpPUSH, 0),
                        form_dest_src_LDST(OpPUSH, 1),
                        form_dest_src_LDST(OpPUSH, 2)
                        );

        SP_test_data_ld := (
                        -- Reset the stack
                        (NUM_DATA_BITS-1 downto 0 => 'Z'),
                        (NUM_DATA_BITS-1 downto 0 => 'Z'),
                        (NUM_DATA_BITS-1 downto 0 => 'Z'),
                        (NUM_DATA_BITS-1 downto 0 => 'Z'),
                        -- PUSH
                        int_to_std_vector(0 , NUM_DATA_BITS),
                        int_to_std_vector(1 , NUM_DATA_BITS),
                        int_to_std_vector(2 , NUM_DATA_BITS),
                        -- POP
                        (NUM_DATA_BITS-1 downto 0 => 'Z'),
                        (NUM_DATA_BITS-1 downto 0 => 'Z'),
                        (NUM_DATA_BITS-1 downto 0 => 'Z')
                        );

        SP_test_corr_data := (
                        -- Reset the stack
                        (NUM_DATA_BITS-1 downto 0 => 'Z'),
                        (NUM_DATA_BITS-1 downto 0 => 'Z'),
                        (NUM_DATA_BITS-1 downto 0 => 'Z'),
                        (NUM_DATA_BITS-1 downto 0 => 'Z'),
                        -- PUSH
                        (NUM_DATA_BITS-1 downto 0 => 'Z'),
                        (NUM_DATA_BITS-1 downto 0 => 'Z'),
                        (NUM_DATA_BITS-1 downto 0 => 'Z'),
                        -- POP
                        int_to_std_vector(0 , NUM_DATA_BITS),
                        int_to_std_vector(1 , NUM_DATA_BITS),
                        int_to_std_vector(2 , NUM_DATA_BITS)
                        );

        SP_test_corr_addr := (
                        -- LOAD IN VALUES
                        (NUM_ADDRESS_BITS-1 downto 0 => '-'),
                        (NUM_ADDRESS_BITS downto 0 => '-'),
                        (NUM_ADDRESS_BITS downto 0 => '-'),
                        (NUM_ADDRESS_BITS downto 0 => '-'),
                        -- MOV Around
                        int_to_std_vector(10 , NUM_ADDRESS_BITS),
                        int_to_std_vector(9 , NUM_ADDRESS_BITS),
                        int_to_std_vector(8 , NUM_ADDRESS_BITS),
                        -- Check by outputting
                        int_to_std_vector(8 , NUM_ADDRESS_BITS),
                        int_to_std_vector(9 , NUM_ADDRESS_BITS),
                        int_to_std_vector(10 , NUM_ADDRESS_BITS)
                        );

        MOV_test_coor_rd := "0000111000"; 
        MOV_test_corr_wr := "0000000111";
        -- Memory Tests
        MEM_test_opcodes := (
                        -- LOAD TESTS
                        form_dest_src_LDST(OpLDS, 0),
                        form_dest_src_LDST(OpLDS, 1),
                        form_dest_src_LDST(OpLDS, 2),
                        -- STORE TESTS
                        form_dest_src_LDST(OpSTS, 0),
                        form_dest_src_LDST(OpSTS, 1),
                        form_dest_src_LDST(OpSTS, 2)
                        );

        MEM_test_data_ld := (
                        -- LOAD TESTS
                        int_to_std_vector(0, NUM_DATA_BITS),
                        int_to_std_vector(1, NUM_DATA_BITS),
                        int_to_std_vector(2, NUM_DATA_BITS),
                        -- STORE TESTS
                        (NUM_DATA_BITS-1 downto 0 => 'Z'),
                        (NUM_DATA_BITS-1 downto 0 => 'Z'),
                        (NUM_DATA_BITS-1 downto 0 => 'Z')
                        );  

        MEM_test_corr_data := (
                        -- LOAD TESTS
                        (NUM_DATA_BITS-1 downto 0 => '-'),
                        (NUM_DATA_BITS-1 downto 0 => '-'),
                        (NUM_DATA_BITS-1 downto 0 => '-'),
                        -- STORE TESTS
                        int_to_std_vector(0, NUM_DATA_BITS),
                        int_to_std_vector(1, NUM_DATA_BITS),
                        int_to_std_vector(2, NUM_DATA_BITS)
                        );

        MEM_test_progDB := (
                        -- LOAD TESTS
                        int_to_std_vector(16   , NUM_ADDRESS_BITS),
                        int_to_std_vector(24   , NUM_ADDRESS_BITS),
                        int_to_std_vector(65535, NUM_ADDRESS_BITS),
                        -- STORE TESTS
                        int_to_std_vector(16   , NUM_ADDRESS_BITS),
                        int_to_std_vector(24   , NUM_ADDRESS_BITS),
                        int_to_std_vector(65535, NUM_ADDRESS_BITS)            
                        );

        MEM_test_corr_addr := (
                        -- LOAD TESTS
                        int_to_std_vector(16   , NUM_ADDRESS_BITS),
                        int_to_std_vector(24   , NUM_ADDRESS_BITS),
                        int_to_std_vector(65535, NUM_ADDRESS_BITS),
                        -- STORE TESTS
                        int_to_std_vector(16   , NUM_ADDRESS_BITS),
                        int_to_std_vector(24   , NUM_ADDRESS_BITS),
                        int_to_std_vector(65535, NUM_ADDRESS_BITS)
                        );

        MEM_test_coor_rd := "111000"; 
        MEM_test_corr_wr := "000111";

        -- Initialize signals
        ProgDB   <= (others => 'X');
        Reset    <= '1';
        IR_input <= (others => 'X');

        wait for 4*CLOCK_PERIOD;

        report "-------- STARTING TESTS --------";

        -- Initially put zeros in all registers using LDI
        for i_LDI in 0 to 2 ** NUM_REG_LOG - 1 loop
            
            -- Pretend we are doing a LDI
            IR_input <= form_imm_load(OpLDI, i_LDI, i_LDI);

            wait for CLOCK_PERIOD;
        end loop;

        -- Test all of the X Reg. operations
        
        for i_X_TEST in 0 to NUM_X_TESTS-1 loop

            IR_input <= X_test_opcodes(i_X_TEST);
            wait for CLOCK_PERIOD;

            -- Check Data AB
            assert (std_match(DataAB, X_test_corr_addr(i_X_TEST)))
            report  "Data Address Bus Failure" & 
                    " Got: "                   & std_logic_vec_to_string(DataAB) &
                    " Expected: "              & std_logic_vec_to_string(X_test_corr_addr(i_X_TEST))&
                    " For X Test: "            & i_X_TEST'image                                                          
            severity  ERROR;

            -- Load in the DB
            DataDB <= X_test_data_ld(i_X_TEST);

            -- Sample 75% into clock
            wait for 3 * (CLOCK_PERIOD/4);

            -- Check DB
            assert (std_match(DataDB, X_test_corr_data(i_X_TEST)))
            report  "Data Data Bus Failure"    & 
                    " Got: "                   & std_logic_vec_to_string(DataDB) &
                    " Expected: "              & std_logic_vec_to_string(X_test_corr_data(i_X_TEST))&
                    " For X Test: "            & i_X_TEST'image                                                          
            severity  ERROR;

            -- Check Rd/Wr

            assert (DataRd = X_test_coor_rd(i_X_TEST))
            report  "Data Data Bus Failure"    & 
                    " Got: "                   & DataRd'image &
                    " Expected: "              & X_test_coor_rd(i_X_TEST)'image &
                    " For X Test: "            & i_X_TEST'image                                                          
            severity  ERROR;    

            assert (DataWr = X_test_coor_wr(i_X_TEST))
            report  "Data Data Bus Failure"    & 
                    " Got: "                   & DataWr'image &
                    " Expected: "              & X_test_coor_wr(i_X_TEST)'image &
                    " For X Test: "            & i_X_TEST'image                                                          
            severity  ERROR;

            -- Wait for next clock
            wait until clk = '1';
        end loop;

        -- End of stimulus events
        END_SIM <= TRUE;
        
        -- Report that all tests are done.
        report "-------- ALL TESTS DONE --------";
        
        wait;

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

end architecture;