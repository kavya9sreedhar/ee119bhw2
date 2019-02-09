----------------------------------------------------------------------------
--
--  Test Bench for AVR Registers
--
--  This is a test bench for the REG_TEST entity.
--  Tests that registers are correctly outputted for instructions.
--
--  The test bench entity is called Register_Tester.
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

use work.ALU_CONSTANTS.all;
use work.opcodes.all;
use work.FlagConstants.all;
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
    -- The input into the register file.
    signal Reg_input : std_logic_vector(NUM_DATA_BITS-1 downto 0);

    -- Register outputs
    signal RegAOut   : std_logic_vector(NUM_DATA_BITS-1 downto 0);
    signal RegBOut   : std_logic_vector(NUM_DATA_BITS-1 downto 0);

    -- Signal used to stop clock signal generators
    signal  END_SIM  :  BOOLEAN := FALSE;

    -- Array of operations
    type op_lst_t is array (integer range <>) of opcode_word;

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
    end std_logic_vec_to_string;

    -- form_two_reg_ALU_opcode
    -- Takes in an ALU OP code that has two reg args and returns the
    --   fully formed OP code.
    --
    -- Params:
    --   OPcode
    --     The 2-reg arg. opcode.
    --   Register1
    --     The first register. (5 bit select)
    --   Register2
    --     The second register. (5 bit select)
    
    function form_two_reg_ALU_opcode (OPcode : opcode_word; Register1:  integer; Register2 : integer) 
      return opcode_word is

        variable reg1           : std_logic_vector(NUM_REG_LOG-1 downto 0);
        variable reg2           : std_logic_vector(NUM_REG_LOG-1 downto 0);

        variable FormedOPcode   : opcode_word;
    begin
        reg1 := std_logic_vector(to_unsigned(Register1, NUM_REG_LOG));
        reg2 := std_logic_vector(to_unsigned(Register2, NUM_REG_LOG));

        FormedOPcode := OPcode(15 downto 10) & reg2(4) & reg1(4 downto 0) & reg2(3 downto 0);
    return FormedOPcode;
    end form_two_reg_ALU_opcode;

    -- form_one_reg_ALU_opcode
    -- Takes in an ALU OP code that has one reg args and returns the
    --   fully formed OP code.
    --
    -- Params:
    --   OPcode
    --     The 1-reg arg. opcode.
    --   Register1
    --     The first register. (5 bit select)
    --
    function form_one_reg_ALU_opcode (OPcode : opcode_word; Register1: integer) 
      return opcode_word is

        variable reg1           : std_logic_vector(NUM_REG_LOG-1 downto 0);

        variable FormedOPcode   : opcode_word;
    begin
        reg1 := std_logic_vector(to_unsigned(Register1, NUM_REG_LOG));

        FormedOPcode := OPcode(15 downto 9) & reg1(4 downto 0) & OPcode(3 downto 0);
    return FormedOPcode;
    end form_one_reg_ALU_opcode;


    -- form_reg8_imm_ALU_opcode
    -- Takes in an ALU OP code that has one reg args and one imm and returns the
    --   fully formed OP code.
    --
    -- Params:
    --   OPcode
    --     The 2-reg arg. opcode.
    --   Register1
    --     4-bit reg select
    --   Imm
    --     8 bit imm value
    function form_reg8_imm_ALU_opcode (OPcode : opcode_word; Register1: integer; Immediate: integer) 
      return opcode_word is

        variable reg1           : std_logic_vector(3 downto 0);
        variable imm            : std_logic_vector(NUM_DATA_BITS-1 downto 0);

        variable FormedOPcode   : opcode_word;
    begin
        reg1 := std_logic_vector(to_unsigned(Register1, NUM_REG_LOG));
        imm  := std_logic_vector(to_unsigned(Immediate, 8));

        FormedOPcode := OPcode(15 downto 12) & imm(7 downto 4) & reg1(3 downto 0) & imm(3 downto 0);
    return FormedOPcode;
    end form_reg8_imm_ALU_opcode;

    -- form_reg16_imm_ALU_opcode
    -- Takes in an ALU OP code that has one 16bit reg arg and an imm and returns the
    --   fully formed OP code.
    --
    -- Params:
    --   OPcode
    --     The 2-reg arg. opcode.
    --   Register1
    --     4-bit reg select
    --   Imm
    --     8 bit imm value
    function form_reg16_imm_ALU_opcode (OPcode : opcode_word; Register1: integer; Immediate: integer) 
      return opcode_word is

        variable reg1           : std_logic_vector(1 downto 0);
        variable imm            : std_logic_vector(5 downto 0);

        variable FormedOPcode   : opcode_word;
    begin
        reg1 := std_logic_vector(to_unsigned(Register1, 2));
        imm  := std_logic_vector(to_unsigned(Immediate, 6));

        FormedOPcode := OPcode(15 downto 8) & imm(5 downto 4) & reg1(1 downto 0) & imm(3 downto 0);
    return FormedOPcode;
    end form_reg16_imm_ALU_opcode;
    
    -- form_bitop_ALU_opcode
    -- Takes in an ALU OP code that has one reg arg and a bit and returns the
    --   fully formed OP code.
    --
    -- Params:
    --   OPcode
    --     The bit operation opcode.
    --   Register1
    --     5-bit reg select
    --   BitToOp
    --     3-bit bit to op
    function form_bitop_ALU_opcode (OPcode : opcode_word; Register1: integer; BitToOp: integer) 
      return opcode_word is

        variable reg1           : std_logic_vector(4 downto 0);
        variable imm            : std_logic_vector(2 downto 0);

        variable FormedOPcode   : opcode_word;
    begin
        reg1 := std_logic_vector(to_unsigned(Register1, 5));
        imm  := std_logic_vector(to_unsigned(BitToOp, 3));

        FormedOPcode := OPcode(15 downto 9) & reg1(4 downto 0) & OPcode(3) & imm(2 downto 0);
    return FormedOPcode;     
    end form_bitop_ALU_opcode;

    -- form_SREG_bitop_ALU_opcode
    -- Takes in an ALU OP code that has one reg arg and a bit and returns the
    --   fully formed OP code.
    --
    -- Params:
    --   OPcode
    --     The SREG bit operation opcode
    --   BitToOp
    --     3-bit bit to op
    function form_SREG_bitop_ALU_opcode (OPcode : opcode_word; BitToOp: integer) 
      return opcode_word is

        variable imm            : std_logic_vector(2 downto 0);

        variable FormedOPcode   : opcode_word;
    begin
        imm  := std_logic_vector(to_unsigned(BitToOp, 3));

        FormedOPcode := OPcode(15 downto 7) & imm(2 downto 0) & OPcode(3 downto 0);
    return FormedOPcode;
    end form_SREG_bitop_ALU_opcode;

begin

    UUT: entity work.REG_TEST(structural)
    port map(

        -- Hook up the clock
        clock => clk,
        -- Hook up the register in
        RegIn => Reg_input,
        -- Hook up the IR in
        IR => IR_input,

        -- Hook up the register outputs
        RegAOut => RegAOut,
        RegBOut => RegBOut

    );

    -----------------------
    --   START TESTING   --
    -----------------------
    test_main: process  
        variable two_reg_ops     : op_lst_t(0 to 8);
        variable one_reg_ops     : op_lst_t(0 to 6);
        variable one_reg_imm_ops : op_lst_t(0 to 4);
        variable one_16_reg_ops  : op_lst_t(0 to 1);
        variable bit_set_ops     : op_lst_t(0 to 1);
    begin

        -- Opcode lists
        two_reg_ops := (OpADC,
                        OpADD,
                        OpAND,
                        OpCP,
                        OpCPC,
                        OpEOR,
                        OpOR,
                        OpSBC,
                        OpSUB);

        one_reg_ops := (OpASR,
                        OpCOM,
                        OpDEC,
                        OpINC,
                        OpLSR,
                        OpNEG,
                        OpROR);
        
        one_reg_imm_ops := (OpANDI,
                            OpCPI,
                            OpORI,
                            OpSBCI,
                            OpSUBI);
                            
        one_16_reg_ops := (OpADIW,
                           OpSBIW);                    
        
        bit_set_ops := (OpBLD,
                        OpBST);

        -- Run for a few clocks
        wait for 4 * CLOCK_PERIOD;

        report "-------- STARTING ALL TESTS --------";

        for i in 0 to 2 ** NUM_REG_LOG - 1 loop

            -- Load a value corresponding to the register number
            Reg_input  <= std_logic_vector(to_unsigned(i, NUM_DATA_BITS));
            
            -- Pretend we are doing a certain operation to load it in.
            IR_input   <= form_one_reg_ALU_opcode(OpNEG, i);

            wait for CLOCK_PERIOD;

        end loop;

        -- Go through all operations that have two registers.
        for i_OPCODE in 0 to two_reg_ops'length-1 loop
            for i_A in 0 to 2 ** NUM_REG_LOG - 1 loop
                for i_B in 0 to 2 ** NUM_REG_LOG - 1 loop

                    IR_input   <= form_two_reg_ALU_opcode(two_reg_ops(i_OPCODE), i_A, i_B);
                    Reg_input  <= std_logic_vector(to_unsigned(i_A, NUM_DATA_BITS));

                    wait for CLOCK_PERIOD/2;

                    -- Check match
                    assert (std_match(RegAOut, std_logic_vector(to_unsigned(i_A, NUM_DATA_BITS))))
                    report  "RegA Selection Failure"  & 
                            " Got: "                  & std_logic_vec_to_string(RegAOut) &
                            " Expected: "             & std_logic_vec_to_string(
                                                          std_logic_vector(to_unsigned(i_A, NUM_DATA_BITS))) &
                            " For OPCODE: "           & std_logic_vec_to_string(two_reg_ops(i_OPCODE))
                    severity  ERROR;

                    assert (std_match(RegBOut, std_logic_vector(to_unsigned(i_B, NUM_DATA_BITS))))
                    report  "RegB Selection Failure"  & 
                            " Got: "                  & std_logic_vec_to_string(RegBOut) &
                            " Expected: "             & std_logic_vec_to_string(
                                                          std_logic_vector(to_unsigned(i_B, NUM_DATA_BITS)))&
                            " For OPCODE: "           & std_logic_vec_to_string(two_reg_ops(i_OPCODE))                                                          
                    severity  ERROR;

                    wait for CLOCK_PERIOD/2;

                end loop;    
            end loop;
        end loop;
        
        -- Go through all operations that have one register.
        for i_OPCODE in 0 to one_reg_ops'length-1 loop        
            for i_A in 0 to 2 ** NUM_REG_LOG - 1 loop

                IR_input   <= form_one_reg_ALU_opcode(one_reg_ops(i_OPCODE), i_A);
                Reg_input  <= std_logic_vector(to_unsigned(i_A, NUM_DATA_BITS));

                wait for CLOCK_PERIOD/2;

                -- Check match
                assert (std_match(RegAOut, std_logic_vector(to_unsigned(i_A, NUM_DATA_BITS))))
                report  "RegA Selection Failure"  & 
                        " Got: "                  & std_logic_vec_to_string(RegAOut) &
                        " Expected: "             & std_logic_vec_to_string(
                                                      std_logic_vector(to_unsigned(i_A, NUM_DATA_BITS))) &
                        " For OPCODE: "           & std_logic_vec_to_string(one_reg_ops(i_OPCODE))
                severity  ERROR;

                wait for CLOCK_PERIOD/2;

            end loop;
        end loop;

        -- Go through all operations that have one register and one imm.
        for i_OPCODE in 0 to one_reg_imm_ops'length-1 loop        
            for i_A in 0 to 2 ** (NUM_REG_LOG-1) - 1 loop
                IR_input   <= form_reg8_imm_ALU_opcode(one_reg_imm_ops(i_OPCODE), i_A, 0);
                Reg_input  <= std_logic_vector(to_unsigned(i_A, NUM_DATA_BITS));

                wait for CLOCK_PERIOD/2;

                -- Check match
                assert (std_match(RegAOut, std_logic_vector(to_unsigned(i_A, NUM_DATA_BITS))))
                report  "RegA Selection Failure"  & 
                        " Got: "                  & std_logic_vec_to_string(RegAOut) &
                        " Expected: "             & std_logic_vec_to_string(
                                                      std_logic_vector(to_unsigned(i_A, NUM_DATA_BITS))) &
                        " For OPCODE: "           & std_logic_vec_to_string(one_reg_imm_ops(i_OPCODE))
                severity  ERROR;

                wait for CLOCK_PERIOD/2;

            end loop;
        end loop;
        
        -- Go through all operations that have one 16bit register arg.
        for i_OPCODE in 0 to one_16_reg_ops'length-1 loop    
            for i_R16 in 0 to 1 loop

                IR_input   <= form_reg16_imm_ALU_opcode(one_16_reg_ops(i_OPCODE), i_R16, 0);
                Reg_input  <= std_logic_vector(to_unsigned(i_R16*2 + 24, NUM_DATA_BITS));

                wait for CLOCK_PERIOD/2;

                -- Check match
                assert (std_match(RegAOut, std_logic_vector(to_unsigned(i_R16*2 + 24, NUM_DATA_BITS))))
                report  "RegA Selection Failure"  & 
                        " Got: "                  & std_logic_vec_to_string(RegAOut) &
                        " Expected: "             & std_logic_vec_to_string(
                                                      std_logic_vector(to_unsigned(i_R16*2 + 24, NUM_DATA_BITS))) &
                        " For OPCODE: "           & std_logic_vec_to_string(one_16_reg_ops(i_OPCODE))
                severity  ERROR;

                assert (std_match(RegBOut, std_logic_vector(to_unsigned(i_R16*2 + 25, NUM_DATA_BITS))))
                report  "RegB Selection Failure"  & 
                        " Got: "                  & std_logic_vec_to_string(RegBOut) &
                        " Expected: "             & std_logic_vec_to_string(
                                                      std_logic_vector(to_unsigned(i_R16*2 + 25, NUM_DATA_BITS)))&
                        " For OPCODE: "           & std_logic_vec_to_string(one_16_reg_ops(i_OPCODE))                                                          
                severity  ERROR;

                wait for CLOCK_PERIOD/2;

            end loop;
        end loop;
        
        -- Go through all operations that have one registers.
        for i_OPCODE in 0 to bit_set_ops'length-1 loop        
            for i_A in 0 to 2 ** NUM_REG_LOG - 1 loop

                IR_input   <= form_bitop_ALU_opcode(bit_set_ops(i_OPCODE), i_A, 0);
                Reg_input  <= std_logic_vector(to_unsigned(i_A, NUM_DATA_BITS));

                wait for CLOCK_PERIOD/2;

                -- Check match
                assert (std_match(RegAOut, std_logic_vector(to_unsigned(i_A, NUM_DATA_BITS))))
                report  "RegA Selection Failure"  & 
                        " Got: "                  & std_logic_vec_to_string(RegAOut) &
                        " Expected: "             & std_logic_vec_to_string(
                                                      std_logic_vector(to_unsigned(i_A, NUM_DATA_BITS))) &
                        " For OPCODE: "           & std_logic_vec_to_string(bit_set_ops(i_OPCODE))
                severity  ERROR;

                wait for CLOCK_PERIOD/2;

            end loop;
        end loop;

        -- Final Test for Swap

        for i_A in 0 to 2 ** NUM_REG_LOG - 1 loop

            IR_input   <= form_one_reg_ALU_opcode(OpSWAP, i_A);
            Reg_input  <= std_logic_vector(to_unsigned(i_A, NUM_DATA_BITS));

            wait for CLOCK_PERIOD/2;

            -- Check match
            assert (std_match(RegAOut, std_logic_vector(to_unsigned(i_A, NUM_DATA_BITS))))
            report  "RegA Selection Failure"  & 
                    " Got: "                  & std_logic_vec_to_string(RegAOut) &
                    " Expected: "             & std_logic_vec_to_string(
                                                  std_logic_vector(to_unsigned(i_A, NUM_DATA_BITS))) &
                    " For OPCODE: "           & std_logic_vec_to_string(OpSWAP)
            severity  ERROR;

            wait for CLOCK_PERIOD/2;

        end loop;

        report "-------- FINISHED ALL TESTS --------";

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