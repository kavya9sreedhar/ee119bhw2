----------------------------------------------------------------------------
--
--  Functions for generating AVR opcodes in VHDL
--
--  Functions that take in proper arguments and returns the opcode that
--  corresponds to it.
--
--  Revision History:
--     02/10/19  Daniel Xu and Kavya Sreedhar   Initial revision.
----------------------------------------------------------------------------
library ieee;
library work;

use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.std_logic_arith.ALL;

use work.opcodes.all;

package AVR_Opcode_Formation is

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

    -- int_to_std_vector
    -- Takes in an int and number of bits and returns a std_logic_vector
    function int_to_std_vector(num: integer; bits: integer)
        return std_logic_vector is
    begin
        return std_logic_vector(to_unsigned(num, bits));
    end int_to_std_vector;

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
        reg1 := int_to_std_vector(Register1, NUM_REG_LOG);
        reg2 := int_to_std_vector(Register2, NUM_REG_LOG);

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
        reg1 := int_to_std_vector(Register1, NUM_REG_LOG);

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
        reg1 := int_to_std_vector(Register1, NUM_REG_LOG);
        imm  := int_to_std_vector(Immediate, 8);

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
        reg1 := int_to_std_vector(Register1, 2);
        imm  := int_to_std_vector(Immediate, 6);

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
        reg1 := int_to_std_vector(Register1, 5);
        imm  := int_to_std_vector(BitToOp, 3);

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
        imm  := int_to_std_vector(BitToOp, 3);

        FormedOPcode := OPcode(15 downto 7) & imm(2 downto 0) & OPcode(3 downto 0);
        return FormedOPcode;
    end form_SREG_bitop_ALU_opcode;

    -- form_dest_src_LDST
    -- Takes in a LD or ST OP code that has one argument and returns the fully formed OP code.
    --
    -- Params:
    --   OPcode
    --     The LD or ST OP code with one arg.
    --   src_dest
    --     5-bit dest/src
    function form_dest_src_LDST (OPcode : opcode_word; src_dest: integer) 
      return opcode_word is

        variable reg            : std_logic_vector(4 downto 0);
        variable FormedOPcode   : opcode_word;

    begin
        reg  := int_to_std_vector(src_dest, 5);

        FormedOPcode := OPcode(15 downto 9) & reg(4 downto 0) & OPcode(3 downto 0);
        return FormedOPcode;
    end form_dest_src_LDST;

    -- form_imm_load
    -- Takes in a LDI code that has one argument and returns the fully formed OP code.
    --
    -- Params:
    --   OPcode
    --     The LDI opcode
    --   src_dest
    --     5-bit dest/src
    function form_imm_load (OPcode : opcode_word; src_dest: integer; val:integer) 
      return opcode_word is

        variable reg            : std_logic_vector(4 downto 0);
        variable imm            : std_logic_vector(7 downto 0);
        variable FormedOPcode   : opcode_word;

    begin
        reg  := int_to_std_vector(src_dest, 5);
        imm  := int_to_std_vector(val, 8);

        FormedOPcode := OPcode(15 downto 12) & imm(7 downto 4) & reg(4 downto 0) & imm(3 downto 0);
        return FormedOPcode;
    end form_imm_load;    

    -- form_dest_src_LDST_with_disp
    -- Takes in a LD or ST OP code that has a source/destination and a displacement
    -- and returns the fully formed OP code.
    --
    -- Params:
    --   OPcode
    --     THe LD/ST with disp opcode
    --   src_dest
    --     5-bit dest/src
    --   disp
    --     6-bit displacement
    function form_dest_src_LDST_with_disp (OPcode : opcode_word; src_dest: integer; disp: integer) 
      return opcode_word is

        variable reg            : std_logic_vector(4 downto 0);
        variable displacement   : std_logic_vector(5 downto 0);
        variable FormedOPcode   : opcode_word;

    begin
        reg  := int_to_std_vector(src_dest, 5);
        displacement := int_to_std_vector(disp, 6);

        FormedOPcode := OPcode(15 downto 14) & displacement(5) & OPcode(12) & 
                        displacement(4 downto 3) & OPcode(9) & reg(4 downto 0) & 
                        OPcode(3) & displacement(2 downto 0);
        return FormedOPcode;
    end form_dest_src_LDST_with_disp;    

    -- form_mov_operation
    -- Takes in a MOV OP code that has a source and destination and returns the fully formed OP code.
    --
    -- Params:
    --   OPcode
    --     MOV opcode
    --   src_reg
    --     5-bit src
    --   dst_reg
    --     5-bit destination
    function form_mov_operation OPcode : opcode_word; src_reg: integer; dest_reg: integer)
        return opcode_word is
            variable sreg            : std_logic_vector(4 downto 0);
            variable dreg            : std_logic_vector(4 downto 0);
            variable FormedOPcode   : opcode_word;
    begin

        sreg  := int_to_std_vector(src_reg, 5);
        dreg  := int_to_std_vector(dest_reg, 5);

        FormedOPcode := OPcode(15 downto 10) & sreg(4) & dreg(4 downto 0) & sreg(3 downto 0);

        return FormedOPcode;
    end form_mov_operation;

    -- form_inout_operation
    -- Takes in an in/out OP code and creates teh fully formed OP code.
    --
    -- Params:
    --   OPcode
    --     IN or OUT opcode
    --   GP_reg
    --     5-bit src
    --   IO_reg
    --     6-bit destination
    function form_inout_operation OPcode : opcode_word; gp_register: integer; io_register: integer)
        return opcode_word is
            variable GPreg           : std_logic_vector(4 downto 0);
            variable IOreg           : std_logic_vector(5 downto 0);
            variable FormedOPcode    : opcode_word;
    begin

        GPreg  := int_to_std_vector(gp_register, 5);
        IOreg  := int_to_std_vector(io_register, 6);

        FormedOPcode := OPcode(15 downto 11) & IOreg(5 downto 4) & GPreg(4 downto 0) & IOreg(3 downto 0);

        return FormedOPcode;
    end form_inout_operation;    
end package