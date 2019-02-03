----------------------------------------------------------------------------
--
--  Atmel AVR ALU Test Entity Declaration
--
--  This is the entity declaration which must be used for building the ALU
--  portion of the AVR design for testing.
--
--  Revision History:
--     17 Apr 98  Glen George       Initial revision.
--     20 Apr 98  Glen George       Fixed minor syntax bugs.
--     18 Apr 04  Glen George       Updated comments and formatting.
--     21 Jan 06  Glen George       Updated comments.
--
----------------------------------------------------------------------------


--
--  ALU_TEST
--
--  This is the ALU testing interface.  It just brings all the important
--  ALU signals out for testing along with the Instruction Register.
--
--  Inputs:
--    IR       - Instruction Register (16 bits)
--    OperandA - first operand to ALU (8 bits) - looks like the output
--               of the register array
--    OperandB - second operand to ALU (8 bits) - looks like the output
--               of the register array
--    clock    - the system clock
--
--  Outputs:
--    Result   - result of the ALU operation selected by the Instruction
--               Register (8 bits)
--    StatReg  - Status Register contents (8 bits)
--

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.numeric_std.all;

library work;
use work.opcodes.all;
use work.CPU_CONSTANTS.all;
use work.FlagConstants.all;


entity  ALU_TEST  is

    port(
        IR        :  in  opcode_word;                       -- Instruction Register
        OperandA  :  in  std_logic_vector(7 downto 0);      -- first operand
        OperandB  :  in  std_logic_vector(7 downto 0);      -- second operand
        clock     :  in  std_logic;                         -- system clock
        Result    :  out std_logic_vector(7 downto 0);      -- ALU result
        StatReg   :  out std_logic_vector(7 downto 0)       -- status register
    );

end  ALU_TEST;

architecture standard of ALU_TEST is

        -- Fake flags
        signal status_register:            std_logic_vector(NUM_DATA_BITS - 1 downto 0);
        signal new_flags:                  std_logic_vector(NUM_DATA_BITS - 1 downto 0);

        -- ALU control signals
        signal ALU_result_select:          std_logic_vector(1 downto 0);
        signal Shifter_low_bit_select:     std_logic_vector(1 downto 0);
        signal Shifter_middle_bits_select: std_logic;
        signal Shifter_high_bit_select:    std_logic_vector(2 downto 0);
        signal F_Block_Select:             std_logic_vector(3 downto 0);
        signal Subtract:                   std_logic;
        signal ALU_op_with_carry:          std_logic;
        signal AddSub_Op_1_Select:         std_logic;
        signal AddSub_Op_2_Select:         std_logic_vector(1 downto 0);

        -- Operands
        signal OperandA:                   std_logic_vector(NUM_DATA_BITS - 1 downto 0);
        signal OperandB:                   std_logic_vector(NUM_DATA_BITS - 1 downto 0);

        -- Flag update control
        signal TBit_Select             : std_logic_vector(DATA_BITS_LOG-1 downto 0);
        signal Interrupt_Flag_Sel      : std_logic_vector(NUM_I_FLAG_BITS-1 downto 0);
        signal Transfer_Flag_Sel       : std_logic_vector(NUM_T_FLAG_BITS-1 downto 0);
        signal Half_Carry_Flag_Sel     : std_logic_vector(NUM_H_FLAG_BITS-1 downto 0);
        signal Corrected_Sign_Flag_Sel : std_logic_vector(NUM_S_FLAG_BITS-1 downto 0);
        signal Signed_OF_Flag_Sel      : std_logic_vector(NUM_V_FLAG_BITS-1 downto 0);
        signal Neg_Flag_Sel            : std_logic_vector(NUM_N_FLAG_BITS-1 downto 0);
        signal Zero_Flag_Sel           : std_logic_vector(NUM_Z_FLAG_BITS-1 downto 0);
        signal Carry_Flag_Sel          : std_logic_vector(NUM_C_FLAG_BITS-1 downto 0);

        -- GP Register control
        signal GP_Input_Select         : std_logic_vector(NUM_GP_INP_SELECT_BITS-1 downto 0);
        signal GP_Write_Enable         : std_logic;
        signal GP_Swap_Nibbles         : std_logic;
        signal GP_Dst_Select           : std_logic_vector(NUM_REG_LOG-1 downto 0);
        signal GP_Src_SelectA          : std_logic_vector(NUM_REG_LOG-1 downto 0);
        signal GP_Src_SelectB          : std_logic_vector(NUM_REG_LOG-1 downto 0);
        
        -- IO Register control
        signal IO_Input_Select         : std_logic;
        signal IO_Write_Enable         : std_logic;
        signal IO_Dst_Select           : std_logic_vector(NUM_IO_LOG-1 downto 0);
        signal IO_Src_SelectA          : std_logic_vector(NUM_IO_LOG-1 downto 0);
        signal IO_Src_SelectB          : std_logic_vector(NUM_IO_LOG-1 downto 0);

        -- Inputs (IOREG)
        signal Updated_SREG            : std_logic_vector(NUM_DATA_BITS-1 downto 0);
        -- Outputs (IOREG)
        signal IO_outA                 : std_logic_vector(NUM_DATA_BITS-1 downto 0);
        signal IO_outB                 : std_logic_vector(NUM_DATA_BITS-1 downto 0)

begin

    ControlUnit : entity work.control_unit(control_arch)
    port map (

        -- program data bus
        Program_Data_Bus => IR,
        
        -- ALU control signals
        ALU_result_select          => ALU_result_select,
        Shifter_low_bit_select     => Shifter_low_bit_select,
        Shifter_middle_bits_select => Shifter_middle_bits_select,
        Shifter_high_bit_select    => Shifter_high_bit_select,
        F_Block_Select             => F_Block_Select,
        Subtract                   => Subtract,
        ALU_op_with_carry          => ALU_op_with_carry,
        AddSub_Op_1_Select         => AddSub_Op_1_Select,
        AddSub_Op_2_Select         => AddSub_Op_2_Select,

        -- Operands
        OperandA                   => OperandA,
        OperandB                   => OperandB,

        -- Flag update control
        TBit_Select                => TBit_Select,
        Interrupt_Flag_Sel         => Interrupt_Flag_Sel,
        Transfer_Flag_Sel          => Transfer_Flag_Sel,
        Half_Carry_Flag_Sel        => Half_Carry_Flag_Sel,
        Corrected_Sign_Flag_Sel    => Corrected_Sign_Flag_Sel,
        Signed_OF_Flag_Sel         => Signed_OF_Flag_Sel,
        Neg_Flag_Sel               => Neg_Flag_Sel,
        Zero_Flag_Sel              => Zero_Flag_Sel,
        Carry_Flag_Sel             => Carry_Flag_Sel,
        
        -- Register control signals
        GP_Input_Select            => GP_Input_Select,
        GP_Write_Enable            => GP_Write_Enable,
        GP_Swap_Nibbles            => GP_Swap_Nibbles,
        GP_Dst_Select              => GP_Dst_Select,
        GP_Src_SelectA             => GP_Src_SelectA,
        GP_Src_SelectB             => GP_Src_SelectB,

        GP_outA                    => RegAOut,
        GP_outB                    => RegBOut,

        IO_Input_Select            => IO_Input_Select,
        IO_Write_Enable            => IO_Write_Enable,
        IO_Dst_Select              => IO_Dst_Select,
        IO_Src_SelectA             => IO_Src_SelectA,
        IO_Src_SelectB             => IO_Src_SelectB
    );

    ALU : entity work.alu(ALU_arch)
    port map (
        -- Hook up clock
        clk                        => clock,

        -- ALU Control signals
        ALU_result_select          => ALU_result_select,
        Shifter_low_bit_select     => Shifter_low_bit_select,
        Shifter_middle_bits_select => Shifter_middle_bits_select,
        Shifter_high_bit_select    => Shifter_high_bit_select,
        F_Block_Select             => F_Block_Select,
        Subtract                   => Subtract,
        ALU_op_with_carry          => ALU_op_with_carry,
        AddSub_Op_1_Select         => AddSub_Op_1_Select,
        AddSub_Op_2_Select         => AddSub_Op_2_Select,

        -- Flag updating control signals
        TBit_Select                => TBit_Select,
        Interrupt_Flag_Sel         => Interrupt_Flag_Sel,
        Transfer_Flag_Sel          => Transfer_Flag_Sel,
        Half_Carry_Flag_Sel        => Half_Carry_Flag_Sel,
        Corrected_Sign_Flag_Sel    => Corrected_Sign_Flag_Sel,
        Signed_OF_Flag_Sel         => Signed_OF_Flag_Sel,
        Neg_Flag_Sel               => Neg_Flag_Sel,
        Zero_Flag_Sel              => Zero_Flag_Sel,
        Carry_Flag_Sel             => Carry_Flag_Sel,

        -- Operands
        OperandA                   => OperandA,
        OperandB                   => OperandB,

        -- Old flag value
        old_flags                  => status_register,
        Result                     => Result,

        -- New flag value
        new_flags                  => new_flags
    );

    process(clock)
    begin
        if (rising_edge(clock)) then
            status_register <= new_flags;
        end if;
    end process;

    StatReg <= status_register;

end architecture;