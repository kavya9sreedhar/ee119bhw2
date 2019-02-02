----------------------------------------------------------------------------
--  Flag generator
--
--	This file contains a flag generator/updater for the AVR.
--  Flags:
--  I 7 Global Interrupt Enable/Disable
--  T 6 Transfer bit used by BLD and BST instructions
--  H 5 Half Carry (carry out of bit 3, into bit 4)
--  S 4 Corrected Signed (N xor V)
--  V 3 Signed Overflow (carry out of bit 6 doesn't match carry out of bit 7),
--      is zero (0) for logical operations and N xor C for shift operations
--  N 2 Negative (sign bit, high bit of result)
--  Z 1 Zero (result is zero)
--  C 0 Carry (carry out of bit 7), set for COM instruction
--
--  Revision History:
--	30 Jan 19	Kavya Sreedhar & Dan Xu 	Initial Revision
----------------------------------------------------------------------------

library ieee;
library CPU_CONSTANTS;
library FlagConstants;

use CPU_CONSTANTS.all;
use FlagConstants.all;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

-- FlagGenerator entity declarations
-- 
-- Ports
-- Inputs:
--
-- old_flags [N_FLAGS-1..0]
--   The orginal flag register value
--.
-- Inputs from rest of ALU:
-- ALU_input_LSB
--   The LSB of the input into the ALU that goes into the shifter
-- ALU_OF
--   The signed overflow calculated by the ALU, specifically the Add/Subtracter
-- ALU_CF
--   The carry flag calculated by the ALU, specifically the Add/Subtractor
-- ALU_HF
--   The half carry flag calculated by the ALU, specifially the Adder/Subtractor
-- ALU_Output [NUM_DATA_BITS-1..0]
--   The output of the ALU.
--
-- Inputs from Control Unit:
-- TBit_Select [DATA_BITS_LOG-1..0]
--   The bit to select for setting the T bit.
-- Interrupt_Flag_Sel [NUM_I_FLAG_BITS-1..0]
--   Select source for the new interrupt flag.
-- Transfer_Flag_Sel [NUM_T_FLAG_BITS-1..0]
--   Select source for the new transfer flag.
-- Half_Carry_Flag_Sel [NUM_H_FLAG_BITS-1..0]
--   Select source for the new half carry flag.
-- Corrected_Sign_Flag_Sel [NUM_S_FLAG_BITS-1..0]
--   Select source for the new corrected sign flag.
-- Signed_OF_Flag_Sel [NUM_V_FLAG_BITS-1..0]
--   Select source for the new signed OF flag.
-- Neg_Flag_Sel [NUM_N_FLAG_BITS-1..0]
--   Select source for the new neg flag.
-- Zero_Flag_Sel [NUM_Z_FLAG_BITS-1..0]
--   Select source for the new zero flag.
-- Carry_Flag_Sel [NUM_C_FLAG_BITS-1..0]
--   Select source for the new carry flag.
--
-- Outputs:
--
-- new_flags [N_FLAGS-1..0]
--   The calcuated new flag register value.
--
entity FlagGenerator is
	port(
        -- The old flags comming in.
        old_flags               : in std_logic_vector(N_FLAGS-1 downto 0);
        -- updated flags
        new_flags               : out std_logic_vector(N_FLAGS-1 downto 0);

        -- The lowest bit of the rotation argument
        ALU_Input_LSB           : in std_logic;

        -- ALU block update signals
        ALU_OF                  : in std_logic;
        ALU_CF                  : in std_logic;
        ALU_HF                  : in std_logic;
        ALU_Output              : in std_logic_vector(NUM_DATA_BITS-1 downto 0);

        -- Flag control signals
        TBit_Select             : in std_logic_vector(DATA_BITS_LOG-1 downto 0);
        Interrupt_Flag_Sel      : in std_logic_vector(NUM_I_FLAG_BITS-1 downto 0);
        Transfer_Flag_Sel       : in std_logic_vector(NUM_T_FLAG_BITS-1 downto 0);
        Half_Carry_Flag_Sel     : in std_logic_vector(NUM_H_FLAG_BITS-1 downto 0);
        Corrected_Sign_Flag_Sel : in std_logic_vector(NUM_S_FLAG_BITS-1 downto 0);
        Signed_OF_Flag_Sel      : in std_logic_vector(NUM_V_FLAG_BITS-1 downto 0);
        Neg_Flag_Sel            : in std_logic_vector(NUM_N_FLAG_BITS-1 downto 0);
        Zero_Flag_Sel           : in std_logic_vector(NUM_Z_FLAG_BITS-1 downto 0);
        Carry_Flag_Sel          : in std_logic_vector(NUM_C_FLAG_BITS-1 downto 0)
    );
end entity;

-- Architecture for the flag updater
architecture standard of FlagGenerator is
begin
    
    -- Process to update the interrupt flag
    UpdateInterruptFlag : process(Interrupt_Flag_Sel, old_flags(IFLAG))
    begin
        -- Selection of what to make the I flag.
        case Interrupt_Flag_Sel is
            -- Hold the current value
            when I_HOLD_VALUE  =>
                new_flags(IFLAG) <= old_flags(IFLAG);
            -- Set the I flag
            when I_SET_VALUE   =>
                new_flags(IFLAG) <= '1';
            -- Clr the I flag    
            when I_CLEAR_VALUE =>
                new_flags(IFLAG) <= '0';
            -- Shouldn't happen
            when others => 
                new_flags(IFLAG) <= 'X';
        end case;

    end process UpdateInterruptFlag;

    -- Process to update the Transfer flag
    UpdateTransferFlag : process(Transfer_Flag_Sel, old_flags(TFLAG), TBit_Select)
    begin

        -- Select of what to make the T flag
        case Transfer_Flag_Sel is
            -- Keep T flag the same
            when T_HOLD_VALUE  =>
                new_flags(IFLAG) <= old_flags(IFLAG);
            -- Set the T flag
            when T_SET_VALUE   =>
                new_flags(IFLAG) <= '1';
            -- Clear the T flag
            when T_CLEAR_VALUE =>
                new_flags(IFLAG) <= '0';
            -- Get the bit from one of the registers
            when T_GET_BIT =>
                new_flags(IFLAG) <= register_file(to_integer(unsigned(TBit_Select)));
            -- Should not occur
            when others => 
                new_flags(TFLAG) <= 'X';
        end case;

    end process UpdateTransferFlag;

    -- Process to update the Half Carry flag
    UpdateHalfCarryFlag : process(Half_Carry_Flag_Sel, old_flags(HFLAG), ALU_HF)
    begin

        -- Select of what to make the H flag
        case Half_Carry_Flag_Sel is
            -- Keep T flag the same
            when H_HOLD_VALUE  =>
                new_flags(HFLAG) <= old_flags(HFLAG);
            -- Set the T flag
            when H_SET_VALUE   =>
                new_flags(HFLAG) <= '1';
            -- Clear the T flag
            when H_CLEAR_VALUE =>
                new_flags(HFLAG) <= '0';
            -- Get the bit from the ALU
            when H_FROM_ALU    =>
                new_flags(HFLAG) <= ALU_HF;
            -- Should not occur
            when others => 
                new_flags(HFLAG) <= 'X';
        end case;

    end process UpdateHalfCarryFlag;

    -- Update the corrected sign flag
    UpdateCorrectedSgnFlag : process(Corrected_Sign_Flag_Sel, 
      old_flags(SFLAG), new_flags(NFLAG), new_flags(VFLAG))
    begin 
        
        -- Select of what to make the S flag
        case Corrected_Sign_Flag_Sel is
            -- Keep S flag the same
            when S_HOLD_VALUE  =>
                new_flags(SFLAG) <= old_flags(SFLAG);
            -- Set the S flag
            when S_SET_VALUE   =>
                new_flags(SFLAG) <= '1';
            -- Clear the S flag
            when S_CLEAR_VALUE =>
                new_flags(SFLAG) <= '0';
            -- Get the bit from the ALU
            when S_FROM_ALU    =>
                new_flags(SFLAG) <= new_flags(NFLAG) xor new_flags(VFLAG);
            -- Should not occur
            when others => 
                new_flags(SFLAG) <= 'X';
        end case;

    end process UpdateCorrectedSgnFlag;

    -- Update the signed overflow flag
    UpdateSignedOFFlag : process(Signed_OF_Flag_Sel, old_flags(VFLAG), ALU_OF, 
      new_flags(NFLAG), new_flags(CFLAG))
    begin

        -- Select the source of the V Flag
        case Signed_OF_Flag_Sel is
            -- Keep V flag the same
            when V_HOLD_VALUE  =>
                new_flags(VFLAG) <= old_flags(VFLAG);
            -- Set the V flag
            when V_SET_VALUE   =>
                new_flags(VFLAG) <= '1';
            -- Clear the V flag
            when V_CLEAR_VALUE =>
                new_flags(VFLAG) <= '0';
            -- Get the bit from the ALU
            when V_FROM_ALU    =>
                new_flags(VFLAG) <= ALU_OF;
            -- Calculate from carry and neg.
            when V_C_XOR_N     =>
                new_flags(VFLAG) <= new_flags(NFLAG) xor new_flags(CFLAG);
            -- Should not occur
            when others => 
                new_flags(VFLAG) <= 'X';
        end case;

    end process UpdateSignedOFFlag;

    -- Update the negative flag
    UpdateNegFlag : process(Neg_Flag_Sel, old_flags(NFLAG), ALU_Output(NUM_DATA_BITS-1))
    begin
        case Neg_Flag_Sel is
            -- Keep N flag the same
            when N_HOLD_VALUE  =>
                new_flags(NFLAG) <= old_flags(NFLAG);
            -- Set the N flag
            when N_SET_VALUE   =>
                new_flags(NFLAG) <= '1';
            -- Clear the N flag
            when N_CLEAR_VALUE =>
                new_flags(NFLAG) <= '0';
            -- Get the bit from the ALU output
            when N_FROM_ALU    =>
                new_flags(NFLAG) <= ALU_Output(NUM_DATA_BITS-1);
            -- Shouldn't happen            
            when others => 
                new_flags(NFLAG) <= 'X';
        end case;
    end process UpdateNegFlag;

    -- Update the zero flag
    UpdateZeroFlag : process(Zero_Flag_Sel, old_flags(ZFLAG), ALU_Output)
    begin
        -- Check zero flag source
        case Zero_Flag_Sel is
            -- Keep Z flag the same
            when Z_HOLD_VALUE  =>
                new_flags(ZFLAG) <= old_flags(ZFLAG);
            -- Set the Z flag
            when Z_SET_VALUE   =>
                new_flags(ZFLAG) <= '1';
            -- Clear the Z flag
            when Z_CLEAR_VALUE =>
                new_flags(ZFLAG) <= '0';
            -- Check ALU output
            when Z_FROM_ALU    =>
                -- Set if zero, clear otherwise
                if ALU_Output = (NUM_DATA_BITS-1 downto 0 => '0') then
                    new_flags(ZFLAG) <= '1';
                else
                    new_flags(ZFLAG) <= '0';
                end if;
            -- Shouldn't happen            
            when others => 
                new_flags(ZFLAG) <= 'X';
        end case;
    end process UpdateZeroFlag;
   
    -- Update the carry flag
    UpdateCarryFlag : process(Carry_Flag_Sel, old_flags(CFLAG), 
      ALU_CF, ALU_Input_LSB)
    begin
        case Carry_Flag_Sel is
            -- Keep N flag the same
            when C_HOLD_VALUE  =>
                new_flags(CFLAG) <= old_flags(CFLAG);
            -- Set the N flag
            when C_SET_VALUE   =>
                new_flags(CFLAG) <= '1';
            -- Clear the N flag
            when C_CLEAR_VALUE =>
                new_flags(CFLAG) <= '0';
            -- Get the bit from the ALU output
            when C_FROM_ALU    =>
                new_flags(CFLAG) <= ALU_CF;
            -- LSB of the orginal
            when C_FROM_LSB =>
                new_flags(CFLAG) <= ALU_Input_LSB;
            -- Shouldn't happen            
            when others => 
                new_flags(CFLAG) <= 'X';
        end case;
    end process UpdateCarryFlag;   
    
end architecture;