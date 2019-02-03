----------------------------------------------------------------------------
--  ALU Constants
--
--	This file contains contains constants used for the 
--	ALU implementation specifically for an 8-bit AVR architecture.
--
--  Revision History:
--	28 Jan 19	Kavya Sreedhar & Dan Xu 	Initial revision
--	1  Feb 19	Kavya Sreedhar & Dan Xu		Updated revision history
----------------------------------------------------------------------------

-- declaration of libraries used
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

-- constants for the ALU implementation only
package ALU_constants is 
    
	-- indicates various types of ALU operations
	constant F_Block_Operation: std_logic_vector(1 downto 0) := "00";
	constant Adder_Subtractor_Operation: std_logic_vector(1 downto 0) := "01";
	constant Shifter_Rotater_Operation: std_logic_vector(1 downto 0) := "10";
	
	-- SHIFT / ROTATE CONSTANTS
	
	-- indicates possibilities for update for lowest bit during a shift / rotate
	-- operation
	constant Shifter_low_bit_highest_bit: std_logic_vector(1 downto 0) := "00";
	constant Shifter_low_bit_0: std_logic_vector(1 downto 0) := "01";
	constant Shifter_low_bit_bit_1: std_logic_vector(1 downto 0) := "10";
	constant Shifter_low_bit_carry: std_logic_vector(1 downto 0) := "11";
	
	-- indicates possibilities for update for middle bits during a shift / rotate
	-- operation
	constant Shifter_middle_bits_select_immediate_right: std_logic := '0';
	constant Shifter_middle_bits_select_immediate_left: std_logic := '1';
	
	-- indicates possibilities for update for highest bit during a shift / rotate
	-- operation
	constant Shifter_high_bit_select_second_highest_bit: std_logic_vector := "000";
	constant Shifter_high_bit_select_highest_bit: std_logic_vector := "001";
	constant Shifter_high_bit_select_lowest_bit: std_logic_vector := "010";
	constant Shifter_high_bit_select_0: std_logic_vector := "011";
	constant Shifter_high_bit_select_carry: std_logic_vector := "100";
		
	-- F BLOCK CONSTANTS
	
	-- indicates possibilities for control signals for various F Block outputs
	constant F_Block_Select_0: std_logic_vector(3 downto 0) := "0000";
	constant F_Block_Select_nor: std_logic_vector(3 downto 0) := "0001";
	constant F_Block_Select_not_A: std_logic_vector(3 downto 0) := "0011";
	constant F_Block_Select_not_B: std_logic_vector(3 downto 0) := "0101";
	constant F_Block_Select_xor: std_logic_vector(3 downto 0) := "0110";
	constant F_Block_Select_nand: std_logic_vector(3 downto 0) := "0111";
	constant F_Block_Select_and: std_logic_vector(3 downto 0) := "1000";
	constant F_Block_Select_xnor: std_logic_vector(3 downto 0) := "1001";
	constant F_Block_Select_or: std_logic_vector(3 downto 0) := "1110";
	constant F_Block_Select_1: std_logic_vector(3 downto 0) := "1111";
	
	-- ADDITION / SUBTRACTION CONSTANTS
	
	-- indicates subtraction versus addition for addition / subtraction operation
	constant Subtraction: std_logic := '1';
	constant Addition: std_logic := '0';
	
	-- indicates possibilities for first operand for addition / subtraction operation
	constant AddSub_Op_1_Select_OperandA: std_logic_vector(1 downto 0) := "00";
	constant AddSub_Op_1_Select_FF: std_logic_vector(1 downto 0) := "01";
	constant AddSub_Op_1_Select_0: std_logic_vector(1 downto 0) := "10";
	
	-- indicates possibilities for second operand for addition / subtraction operation
	constant AddSub_Op_2_Select_0: std_logic_vector(1 downto 0) := "00";
	constant AddSub_Op_2_Select_1: std_logic_vector(1 downto 0) := "01";
	constant AddSub_Op_2_Select_OperandB: std_logic_vector(1 downto 0) := "10";
end package;