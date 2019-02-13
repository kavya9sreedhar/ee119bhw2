----------------------------------------------------------------------------
--  Data Memory Access Unit Constants
--
--	This file contains contains constants used for the 
--	Data Memory Access Unit implementation specifically for 
--	an 8-bit AVR architecture.
--
--  Revision History:
--	04 Feb 2019		Kavya & Dan		Initial Revision
--  04 Feb 2019		Kavya & Dan		Updated comments
----------------------------------------------------------------------------

-- declaration of libraries used
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

-- constants for the Data Memory Access Unit implementation only
package PMAU_constants is 
    
	constant num_bits_Prog_Addr_Src_Sel: integer: 3;
	constant Prog_Addr_Src_Sel_Immed: std_logic_vector(num_bits_Prog_Addr_Src_Sel - 1 downto 0) := "000";
	constant Prog_Addr_Src_Sel_1: std_logic_vector(num_bits_Prog_Addr_Src_Sel - 1 downto 0) := "001";
	constant Prog_Addr_Src_Sel_2: std_logic_vector(num_bits_Prog_Addr_Src_Sel - 1 downto 0) := "010";
	constant Prog_Addr_Src_Sel_3: std_logic_vector(num_bits_Prog_Addr_Src_Sel - 1 downto 0) := "011";
	constant Prog_Addr_Src_Sel_Z: std_logic_vector(num_bits_Prog_Addr_Src_Sel - 1 downto 0) := "100";
	constant Prog_Addr_Src_Sel_DDataBus: std_logic_vector(num_bits_Prog_Addr_Src_Sel - 1 downto 0) := "101";
	constant Prog_Addr_Src_Sel_0: std_logic_vector(num_bits_Prog_Addr_Src_Sel - 1 downto 0) := "110";
	
	-- 000 select immediate
	-- 001 select 1 (advance to next instruction)
	-- 010 select 2 (skipping next instruction)
	-- 011 select 3 (skipping next two instructions)
	-- 100 select Z_register and PC <- z
	-- 101 select Data_Data_Bus
	-- 110 select 0
	
	-- possible offset select options to add to data address
	constant num_bits_Offset_Src_Sel: integer := 3;
	constant Offset_Src_Sel_pos_1: std_logic_vector(num_bits_Offset_Src_Sel-1 downto 0) := "000";
	constant Offset_Src_Sel_neg_1: std_logic_vector(num_bits_Offset_Src_Sel-1 downto 0) := "001";
	constant Offset_Src_Sel_0: std_logic_vector(num_bits_Offset_Src_Sel-1 downto 0) := "010";
	constant Offset_Src_Sel_offset: std_logic_vector(num_bits_Offset_Src_Sel-1 downto 0) := "011";
	constant Offset_Src_Sel_unsigned_q: std_logic_vector(num_bits_Offset_Src_Sel-1 downto 0) := "100";
	
	-- possible data address options to add offset to (immediate address, 
	-- registers, stack pointer)
	constant num_bits_Data_Addr_Src_Sel: integer := 3;
	constant Data_Addr_Src_Sel_Imm_Addr: std_logic_vector(2 downto 0) := "000";
	constant Data_Addr_Src_Sel_X: std_logic_vector(2 downto 0) := "001";
	constant Data_Addr_Src_Sel_Y: std_logic_vector(2 downto 0) := "010";
	constant Data_Addr_Src_Sel_Z: std_logic_vector(2 downto 0) := "011";
	constant Data_Addr_Src_Sel_SP: std_logic_vector(2 downto 0) := "100";
	
	-- options for choosing between pre or post select
	constant Pre_Post_Sel_Pre: std_logic := '0';
	constant Pre_Post_Sel_Post: std_logic := '1';
	
	constant DMAU_Reg_high_bit: integer := 8;
	constant DMAU_Reg_low_bit: integer := 4;
	
	constant IMMED_VAL_HIGH_BYTE1: integer := 11;
	constant IMMED_VAL_LOW_BYTE1: integer := 8;
	constant IMMED_VAL_HIGH_BYTE2: integer := 3;
	constant IMMED_VAL_LOW_BYTE2: integer := 0;
	
end package;