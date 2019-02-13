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
	
end package;