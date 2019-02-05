----------------------------------------------------------------------------
--  Data Memory Access Unit 
--
--  This file contains an implementation of the data memory access unit for 
--  an 8-bit AVR architecture.
--
--  Revision History:
--	04 Feb 2019		Kavya Sreedhar & Dan Xu 	Initial Revision
----------------------------------------------------------------------------

-- declaration of libraries used
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;

library work;
-- general CPU constants
use work.CPU_CONSTANTS.all;
-- Data Memory Access Unit specific constants
use work.DMAU_constants.all;

--
-- Data Memory Access entity declaration
-- TODO ADD MORE SUBSTANCE
--
entity Data_Memory_Access is
	port(
		-- control signal inputs
		-- selects address source
		-- 	000 selects instruction register
		-- 	001 selects program data bus
		-- 	010 selects register X
		-- 	011 selects register Y
		-- 	100 selects register Z
		-- 	101 selects register SP
		-- 	110 selects Y with a 6-bit unsigned offset
		-- 	111 selects Z with a 6-bit unsigned offset
		Data_Addr_Src_Sel: in std_logic_vector(2 downto 0);
		-- selects offset source
		--  00 select 0
		--  01 select +1
		--  10 select -1
		--  11 select q (for Y and Z registers with q unsigned offset)
		Offset_Src_Sel: in std_logic_vector(3 downto 0);
		-- indicates whether or not pre/post-increment/decrement was 
		-- part of instruction
		Pre_Post_Sel: in std_logic;
		
		-- other inputs
		-- second word of instruction
		Program_Data_Bus: in std_logic_vector(NUM_ADDRESS_BITS - 1 downto 0);
		X_register: in std_logic_vector(15 downto 0);
		Y_register: in std_logic_vector(15 downto 0);
		Z_register: in std_logic_vector(15 downto 0);
		SP_register: in std_logic_vector(15 downto 0);
		
		-- outputs
		Data_Address_Bus: out std_logic_vector(15 downto 0);
		Data_Data_Bus: out std_logic_vector(7 downto 0);
		-- active low control line indicating data memory is being read
		-- active only during 2nd half of the clock in the 2nd cycle
		Data_Read: out std_logic;
		-- active low control line indicating data memory is being written
		-- active only during 2nd half of the clock in the 2nd cycle
		Data_Write: out std_logic
        );
end entity;

architecture Data_arch of Data_Memory_Access is
	signal offset: std_logic_vector(15 downto 0);
	signal data_addr_src: std_logic_vector(15 downto 0);
	signal adder_subtractor_result: std_logic_vector(15 downto 0);
	
begin
	data_addr_src <= --TODO when Data_Addr_Src_Sel = Data_Addr_Src_Sel_Imm_Addr else
					X_register when Data_Addr_Src_Sel = Data_Addr_Src_Sel_X else
					Y_register when Data_Addr_Src_Sel = Data_Addr_Src_Sel_Y else
					Z_register when Data_Addr_Src_Sel = Data_Addr_Src_Sel_Z else
					SP_register;
					
	offset <= 
		(0 => '1', others => '0') when 	Offset_Src_Sel = Offset_Src_Sel_pos_1 or
										Offset_Src_Sel = Offset_Src_Sel_neg_1 else
		(others => '0');
	
	Subtract <= '1' when Offset_Src_Sel_neg_1 else
				'0';
				
	Operand1 <= data_addr_src;
	Operand2 <= offset;
			
	-- bit 0 for addition / subtraction result
	adder_subtractor_result(0) <= 
		Operand1(0) xor (Operand2(0) xor Subtract) xor 
		(Subtract);
	-- carry out from bit 0 addition / subtraction
	carry_outs(0) <= 	(Operand1(0) and (Operand2(0) xor Subtract)) or 
						(Subtract and (Operand1(0) xor (Operand2(0) xor Subtract)));
	
	-- calculate bits 1 through n - 1 bits for addition / subtraction result
	get_adder_subtractor_bits: for i in range 1 to NUM_ADDRESS_BITS - 1 generate
		adder_subtractor_result(i) <=
			Operand1(i) xor (Operand2(i) xor Subtract) xor carry_outs(i - 1);
		-- calculate carry out from bits addition / subtraction for bits 1
		-- through n - 1
		carry_outs(i) <= 
			(Operand1(i) and (Operand2(i) xor Subtract)) or 
			(carry_outs(i - 1) and (Operand1(i) xor (Operand2(i) xor Subtract)));
	end generate get_adder_subtractor_bits;
	
	Data_Address_Bus <= data_addr_src when Pre_Post_Sel = Pre_Post_Sel_Pre else
						adder_subtractor_result;
	
end architecture;
