----------------------------------------------------------------------------
--  Data Memory Access Unit 
--
--  This file contains an implementation of the data memory access unit for 
--  an 8-bit AVR architecture. The data memory access unit generates addresses
--	and reads or writes the data for the data memory. The data memory is 
--  addressed as bytes with 16-bits of address (64 Kbytes of total data memory
--  space). The address bus is output from this unit, while the data data bus
--  is updated with the contents from the address output and the read and the 
--  write lines are accordingly updated from the control unit depending on 
--  whether there was a load or store instruction. An address may come from
--  the second word of the instruction, the X, Y, Z, or SP registers (unchanged
--  or with pre/post-increment/decrement), or the Y or Z registers with a 
--  6-bit unsigned offset (range 0 to 63).
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
		Data_Addr_Src_Sel: in std_logic_vector(
								num_bits_Data_Addr_Src_Sel - 1 downto 0);
		-- selects offset source
		Offset_Src_Sel: in std_logic_vector(num_bits_Offset_Src_Sel - 1 downto 0);
		unsigned_displacement: in std_logic_vector(NUM_ADDRESS_BITS - 1 downto 0);
		-- indicates whether or not pre/post-increment/decrement was 
		-- part of instruction
		Pre_Post_Sel: in std_logic;
		
		-- other inputs
		-- 8 bits of data, zero padded upper bits
		Immediate_Data_Address: in std_logic_vector(NUM_ADDRESS_BITS - 1 downto 0);
		X_register: in std_logic_vector(NUM_ADDRESS_BITS - 1 downto 0);
		Y_register: in std_logic_vector(NUM_ADDRESS_BITS - 1 downto 0);
		Z_register: in std_logic_vector(NUM_ADDRESS_BITS - 1 downto 0);
		SP_register: in std_logic_vector(NUM_ADDRESS_BITS - 1 downto 0);
		
		-- outputs
		Data_Address_Bus: out std_logic_vector(NUM_ADDRESS_BITS - 1 downto 0);
		Summed_Signal: out std_logic_vector(NUM_ADDRESS_BITS - 1 downto 0)
        );
end entity;

architecture Data_arch of Data_Memory_Access is

	-- signal that contains the offset value to add to the data address to update
	signal offset: std_logic_vector(NUM_ADDRESS_BITS - 1 downto 0);
	-- signal that contains the data address value to update with the offset value
	signal data_addr_src: std_logic_vector(NUM_ADDRESS_BITS - 1 downto 0);
	-- contains the sum of the data address value and offset i.e. the updated data 
	-- address value
	signal adder_subtractor_result: std_logic_vector(NUM_ADDRESS_BITS - 1 downto 0);
	signal Subtract: std_logic;
	signal Operand1: std_logic_vector(NUM_ADDRESS_BITS - 1 downto 0);
	signal Operand2: std_logic_vector(NUM_ADDRESS_BITS - 1 downto 0);
	signal carry_outs: std_logic_vector(NUM_ADDRESS_BITS - 1 downto 0);
	
begin
	-- choose which data address to update (immediate address versus register value)
	data_addr_src <= 	Immediate_Data_Address 
							when Data_Addr_Src_Sel = Data_Addr_Src_Sel_Imm_Addr else
						X_register when Data_Addr_Src_Sel = Data_Addr_Src_Sel_X else
						Y_register when Data_Addr_Src_Sel = Data_Addr_Src_Sel_Y else
						Z_register when Data_Addr_Src_Sel = Data_Addr_Src_Sel_Z else
						SP_register;
					
	-- choose what offset to add to data address depending on instruction
	offset <= 
		-- add or subtract 1
		(0 => '1', others => '0') when 	Offset_Src_Sel = Offset_Src_Sel_pos_1 or
										Offset_Src_Sel = Offset_Src_Sel_neg_1 else
		-- do not change current data address value
		(others => '0') when Offset_Src_Sel = Offset_Src_Sel_0 else
		unsigned_displacement when Offset_Src_Sel = Offset_Src_Sel_unsigned_q;
	
	-- depending on whether offset is positive or negative, add or subtract the
	-- magnitude of the offset value to the data address
	Subtract <= '1' when Offset_Src_Sel = Offset_Src_Sel_neg_1 else
				'0';
				
	-- set operands for addition / subtraction operation between 
	-- data address and offset
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
	get_adder_subtractor_bits: for i in 1 to NUM_ADDRESS_BITS - 1 generate
		adder_subtractor_result(i) <=
			Operand1(i) xor (Operand2(i) xor Subtract) xor carry_outs(i - 1);
		 -- calculate carry out from bits addition / subtraction for bits 1
		 -- through n - 1
		carry_outs(i) <= 
			(Operand1(i) and (Operand2(i) xor Subtract)) or 
			(carry_outs(i - 1) and (Operand1(i) xor (Operand2(i) xor Subtract)));
	end generate get_adder_subtractor_bits;
	
	-- choose whether or not to update address depending on whether pre or post
	-- select was indicated as part of instruction
	Data_Address_Bus <= data_addr_src when Pre_Post_Sel = Pre_Post_Sel_Pre else
						adder_subtractor_result;
						
	Summed_Signal <= adder_subtractor_result;
	
end architecture;
