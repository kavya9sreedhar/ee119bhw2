----------------------------------------------------------------------------
--  title
--	description
--
--  Revision History:
--
----------------------------------------------------------------------------

--
-- declaration of libraries used
--
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library opcodes;
use opcodes.opcodes.all;

--
-- ALU entity declaration
--
entity ALU is
	generic(
		-- TODO need better name
		constant NUM_BITS: integer := 8;
		constant NUM_ADDRESS_BITS: integer := 16
		);
	
	port(
		-- control signal inputs
		-- selects ALU operation to perform
		-- 	00 F Block operation
		-- 	01 Adder/Subtractor operation
		-- 	10 Shifter/Rotater operation
		ALU_result_select: in std_logic_vector(1 downto 0);
		-- selects what to update lowest bit of shifter / rotater with
		--	00 select current highest bit
		-- 	01 select 0
		-- 	10 select current bit 1
		-- 	11 select current carry flag bit
		Shifter_low_bit_select: in std_logic_vector(1 downto 0);
		-- when ALU_result_select indicates F Block operation
		--	F Block inputs to mux for F Block operations
		-- when ALU_result_select indicates Adder/Subtractor operation
		--	lowest bit of this vector is the Subtract control signal (all others
		-- 	bits are ignored)
		-- when ALU_result_select indicates Shifter/Rotater operation
		-- 	bits 3 downto 1 selects high bit value:
		--		000 select current second highest bit
		--		001	select current highest bit
		--		010	select current lowest bit
		--		011	select 0
		--		100	select current carry flag bit
		--	bit 0 selects value for middle bits:
		--		0 select bit to the immediate right
		--		1 select bit to the immediate left
		Shift_mid_high_bits_FBlock_Subtract: in std_logic_vector(3 downto 0);
		
		-- other inputs
		-- instruction register
		IR: in opcode_word;
		-- first operand
		OperandA: in std_logic_vector(NUM_BITS - 1 downto 0);
		-- second operand
		OperandB: in std_logic_vector(NUM_BITS - 1 downto 0);
		
		-- outputs
		-- ALU result (from F Block, Adder/Subtractor, or Shifter/Rotater)
		Result: out std_logic_vector(NUM_BITS - 1 downto 0);
		-- updated status register
		Status_Register: out std_logic_vector(NUM_BITS - 1 downto 0)
        );
end entity;

--
-- Registers entity declaration
--
entity Registers is
	port(
        );
end entity;

--
-- Data Memory Access entity declaration
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
		Address_Source_Select: in std_logic_vector(2 downto 0);
		-- TODO check this number, add all offset sources
		-- selects offset source
		Offset_Source_Select: in std_logic_vector(3 downto 0);
		-- indicates whether or not pre/post-increment/decrement was 
		-- part of instruction
		Pre_or_Post_Select: in std_logic;
		
		-- other inputs
		-- instruction register
		IR: in opcode_word;
		-- second word of instruction
		Program_Data_Bus: in std_logic_vector(NUM_ADDRESS_BITS - 1 downto 0);
		-- TODO fix magic numbers
		X_register: in std_logic_vector(15 downto 0);
		Y_register: in std_logic_vector(15 downto 0);
		Z_register: in std_logic_vector(15 downto 0);
		SP_register: in std_logic_vector(15 downto 0);
		Y_register_with_unsigned_offset: in std_logic_vector(15 downto 0);
		Z_register_with_unsigned_offset: in std_logic_vector(15 downto 0);
		
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

--
-- Program Memory Access entity declaration
--
entity Program_Memory_Access is
	port(
		-- control signal inputs
		-- whether or not to load the current program counter value
		Load: in std_logic;
		-- what to add to either the current program counter value or 0
		-- 000 select instruction register
		-- 001 select 1 (advance to next instruction)
		-- 010 select 2 (skip 1 instruction)
		-- 011 select 3 (skip 2 instructions)
		-- 100 select register Z and PC <- Z
		Program_Address_Source_Select: in std_logic_vector(TODO downto 0);
		
		-- inputs
		-- program counter: contains address of current instruction
		Program_Counter: in std_logic_vector(15 downto 0);
		-- program address bus
		Program_Address_Bus: in std_logic_vector(NUM_ADDRESS_BITS - 1 downto 0);
		
		-- outputs
		-- program data bus
		Program_Data_Bus: out std_logic_vector(NUM_ADDRESS_BITS - 1 downto 0)
        );
end entity;

--
-- Control Unit entity declaration
--
entity Control_Unit is
	port(
		-- control signal outputs
		
		-- to ALU
		-- selects ALU operation to perform
		ALU_result_select: out std_logic_vector(1 downto 0);
		-- selects what to update lowest bit of shifter / rotater with
		Shifter_low_bit_select: out std_logic_vector(1 downto 0);
		-- when ALU_result_select indicates F Block operation
		--	F Block inputs to mux for F Block operations
		-- when ALU_result_select indicates Adder/Subtractor operation
		--	lowest bit of this vector is the Subtract control signal (all others
		-- 	bits are ignored)
		-- when ALU_result_select indicates Shifter/Rotater operation
		-- 	bits 3 downto 1 selects high bit value
		--	bit 0 selects value for middle bits
		Shift_mid_high_bits_FBlock_Subtract: out std_logic_vector(3 downto 0);
		
		-- to Data_Memory_Access
		-- selects address source
		Address_Source_Select: in std_logic_vector(2 downto 0);
		-- selects offset source
		Offset_Source_Select: in std_logic_vector(TODO downto 0);
		-- indicates whether or not pre/post-increment/decrement was 
		-- part of instruction
		Pre_or_Post_Select: in std_logic;
		
		-- to Program_Memory_Access
		-- whether or not to load the current program counter value
		Load: in std_logic;
		-- what to add to either the current program counter value or 0
		Program_Address_Source_Select: in std_logic_vector(TODO downto 0);
        );
end entity;