----------------------------------------------------------------------------
--  Block Entity Declarations Atmel AVR CPU Design
--
--	This file contains the entity declarations in VHDL for the following blocks
--	part of the overall design for the Atmel AVR CPU:
--	1. Control Unit
--	2. Program Memory Access Unit
--	3. Data Memory Access Unit
--	4. Registers Unit (Also used for I/O Space)
--	5. ALU Unit (including Status Register)
--
--  Revision History:
--	23 Jan 19	Kavya Sreedhar & Dan Xu 	Initial Revision
--  23 Jan 19	Kavya Sreedhar & Dan Xu		Modified registers unit, added IO
--  24 Jan 19	Kavya Sreedhar & Dan Xu 	Updated Comments
----------------------------------------------------------------------------



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
		-- selects offset source
		--  00 select 0
		--  01 select +1
		--  10 select -1
		--  11 select q (for Y and Z registers with q unsigned offset)
		Offset_Source_Select: in std_logic_vector(3 downto 0);
		-- indicates whether or not pre/post-increment/decrement was 
		-- part of instruction
		Pre_or_Post_Select: in std_logic;
		
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

--
-- Program Memory Access entity declaration
--
entity Program_Memory_Access is
	port(
		-- control signal inputs
		-- whether or not to load the current program counter value
		Load: in std_logic;
		-- what to add to either the current program counter value or 0
		-- 000 select immediate
		-- 001 select 1 (advance to next instruction)
		-- 010 select 2 (skipping next instruction)
		-- 011 select 3 (skipping next two instructions)
		-- 100 select Z_register and PC <- z
		-- 101 select Data_Data_Bus
		-- 110 select 0
		Program_Address_Source_Select: out std_logic_vector(2 downto 0);
		
		-- program counter: contains address of current instruction, updated by entity
		Program_Counter: buffer std_logic_vector(NUM_ADDRESS_BITS downto 0);
		
		-- outputs
		-- program address bus
		Program_Address_Bus: out std_logic_vector(NUM_ADDRESS_BITS - 1 downto 0)
        );
end entity;