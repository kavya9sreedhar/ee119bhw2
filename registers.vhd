--
-- Registers entity declaration: used for general purpose registers and IO Space
--
entity Registers is
	port(
		-- array containing contents of all bits of all NUM_REGISTERS registers
		-- individual registers can be accessed by indexing according to the
		-- register number * NUM_BITS_PER_REGISTER appropriately
		Register_contents: buffer std_logic_vector(NUM_REGISTERS * 
			NUM_BITS_PER_REGISTER - 1 downto 0);
		
		-- if general purpose registers:
		--	00 IO Register
		-- 	01 Data_Data_Bus
		--	10 Output from ALU
		-- if IO Space:
		--	00 Data Data Bus
		--	01 Status Register Output from ALU
		Register_val_select: in std_logic_vector(1 downto 0);
		-- enable writing to general purpose registers
		Register_Write_Enable: out std_logic;
		-- select which register
		Register_select: out std_logic_vector(log2(NUM_REGISTERS) - 1 downto 0);
		-- indicates nibbles of a register should be swapped
		Swap: in std_logic;
		-- bit in register to set transfer bit in status register to or vice versa
		T_bit: in std_logic_vector(2 downto 0);
		-- indicates that a bit in a register should be updated to transfer bit
		-- or vice versa
		Transfer: in std_logic
        );
end entity;