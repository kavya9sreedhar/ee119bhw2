----------------------------------------------------------------------------
--  Register
--
--	This file contains an implementation of a single input, two output
--  register file. Two was chosen because this was designed for a max
--  two argument instruction set.
--
--  Allows for the specification of:
--    NUM_BITS
--        The number of bits in a register.
--    NUM_REGISTERS
--        The number of registers in the register file.
--    NUM_OUTPUTS
--        The number of register outputs for the register file.
--
--  Revision History:
--	30 Jan 19	Kavya Sreedhar & Dan Xu 	Initial Revision
----------------------------------------------------------------------------
library ieee;

use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

--
-- Registers entity declarations
-- Generics
-- NUM_BITS
--     The number of bits in a register.
-- NUM_REGISTERS
--     The number of registers as a power of 2.
-- Ports
-- clk
--     The clock signal into the system.
--
-- reg_in [NUM_BITS-1..0]
--     The input to the register file.
--
-- reg_outA [NUM_BITS-1..0]
--     The first register output
-- reg_outB [NUM_BITS-1..0]
--     The second register output
--
-- Register_Write_Enable
--     Enable writing to the registers.
-- Register_Dst_Select
--     The destination for the register input.
-- Register_Src_SelectA
--     The select for the first register output.
-- Register_Src_SelectB
--     The select for the second register output.
entity Registers is

	generic(
		NUM_BITS              : positive := 8;
		NUM_REGISTERS         : natural  := 5
	);

	port(
		-- Clock
		clk                   : in std_logic;   
		
		-- Input
		reg_in                : in std_logic_vector(NUM_BITS-1 downto 0);

		-- Outputs (Comes as a buffer with all bits)
		reg_outA              : out std_logic_vector(NUM_BITS-1 downto 0);
		reg_outB              : out std_logic_vector(NUM_BITS-1 downto 0);

		-- Ctrl signals
		Register_Write_Enable : in std_logic;
		Register_Dst_Select   : in std_logic_vector(NUM_REGISTERS-1 downto 0);
		Register_Src_SelectA  : in std_logic_vector(NUM_REGISTERS-1 downto 0);
		Register_Src_SelectB  : in std_logic_vector(NUM_REGISTERS-1 downto 0)
		
     );
end entity;

-- Standard Register Architecture
architecture standard of Registers is

    -- Defined types
	-- The register data type
	subtype register_t is std_logic_vector(NUM_BITS-1 downto 0);
	-- The register bank data type
	type reg_bank_t is array(integer range <>) of register_t;

	-- The registers
	signal register_file      : reg_bank_t((2**NUM_REGISTERS)-1 downto 0);

begin

	load : process(clk)
	begin
		if (rising_edge(clk)) then
			if (Register_Write_Enable = '1') then 
				register_file(to_integer(unsigned(Register_Dst_Select))) <= reg_in;
			end if;
		end if;
	end process load;

	-- Output the proper register
	reg_outA <= register_file(to_integer(unsigned(Register_Src_SelectA)));
	reg_outB <= register_file(to_integer(unsigned(Register_Src_SelectB)));

end architecture;