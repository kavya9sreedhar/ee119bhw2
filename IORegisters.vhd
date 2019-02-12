----------------------------------------------------------------------------
--  IORegisters
--
--	This file contains an implementation of a dual input, two output
--  register file. Two was chosen because this was designed for a max
--  two argument instruction set.
--
--  Allows for the specification of:
--    NUM_BITS
--        The number of bits in a register.
--    LNUM_REGISTERS
--        The number of registers in the register file. (LOG)
--    NUM_OUTPUTS
--        The number of register outputs for the register file.
--
--  Revision History:
--	30 Jan 19	Kavya Sreedhar & Dan Xu 	Initial revision
--  1  Feb 19	Kavya Sreedhar & Dan Xu		Updated comments
--	08 Feb 19	Kavya Sreedhar & Dan Xu 	Added wide loading functionality
----------------------------------------------------------------------------

-- library declarations
library ieee;
library work;

use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.RegConstants.all;

--
-- GPRegisters entity declarations
-- Generics
-- NUM_BITS
--     The number of bits in a register.
-- NUM_REGISTERS
--     The number of registers as a power of 2.
-- Ports
--
-- Inputs:
-- clk
--     The clock signal into the system.
-- rst
--     The reset signal
-- reg_inA [NUM_BITS-1..0]
--     The regular input to the register file.
-- reg_inB [2*NUM_BITS-1..0]
--     The wide input to the register file.
-- Register_Write_EnableA
--     Enable writing to the registers.
-- Register_Dst_SelectA
--     The destination for the register input.
-- Register_Write_EnableB
--     Enable wide bus writing to the registers.
-- Register_Dst_SelectA
--     The destination for the regular register input.
-- Register_Src_SelectA
--     The select for the first register output.
-- Register_Src_SelectB
--     The select for the second register output.
--
-- Outputs:
-- reg_outA [NUM_BITS-1..0]
--     The first register output
-- reg_outB [NUM_BITS-1..0]
--     The second register output
entity IORegisters is

	generic(
		NUM_BITS              : positive := 8;
		LNUM_REGISTERS        : natural  := 5
	);

	port(
		-- Clock
		clk                    : in std_logic;
		-- Reset signal
		rst                    : in std_logic;
		
		-- Input
		reg_inA                : in std_logic_vector(NUM_BITS-1 downto 0);
		reg_inB                : in std_logic_vector(2*NUM_BITS-1 downto 0);

		-- Outputs (Comes as a buffer with all bits)
		reg_outA               : out std_logic_vector(NUM_BITS-1 downto 0);
		reg_outB               : out std_logic_vector(NUM_BITS-1 downto 0);

		-- Ctrl signals
		Register_Write_EnableA : in std_logic;
		Register_Dst_SelectA   : in std_logic_vector(LNUM_REGISTERS-1 downto 0);

		Register_Write_EnableB : in std_logic;

		Register_Src_SelectA   : in std_logic_vector(LNUM_REGISTERS-1 downto 0);
		Register_Src_SelectB   : in std_logic_vector(LNUM_REGISTERS-1 downto 0)
     );
end entity;

-- Standard Register Architecture
architecture standard of IORegisters is

    -- Defined types
	-- The register data type
	subtype register_t is std_logic_vector(NUM_BITS-1 downto 0);
	-- The register bank data type
	type reg_bank_t is array(integer range <>) of register_t;

	-- The registers
	signal register_file      : reg_bank_t((2**LNUM_REGISTERS)-1 downto 0);

begin

	load : process(clk)
	begin
		-- Only load on rising edges
		if (rising_edge(clk)) then
			
			-- Check that we are enabled to load
            if (Register_Write_EnableA = '1') then
                register_file(to_integer(unsigned(Register_Dst_SelectA))) <= reg_inA;
			end if;
			
			-- Check if we load to 16 bit registers
			if (Register_Write_EnableB = '1') then
                register_file(SPH) <= reg_inB(NUM_BITS-1 downto 0);
                register_file(SPL) <= reg_inB(2*NUM_BITS-1 downto NUM_BITS);
			end if;

			-- Active low reset
			if (rst = '0') then
				register_file(SPH) <= (NUM_BITS-1 downto 0 => '1');
                register_file(SPL) <= (NUM_BITS-1 downto 0 => '1');
			end if;

		end if;
	end process load;
    
	-- Output the proper register
	reg_outA <= register_file(to_integer(unsigned(Register_Src_SelectA)));
	reg_outB <= register_file(to_integer(unsigned(Register_Src_SelectB)));

end architecture;