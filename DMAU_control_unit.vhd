----------------------------------------------------------------------------
--  AVR Control Unit
--
--    This file contains an implementation of the control for an 8-bit
--  AVR architecture. This implementation consists of a FSM and instruction
--  decoder to process instructions from the program data bus.
--
--  Revision History:
--	28 Jan 19   Kavya Sreedhar & Dan Xu    Initial revision
-- 	30 Jan 19   Kavya Sreedhar & Dan Xu    Updated more control signals 
--    1  Feb 19   Kavya Sreedhar & Dan Xu    Updated revision history
--    2  Feb 19   Kavya Sreedhar & Dan Xu    Fixed syntax errors, updated comments
----------------------------------------------------------------------------

-- declaration of libraries used
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;

-- contains constants used for all CPU units
library work;
use work.CPU_CONSTANTS.all;
use work.DMAU_constants.all;
use work.opcodes.all;

--
-- Control Unit entity declaration
--
entity Control_Unit is
    port(
    
    -- The clock.
    clk: in std_logic;
    
    -- inputs
    -- program data bus
    Program_Data_Bus        : in std_logic_vector(15 downto 0);
    -- instruction register
    IR                      : in opcode_word;
 
    GP_Swap_Nibbles         : out std_logic;
    GP_Dst_SelectA          : out std_logic_vector(NUM_REG_LOG-1 downto 0);
    GP_Src_SelectA          : out std_logic_vector(NUM_REG_LOG-1 downto 0);
    GP_Src_SelectB          : out std_logic_vector(NUM_REG_LOG-1 downto 0);
	GP_Dst_SelectB          : out std_logic_vector(NUM_REG_LOG-1 downto 0);

    GP_outA                 : in std_logic_vector(NUM_DATA_BITS-1 downto 0);
    GP_outB                 : in std_logic_vector(NUM_DATA_BITS-1 downto 0);    
	
	-- Data Memory Access Unit Control Signals and values
	Data_Addr_Src_Sel		: out std_logic_vector(num_bits_Data_Addr_Src_Sel - 1 downto 0);
	Offset_Src_Sel			: out std_logic_vector(num_bits_Offset_Src_Sel - 1 downto 0);
	unsigned_displacement	: out std_logic_vector(NUM_ADDRESS_BITS - 1 downto 0);
	Pre_Post_Sel			: out std_logic;
	
	Immediate_Data_Address	: out std_logic_vector(NUM_ADDRESS_BITS - 1 downto 0);
	X_register				: out std_logic_vector(NUM_ADDRESS_BITS - 1 downto 0);
	Y_register				: out std_logic_vector(NUM_ADDRESS_BITS - 1 downto 0);
	Z_register				: out std_logic_vector(NUM_ADDRESS_BITS - 1 downto 0);
	SP_register				: out std_logic_vector(NUM_ADDRESS_BITS - 1 downto 0);
	
	-- load immediate signals
	LDI_op					: out std_logic;
	immed_val				: out std_logic_vector(NUM_DATA_BITS - 1 downto 0);
	
	Store					: out std_logic;
	
	-- active low control line indicating data memory is being read
	-- active only during 2nd half of the clock in the 2nd cycle
	DataRd					: out std_logic;
	-- active low control line indicating data memory is being written
	-- active only during 2nd half of the clock in the 2nd cycle
	DataWr					: out std_logic
    
    );
end entity;

architecture control_arch of Control_Unit is
    
	type state_type is (Clock1, Clock2, Clock3); 
	signal state: state_type;
	
begin

    process (clk)
    
    begin
    
    if rising_edge(clk) then
		
		case State is
		
		when Clock1 =>

			-- DATA MEMORY ACCESS UNIT INSTRUCTIONS clock 1
		
			if std_match(IR, OpLDX) then
				LDI_op <= '0';
				DataRd <= '1';
				DataWr <= '1';
				Data_Addr_Src_Sel <= Data_Addr_Src_Sel_X;
				Offset_Src_Sel <= Offset_Src_Sel_0;
				-- value does not matter
				unsigned_displacement <= (others => '0');
				-- address is not changed
				Pre_Post_Sel <= Pre_Post_Sel_Pre;
				-- value does not matter
				Immediate_Data_Address <= (others => '0');
				GP_Src_SelectA <= X_REG_HIGH_BYTE;
				GP_Src_SelectB <= X_REG_LOW_BYTE;
				X_register <= GP_outA & GP_outB;
				-- value does not matter
				Y_register <= (others => '0');
				-- value does not matter
				Z_register <= (others => '0');
				-- value does not matter
				SP_register <= (others => '0');
				State <= Clock2;
			end if;
			
			if std_match(IR, OpLDXI) then
				LDI_op <= '0';
				DataRd <= '1';
				DataWr <= '1';
				Data_Addr_Src_Sel <= Data_Addr_Src_Sel_X;
				Offset_Src_Sel <= Offset_Src_Sel_pos_1;
				-- value does not matter
				unsigned_displacement <= (others => '0');
				Pre_Post_Sel <= Pre_Post_Sel_Post;
				-- value does not matter
				Immediate_Data_Address <= (others => '0');
				GP_Src_SelectA <= X_REG_HIGH_BYTE;
				GP_Src_SelectB <= X_REG_LOW_BYTE;
				X_register <= GP_outA & GP_outB;
				-- value does not matter
				Y_register <= (others => '0');
				-- value does not matter
				Z_register <= (others => '0');
				-- value does not matter
				SP_register <= (others => '0');
				State <= Clock2;
			end if;
			
			if std_match(IR, OpLDXD) then
				LDI_op <= '0';
				DataRd <= '1';
				DataWr <= '1';
				Data_Addr_Src_Sel <= Data_Addr_Src_Sel_X;
				Offset_Src_Sel <= Offset_Src_Sel_neg_1;
				-- value does not matter
				unsigned_displacement <= (others => '0');
				Pre_Post_Sel <= Pre_Post_Sel_Pre;
				-- value does not matter
				Immediate_Data_Address <= (others => '0');
				GP_Src_SelectA <= X_REG_HIGH_BYTE;
				GP_Src_SelectB <= X_REG_LOW_BYTE;
				X_register <= GP_outA & GP_outB;
				-- value does not matter
				Y_register <= (others => '0');
				-- value does not matter
				Z_register <= (others => '0');
				-- value does not matter
				SP_register <= (others => '0');
				State <= Clock2;
			end if;
			
			if std_match(IR, OpLDYI) then
				LDI_op <= '0';
				DataRd <= '1';
				DataWr <= '1';
				Data_Addr_Src_Sel <= Data_Addr_Src_Sel_Y;
				Offset_Src_Sel <= Offset_Src_Sel_pos_1;
				-- value does not matter
				unsigned_displacement <= (others => '0');
				Pre_Post_Sel <= Pre_Post_Sel_Post;
				Immediate_Data_Address <= (others => '0');
				X_register <= (others => '0');
				GP_Src_SelectA <= Y_REG_HIGH_BYTE;
				GP_Src_SelectB <= Y_REG_LOW_BYTE;
				Y_register <= GP_outA & GP_outB;
				Z_register <= (others => '0');
				SP_register <= (others => '0');
				State <= Clock2;
			end if;
			
			if std_match(IR, OpLDYD) then
				LDI_op <= '0';
				DataRd <= '1';
				DataWr <= '1';
				Data_Addr_Src_Sel <= Data_Addr_Src_Sel_Y;
				Offset_Src_Sel <= Offset_Src_Sel_neg_1;
				-- value does not matter
				unsigned_displacement <= (others => '0');
				Pre_Post_Sel <= Pre_Post_Sel_Pre;
				Immediate_Data_Address <= (others => '0');
				X_register <= (others => '0');
				GP_Src_SelectA <= Y_REG_HIGH_BYTE;
				GP_Src_SelectB <= Y_REG_LOW_BYTE;
				Y_register <= GP_outA & GP_outB;
				Z_register <= (others => '0');
				SP_register <= (others => '0');
				State <= Clock2;
			end if;
			
			if std_match(IR, OpLDDY) then
				LDI_op <= '0';
				DataRd <= '1';
				DataWr <= '1';
				Data_Addr_Src_Sel <= Data_Addr_Src_Sel_Y;
				Offset_Src_Sel <= Offset_Src_Sel_unsigned_q;
				unsigned_displacement <= 
					(5 downto 0 => IR(13) & 
					IR(11 downto 10) & 
					IR(2 downto 0), 
					others => '0');
				Pre_Post_Sel <= Pre_Post_Sel_Post;
				Immediate_Data_Address <= (others => '0');
				X_register <= (others => '0');
				GP_Src_SelectA <= Y_REG_HIGH_BYTE;
				GP_Src_SelectB <= Y_REG_LOW_BYTE;
				Y_register <= GP_outA & GP_outB;
				Z_register <= (others => '0');
				SP_register <= (others => '0');
				State <= Clock2;
			end if;
			
			if std_match(IR, OpLDZI) then
				LDI_op <= '0';
				DataRd <= '1';
				DataWr <= '1';
				Data_Addr_Src_Sel <= Data_Addr_Src_Sel_Z;
				Offset_Src_Sel <= Offset_Src_Sel_pos_1;
				-- value does not matter
				unsigned_displacement <= (others => '0');
				Pre_Post_Sel <= Pre_Post_Sel_Post;
				Immediate_Data_Address <= (others => '0');
				X_register <= (others => '0');
				Y_register <= (others => '0');
				GP_Src_SelectA <= Y_REG_HIGH_BYTE;
				GP_Src_SelectB <= Y_REG_LOW_BYTE;
				Z_register <= GP_outA & GP_outB;
				SP_register <= (others => '0');
				State <= Clock2;
			end if;
			
			if std_match(IR, OpLDZD) then
				LDI_op <= '0';
				DataRd <= '1';
				DataWr <= '1';
				Data_Addr_Src_Sel <= Data_Addr_Src_Sel_Z;
				Offset_Src_Sel <= Offset_Src_Sel_neg_1;
				-- value does not matter
				unsigned_displacement <= (others => '0');
				Pre_Post_Sel <= Pre_Post_Sel_Pre;
				Immediate_Data_Address <= (others => '0');
				X_register <= (others => '0');
				Y_register <= (others => '0');
				GP_Src_SelectA <= Y_REG_HIGH_BYTE;
				GP_Src_SelectB <= Y_REG_LOW_BYTE;
				Z_register <= GP_outA & GP_outB;
				SP_register <= (others => '0');
				State <= Clock2;
			end if;
			
			if std_match(IR, OpLDDZ) then
				LDI_op <= '0';
				DataRd <= '1';
				DataWr <= '1';
				Data_Addr_Src_Sel <= Data_Addr_Src_Sel_Z;
				Offset_Src_Sel <= Offset_Src_Sel_unsigned_q;
				unsigned_displacement <= 
					(5 downto 0 => IR(13) & 
					IR(11 downto 10) & 
					IR(2 downto 0), 
					others => '0');
				Pre_Post_Sel <= Pre_Post_Sel_Post;
				Immediate_Data_Address <= (others => '0');
				X_register <= (others => '0');
				Y_register <= (others => '0');
				GP_Src_SelectA <= Y_REG_HIGH_BYTE;
				GP_Src_SelectB <= Y_REG_LOW_BYTE;
				Z_register <= GP_outA & GP_outB;
				SP_register <= (others => '0');
				State <= Clock2;
			end if;
			
			if std_match(IR, OpLDI) then
				LDI_op <= '1';
				GP_Dst_SelectA <= 
					'1' & IR(DMAU_Reg_high_bit - 1 downto DMAU_Reg_low_bit);
				immed_val <= IR(IMMED_VAL_HIGH_BYTE1 downto IMMED_VAL_LOW_BYTE1)
							& IR(IMMED_VAL_HIGH_BYTE2 downto IMMED_VAL_LOW_BYTE2);
				State <= Clock1;
			end if;
			
--			if std_match(IR, OpLDS) then
--				DataRd <= '1';
--				DataWr <= '1';
--			end if;
			
--			if std_match(IR, OpMOV) then
			
--			end if;
			
			if std_match(IR, OpSTX) then
				LDI_op <= '0';
				DataRd <= '1';
				DataWr <= '1';
				Data_Addr_Src_Sel <= Data_Addr_Src_Sel_X;
				Offset_Src_Sel <= Offset_Src_Sel_0;
				-- value does not matter
				unsigned_displacement <= (others => '0');
				Pre_Post_Sel <= Pre_Post_Sel_Pre;
				-- value does not matter
				Immediate_Data_Address <= (others => '0');
				GP_Src_SelectA <= X_REG_HIGH_BYTE;
				GP_Src_SelectB <= X_REG_LOW_BYTE;
				X_register <= GP_outA & GP_outB;
				-- value does not matter
				Y_register <= (others => '0');
				-- value does not matter
				Z_register <= (others => '0');
				-- value does not matter
				SP_register <= (others => '0');
				State <= Clock2;
			end if;
			
			if std_match(IR, OpSTXI) then
				LDI_op <= '0';
				DataRd <= '1';
				DataWr <= '1';
				Data_Addr_Src_Sel <= Data_Addr_Src_Sel_X;
				Offset_Src_Sel <= Offset_Src_Sel_pos_1;
				-- value does not matter
				unsigned_displacement <= (others => '0');
				Pre_Post_Sel <= Pre_Post_Sel_Post;
				-- value does not matter
				Immediate_Data_Address <= (others => '0');
				GP_Src_SelectA <= X_REG_HIGH_BYTE;
				GP_Src_SelectB <= X_REG_LOW_BYTE;
				X_register <= GP_outA & GP_outB;
				-- value does not matter
				Y_register <= (others => '0');
				-- value does not matter
				Z_register <= (others => '0');
				-- value does not matter
				SP_register <= (others => '0');
				State <= Clock2;
			end if;
			
			if std_match(IR, OpSTXD) then
				LDI_op <= '0';
				DataRd <= '1';
				DataWr <= '1';
				Data_Addr_Src_Sel <= Data_Addr_Src_Sel_X;
				Offset_Src_Sel <= Offset_Src_Sel_neg_1;
				-- value does not matter
				unsigned_displacement <= (others => '0');
				Pre_Post_Sel <= Pre_Post_Sel_Pre;
				-- value does not matter
				Immediate_Data_Address <= (others => '0');
				GP_Src_SelectA <= X_REG_HIGH_BYTE;
				GP_Src_SelectB <= X_REG_LOW_BYTE;
				X_register <= GP_outA & GP_outB;
				-- value does not matter
				Y_register <= (others => '0');
				-- value does not matter
				Z_register <= (others => '0');
				-- value does not matter
				SP_register <= (others => '0');
				State <= Clock2;
			end if;
			
			if std_match(IR, OpSTYI) then
				LDI_op <= '0';
				DataRd <= '1';
				DataWr <= '1';
				Data_Addr_Src_Sel <= Data_Addr_Src_Sel_Y;
				Offset_Src_Sel <= Offset_Src_Sel_pos_1;
				-- value does not matter
				unsigned_displacement <= (others => '0');
				Pre_Post_Sel <= Pre_Post_Sel_Post;
				Immediate_Data_Address <= (others => '0');
				X_register <= (others => '0');
				GP_Src_SelectA <= Y_REG_HIGH_BYTE;
				GP_Src_SelectB <= Y_REG_LOW_BYTE;
				Y_register <= GP_outA & GP_outB;
				Z_register <= (others => '0');
				SP_register <= (others => '0');
				State <= Clock2;
			end if;
			
			if std_match(IR, OpSTYD) then
				LDI_op <= '0';
				DataRd <= '1';
				DataWr <= '1';
				Data_Addr_Src_Sel <= Data_Addr_Src_Sel_Y;
				Offset_Src_Sel <= Offset_Src_Sel_neg_1;
				-- value does not matter
				unsigned_displacement <= (others => '0');
				Pre_Post_Sel <= Pre_Post_Sel_Pre;
				Immediate_Data_Address <= (others => '0');
				X_register <= (others => '0');
				GP_Src_SelectA <= Y_REG_HIGH_BYTE;
				GP_Src_SelectB <= Y_REG_LOW_BYTE;
				Y_register <= GP_outA & GP_outB;
				Z_register <= (others => '0');
				SP_register <= (others => '0');
				State <= Clock2;
			end if;
			
			if std_match(IR, OpSTDY) then
				LDI_op <= '0';
				DataRd <= '1';
				DataWr <= '1';
				Data_Addr_Src_Sel <= Data_Addr_Src_Sel_Y;
				Offset_Src_Sel <= Offset_Src_Sel_unsigned_q;
				unsigned_displacement <= 
					(5 downto 0 => IR(13) & 
					IR(11 downto 10) & 
					IR(2 downto 0), 
					others => '0');
				Pre_Post_Sel <= Pre_Post_Sel_Post;
				Immediate_Data_Address <= (others => '0');
				X_register <= (others => '0');
				GP_Src_SelectA <= Y_REG_HIGH_BYTE;
				GP_Src_SelectB <= Y_REG_LOW_BYTE;
				Y_register <= GP_outA & GP_outB;
				Z_register <= (others => '0');
				SP_register <= (others => '0');
				State <= Clock2;
			end if;
			
			if std_match(IR, OpSTZI) then
				LDI_op <= '0';
				DataRd <= '1';
				DataWr <= '1';
				Data_Addr_Src_Sel <= Data_Addr_Src_Sel_Z;
				Offset_Src_Sel <= Offset_Src_Sel_pos_1;
				-- value does not matter
				unsigned_displacement <= (others => '0');
				Pre_Post_Sel <= Pre_Post_Sel_Post;
				Immediate_Data_Address <= (others => '0');
				X_register <= (others => '0');
				Y_register <= (others => '0');
				GP_Src_SelectA <= Y_REG_HIGH_BYTE;
				GP_Src_SelectB <= Y_REG_LOW_BYTE;
				Z_register <= GP_outA & GP_outB;
				SP_register <= (others => '0');
				State <= Clock2;
			end if;
			
			if std_match(IR, OpSTZD) then
				LDI_op <= '0';
				DataRd <= '1';
				DataWr <= '1';
				Data_Addr_Src_Sel <= Data_Addr_Src_Sel_Z;
				Offset_Src_Sel <= Offset_Src_Sel_neg_1;
				-- value does not matter
				unsigned_displacement <= (others => '0');
				Pre_Post_Sel <= Pre_Post_Sel_Pre;
				Immediate_Data_Address <= (others => '0');
				X_register <= (others => '0');
				Y_register <= (others => '0');
				GP_Src_SelectA <= Y_REG_HIGH_BYTE;
				GP_Src_SelectB <= Y_REG_LOW_BYTE;
				Z_register <= GP_outA & GP_outB;
				SP_register <= (others => '0');
				State <= Clock2;
			end if;
			
			if std_match(IR, OpSTDZ) then
				LDI_op <= '0';
				DataRd <= '1';
				DataWr <= '1';
				Data_Addr_Src_Sel <= Data_Addr_Src_Sel_Z;
				Offset_Src_Sel <= Offset_Src_Sel_unsigned_q;
				unsigned_displacement <= 
					(5 downto 0 => IR(13) & 
					IR(11 downto 10) & 
					IR(2 downto 0), 
					others => '0');
				Pre_Post_Sel <= Pre_Post_Sel_Post;
				Immediate_Data_Address <= (others => '0');
				X_register <= (others => '0');
				Y_register <= (others => '0');
				GP_Src_SelectA <= Y_REG_HIGH_BYTE;
				GP_Src_SelectB <= Y_REG_LOW_BYTE;
				Z_register <= GP_outA & GP_outB;
				SP_register <= (others => '0');
				State <= Clock2;
			end if;
			
--			if std_match(IR, OpSTS) then
--				DataRd <= '1';
--				DataWr <= '1';
--			end if;

			if std_match(IR, OpIN) then
			
			end if;
			
			if std_match(IR, OpOUT) then
			
			end if;
			
--		when Clock2 =>
		
--			-- DATA MEMORY ACCESS UNIT INSTRUCTIONS clock 2
		
			if std_match(IR, OpLDX) then
				--DataRd <= not ('1' and not clk);
				DataRd <= clk;
				DataWr <= '1';
				GP_Dst_SelectB <= GP_Dst_SelectB_X;
				Store <= '0';
				GP_Dst_SelectA <= 
					Program_Data_Bus(DMAU_Reg_high_bit downto DMAU_Reg_low_bit);
				State <= Clock1;
			end if;
			
			if std_match(IR, OpLDXI) then
				DataRd <= clk;
				DataWr <= '1';
				GP_Dst_SelectB <= GP_Dst_SelectB_X;
				Store <= '0';
				GP_Dst_SelectA <= 
					Program_Data_Bus(DMAU_Reg_high_bit downto DMAU_Reg_low_bit);
				State <= Clock1;
			end if;
			
			if std_match(IR, OpLDXD) then
				DataRd <= clk;
				DataWr <= '1';
				GP_Dst_SelectB <= GP_Dst_SelectB_X;
				Store <= '0';
				GP_Dst_SelectA <= 
					Program_Data_Bus(DMAU_Reg_high_bit downto DMAU_Reg_low_bit);
				State <= Clock1;
			end if;
			
			if std_match(Program_Data_Bus, OpLDYI) then
				DataRd <= clk;
				DataWr <= '1';
				GP_Dst_SelectB <= GP_Dst_SelectB_Y;
				Store <= '0';
				GP_Dst_SelectA <= 
					Program_Data_Bus(DMAU_Reg_high_bit downto DMAU_Reg_low_bit);
				State <= Clock1;
			end if;
			
			if std_match(Program_Data_Bus, OpLDYD) then
				DataRd <= clk;
				DataWr <= '1';
				GP_Dst_SelectB <= GP_Dst_SelectB_Y;
				Store <= '0';
				GP_Dst_SelectA <= 
					Program_Data_Bus(DMAU_Reg_high_bit downto DMAU_Reg_low_bit);
				State <= Clock1;
			end if;
			
			if std_match(Program_Data_Bus, OpLDDY) then
				DataRd <= clk;
				DataWr <= '1';
				GP_Dst_SelectB <= GP_Dst_SelectB_Y;
				Store <= '0';
				GP_Dst_SelectA <= 
					Program_Data_Bus(DMAU_Reg_high_bit downto DMAU_Reg_low_bit);
				State <= Clock1;
			end if;
			
			if std_match(Program_Data_Bus, OpLDZI) then
				DataRd <= clk;
				DataWr <= '1';
				GP_Dst_SelectB <= GP_Dst_SelectB_Z;
				Store <= '0';
				GP_Dst_SelectA <= 
					Program_Data_Bus(DMAU_Reg_high_bit downto DMAU_Reg_low_bit);
				State <= Clock1;
			end if;
			
			if std_match(Program_Data_Bus, OpLDZD) then
				DataRd <= clk;
				DataWr <= '1';
				GP_Dst_SelectB <= GP_Dst_SelectB_Z;
				Store <= '0';
				GP_Dst_SelectA <= 
					Program_Data_Bus(DMAU_Reg_high_bit downto DMAU_Reg_low_bit);
				State <= Clock1;
			end if;
			
			if std_match(Program_Data_Bus, OpLDDZ) then
				DataRd <= clk;
				DataWr <= '1';
				GP_Dst_SelectB <= GP_Dst_SelectB_Z;
				Store <= '0';
				GP_Dst_SelectA <= 
					Program_Data_Bus(DMAU_Reg_high_bit downto DMAU_Reg_low_bit);
				State <= Clock1;
			end if;

			-- will never get here, instruction does not require 2 clocks
			if std_match(IR, OpLDI) then

			end if;
			
--			if std_match(IR, OpLDS) then
--				DataRd <= '1';
--				DataWr <= '1';
--			end if;
			
--			if std_match(IR, OpMOV) then
--				-- 1 clock instruction, nothing here
--				DataRd <= '1';
--				DataWr <= '1';
--			end if;
			
			if std_match(IR, OpSTX) then
				DataRd <= '1';
				DataWr <= clk;
				GP_Dst_SelectB <= GP_Dst_SelectB_X;
				Store <= '1';
				GP_Dst_SelectA <= 
					Program_Data_Bus(DMAU_Reg_high_bit downto DMAU_Reg_low_bit);
				State <= Clock1;
			end if;
			
			if std_match(IR, OpSTXI) then
				DataRd <= '1';
				DataWr <= clk;
				GP_Dst_SelectB <= GP_Dst_SelectB_X;
				Store <= '1';
				GP_Dst_SelectA <= 
					Program_Data_Bus(DMAU_Reg_high_bit downto DMAU_Reg_low_bit);
				State <= Clock1;
			end if;
			
			if std_match(IR, OpSTXD) then
				DataRd <= '1';
				DataWr <= clk;
				GP_Dst_SelectB <= GP_Dst_SelectB_X;
				Store <= '1';
				GP_Dst_SelectA <= 
					Program_Data_Bus(DMAU_Reg_high_bit downto DMAU_Reg_low_bit);
				State <= Clock1;
			end if;
			
			if std_match(IR, OpSTYI) then
				DataRd <= '1';
				DataWr <= clk;
				GP_Dst_SelectB <= GP_Dst_SelectB_Y;
				Store <= '1';
				GP_Dst_SelectA <= 
					Program_Data_Bus(DMAU_Reg_high_bit downto DMAU_Reg_low_bit);
				State <= Clock1;
			end if;
			
			if std_match(IR, OpSTYD) then
				DataRd <= '1';
				DataWr <= clk;
				GP_Dst_SelectB <= GP_Dst_SelectB_Y;
				Store <= '1';
				GP_Dst_SelectA <= 
					Program_Data_Bus(DMAU_Reg_high_bit downto DMAU_Reg_low_bit);
				State <= Clock1;
			end if;
			
			if std_match(IR, OpSTDY) then
				DataRd <= '1';
				DataWr <= clk;
				GP_Dst_SelectB <= GP_Dst_SelectB_Y;
				Store <= '1';
				GP_Dst_SelectA <= 
					Program_Data_Bus(DMAU_Reg_high_bit downto DMAU_Reg_low_bit);
				State <= Clock1;
			end if;
			
			if std_match(IR, OpSTZI) then
				DataRd <= '1';
				DataWr <= clk;
				GP_Dst_SelectB <= GP_Dst_SelectB_Z;
				Store <= '1';
				GP_Dst_SelectA <= 
					Program_Data_Bus(DMAU_Reg_high_bit downto DMAU_Reg_low_bit);
				State <= Clock1;
			end if;
			
			if std_match(IR, OpSTZD) then
				DataRd <= '1';
				DataWr <= clk;
				GP_Dst_SelectB <= GP_Dst_SelectB_Z;
				Store <= '1';
				GP_Dst_SelectA <= 
					Program_Data_Bus(DMAU_Reg_high_bit downto DMAU_Reg_low_bit);
				State <= Clock1;
			end if;
			
			if std_match(IR, OpSTDZ) then
				DataRd <= '1';
				DataWr <= clk;
				GP_Dst_SelectB <= GP_Dst_SelectB_Z;
				Store <= '1';
				GP_Dst_SelectA <= 
					Program_Data_Bus(DMAU_Reg_high_bit downto DMAU_Reg_low_bit);
				State <= Clock1;
			end if;
			
--			if std_match(IR, OpSTS) then
--				DataRd <= '1';
--				DataWr <= '1';
--			end if;
			
		if std_match(IR, OpIN) then
			
		end if;
		
		if std_match(IR, OpOUT) then
		
		end if;
			
--		when Clock3 =>
		
--			-- DATA MEMORY ACCESS UNIT INSTRUCTIONS clock 3
		
			-- will never get here, instruction does not require 3 clocks
			if std_match(IR, OpLDX) then
		
			end if;
			
			-- will never get here, instruction does not require 3 clocks
			if std_match(IR, OpLDXI) then
			
			end if;
			
			-- will never get here, instruction does not require 3 clocks
			if std_match(IR, OpLDXD) then
			
			end if;
			
			-- will never get here, instruction does not require 3 clocks
			if std_match(IR, OpLDYI) then
				
			end if;
			
			-- will never get here, instruction does not require 3 clocks
			if std_match(IR, OpLDYD) then
			
			end if;
			
			-- will never get here, instruction does not require 3 clocks
			if std_match(IR, OpLDDY) then
				
			end if;
			
			-- will never get here, instruction does not require 3 clocks
			if std_match(IR, OpLDZI) then
				
			end if;
			
			-- will never get here, instruction does not require 3 clocks
			if std_match(IR, OpLDZD) then
				
			end if;
			
			-- will never get here, instruction does not require 3 clocks
			if std_match(IR, OpLDDZ) then
			
			end if;
			
			-- will never get here, instruction does not require 3 clocks
			if std_match(IR, OpLDI) then
				
			end if;
			
--			if std_match(IR, OpLDS) then

--			end if;
			
			-- will never get here, instruction does not require 3 clocks
			if std_match(IR, OpMOV) then

			end if;
			
			-- will never get here, instruction does not require 3 clocks
			if std_match(IR, OpSTX) then
			
			end if;
			
			-- will never get here, instruction does not require 3 clocks
			if std_match(IR, OpSTXI) then
				
			end if;
			
			-- will never get here, instruction does not require 3 clocks
			if std_match(IR, OpSTXD) then
				
			end if;
			
			-- will never get here, instruction does not require 3 clocks
			if std_match(IR, OpSTYI) then
				
			end if;
			
			-- will never get here, instruction does not require 3 clocks
			if std_match(IR, OpSTYD) then
				
			end if;
			
			-- will never get here, instruction does not require 3 clocks
			if std_match(IR, OpSTDY) then
				
			end if;
			
			-- will never get here, instruction does not require 3 clocks
			if std_match(IR, OpSTZI) then
				
			end if;
			
			-- will never get here, instruction does not require 3 clocks
			if std_match(IR, OpSTZD) then
				
			end if;
			
			-- will never get here, instruction does not require 3 clocks
			if std_match(IR, OpSTDZ) then
				
			end if;
			
--			if std_match(IR, OpSTS) then
			
--			end if;
			
			if std_match(IR, OpIN) then
			
			end if;
			
			if std_match(IR, OpOUT) then
			
			end if;
			
		end case;
		
    
    end if;
    
    end process;
end architecture;