package CPU_CONSTANTS is
	-- number of general purpose registers
	constant NUM_REGISTERS: 		integer := 32;
	-- number of bits in data busses
	constant NUM_DATA_BITS: 		integer := 8;
	-- number of bits in address busses
	constant NUM_ADDRESS_BITS: 		integer := 16;
	-- number of bits in each general-purpose register
	constant NUM_BITS_PER_REGISTER: integer := 8;
end package;

package INSTRUCTION_CONSTANTS is
	constant NUM_INSTRUCTION_BITS: integer := 16;
	constant ADC_instruction: 	unsigned(NUM_INSTRUCTION_BITS - 1 downto 0)
		:= "000111----------";
	constant ADD_instruction: 	unsigned(NUM_INSTRUCTION_BITS - 1 downto 0)
		:= "000011----------";
	-- 2 clocks
	constant ADIW_instruction: 	unsigned(NUM_INSTRUCTION_BITS - 1 downto 0)
		:= "10010110--------";
	constant AND_instruction: 	unsigned(NUM_INSTRUCTION_BITS - 1 downto 0)
		:= "10010110--------";
	constant ANDI_instruction: 	unsigned(NUM_INSTRUCTION_BITS - 1 downto 0)
		:= "0111------------";
	constant ASR_instruction: 	unsigned(NUM_INSTRUCTION_BITS - 1 downto 0)
		:= "1001010-----0101";
	constant BCLR_instruction: 	unsigned(NUM_INSTRUCTION_BITS - 1 downto 0)
		:= "100101001---1000";
	constant BLD_instruction: 	unsigned(NUM_INSTRUCTION_BITS - 1 downto 0)
		:= "1111100-----0---";
	constant BSET_instruction: 	unsigned(NUM_INSTRUCTION_BITS - 1 downto 0)
		:= "100101000---1000";
	constant BST_instruction: 	unsigned(NUM_INSTRUCTION_BITS - 1 downto 0)
		:= "1001010-----0101";
	constant COM_instruction: 	unsigned(NUM_INSTRUCTION_BITS - 1 downto 0)
		:= "1001010-----0000";
	constant CP_instruction: 	unsigned(NUM_INSTRUCTION_BITS - 1 downto 0)
		:= "000101----------";
	constant CPC_instruction: 	unsigned(NUM_INSTRUCTION_BITS - 1 downto 0)
		:= "000001----------";
	constant CPI_instruction: 	unsigned(NUM_INSTRUCTION_BITS - 1 downto 0)
		:= "0011------------";
	constant DEC_instruction: 	unsigned(NUM_INSTRUCTION_BITS - 1 downto 0)
		:= "1001010-----1010";
	
end package;
	
package ALU_constants is 
	constant F_Block_Operation: std_logic_vector(1 downto 0) := "00";
	constant Adder_Subtractor_Operation: std_logic_vector(1 downto 0) := "01";
	constant Shifter_Rotater_Operation: std_logic_vector(1 downto 0) := "10";
	
	constant Shifter_low_bit_highest_bit: std_logic_vector(1 downto 0) := "00";
	constant Shifter_low_bit_0: std_logic_vector(1 downto 0) := "01";
	constant Shifter_low_bit_bit_1: std_logic_vector(1 downto 0) := "10";
	constant Shifter_low_bit_carry: std_logic_vector(1 downto 0) := "11";
	
	constant Shifter_middle_bits_select_immediate_right: std_logic := '0';
	constant Shifter_middle_bits_select_immediate_left: std_logic := '0';
	
	constant Shifter_high_bit_select_second_highest_bit: std_logic_vector := "000";
	constant Shifter_high_bit_select_highest_bit: std_logic_vector := "001";
	constant Shifter_high_bit_select_lowest_bit: std_logic_vector := "010";
	constant Shifter_high_bit_select_0: std_logic_vector := "011";
	constant Shifter_high_bit_select_carry: std_logic_vector := "100";
		
	constant F_Block_Select_0: std_logic_vector(3 downto 0) := "0000";
	constant F_Block_Select_nor: std_logic_vector(3 downto 0) := "0001";
	constant F_Block_Select_not_A: std_logic_vector(3 downto 0) := "0011";
	constant F_Block_Select_not_B: std_logic_vector(3 downto 0) := "0101";
	constant F_Block_Select_xor: std_logic_vector(3 downto 0) := "0110";
	constant F_Block_Select_nand: std_logic_vector(3 downto 0) := "0111";
	constant F_Block_Select_and: std_logic_vector(3 downto 0) := "1000";
	constant F_Block_Select_xnor: std_logic_vector(3 downto 0) := "1001";
	constant F_Block_Select_or: std_logic_vector(3 downto 0) := "1110";
	constant F_Block_Select_1: std_logic_vector(3 downto 0) := "1111";
	
	constant Subtraction: std_logic := '1';
	constant Addition: std_logic := '0';
	
	constant AddSub_Op_Select_0: std_logic_vector(1 downto 0) := "00";
	constant AddSub_Op_Select_1: std_logic_vector(1 downto 0) := "01";
	constant AddSub_Op_Select_OperandB: std_logic_vector(1 downto 0) := "10";
end package;