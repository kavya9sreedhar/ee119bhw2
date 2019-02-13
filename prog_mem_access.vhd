--
-- Program Memory Access entity declaration
--
entity Program_Memory_Access is
	port(
		-- control signal inputs
		-- whether or not to load the current program counter value
		Load: in std_logic;
		-- what to add to either the current program counter value or 0
		Prog_Addr_Src_Sel: out std_logic_vector(num_bits_Prog_Addr_Src_Sel - 1 downto 0);
		
		Z_register: in std_logic_vector(NUM_ADDRESS_BITS - 1 downto 0);
		Data_Data_Bus: in std_logic_vector(NUM_DATA_BITS - 1 downto 0);
		immed_val: in std_logic_vector(NUM_ADDRESS_BITS - 1 downto 0);
		
		-- program counter: contains address of current instruction, updated by entity
		Program_Counter: buffer std_logic_vector(NUM_ADDRESS_BITS - 1 downto 0);
		
		-- outputs
		-- program address bus
		Program_Address_Bus: out std_logic_vector(NUM_ADDRESS_BITS - 1 downto 0)
        );
end entity;

architecture prog_mem_access_arch of Program_Memory_Access is

	prog_addr_src: std_logic_vector(NUM_ADDRESS_BITS - 1 downto 0);
	Operand1: std_logic_vector(NUM_ADDRESS_BITS - 1 downto 0);
	Operand2: std_logic_vector(NUM_ADDRESS_BITS - 1 downto 0);

	begin

	prog_addr_src <= 
		immed_val when Prog_Addr_Src_Sel = Prog_Addr_Src_Sel_Immed else
		(0 => '1', others => '0') when Prog_Addr_Src_Sel = Prog_Addr_Src_Sel_1 else
		(1 => '1', others => '0') when Prog_Addr_Src_Sel = Prog_Addr_Src_Sel_2 else
		(0 => '1', 1 => '1', others => '0') 
			when Prog_Addr_Src_Sel = Prog_Addr_Src_Sel_3 else
		Z_register when Prog_Addr_Src_Sel = Prog_Addr_Src_Sel_Z else
		(7 downto 0 => Data_Data_Bus, others => '0') 
			when Prog_Addr_Src_Sel = Prog_Addr_Src_Sel_DDataBus else
		(others => '0');
		
	-- set operands for addition / subtraction operation
	Operand1 <= prog_addr_src;
	Operand2 <= Program_Counter when Load = '1' else
				(others => '0');
	Subtract <= '0';
	
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
	
	Program_Counter <= adder_subtractor_result;
	
end architecture;