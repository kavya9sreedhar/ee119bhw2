--
-- Control Unit entity declaration
--
entity Control_Unit is
	port(
		-- inputs
		-- program data bus
		Program_Data_Bus: in opcode_word;
		-- instruction register
		-- IR: in opcode_word;
		
    );
end entity;

architecture control_arch of Control_Unit
	
	component ALU port (
		-- system clk
		clk: in std_logic;
		
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
		-- when ALU_result_select indicates Adder/Subtractor operation
		--	lowest bit of this vector is the Subtract control signal (all others
		-- 	bits are ignored)
		-- when ALU_result_select indicates Shifter/Rotater operation
		--	bit 0 selects value for middle bits:
		--		0 select bit to the immediate right
		--		1 select bit to the immediate left
		Shifter_middle_bits_select: in std_logic;
		-- 	bits 3 downto 1 selects high bit value:
		--		000 select current second highest bit
		--		001	select current highest bit
		--		010	select current lowest bit
		--		011	select 0
		--		100	select current carry flag bit
		Shifter_high_bit_select: in std_logic_vector(2 downto 0);
		-- when ALU_result_select indicates F Block operation
		--	F Block inputs to mux for F Block operations
		F_Block_Select: in std_logic_vector(3 downto 0);
		-- indicates whether an addition or subtraction operation should occur
		Subtract: in std_logic;
		-- indicating whether performing ALU operation involving current carry / 
		-- borrow bit
		ALU_op_with_carry: in std_logic;
		-- chooses value of second operand for addition / subtraction
		-- 	00 select 0
		--  01 select 1
		--  10 select OperandB
		AddSub_Op_Select: in std_logic_vector(1 downto 0);
		-- flag mask indicating which flag values to update after ALU operation
		Status_Register_Mask: in std_logic_vector(7 downto 0);
		
		-- other inputs
		-- first operand
		OperandA: in std_logic_vector(NUM_DATA_BITS - 1 downto 0);
		-- second operand
		OperandB: in std_logic_vector(NUM_DATA_BITS - 1 downto 0);
		
		-- outputs
		-- ALU result (from F Block, Adder/Subtractor, or Shifter/Rotater)
		Result: out std_logic_vector(NUM_DATA_BITS - 1 downto 0);
		-- updated status register
		Status_Register: out std_logic_vector(NUM_DATA_BITS - 1 downto 0)
		);
	end component;
	
begin
	process (clk)
	begin
		if rising_edge(clk) then
			if std_match(Program_Data_Bus, ADC_instruction) then
				ALU_operation: ALU port map (
					clk => clk,
					ALU_result_select => Adder_Subtractor_Operation,
					Shifter_low_bit_select => ,
					Shifter_middle_bits_select => ,
					Shifter_high_bit_select => ,
					F_Block_Select => ,
					Subtract => Subtraction,
					ALU_op_with_carry => '1',
					AddSub_Op_Select => AddSub_Op_Select_OperandB,
					Status_Register_Mask => ,
					OperandA => ,
					OperandB => ,
					Result => Result,
					Status_Register =>
				);
			end if;
			
			if std_match(Program_Data_Bus, ADD_instruction) then
			
			end if;
			
			if std_match(Program_Data_Bus, ADC_instruction) then
			
			end if;
			
			if std_match(Program_Data_Bus, ADIW_instruction) then
			
			end if;

			if std_match(Program_Data_Bus, AND_instruction) then
			
			end if;
			
			if std_match(Program_Data_Bus, ANDI_instruction) then
			
			end if;
			
			if std_match(Program_Data_Bus, ASR_instruction) then
			
			end if;

			if std_match(Program_Data_Bus, BCLR_instruction) then
			
			end if;
			
			if std_match(Program_Data_Bus, BSET_instruction) then
			
			end if;
			
			if std_match(Program_Data_Bus, BST_instruction) then
			
			end if;
			
			if std_match(Program_Data_Bus, COM_instruction) then
			
			end if;
			
			if std_match(Program_Data_Bus, CP_instruction) then
			
			end if;
			
			if std_match(Program_Data_Bus, CPC_instruction) then
			
			end if;
			
			if std_match(Program_Data_Bus, CPI_instruction) then
			
			end if;
			
			if std_match(Program_Data_Bus, DEC_instruction) then
			
			end if;
		end if;
	end process;
end architecture;