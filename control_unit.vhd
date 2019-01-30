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
		-- flag mask indicating which flag values to update after ALU operation
		Status_Register_Mask: out std_logic_vector(7 downto 0);
		-- bit in register to set transfer bit in status register to or vice versa
		T_bit: out std_logic_vector(2 downto 0);
		-- indicates that a bit in a register should be updated to transfer bit
		-- or vice versa
		Transfer: out std_logic;
		-- indicating whether performing ALU operation involving current carry / 
		-- borrow bit
		ALU_op_with_carry: out std_logic;
		-- chooses value of second operand for addition / subtraction
		AddSub_Op_Select: out std_logic_vector(1 downto 0);
		-- first operand
		OperandA: in std_logic_vector(NUM_DATA_BITS - 1 downto 0);
		-- second operand
		OperandB: in std_logic_vector(NUM_DATA_BITS - 1 downto 0);
		
		-- to Data_Memory_Access
		-- selects address source
		Address_Source_Select: out std_logic_vector(2 downto 0);
		-- selects offset source
		-- selects offset source
		--  00 select 0
		--  01 select +1
		--  10 select -1
		--  11 select q (for Y and Z registers with q unsigned offset)
		Offset_Source_Select: out std_logic_vector(3 downto 0);
		-- indicates whether or not pre/post-increment/decrement was 
		-- part of instruction
		Pre_or_Post_Select: out std_logic;
		
		-- to Program_Memory_Access
		-- whether or not to load the current program counter value
		Load: out std_logic;
		-- what to add to either the current program counter value or 0
		-- 000 select immediate
		-- 001 select 1 (advance to next instruction)
		-- 010 select 2 (skipping next instruction)
		-- 011 select 3 (skipping next two instructions)
		-- 100 select Z_register and PC <- z
		-- 101 select Data_Data_Bus
		-- 110 select 0
		Program_Address_Source_Select: out std_logic_vector(2 downto 0);
		
		-- to Registers
		-- selects what value to load into general-purpose registers or IO space
		Register_IO_val_select: out std_logic_vector(1 downto 0)
		-- enable writing to general purpose registers
		Register_Write_Enable: out std_logic;
		-- enable writing to IO registers
		IO_Write_Enable: out std_logic
		-- select register
		Register_select: out std_logic_vector(log2(NUM_REGISTERS) - 1 downto 0);
		-- indicates nibbles of a register should be swapped
		Swap: out std_logic
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