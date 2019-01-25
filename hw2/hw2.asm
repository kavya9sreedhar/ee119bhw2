.include "4414def.inc"
.cseg

; AVR Processor Test Code 
;
; Description: This file contains the test suite for the AVR
; Processor testing the Control Unit, Program Memory
; Access Unit, Data Memory Access Unit, Registers, and
; ALU instructions with the AT90S4414 device.
;
; Please note that some instructions including JMP and CALL
; were reported to be unsupported on the AT90S4414, so those
; instruction tests have been included but commented out for
; simulation purposes with AVR Studio.
;
; Revision History:
;	24 Jan 2019		Kavya Sreedhar & Dan Xu Initial Revision
;	24 Jan 2019		Kavya Sreedhar & Dan Xu Data & Program Memory Access Unit
;	24 Jan 2019		Kavya Sreedhar & Dan Xu ALU

AVR_Processor_Testing:

	LDI		R27, 100
	LDI		R26, 200
	LDI		R25, 0
	ADD		R27, R26
	; check flags
	RJMP 	6
	NOP
	ADC		R25, R27
	BRBC	0, 9
	NOP
	ADD		R27, R25
	NEG		R27
	LDI		R24, 0
	NEG		R24

	LDI		R27, 55
	LDI		R26, 55
	CP		R26, R27
	DEC		R26
	CP		R26, R27
	INC		R27
	CP		R26, R27
	SUBI	R27, 2
	CP		R26, R27
	SUB		R26, R27
	; check zero flag set
	;JMP 	TestDataMemoryAccessUnit	; test unconditional JMP instruction,
										;  should skip subsequent NOP instruction
	NOP	

; test loads and stores, push and pop
TestDataMemoryAccessUnit:

	; test basic loading and MOV
	LDI		R27, 55		; set X to 5555
	MOV		R26, R27	; test MOV instruction
	; check zero flag is set
	CP		R26, R27	;  check whether R26 and R27
						;  have the same value
	LDI		R29, 38		; set Y to 3891
	LDI		R28, 91
	LDI		R31, 12		; set Z to 1297
	LDI		R30, 97	
	MOV		R15, R27	; test MOV instruction
	; check zero flag set
	CP		R15, R26	;  check whether R15 and R26
						;  have the same value
	CP		R15, R29	;  check that R15 and R29 do
						;  not have the same value
						;  to clear zero flag
	; check zero flag is set
	CP		R26, R27	; check that MOV from earlier
						;  is preserved and zero flag
						;  is correctly updated and set
						;  to reflect that these registers
						;  have the same value

	; check X, Y, Z register load instructions using register
	;  operations on registers corresponding to those registers
	LD		R13, X		; load indirect with X
	LD		R14, X+		; load indirect with X and post-increment
	CP		R13, R14	; R13 and R14 should have the same value
						; since post-increment
	; check zero flag set
	LD		R15, -X		; load indirect with X and pre-decrement
	LDI		R29, 255
	ST		X, R13
	LD		R14, X
	LD		R13, Y+		; load indirect with Y and post-increment
	LD		R14, -Y		; load indirect with Y and pre-decrement	
	LD		R16, Z+		; load indirect with Z and post-increment
	LD		R17, -Z		; load indirect with Z and pre-decrement
	LDD		R18, Y + 62	; load indirect with Y and 
						; some unsigned displacement
	LDD		R19, Z + 5	; load indirect with Z and 
						; some unsigned displacement

	; check X register load instructions with store instructions
	;  to test store instructions
	ST		X+, R19		; store indirect with X and post increment
	LD		R14, -X		; load indirect with X and pre decrement
	CP		R14, R19	; R14 and R19 should have the same value
						;  so confirm zero flag is set
	; check zero flag is set
	LD		R12, X		; get current value of X
	LDI		R20, 232
	ST		-X, R20		; store indirect with X and pre-decrement
	LD		R13, X		; check whether value was correctly stored
	CP		R13, R20
	; check zero flag is set
	CP		R13, R12	; check that X was properly decremented as
						;  the values of X and X + 1 are different
						;  values now and should not be equal
	
	; check Y register load instructions with store instructions
	;  to test store instructions
	ST		Y+, R19		; store indirect with Y and post increment
	LD		R20, -Y		; load indirect with Y and pre decrement
	CP		R20, R19	; R14 and R19 should have the same value
						;  so confirm zero flag is set
	; check zero flag is set
	LD		R12, Y		; get current value of Y
	LDI		R20, 232
	ST		-Y, R14		; store indirect with Y and pre-decrement
	LD		R13, X		; check whether value was correctly stored
	CP		R13, R20
	; check zero flag is set
	CP		R13, R12	; check that Y was properly decremented as
						;  the values of Y and Y + 1 are different
						;  values now and should not be equal

	; check Z register load instructions with store instructions
	;  to test store instructions
	ST		Z+, R19		; store indirect with Z and post increment
	LD		R14, -Z		; load indirect with Z and pre decrement
	CP		R14, R19	; R14 and R19 should have the same value
						;  so confirm zero flag is set
	; check zero flag is set
	LD		R12, Z		; get current value of Z
	LDI		R20, 232
	ST		-Z, R20		; store indirect with Z and pre-decrement
	LD		R13, Z		; check whether value was correctly stored
	CP		R13, R20
	; check zero flag is set
	CP		R13, R12	; check that Z was properly decremented as
						;  the values of Z and Z + 1 are different
						;  values now and should not be equal

	; check zero flag is not set
	LDI		R19, 156	; check push and pop instructions
	PUSH	R19			; push R19 value and check if operation
						;  worked correctly with pop instruction
						;  after several other instructions
	STS		65535, R19	; store values to memory
	STS		65530, R18	; test values at ends of memory
	STS		0, R17		;  as well as low and high values
	STS		3, R16		;  and a few random values in between
	STS		20007, R15	; load stored values into R20 to 
	STS		54398, R14	;  verify they were written into memory
	LDS		R20, 65535	;  correctly and verify operation of 
	LDS		R20, 0		;  LDS instruction
	LDS		R20, 54398
	LDS		R19, 3		; verify next few values with a 
	LDS		R19, 20007	;  different register, R19
	LDS		R19, 65530
	POP		R19			; check that R19 value when previously
						;  pushed was correctly preserved
	LDI		R20, 156	; 
	CP		R19, R20
	; check zero flag is set


TestProgramMemoryAccessUnit:
	; test function call and return instructions
	; CALL	Test_Func
	; CALL  Test_Interrupt_Func
	; LDI		R20, 134		; load address values in registers to use with Z
	; LDI		R21, 124
	; ST		-Z, R20		; store indirect with Z and pre-decrement
	; ST		Z+, R20
	; IJMP				; PC should not change between the next two instructions
	; ICALL
	; ST		-Z, R21		; store indirect with Z and pre-decrement
	; IJMP


	; test skip instructions
	LDI		R20, 1		; load two registers with the same value
	MOV		R13, R20
	CPSE	R20, R13	; since they are equal, the NOP instruction
						;  should be skipped
	NOP
	INC		R20
	CPSE	R13, R20	; since these registers are no longer equal,
						;  the NOP instruction should be executed
	NOP
	SBRC	R20, 5		; this bit is clear, so the NOP instruction
						;  should be skipped
	NOP
	SBRC	R20, 2		; this bit is set, so the NOP instruction
						;  should be executed
	NOP
	SBRS	R20, 7		; this bit is set, so the NOP instruction
	  					;  should be skipped
	NOP
	SBRS	R20, 3		; this bit is clear, so the NOP instruction
	  					;  should be executed
	NOP

	; test conditional branches
	LDI		R20, 255	; add FF + FF
	MOV		R21, R20
	ADD		R20, R21
	BRBC	0, NOP1		; there is a carry, so following NOP should be executed
	NOP
	NOP1: NOP
	BRBS	0, NOP2		; there is a carry, so following NOP should be skipped
	NOP
	NOP2: NOP
	BRBC	1, NOP3		; result is not zero, so following NOP should be skipped
	NOP
	NOP3: NOP
	BRBS	1, NOP4		; result is not zero, so following NOP should be executed
	NOP
	NOP4: NOP
	BRBC	2, NOP5		; sign bit is set, so following NOP should be executed
	NOP
	NOP5: NOP
	BRBS	2, NOP6		; sign bit is set, so following NOP should be skipped
	NOP
	NOP6: NOP	
	BRBC	3, NOP7		; there is signed overflow, so following NOP should be executed
	NOP
	NOP7: NOP
	BRBS	3, NOP8		; there is signed overflow, so following NOP should be skipped
	NOP
	NOP8: NOP
	BRBC	4, NOP9		; corrected sign is clear, so following NOP should be skipped
	NOP
	NOP9: NOP
	BRBS	4, NOP10	; corrected sign is clear, so following NOP should be executed
	NOP
	NOP10: NOP
	BRBC	5, NOP11	; there is half carry, so following NOP should be executed
	NOP
	NOP11: NOP
	BRBS	5, NOP12	; there is half carry, so following NOP should be skipped
	NOP
	NOP12: NOP
	
	LDI		R20, 0		; add 0 + 0
	MOV		R21, R20
	ADD		R21, R20
	BRBC	0, NOP13		; there is no carry, so following NOP should be skipped
	NOP
	NOP13: NOP
	BRBS	0, NOP14		; there is no carry, so following NOP should be executed
	NOP
	NOP14: NOP
	BRBC	1, NOP15		; result is zero, so following NOP should be executed
	NOP
	NOP15: NOP
	BRBS	1, NOP16		; result is zero, so following NOP should be skipped
	NOP
	NOP16: NOP
	BRBC	2, NOP17		; sign bit is not set, so following NOP should be skipped
	NOP
	NOP17: NOP
	BRBS	2, NOP18		; sign bit is not set, so following NOP should be executed
	NOP
	NOP18: NOP	
	BRBC	3, NOP19		; there is not signed overflow, so following NOP should be skipped
	NOP
	NOP19: NOP
	BRBS	3, NOP20		; there is not signed overflow, so following NOP should be executed
	NOP
	NOP20: NOP
	BRBC	4, NOP21		; corrected sign is clear, so following NOP should be skipped
	NOP
	NOP21: NOP
	BRBS	4, NOP22		; corrected sign is clear, so following NOP should be executed
	NOP
	NOP22: NOP
	BRBC	5, NOP23		; there is not half carry, so following NOP should be skipped
	NOP
	NOP23: NOP
	BRBS	5, NOP24		; there is not half carry, so following NOP should be executed
	NOP
	NOP24: NOP

		

Finish__Testing:
	RET						; conclude testing


; helper functions

Test_Func:	; test function to test CALL, RCALL,
			; and RET instructions
	NOP
	; test RCALL by calling Test_Interrupt_Func
	RCALL	2
	RET

Test_RCALL_Func:	; test function to test
					; RCALL by calling this function
					; from Test_Func
	NOP
	RET

Test_Interrupt_Func: ; test function to test interrupt
					 ; return
	NOP
	RETI

