.include "4414def.inc"
.cseg

; AVR Processor Test Code 
;
;
; Description:
;
; Operation:
;
; Arguments:
; Return Values:
;
; Local Variables:
; Shared Variables:
; Global Variables:
;
; Input:
; Output:
;
; Error Handling:
;
; Registers Changed:
; Stack Depth:
;
; Algorithms:
; Data Structures:
;
; Known Bugs:
; Limitations:
;
; Revision History:
;

AVR_Processor_Testing:

	LDI		R27, 100
	LDI		R26, 200
	LDI		R25, 0
	ADD		R27, R26
	; check flags
	RJMP 	6
	NOP
	ADC		R25, R27
	BRCC	9
	NOP
	ADD		R27, R25
	NEG		R27
	LDI		R24, 0
	NEG		R24

	LDI		R27, 55
	;ST		R25+, R27
	;ST 		Y+, Y
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
	;JMP 	TestLoadsandStores
	NOP	

TestLoadsandStores:	
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
	LDI		R20, 156
	CP		R19, R20
	; check zero flag is set

Finish__Testing:
	RET
