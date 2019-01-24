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
	
	
Finish__Testing:
	RET
