; Constants.asm: 
; Author: Isabelle, in class
; Date: 2016/04/07
; Operating System: Windows
; IDE: Visual Studio/MASM

.386
.model flat, stdcall
.stack 4096
ExitProcess proto,dwExitCode:dword

.data
	array1 BYTE 10h, 30h, 0F0h, 20h, 50h, 12h
	array2 BYTE 0E0h, 40h, 22h, 0E5h, 40h, 55h
	array3 BYTE 12 DUP(0)

.data?


.code

;--------------------------------------------------------------------------------------------------------
AppendArray PROC
; Input:
;			eax: address of the first array
;			ebx: address of the second array
;			edi: address of the third array (has to be size of array1+size of array2)
; OutPut:	[edx]: contains array1, then array2
; Requires:	nothing
;--------------------------------------------------------------------------------------------------------
	; saves the register to be used
	PUSH ecx
	PUSH esi
	PUSH edx

	; loop going through array1 and coping in into array3
	MOV ecx, LENGTHOF array1	; initialize loop counter (goes through array1)
	MOV esi, 0					; initialize counter for indirect addressing
	AppendArray_copy1:			; begining of the loop
		MOV dh, [eax + esi]		; copie a value of the first array into a registers
		MOV [edi+ esi], dh		; moves the value from the register to array3 in memory
		ADD esi, (SIZEOF array1/LENGTHOF array1)

	; restors the registers
	PUSH edx
	POP esi
	POP ecx

	ret
	AppendArray ENDP ; end of the AppendArray procedure
;--------------------------------------------------------------------------------------------------------
main PROC
	nop
	;Start of User Code


	;End of User Code
	nop
	invoke ExitProcess, 0

main endp
end main