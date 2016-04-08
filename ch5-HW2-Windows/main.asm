; Constants.asm: 
; Author: Isabelle, in class
; Date: 2016/04/07
; Operating System: Windows
; IDE: Visual Studio/MASM

.386
.model flat, stdcall
.stack 4096
ExitProcess proto,dwExitCode:dword

; Required by Bill's print to console function
getstdout = -11 
WriteFile PROTO NEAR32 stdcall, \ 
        handle:dword,			\ 
        buffer:ptr byte,		\ 
        bytes:dword,			\ 
        written: ptr dword,		\ 
        overlapped: ptr byte 
GetStdHandle PROTO NEAR32, device:dword 
ExitProcess PROTO NEAR32, exitcode:dword 

.data
	array1 BYTE 10h, 30h, 0F0h, 20h, 50h, 12h
	array2 BYTE 0E0h, 40h, 22h, 0E5h, 40h, 55h
	array3 BYTE 12 DUP(0)

	; Required by Bill's print to console functions
	message db "Hello World!", 13, 10 
	msg_size dd $ - offset message
	szLF  BYTE  0dh, 0ah, 0h						; goes to the next line

.data?
	buffer BYTE 12 DUP(?)
	; Required by Bill's print to console function
	written dd ?
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

	; loop going through array1 and coping it into array3
	MOV ecx, LENGTHOF array1	; initialize loop counter (goes through array1)
	MOV esi, 0					; initialize counter for indirect addressing
	AppendArray_copy1:			; begining of the loop
		MOV dh, [eax + esi]							; copie a value of the first array into a registers
		MOV [edi+ esi], dh							; moves the value from the register to array3 in memory
		ADD esi, (SIZEOF array1/LENGTHOF array1)	; goes to the next element in the array
		LOOP AppendArray_copy1						; goes to the begining of the loop (dec ecx)

	; loop going through array2 and coping it into array3
	MOV ecx, LENGTHOF array2	; initialize loop counter (goes through array2)
	MOV esi, 0					; initialize counter for indirect addressing
	AppendArray_copy2:			; begining of the loop
		MOV dh, [eax + esi]							; copie a value of the first array into a registers
		MOV [edi+ esi + LENGTHOF array1], dh		; moves the value from the register to array3
		ADD esi, (SIZEOF array1/LENGTHOF array1)	; goes to the next element in the array
		LOOP AppendArray_copy2						; goes to the begining of the loop (dec ecx)

	; restor the registers
	POP edx
	POP esi
	POP ecx

	ret
	AppendArray ENDP ; end of the AppendArray procedure
;--------------------------------------------------------------------------------------------------------


;--------------------------------------------------------------------------------------------------------
; Bill's code
DisplayText proc
;Assumes that the message variable and the msg_size variable have been set
    invoke GetStdHandle, getstdout
    invoke WriteFile,		\
           eax,				\
           OFFSET message,	\
           msg_size,		\
           OFFSET written,	\
           0
	ret
DisplayText endp
;------------------------------------------------------------------------------------------


;------------------------------------------------------------------------------------------
DisplayNL PROC
;		Assumes that the szFL has been defined in the .data section;
;		szLF  BYTE  0dh, 0ah, 0h
;------------------------------------------------------------------------------------------
    invoke GetStdHandle, getstdout
    invoke WriteFile,		\
           eax,				\
           OFFSET szLF,		\
           3,				\
           OFFSET written,	\
           0
	ret
DisplayNL ENDP
;-------------------------------------------------------------------------------------------------


;-----------------------------------------------------------------------
ConvertIntToAscii:
;	Converts an unsigned integer to ascii and prints it. Max 16-bits value
;	Receives:	the value to be converted into ax (a 16-bit value)
;				the address to store the new value into esi
;	Returns:	The new ascii value into the address pointed byt esi
; 	Requires:	
;-----------------------------------------------------------------------
	; saves the value of ecx, ebx, and edi
	PUSH ebx
	PUSH ecx
	PUSH edx
	PUSH edi
	push esi
	
	MOV	edx, 0h						; clear edx
	MOV	bl, 0ah						; get ready to divide by 10
	MOV	edi, 15						; initialize position counter

	ContConvert:
		div	bl						; divide. Remainder will be in ah, result in al					PROGRAM RECEIVED DIGNAL SIGFPE, Arthimetic exception
		add ah, 30h					; add 48 (30h) to ah to convert to ascii
		
		MOV [esi+edi], ah			; move the ascii value into the inidicated storage address+edi
		
		MOV ah, 0h					; clears out ah - no longger needed
		CMP al, 0h					; Check al and make sur it is not zero
		JE PrintResult				; Leave the loop if al is 0
									; Else continue
		DEC edi						; Decrement the number of elements
		
	jmp ContConvert					; Loop again

	PrintResult:					; Print the result
		
		MOV edx, 16
		SUB edx, edi				; The length of the data to print is moved to edx (required by the DisplayText function)
		MOV msg_size, edx
		
				
		MOV ecx, esi				; The address of the data to print is moved to ecx (required by the DisplayText function)
		ADD ecx, edi
		MOV esi, ecx
		;MOV bx, WORD PTR[ecx]
		;MOV WORD PTR message, bx
		;CALL DisplayText
		
	; restors the value of ecx, ebx, and edi
	POP esi
	POP edi
	POP edx
	POP ecx
	POP ebx
	
	RET								;Exit the procedue
;end of ConvertIntToAscii
;------------------------------------------------------------------------------------------------

main PROC
	nop
	PUSHAD
	;Start of User Code
	

	; get ready to call the AppendArray procedure
	MOV eax, OFFSET array1
	MOV ebx, OFFSET array2
	MOV edi, OFFSET array3
	; appends array2 to array1 and stor them into array3
	CALL AppendArray

	MOV esi, OFFSET message 
	MOVZX ax, [array3]
	CALL ConvertIntToAscii
	

	;End of User Code
	POPAD
	nop
	invoke ExitProcess, 0

main endp
end main