
INCLUDE Irvine32.inc 
INCLUDE macros.inc 
BUFFER_SIZE = 5000
BUFFER_SIZE2 = 20

.data 

;------------------------
; Data for Time :
sysTime SYSTEMTIME <>
XYPos COORD <10,5>
consoleHandle DWORD ?
;------------------------
;All Boards:
Initialboard Byte 81 DUP(?) 
CurrentBoard Byte 81 Dup(?)   
FinalBoard Byte 81 DUP(?)
boolboard Byte 81 DUP (?)
MatrixDetails byte 6 dup(?)
ColorBoard byte 81 Dup(?)
;-------------------------
;Counters:
;Game stats counters

myCounter dword 0
wrongCounter byte 0
correctCounter byte 0
remainingCellsCount byte 0
;------------------------------
; some val 
xRow Byte ?		; we use it in edit cell
yCol word ?     ; we use it in edit cell
numInput Byte ?   
LevelKind Byte ?	;1 Easy, 2 Medium, 3 Hard
;-----------------------
;for time
Hour Byte ?     
Minute Byte ?    
Hour2 Byte ?
Minute2 Byte ?
RemainHour Byte ? 
RemainMinute Byte ?
BoolVal Byte ?

;-------------------------------------
;Data files paths

fileName Byte "sudoku_boards/diff_?_?.txt",0 ; 19    21
solvedFileName Byte "sudoku_boards/diff_?_?_solved.txt",0
lastGameInitialFile Byte "sudoku_boards/Pervious_Game/Initialboard.txt",0
lastGameFinalFile Byte "sudoku_boards/Pervious_Game/FinalBoard.txt",0
lastGameBoolFile Byte "sudoku_boards/Pervious_Game/BoolBoard.txt",0
lastGameDetailsFile Byte "sudoku_boards/Pervious_Game/PlayerDetail.txt",0
lastGameCurrentFile Byte "sudoku_boards/Pervious_Game/CurrentBoard.txt",0
;--------------------------------
;Variables for reading from file


buffer Byte BUFFER_SIZE DUP(?)
fileHandle HANDLE ?
bytesWritten DWORD ?
str1 BYTE "Cannot create file",0dh,0ah,0
str2 BYTE "Bytes written to file :",0
bufferDetails Byte BUFFER_SIZE2 DUP(?)
newline byte 0Dh,0Ah
;-----------------------------------------
.code
;/////////////////////////////////////  

;-----------------------------------------------------------------------
; Read the Details from the file and put them in MatrixDetails         ;
; edx contains the offset of the details file                          ;
; esi contains the offset of the status of the user                    ;
; ecx contains the size of the file                                    ;
;-----------------------------------------------------------------------
ReadDetails proc 

mov edx , offset lastGameDetailsFile
mov esi , offset MatrixDetails
mov ecx , sizeof lastGameDetailsFile
call openinputfile
mov fileHandle , eax 
cmp eax , INVALID_HANDLE_VALUE
jne File_ok 
mwrite"Not OPen File"
jmp Quit
File_ok:
mov edx, offset bufferDetails
mov ecx , BUFFER_SIZE2 
call readfromfile
jnc CheckBufferSize 
mwrite"Error Read"
call writewindowsmsg
jmp close_file
CheckBufferSize :
cmp  eax ,BUFFER_SIZE2 
jb BufferSizeISOk
mwrite"Error Buffer small "
jmp Quit 
BufferSizeISOk:
mov buffer[eax] , 0
mov edx , offset bufferDetails 
mov ecx , 6
llll1:
  
   mov al , [edx] 
   sub  al, 48 
   mov [esi] , al
   inc edx  
   inc esi   
   newline1: 
   
loop llll1
close_file: 
mov eax  , fileHandle 
call closefile 

;-------- 

mov edi , offset MatrixDetails 

mov bl , [edi]  
mov correctCounter , bl
inc edi

mov bl , [edi]
mov wrongCounter  , bl
inc edi

mov bl , [edi]
mov remainingCellsCount , bl
inc edi

mov bl , [edi]
mov RemainHour, bl
inc edi 

mov bl , [edi]
mov RemainMinute , bl
inc edi 


 
call crlf 

Quit:

ret
ReadDetails endp 
;------------------------------------------------------------
; here we full the matrix with the counters and time         ;
; and we use it when we write status in the file             ;
; edx contains the offset of MatrixDetails                   ;
;                                                            ;
;                                                            ;
;-------------------------------------------------------------
FullMatrixDetails Proc 
    
	mov edx, offset MatrixDetails

	mov bl  , correctCounter
	mov  byte ptr [edx] , bl
	inc edx 

	mov bl  , wrongCounter
	mov  byte ptr [edx] , bl
	inc edx 

	mov bl  , remainingCellsCount
    mov  byte ptr [edx] , bl
	inc edx 

	push edx
    call GetCurrentTime
    mov bl , Hour   
	cmp bl , Hour2 
	pop edx 
	ja isgreaterH 
	jb islowH
	je IsEqualH
	
	IsEqualH:
	
	mov bl , 0 
	mov  byte ptr [edx] , bl
	inc edx 
	jmp doneHour
	
	isgreaterH: 
    sub bl , Hour2 			
    mov  byte ptr [edx] , bl
	inc edx 
	jmp doneHour
	
	islowH:
	mov bl , Hour2
	sub bl , Hour 
	mov  byte ptr [edx] , bl
	inc edx 

	 doneHour: 

	mov bl , Minute 
	cmp bl , Minute2 
	ja isgreaterM 
	jb islowM
	je IsEqualM
	
	IsEqualM:
	mov bl , 0
	 mov  byte ptr [edx] , bl
	 inc edx 
	 jmp doneMinute
	
	isgreaterM: 
    sub bl , Minute2 			
    mov  byte ptr [edx] , bl
	inc edx 
	jmp doneMinute
	
	islowM:
	mov bl , Minute2
	sub bl , Minute 
	mov  byte ptr [edx] , bl
	inc edx 
 
 
 doneMinute:

 
 
 
ret 
FullMatrixDetails endp
;------------------------------------------------------------
; here we write the matrix details in the file but we       ;
; call FullMatrixDetails to full the details in the matrix, ;
; like we reuse the values                                  ;
; edx contains the offset MatrixDetails                     ;
; ebx contains the offset  details file                     ;
;                                                           ;
;-----------------------------------------------------------;

WriteMatrixDetails proc 
call FullMatrixDetails

    mov edx , offset MatrixDetails  ; Matrix
	mov ebx, offset  lastGameDetailsFile 

	PUSH EDX
	;Convert all Numbers of the array to chars to be written in the file
	 MOV ECX,6		 ; Move number of board elements to ECX
	 lk1:
	 MOV EAX,48
	 add [EDX],al
	 INC EDX
	 loop lk1

	 ;Create a new text file and error check.
	 MOV EDX,EBX	;Move file name offset to EDX for CreatOutputFile
	 CALL CreateOutputFile
	 MOV fileHandle,EAX
	 ; Check for errors.
	 CMP EAX, INVALID_HANDLE_VALUE 
	 ; error found? 
	 JNE file_ok	; no: skip
	 MOV EDX,OFFSET str1
     CALL WriteString
	 JMP quit 
	 file_ok:  

     POP EDX     ;contains the address of the array 
     MOV ECX,6	 ;Len of arr

   l5:
	   ;here we write charachter in the file
	   MOV EAX,fileHandle
	   PUSH EDX		 ;Push current character address
	   PUSH ECX		 ;Push the loop iterator
	   MOV ECX,1
	   CALL WriteToFile
	   POP ECX          ; pop the ecx contains the couter

	   POP EDX  ;return the address of the read char
	   INC EDX  ;staging for writing next char
   loop l5

   quit:


ret 
WriteMatrixDetails  endp
;--------------------------------------------------------------;
; write any board to the file                                  ;
; val1 take the offset of the board, and mov it in edx         ;
; val2 take the offset of the file name we need to write on it,;
; mov it in ebx                                                ;
; -------------------------------------------------------------;

WriteBoardToFile PROC, val1:Dword, val2:Dword

	push eax

	mov edx, val1   ; Matrix
	mov ebx, val2

	pop eax

	PUSH EDX
	;Convert all Numbers of the array to chars to be written in the file
	 MOV ECX,81		 ; Move number of board elements to ECX
	 lk2:
	 MOV EAX,48
	 add [EDX],al
	 INC EDX
	 loop lk2

	 ; Create a new text file and error check.
	 MOV EDX,EBX	;Move file name offset to EDX for CreatOutputFile
	 CALL CreateOutputFile
	 MOV fileHandle,EAX
	 ; Check for errors.
	 CMP EAX, INVALID_HANDLE_VALUE 
	 ; error found? 
	 JNE file_ok	; no: skip
	 MOV EDX,OFFSET str1
	 ; display error 
	 CALL WriteString
	 JMP quit 
	 file_ok:  

;Writing in the file
   POP EDX		;contains the address of the array 
   MOV ECX,81	;Len of arr

   l5:
	   ;write charachter in the file
	   MOV EAX,fileHandle
	   PUSH EDX		 ;Push current character address
	   PUSH ECX		 ;Push the loop iterator
	   MOV ECX,1
	   CALL WriteToFile
	   POP ECX

	   ;check if a new line should be printed or not
			MOV dx,0
			DEC ECX
			MOV AX,CX     ;dx = CX-1 % 9
 			MOV BX,9
			div BX

			CMP dx,0 ; if not div by 9 , then no newline required.
			JNE noEndl

			PUSH ECX
			 MOV EAX,fileHandle
			 MOV ECX,lengthof newline
			 MOV EDX,offset newline
			 CALL WriteToFile
	        	POP ECX
	
		noEndl:
	   INC ECX  ;as it was decremented above for calculating modulus
	   POP EDX  ;return the address of the read char
	   INC EDX  ;staging for writing next char
   loop l5

   quit:
         
	ret
WriteBoardToFile ENDP
 
;-----------------------------------------------------------------
; here we read any board from the specific file and but it array ;
; to use while game is run.                                      ;
; esi contains the offset of the array                           ;
; ebx contains the offset of the file name                       ;
; and we take it Depends on user's choice                        ;
;----------------------------------------------------------------;

ReadArray PROC, arrayOffset:Dword, fileNameOffset:Dword
	push eax 
	;Setting ECX with the max string size
	mov esi, arrayOffset
	mov ebx, fileNameOffset


	;Open the file for input
	mov edx,ebx
	call OpenInputFile
	mov fileHandle, eax

	;Check for reading from file errors
	cmp eax, INVALID_HANDLE_VALUE	
	jne FileHandleIsOk	
	mWrite <"Cannot open file", 0dh, 0ah>
	jmp quit

	FileHandleIsOk :
		; Read the file into a buffer
		mov edx, OFFSET buffer
		mov ecx, BUFFER_SIZE
		call ReadFromFile
		jnc CheckBufferSize	;if carry flag =0 then size of the buffer is ok
		mWrite "Error reading file. "	
		call WriteWindowsMsg
		jmp CloseFilee

	CheckBufferSize	 :
	    
		;Check if buffer is large enough
		cmp eax, BUFFER_SIZE	
		jb BufferSizeOk
		mWrite <"Error: Buffer too small for the file", 0dh, 0ah>
		jmp quit

BufferSizeOk :
	;Insert null terminator
	mov buffer[eax], 0

	mov ebx, OFFSET buffer
	mov ecx, 97
	;store the offset of the array in edx to reuse it
	mov edx,esi

	StoreContentInTheArray :
		  mov al, [ebx]
		  inc ebx
		  
		  cmp al, 13  ; 13 and 10 means  endline
		  je SkipBecOfEndl
		  cmp al, 10
		  je SkipBecOfEndl
		  mov [esi], al
		  inc esi
		 SkipBecOfEndl :

	loop StoreContentInTheArray


	mov esi, edx
	;store the offset of the array in edx to reuse it
	
	mov ecx, 81
	push eax 
	push edx 
	
   ConvertFromCharToInt:
          
		  sub byte ptr[esi],48    ; casting bytes to int 
	      ;mov al  , byte ptr[esi]
		  ;call writedec
		  ;call crlf
		  inc esi 
	loop ConvertFromCharToInt
	pop edx 
	pop eax 

	;Return the offset of the filled array in esi
	 mov esi, edx

CloseFilee :
	mov eax, fileHandle
	call CloseFile

	quit :


	ret
ReadArray ENDP
;------------------------------------------------------------;
; here we full the matrix and the file with boolean number's ;
; if not 0 it means we can not edit this cell other wise we  ;
; can edit.                                                  ;
; esi contains the offset of the inital board                ;
; edx contains the offset of the boolBoard                   ;
; ecx contains the counter loop                              ;
;------------------------------------------------------------;
FullBoolBoard proc 

mov esi ,  offset Initialboard
mov edx, offset boolboard
mov ecx , 81

Boolloop:

     mov al , [esi]
	 cmp al , 0
	 jne ISOne
	 mov byte ptr[edx] ,  0
	 inc remainingCellsCount
	 jmp EndBool
	 IsOne:
	 mov byte ptr[edx] ,1
	 
	 EndBool:
	 
	 inc edx
	 inc esi
	 inc ebx

loop Boolloop 

ret 
FullBoolBoard endp

;-------------------------------------------------------------;
; here we toke the level number as a parameter depend's on    ;
; the user choice and Randomize the sub level.                ;
; we call read array 3 times to upload the 3 board's, and     ;
; upload with them the bool board.                            ;   
;-------------------------------------------------------------;

Getboard proc levelnum:byte
 
 call Randomize
 mov eax , 3
 call RandomRange 
 inc eax 
 
 add al , '0'
 mov dl , levelnum
 add dl , '0'
 mov fileName[19] ,dl
 mov fileName[21] ,al

 mov solvedFileName[19] , dl
 mov solvedFileName[21] , al

 Invoke ReadArray, offset Initialboard, offset filename  
 Invoke ReadArray, offset CurrentBoard, offset filename  
 Invoke ReadArray, offset FinalBoard, offset solvedFileName 
 call FullBoolBoard
 
ret 
Getboard  endp
;-------------------------------------------------------------;
; here we need to clear the current matrix to initial matrix  ;
; edi contains the offset of the Initial board                ; 
; esi contains the offset of the Current board                ;
; ecx contains the counter loop                               ;
;-------------------------------------------------------------;
AssignCurrentBoardtoInitial proc 
 
 mov edi , offset Initialboard    
 mov esi , offset CurrentBoard 
 mov ecx  , 81 
 Clearloop:
 
     mov bl , [edi]
	 mov [esi] , bl 
	 inc edi 
	 inc esi 
 loop Clearloop

 mov correctCounter , 0
 mov wrongCounter , 0
 mov remainingCellsCount  , 0
 mov Minute , 0 
 mov Hour , 0
 call FullBoolBoard
ret 
AssignCurrentBoardtoInitial endp


;-----------------------------------------------------------;
; here we print the current matrix every time i uploaded it ;
; edx contains offset of the current board                  ;
; we use mycounter to Compare it by 10 or 13 for new line   ;
;                                                           ;
;                                                           ;
;-----------------------------------------------------------;
PrintCurrentmatrix proc
mov ecx , 81 

mov edx , offset CurrentBoard
mov mycounter , 0
mov ebx , 9
print:
     
     inc mycounter 
	 mov al , [edx]
	 call writedec
	 cmp al , 0
	 jne NotCounterCell
	   
	 NotCounterCell:
	 inc edx
	  
	 cmp myCounter , ebx
		 je newline1 
		 jmp con
		  newline1:
		  call crlf
		  add ebx , 9
		   
         con:	

loop print

 
ret 
PrintCurrentmatrix endp
;----------------------------------------------------------;
; we just print the final matrix from the file             ;
; edx contains the offset Final Board                      ;
; ecx contains the numbers of cell's in the SUDOKU BOARD   ;
; we use mycounter to Compare it by 10 or 13 for new line  ;
;                                                          ;
;----------------------------------------------------------;
PrintFinalMatrix proc

mov ecx , 81 
mov edx , offset FinalBoard
mov esi , offset CurrentBoard
mov edi , offset boolBoard
mov mycounter , 0
mov ebx , 9
;mov eax,14 ; yellow
;call SetTextColor
;mwrite"Congratulation Correct Value"
;mov eax , lightgray+(black*16)
;call SetTextColor
;pop eax

print:
     
     cmp byte ptr[edi] , 1
	 je ThisMainCell
	 jmp cheack
	 ThisMainCell:
	 mov eax,15 ; White Color
     call SetTextColor
	 push eax

	 jmp Enterr

	 cheack:
	 push ebx
	 mov bl , byte ptr [esi]
	 cmp  [edx],  bl
	 jne Solvedd
	 jmp OKK

	 Solvedd:
	 pop ebx
	 mov eax,4 ; Red Color
     call SetTextColor
	 push eax
	 jmp Enterr

	 OKK:
	 pop ebx
	 mov eax,2 ; Green 
     call SetTextColor
	 push eax

	 Enterr:
	 ;------------------------------------
     inc mycounter 
	 pop eax
     mov al , [edx]
	 inc edx 
	 call writedec 
	 cmp myCounter , ebx
		 je newline1 
		 jmp con
		  newline1:
		  call crlf
		  add ebx , 9
		   
         con:	
		 inc edi
		 inc esi
loop print

; here we get the main color in the console

mov eax , lightgray+(black*16)
  call SetTextColor

ret 
PrintFinalMatrix endp

;------------------------------------------------------------;
; here we can edit the specific cell enter the row and colum ;
; and the value by the user, we made our cheak               ;
; edx contains offset the Current Board                      ;
; esi contains Final Board                                   ;              
;                                                            ;
;------------------------------------------------------------;
EditCell proc , row:byte , col:word , val:byte
mov edx , offset CurrentBoard 
mov esi , offset FinalBoard

push ebx
push edx 

;////rowcheck
cmp row ,9
ja Notvalid
cmp row , 1
jb Notvalid

;////colcheck
cmp col ,9
ja Notvalid
cmp col , 1
jb Notvalid

;////valueCheck
cmp val ,9
ja Notvalid
cmp val , 1
jb Notvalid 
;///Get Index
 
mov al , row 
dec al
mov di , 9
mul di 
add ax ,col
dec ax 
pop edx
mov edi , offset boolboard
cmp byte ptr[edi+eax] ,1
je ThisinitialCell

mov bl , [esi+eax]
cmp  bl , val
pop ebx
jne IncorrectValue
jmp CorrectValue

 CorrectValue:
    push eax
	mov eax,2 ; white on blue
    call SetTextColor
    mwrite"Congratulation Correct Value"
    mov eax , lightgray+(black*16)
    call SetTextColor
	pop eax 
	;mov byte ptr [ebx+eax] , 2
	mov cl , val
	mov  [edx+eax] , cl
	inc correctCounter
	dec remainingCellsCount
	jmp ExitFunction1


 IncorrectValue:
    push eax
	mov eax,4 ; white on blue
    call SetTextColor
    mwrite"Opps The Value Incorrect !!"
    mov eax , lightgray+(black*16)
    call SetTextColor
	pop eax
	 ;mov byte ptr [ebx+eax] , 4
	 mov cl , val
	 mov byte ptr [edx+eax] ,cl
	 inc wrongCounter 
	 jmp ExitFunction1

Notvalid :
     mwrite"Inavlid Input for Row or Col Or Value about Range"
	 jmp ExitFunction1
	 ThisinitialCell:
	 mWrite"you don't allow to edit this cell "
ExitFunction1:

ret
EditCell endp
;------------------------------------------------------------;
; here we set the local time in the 2 values hour and minute ;
; this proc we put it from the book like a build in function ;
; sysTime.wHour and sysTime.wMinute recives in eax           ;
;                                                            ;
;-------------------------------------------------------------
;///////////////////////////////////////////////////////  SetTime
SetTime proc  
INVOKE SetConsoleCursorPosition, consoleHandle, XYPos
INVOKE GetLocalTime, ADDR sysTime
; Display the system time (hh:mm:ss).
movzx eax,sysTime.wHour ; hours
mov Hour , al
movzx eax,sysTime.wMinute ; minutes
mov Minute , al 
ret 
SetTime Endp

;------------------------------------------------------------------;
; here we get the final time and we use the buth proc to calculate ;
; the all time by sub hour , hour2 and Minute ,Minute2             ;
; sysTime.wHour and sysTime.wMinute recives in eax                 ;
;                                                                  ;
;------------------------------------------------------------------;

GetCurrentTime proc  
INVOKE SetConsoleCursorPosition, consoleHandle, XYPos
INVOKE GetLocalTime, ADDR sysTime
; Display the system time (hh:mm:ss).
movzx eax,sysTime.wHour ; hours
mov Hour2 , al
movzx eax,sysTime.wMinute ; minutes
mov Minute2 , al 
ret 
GetCurrentTime Endp
;--------------------------------------------------------------;
; here we calculate the all time when the user enter the game  ;
; until the user decide to exite the game                      ;
;                                                              ;
;                                                              ;
;                                                              ;
;----------------------------------------------------------------
GetFinalTime proc   

push eax 
mov eax , 1500
call Delay

call clrscr
pop eax

mwrite "Final Sudoko Board is : "
call crlf
call PrintFinalMatrix 

mwrite"You Solve Susdoko Successfully" 
call crlf
mwrite"The Number Of Times Entered An Incorrect Solution :  "
movzx eax , wrongCounter  
call writedec
call crlf 
mwrite"The Number Of Times Entered An Correct Solution :  "
movzx eax , CorrectCounter
call writedec  
call crlf 

call GetCurrentTime


mov al , Hour 
cmp al , Hour2 
je ISEqualNewH
jb ISbelowNewH
ja ISaboveNewH

ISEqualNewH :
mov al , 0
Jmp PrintTime

ISbelowNewH:
mov al , Hour2 
sub al , Hour 
Jmp PrintTime

ISaboveNewH:
sub al , Hour2 
Jmp PrintTime

DoneH: 

mov al , Minute 
cmp al , Minute2 
je ISEqualNewM
jb ISbelowNewM
ja ISaboveNewM

ISEqualNewM :
mov al , 0
Jmp P

ISbelowNewM:
mov al , Minute2 
sub al , Minute
Jmp P

ISaboveNewM:
sub al , Minute2
Jmp P




PrintTime:
mwrite"Time For Playing is : "
mov Hour  , al 
jmp DoneH
P: 
mov Minute , al 

cmp boolVal , 1 
je IS_P
jne IS_N
IS_P:
mov al , Hour 
add al , RemainHour 
call writedec 
mwrite":"
mov al , Minute
add al , RemainMinute
call writedec 
jmp FinalDone 

IS_N:
mov al , Hour 
call writedec 
mwrite":"
mov al , Minute
call writedec 

FinalDone:
call crlf 

ret 
GetFinalTime endp


;###############################################################################
;#                            our main                                         #
;# this program writen by our FCiS ASu studens:                                #
;#                                          1-Abdallah Hossam Abdallah         #
;#                                          2-Saad ibrahim                     #
;#                                          3-Abdelmoniem Ahmed                #
;#                                          4-Mohamed Taher                    #
;#                                          5-Omnia Abdelrahman                #
;###############################################################################
main PROC

  mwrite"Welcome with Sudoko Game"
  call crlf
  mwrite "New Game Enter n Or Enter p for Perious Game" 
  call crlf
  mwrite"Enter your Select: "
  call readchar
  call writechar
  call crlf 
  cmp al , 'n'
  je NewGame
  cmp al ,'p'
  je PeriousGame
  jmp InvalidInput
  
  PeriousGame:
  Invoke ReadArray, offset Initialboard, offset lastGameInitialFile 
  Invoke ReadArray, offset FinalBoard, offset lastGameFinalFile 
  Invoke ReadArray, offset boolboard, offset lastGameBoolFile
  Invoke ReadArray, offset CurrentBoard, offset lastGameCurrentFile 
  call ReadDetails
  mov BoolVal , 1 
  call SetTime
  jmp WhileGame

  NewGame:
  mwrite"Please Enter Level of your Game: "
  call readdec 
  ;mov FileNum , al
  Invoke Getboard , al
  call SetTime
  mov BoolVal , 0
  jmp WhileGame

  InvalidInput:
  mwrite"Your input Not n or p"
  jmp ExitFromConsole
 
  ClearCurrentBoardToInitailBoardOption:
  call crlf 
  call  AssignCurrentBoardtoInitial
  call crlf  
  push eax
  mov eax , 1500
  call Delay
  pop eax  
  call clrscr  
WhileGame:

  cmp  remainingCellsCount , 0
  je FinishedGame
  ;---------------------------------------------------------------------------;
  ; for five wrong answers for just test                                      ;
  ;---------------------------------------------------------------------------;
  cmp  wrongCounter ,5 
  je  EndWhileGame
 
  call PrintCurrentmatrix
  call crlf 

 mwrite "1:Print the Finished Board "
 call crlf
 mwrite "2:Clear The Board To The Initial Sudoku Board "
 call crlf 
 mwrite "3:Edit a Single Cell In The Board " 
 call crlf 
 mwrite "4:Save and exit "
 call crlf 
 mwrite"Enter your Option : " 
 call readdec 
 cmp eax , 1
 je PrintFinishMatrixAndExitOption
 cmp eax , 2 
 je ClearCurrentBoardToInitailBoardOption
 cmp eax , 3 
 je EditCellOption
 cmp eax , 4 
 je SaveAndExit 
 Jmp WrongInput

EditCellOption : 
mwrite "Enter Row: "
call readdec 
mov xRow , al 

mwrite "Enter Col: "
call readdec 
mov yCol , ax 

mwrite "Enter Value: "
call readdec 
 
mov numInput , al 
invoke EditCell ,xRow , yCol , numInput 
;---------------------------------------------------------------;
; for delay the screen                                          ;
;---------------------------------------------------------------;
push eax
mov eax , 1500
call Delay
pop eax  
call clrscr


jmp WhileGame

SaveAndExit:
;---------------------------------------------------------------;
; for delay the screen                                          ;
;---------------------------------------------------------------;
mov eax , 1500
call Delay
call clrscr

Invoke WriteBoardToFile , offset Initialboard, offset lastGameInitialFile 
Invoke WriteBoardToFile , offset FinalBoard,   offset lastGameFinalFile
Invoke WriteBoardToFile , offset boolboard,    offset lastGameBoolFile 
Invoke WriteBoardToFile , offset CurrentBoard, offset lastGameCurrentFile 
call WriteMatrixDetails
Mwrite"congratulations, Saved Successfully !! "


Jmp ExitFromConsole
 
EndWhileGame:
mwrite"Your are failure"
jmp ExitFromConsole


FinishedGame:

 
mwrite"You Solve Susdoko Successfully" 
call crlf
mwrite"The Number Of Times Entered An Incorrect Solution :  "
movzx eax , wrongCounter  
call writedec
call crlf 
mwrite"The Number Of Times Entered An Correct Solution :  "
movzx eax , CorrectCounter
call writedec  
call crlf 
jmp ExitFromConsole

WrongInput:
mwrite"InValid Option "
jmp ExitFromConsole

PrintFinishMatrixAndExitOption:

call GetFinalTime

ExitFromConsole:

call crlf 

	exit
main ENDP
END main