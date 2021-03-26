	IDEAL

MODEL small
STACK 100h


	GAME_RIGHT_BOUNDARY = 192

	GHOST_WIDTH =  3
	GHOST_HIGHT =  3

	NEXT_POS_ADDED_PIXELS_Y =  GHOST_HIGHT / 2 
	NEXT_POS_ADDED_PIXELS_X =  GHOST_WIDTH / 2 


	;Needed when turn:
	DISTANCE_FROM_BOUNDARY_X = 5; when moving on Y - distance between ghost and boundary	
	DISTANCE_FROM_BOUNDARY_Y = 2; when moving on X - distance between ghost and boundary




DATASEG

	Boolean dw ?

    ;Array will represent boudaries pixels on maze
    boundariesPos_Matrix db 200 dup ()

	currentX dw ? ;First value presents pacman startpoint
	currentY dw ? ;First value presents pacman startpoint

    ;Array (presented as enum) will represent the optional directopn
	direction db 1 dup ('N', 'S', 'W', 'E')

	currentDirection dw ? 

CODESEG
    ORG 100h
start:

	 mov ax, @data
	 mov ds,ax
	 
	 ;start graphic mode
	 mov ax, 13h
	 int 10h
 
	
EXIT:
    
	mov ax, 4C00h ; returns control to dos
  	int 21h
  
END start

;=======================================================
;Proc setPacmanDirection
;Input: 
;By stack:
;- Array adress (specified to baundary pixels) 
;
;- currentX
;- currentY
;
;Output
;- proc will change object current values (X,Y,Direction)
;========================================================

curX equ [currentX]
curY equ [currentY]
boudariesOffset equ [bp + 4]
currentPoint equ [bp - 2]

proc setPacmanDirection

	 push bp
	 mov bp, sp

	sub sp, 2

	 ;push ax

	 ;mov currentPoint, boudariesOffset + 320 * (curY - 1) + curX 
	 mov currentPoint, 320 * (curY - 1) + curX 
	 cmp [currentDirection], 'N'
	 jne Elseif

North:
	
	 cmp (currentPoint - NEXT_POS_ADDED_PIXELS_Y * 320), 1
	 jne continue

	@@TurnWest:
 	 push currentPoint
	 call is_turnLeft 

	 cmp [Boolean], 1
	 jne TurnEast

	 push (-NEXT_POS_ADDED_PIXELS_X) 
	 call setCurrentX
	 
	 jmp Continue

	@@TurnEast:
	 push currentPoint
	 call is_turnRight

	 cmp [Boolean], 1

	 jne TurnSouth

	 push NEXT_POS_ADDED_PIXELS_X
	 call setCurrentX

	 jmp Continue

	@@TurnSouth:

	 push (-NEXT_POS_ADDED_PIXELS_Y)
	 call setCurrentY
	
	 jmp Continue


@@Elseif:
	
	cmp [currentDirection], 'S'
	jne Elseif

South:

 	 cmp (currentPoint + NEXT_POS_ADDED_PIXELS_Y * 320), 1
	 jne continue

	@@TurnWest:
 	 push currentPoint
	 call is_turnLeft 

	 cmp [Boolean], 1
	 jne TurnEast

	 push (-NEXT_POS_ADDED_PIXELS_X) 
	 call setCurrentX

	 jmp Continue

	@@TurnEast:

	 push currentPoint
	 call is_turnRight

	 cmp [Boolean], 1
	 jne TurnNorth

	 push NEXT_POS_ADDED_PIXELS_X
	 call setCurrentX
	 
	 jmp Continue

	@@TurnNorth:

	 push NEXT_POS_ADDED_PIXELS_Y
	 call setCurrentY	 

	 jmp Continue

@@Elseif:

	cmp [currentDirection], 'W'
	jne East

West:

	 cmp (currentPoint - NEXT_POS_ADDED_PIXELS_X), 1
	 jne continue

	@@TurnEast:

	 push currentPoint
	 call is_turnRight

	 cmp [Boolean], 1
	 jne TurnNorth
	 
	 push NEXT_POS_ADDED_PIXELS_X
	 call setCurrentX

	 jmp Continue

	 @@TurnSouth:

	 push currentPoint
	 call is_turnBack

	 cmp [Boolean], 1
	 jne TurnNorth
	 
	 push (-NEXT_POS_ADDED_PIXELS_Y)
	 call setCurrentY

	 jmp Continue

	 @@TurnNorth:

	 push (-NEXT_POS_ADDED_PIXELS_Y)
	 call setCurrentY

	 jmp Continue

East:

	 cmp (currentPoint + NEXT_POS_ADDED_PIXELS_X), 1
	 jne Continue

	@@TurnWest:
 	 push currentPoint
	 call is_turnLeft 

	 cmp [Boolean], 1
	 jne TurnSouth

	 push (-NEXT_POS_ADDED_PIXELS_X) 
	 call setCurrentX

	 jmp Continue

	 @@TurnSouth:

	 push currentPoint
	 call is_turnBack

	 cmp [Boolean], 1
	 jne TurnNorth
	 
	 push (-NEXT_POS_ADDED_PIXELS_Y)
	 call setCurrentY

	 jmp Continue

	 @@TurnNorth:

	 push (-NEXT_POS_ADDED_PIXELS_Y)
	 call setCurrentY


Continue:
	 
	 add sp, 2
	 ;pop ax 
	 pop bp 
	 ret 2

endp setPacmanDirection






;====================================
;Proc setCurrentX
;Input: 
;- value added to next X position
;Output
;- new X value into static integer currentX
;====================================

	xAddition equ [bp + 4]
	nextX equ [bp - 2]

proc setCurrentX

	push bp
	mov bp, sp

	sub sp, 2 ;varible contains X new value
	mov nextX, currentX
	nextX = nextX + xAddition

	cmp nextX, GAME_RIGHT_BOUNDARY
	jb ExitProc

	FixValue:

	nextX  = nextX - GAME_RIGHT_BOUNDARY

	@@ExitProc:

	pop [currentX] ;nextX -> currentX
	pop bp 
	 
	ret

endp setCurrentX
	

;====================================
;Proc setCurrentX
;Input: 
;- value added to next X position
;Output
;- new X value into static integer currentX
;====================================

	yAddition equ [bp + 4]
	nextY equ [bp - 2]

proc setCurrentY

	push bp
	mov bp, sp

	sub sp, 2 ;varible contains X new value
	mov nextY, currentY
	nextY = nextY + yAddition

	pop [currentY] ;nextY -> currentY
	pop bp 
	 
	ret

endp setCurrentY
	
;====================================
;Proc is_turnRight
;Input: 
;-current point (gets from super where calculated)
;Output
;-able or unable to turn [Boolean varible]
;====================================
currentPoint equ [bp + 4] ;pushed

proc is_turnRight

	push bp
	mov bp, sp

	mov [Boolean], 0 ;false

	mov ax,  [currentPoint + DISTANCE_FROM_BOUNDARY_X]
	cmp ax, 1
	jne ExitProc

True:

	mov [Boolean], 1
	
ExitProc:

	;sp points on currentPoint
	;sp sub 2 -> reach bp
	;sp points on ip then pop ip and currentPoint
	sub sp, 2
	pop bp

	ret 2

endp is_turnRight

;====================================
;Proc is_turnLeft
;Input: 
;-current point (gets from super where calculated)
;Output
;-able or unable to turn [Boolean varible]
;====================================
currentPoint equ [bp + 4] ;pushed

proc is_turnLeft

	push bp
	mov bp, sp

	mov [Boolean], 0 ;false

	mov ax,  [currentPoint - DISTANCE_FROM_BOUNDARY_X]
	cmp ax, 1
	jne ExitProc

@@True:

	mov [Boolean], 1
	
@@ExitProc:

	;sp points on currentPoint
	;sp sub 2 -> reach bp
	;sp points on ip then pop ip and currentPoint
	sub sp, 2
	pop bp

	ret 2

endp is_turnLeft

;====================================
;Proc is_turnBack
;Input: 
;-current point (gets from super where calculated)
;Output
;-able or unable to turn [Boolean varible]
;====================================
currentPoint equ [bp + 4] ;pushed

proc is_turnBack

	push bp
	mov bp, sp

	mov [Boolean], 0 ;false

	mov ax,  [currentPoint - DISTANCE_FROM_BOUNDARY_Y * 320]
	cmp ax, 1
	jne ExitProc

@@True:

	mov [Boolean], 1
	
@@ExitProc:

	;sp points on currentPoint
	;sp sub 2 -> reach bp
	;sp points on ip then pop ip and currentPoint
	sub sp, 2
	pop bp

	ret 2

endp is_turnBack

;====================================
;Proc is_turnFront
;Input: 
;-current point (gets from super where calculated)
;Output
;-able or unable to turn [Boolean varible]
;====================================
currentPoint equ [bp + 4] ;pushed

proc is_turnFront

	push bp
	mov bp, sp

	mov [Boolean], 0 ;false

	mov ax,  [currentPoint + DISTANCE_FROM_BOUNDARY_Y * 320]
	cmp ax, 1
	jne ExitProc

@@Tru:

	mov [Boolean], 1
	
@@ExitProc:

	;sp points on currentPoint
	;sp sub 2 -> reach bp
	;sp points on ip then pop ip and currentPoint
	sub sp, 2
	pop bp

	ret 2

endp is_turnFront






;Random ghost new movement direction
;==========================================================================================
; Description  : get RND between any bl and bh includs (max 0 -255)
; Input        : 1. Bl = min (from 0) , BH , Max (till 255)
; 			     2. RndCurrentPos a  word variable,   help to get good rnd number
; 				 	Declre it at DATASEG :  RndCurrentPos dw ,0
;				 3. EndOfCsLbl: is label at the end of the program one line above END start		
; Output:        Al - rnd num from bl to bh  (example 50 - 150)
; More Info:
; 	Bl must be less than Bh 
; 	in order to get good random value again and agin the Code segment size should be 
; 	at least the number of times the procedure called at the same second ... 
; 	for example - if you call to this proc 50 times at the same second  - 
; 	Make sure the cs size is 50 bytes or more 
; 	(if not, make it to be more) 
;==========================================================================================

proc RandomByCs
    push es
	push si
	push di
	
	mov ax, 40h
	mov	es, ax
	
	sub bh,bl  ; we will make rnd number between 0 to the delta between bl and bh
			   ; Now bh holds only the delta
	cmp bh,0
	jz @@ExitP
 
	mov di, [word RndCurrentPos]
	call MakeMask ; will put in si the right mask according the delta (bh) (example for 28 will put 31)
	
RandLoop: ;  generate random number 
	mov ax, [es:06ch] ; read timer counter
	mov ah, [byte cs:di] ; read one byte from memory (from semi random byte at cs)
	xor al, ah ; xor memory and counter
	
	; Now inc di in order to get a different number next time
	inc di
	cmp di,(EndOfCsLbl - start - 1)
	jb @@Continue
	mov di, offset start
@@Continue:
	mov [word RndCurrentPos], di
	
	and ax, si ; filter result between 0 and si (the mask)
	cmp al,bh    ;do again if  above the delta
	ja RandLoop
	
	add al,bl  ; add the lower limit to the rnd num
		 
@@ExitP:	
	pop di
	pop si
	pop es
	ret
endp RandomByCs