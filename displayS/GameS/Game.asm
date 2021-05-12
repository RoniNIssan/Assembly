	IDEAL
MODEL small
STACK 100h


FILENAME_MAZE_DISPLAY equ 'Maze.bmp'
FILENAME_PACMAN_NORTH equ 'PN.bmp'
FILENAME_PACMAN_SOUTH equ 'PS.bmp'
FILENAME_PACMAN_EAST equ 'PE.bmp'
FILENAME_PACMAN_WEST equ 'PW.bmp'

;Maze
;FILE_ROWS_MAZE = 200
;FILE_COLS_MAZE = 191
FILE_ROWS_MAZE = 200
FILE_COLS_MAZE = 176

;Maze colors
DOTS_COLOR = 14
BLOCK_COLOR = 15
BOUNDARIES_COLOR = 1
BACKGROUND_COLOR = 0

;Pacman figure
FILE_ROWS_PACMAN = 9
FILE_COLS_PACMAN = 9

;Quit Banner values
QUIT_RIGHT_COL = 34
QUIT_LEFT_COL = 10
QUIT_TOP_ROW = 6
QUIT_BOTTOM_ROW = 31

;Pacman values
MAZE_RIGHT_BOUNDARY_X = 167
MAZE_LEFT_BOUNDARY_X = 13

NEXT_POS_ADDED_PIXELS_Y = 7
NEXT_POS_ADDED_PIXELS_X = 7

PACMAN_MIDDLE_X_PIXLE_WEST = (FILE_COLS_PACMAN - 1) / 2 + 1
PACMAN_MIDDLE_Y_PIXLE_WEST = (FILE_ROWS_PACMAN - 1) / 2 + 1

;Needed when turn:
DISTANCE_FROM_BOUNDARY_X = 2; when moving on Y - distanc1e between ghost and boundary
DISTANCE_FROM_BOUNDARY_Y = 2; when moving on X - distance between ghost and boundary

;----- Equates Timeer
ticks		EQU	18
BIOSData	EQU	040h
LowTimer	EQU	006Ch
PIC8259		EQU	0020h
EOI		    EQU	0020h

DATASEG


		Filename_Maze db FILENAME_MAZE_DISPLAY, 0
		Filename_PacmanNorth db FILENAME_PACMAN_NORTH, 0
		Filename_PacmanSouth db FILENAME_PACMAN_SOUTH, 0
		Filename_PacmanEast db FILENAME_PACMAN_EAST, 0
		Filename_PacmanWest db FILENAME_PACMAN_WEST, 0
		ScrLine db FILE_COLS_MAZE dup (0)  ; One Color line read buffer

		FileHandle	dw ?
		Header 	    db 54 dup(0)
		Palette 	db 4001h dup (0)

		BmpFileErrorMsg    	db 'Error At Opening Bmp File ',FILE_COLS_PACMAN, 0dh, 0ah,'$'
		ErrorFile           db 0

		BmpLeft dw ?
		BmpTop dw ?
		BmpColSize dw ?
		BmpRowSize dw ?

		matrix dw ?

    ;Current Position
		pacmanX dw 86
    pacmanY dw 146


        currentPoint dw ?
		pacmanCurrentDirection dw 'A'

		;Boolean
		Bool db 0

		isDirectionChanged dw 0

		;Timer
		exitCode1	db	0
		timerSeg	dw	?
		timerOfs	dw	?

pacmanBlank db	0,0,0,0,0,0,0
            db	0,0,0,0,0,0,0
            db	0,0,0,0,0,0,0
            db	0,0,0,0,0,0,0
            db	0,0,0,0,0,0,0
            db	0,0,0,0,0,0,0
            db	0,0,0,0,0,0,0


CODESEG

    ORG 100h

start:
		mov	ax,@data
		mov	ds,ax
		mov	es,ax
		mov	[word cs:difference],ticks


;========TEXT MODE========
;		Openning Scrren

	 call stratGraphicMode
   call StratScreen

   call setPacmanCurrentPoint

	 ; save the current interrupt verctor.
	 ; the timer interrupt number is 1C
 push	es
 mov	ax, 351Ch
 int	21h
 mov	[timerSeg],es
 mov	[timerOfs],bx
 pop	es

	 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	 ; set the new inerrupt vector with our function
 push	ds
 mov	ax,251Ch
 push	cs
 pop	ds
 mov	dx, offset PrintSecondsElapse
 int	21h
 pop	ds

	 push [pacmanCurrentDirection]
	 push [pacmanX]
	 push [pacmanY]
	 call PacmanFigureDisplay



MainLoop:

	 mov ah, 0
	 int 16h

     cmp al, 'W'
     je North
     cmp al, 'w'
     je North

     cmp al, 'S'
     je South
     cmp al, 's'
     je South

     cmp al, 'D'
     je EastShortcut
     cmp al, 'd'
     je EastShortcut

     cmp al, 'A'
     je WestShortcut
     cmp al, 'a'
     je WestShortcut

		 push	ds
		 mov	ax,251Ch
		 mov	dx,[timerOfs]
		 mov	ds,[timerSeg]
		 int	21h
		 pop	ds

     jne MainLoop

North:

	 push [pacmanX]
	 push [pacmanY]
     call removePacman

	 mov [pacmanCurrentDirection], 'W'

	 push [pacmanY]
	 push [pacmanX]
	 call FindNextAddedY_North

	 pop [pacmanY]

	 push [pacmanCurrentDirection]
	 push [pacmanX]
	 push [pacmanY]
	 call PacmanFigureDisplay

     jmp MainLoop

WestShortcut:
     jmp West

EastShortcut:
     jmp East
South:

	 push [pacmanX]
	 push [pacmanY]
     call removePacman

	 mov [pacmanCurrentDirection], 'S'

	 push [pacmanY]
	 push [pacmanX]
	 call FindNextAddedY_South

	 pop [pacmanY]

	 push [pacmanCurrentDirection]
	 push [pacmanX]
	 push [pacmanY]
	 call PacmanFigureDisplay

MainLoopShortcut:
     jmp MainLoop

East:

	 push [pacmanX]
	 push [pacmanY]
     call removePacman

	 mov [pacmanCurrentDirection], 'D'

	 push [pacmanX]
	 push [pacmanY]
	 call FindNextAddedX_East

	 pop [pacmanX]

	 push [pacmanCurrentDirection]
	 push [pacmanX]
	 push [pacmanY]
	 call PacmanFigureDisplay

     jmp MainLoopShortcut

West:

	 push [pacmanX]
	 push [pacmanY]
     call removePacman

	 mov [pacmanCurrentDirection], 'A'

	 push [pacmanX]
	 push [pacmanY]
	 call FindNextAddedX_West

	 pop [pacmanX]

	 push [pacmanCurrentDirection]
	 push [pacmanX]
	 push [pacmanY]
	 call PacmanFigureDisplay

     jmp MainLoopShortcut

EXIT:
		push	ds
		mov	ax,251Ch
		mov	dx,[timerOfs]
		mov	ds,[timerSeg]
		int	21h
		pop	ds

		call finishGraphicMode
		mov	ah,04Ch
		mov	al,[exitCode1]
    int 21h



;======================
;start screen dispaly
;=====================
proc StratScreen

     mov dx, offset Filename_Maze
     mov [BmpLeft], 0
     mov [BmpTop],0
     mov [BmpColSize], FILE_COLS_MAZE
     mov [BmpRowSize] ,FILE_ROWS_MAZE
     call OpenShowBmp

     ret

endp StratScreen
;=============================================
;Find next X value considering bounderies.
;--------------------------------------------
;Input:
;1- CurrentXPos
;2- CurrentYPos
;--------------------------------------------
;Registers:
;bp, cx, dx, ax
;--------------------------------------------
;Output:
;nextX value - stack
;=============================================
currentX equ [bp + 6]
currentY equ [bp + 4]
nextX equ [bp - 8]
normalizedY equ [bp - 10]

proc FindNextAddedX_West

	push bp
	mov bp, sp

	push ax
	push dx
	push cx

	sub sp, 4
;Normalize currentY value to present to its middle pixel according to direction

	mov ax,  currentY
	mov normalizedY, ax
	add normalizedY, PACMAN_MIDDLE_Y_PIXLE_WEST

	mov ax, currentX
	mov nextX, ax
	sub nextX, NEXT_POS_ADDED_PIXELS_X

	mov cx, currentX
	mov dx, normalizedY
	mov ah,0Dh

@@IsTouchingBoundray:

	int 10h

	cmp al, 0FCh
	je @@FindMinSteps

    dec cx
    jmp @@IsTouchingBoundray


@@FindMinSteps:

    inc cx
    cmp cx, nextX
    jb @@CheckAddedSteps

@@CountedSteps:

	mov nextX, cx ;cx value is next X value

@@CheckAddedSteps:

	mov cx, currentX
	sub cx, nextX

	cmp cx, DISTANCE_FROM_BOUNDARY_X
	jnae @@ExitProc

	add nextX, DISTANCE_FROM_BOUNDARY_X

@@ExitProc:

	mov cx, nextX
	mov currentX, cx

	add sp, 4

	pop cx
	pop dx
	pop ax

	pop bp

	ret 2

endp FindNextAddedX_West

;=============================================
;Find next X value considering bounderies.
;--------------------------------------------
;Input:
;1- CurrentXPos
;2- CurrentYPos
;--------------------------------------------
;Registers:
;bp, cx, dx, ax
;--------------------------------------------
;Output:
;nextX value - stack
;=============================================
currentX equ [bp + 6]
currentY equ [bp + 4]
nextX equ [bp - 8]
normalizedY equ [bp - 10]
normalizedX equ [bp - 12]

proc FindNextAddedX_East

	push bp
	mov bp, sp

	push ax
	push dx
	push cx

	sub sp, 6

	;Normalize currentY value to present to its middle pixel according to direction
	mov ax,  currentY
	mov normalizedY, ax
	add normalizedY, PACMAN_MIDDLE_Y_PIXLE_WEST

	;Containing next defualt value
	;CurrentX is top left pacman point -> dosen't present the east muserments properly
	mov ax, currentX
	mov normalizedX, ax
	add normalizedX, FILE_COLS_PACMAN

	mov ax, normalizedX
	mov nextX, ax
	add nextX, NEXT_POS_ADDED_PIXELS_X

	mov cx, normalizedX
	mov dx, normalizedY
	mov ah,0Dh

@@IsTouchingBoundray:

	int 10h

	cmp al, 0FCh
	je @@FindMinSteps

  	inc cx
  	jmp @@IsTouchingBoundray


@@FindMinSteps:

    dec cx
    cmp cx, nextX
    ja @@CheckAddedSteps

@@CountedSteps:

		mov nextX, cx ;cx value is next X value

@@CheckAddedSteps:

	mov cx, nextX
	sub cx, normalizedX

	cmp cx, DISTANCE_FROM_BOUNDARY_X
	jnae @@ExitProc

	sub nextX, DISTANCE_FROM_BOUNDARY_X

@@ExitProc:

	sub nextX, FILE_COLS_PACMAN
	mov cx, nextX
	mov currentX, cx

	add sp, 6

	pop cx
	pop dx
	pop ax
	pop bp

	ret 2

endp FindNextAddedX_East

;=============================================
;Find next Y value considering bounderies.
;--------------------------------------------
;Input:
;1- CurrentYPos
;2- CurrentXPos
;--------------------------------------------
;Registers:
;bp, cx, dx, ax
;--------------------------------------------
;Output:
;nextX value - stack
;=============================================
currentY equ [bp + 6]
currentX equ [bp + 4]
nextY equ [bp - 8]
normalizedX equ [bp - 10]
normalizedY equ [bp - 12]

proc FindNextAddedY_South

	push bp
	mov bp, sp

	push ax
	push dx
	push cx

	sub sp, 6

	mov ax,  currentX
	mov normalizedX, ax
	add normalizedX, PACMAN_MIDDLE_X_PIXLE_WEST

	mov ax, currentY
	mov normalizedY, ax
	add normalizedY, FILE_ROWS_PACMAN

	mov ax, normalizedY
	mov nextY, ax
	add nextY, NEXT_POS_ADDED_PIXELS_Y

	mov cx, normalizedX
	mov dx, normalizedY
	mov ah,0Dh

	@@IsTouchingBoundray:

	int 10h

	cmp al, 0FCh
	je @@FindMinSteps

	inc dx
	jmp @@IsTouchingBoundray


	@@FindMinSteps:

	    dec dx
	    cmp dx, nextY
	    ja @@CheckAddedSteps

	@@CountedSteps:

			mov nextY, dx ;dx value is next Y value

	@@CheckAddedSteps:

		mov dx, nextY
		sub dx, normalizedY

		cmp dx, DISTANCE_FROM_BOUNDARY_Y
		jnae @@ExitProc

		sub nextY, DISTANCE_FROM_BOUNDARY_Y

	@@ExitProc:

		sub nextY, FILE_COLS_PACMAN
		mov dx, nextY
		mov currentY, dx

	add sp, 6

	pop cx
	pop dx
	pop ax
	pop bp

	ret 2

endp FindNextAddedY_South
;=============================================
;Find next Y value considering bounderies.
;--------------------------------------------
;Input:
;1- CurrentYPos
;2- CurrentXPos
;--------------------------------------------
;Registers:
;bp, cx, dx, ax
;--------------------------------------------
;Output:
;nextX value - stack
;=============================================
currentY equ [bp + 6]
currentX equ [bp + 4]
nextY equ [bp - 8]
normalizedX equ [bp - 10]

proc FindNextAddedY_North

	push bp
	mov bp, sp

	push ax
	push dx
	push cx

	sub sp, 4

	mov ax, currentX
	mov normalizedX, ax
	add normalizedX, PACMAN_MIDDLE_X_PIXLE_WEST

	mov ax, currentY
	mov nextY, ax
	sub nextY, NEXT_POS_ADDED_PIXELS_Y

	mov cx, normalizedX
	mov dx, currentY
	mov ah,0Dh

	@@IsTouchingBoundray:

	int 10h

	cmp al, 0FCh
	je @@FindMinSteps

	dec dx
	jmp @@IsTouchingBoundray


	@@FindMinSteps:

	    inc dx
	    cmp dx, nextY
	    jb @@CheckAddedSteps

	@@CountedSteps:

			mov nextY, dx ;dx value is next Y value

	@@CheckAddedSteps:

		mov dx, currentY
		sub dx, nextY

		cmp dx, DISTANCE_FROM_BOUNDARY_Y
		jnae @@ExitProc

		add nextY, DISTANCE_FROM_BOUNDARY_Y

	@@ExitProc:

		mov dx, nextY
		mov currentY, dx

	add sp, 4

	pop cx
	pop dx
	pop ax
	pop bp

	ret 2

endp FindNextAddedY_North
;=============================================
;Remove pacman and dots (9*9)
;--------------------------------------------
;Input:
;1- CurrentXPos
;2- CurrentYPos
;--------------------------------------------
;Registers:
;bp, cx, dx, di, ax
;--------------------------------------------
;Output:
;screen
;=============================================
currentX equ [bp + 6]
currentY equ [bp + 4]

proc removePacman

     push bp
     mov bp, sp

     push cx
	 lea cx, [pacmanBlank]
	 mov [matrix] , cx

     push dx
	 mov dx, FILE_COLS_PACMAN
	 mov cx, FILE_ROWS_PACMAN

     push di
     push ax

     mov di, currentY
     mov ax, currentY
     ;currentY * 320
     shl di, 8
     shl ax, 6

     add di, ax
     add di, currentX

	 call putMatrixInScreen

     pop ax
     pop di
     pop dx
     pop cx
     pop bp

     ret 4

endp removePacman

;=============================================
;Shows pacman on screen
;--------------------------------------------
;Input:
;1- Direction (by big letter [North -> N])
;2- CurrentXPos
;3- CurrentYPos
;--------------------------------------------
;Registers:
; dx, bp
;--------------------------------------------
;Output:
;screen
;=============================================

    dirction equ [bp + 8]
    xPos equ [bp + 6]
    yPos equ [bp + 4]

proc PacmanFigureDisplay

     push bp
     mov bp, sp

     push dx
     push ax


@@North:

	 mov ax, dirction
     cmp ax, 'W'
     jne @@South

     mov dx, offset Filename_PacmanNorth
     jmp PacmanDisplay

@@South:

	 mov ax, dirction
     cmp ax, 'S'
     jne @@East

     mov dx, offset Filename_PacmanSouth
     jmp PacmanDisplay

@@East:

	 mov ax, dirction
     cmp ax, 'D'
     jne @@West

     mov dx, offset Filename_PacmanEast
     jmp PacmanDisplay

@@West:

     mov dx, offset Filename_PacmanWest


PacmanDisplay:

     mov ax, xPos
     mov [BmpLeft],ax
     mov ax, yPos
     mov [BmpTop],ax
     mov [BmpColSize], FILE_COLS_PACMAN
     mov [BmpRowSize] ,FILE_ROWS_PACMAN
     call OpenShowBmp

     pop ax
     pop dx
     pop bp

     ret 6

endp PacmanFigureDisplay


proc setPacmanCurrentPoint

     push di
     push ax

     mov di, [pacmanY]

     ;currentY * right boundary ("smaller screen")
	 mov ax, MAZE_RIGHT_BOUNDARY_X
	 mul di
	 mov di, ax

     add di, [pacmanX]
     mov [currentPoint], di

     pop ax
     pop di

     ret

endp setPacmanCurrentPoint

;============================================================================================
;=======================
;Put bmp file on screen
;======================
 proc OpenShowBmp near


	call OpenBmpFile
	cmp [ErrorFile],1
	je @@ExitProc

	call ReadBmpHeader

	call ReadBmpPalette

	call CopyBmpPalette

	call  ShowBMP


	call CloseBmpFile

@@ExitProc:
	ret
endp OpenShowBmp




; input dx filename to open
proc OpenBmpFile	near
	mov ah, 3Dh
	xor al, al
	int 21h
	jc @@ErrorAtOpen
	mov [FileHandle], ax
	jmp @@ExitProc

@@ErrorAtOpen:
	mov [ErrorFile],1
@@ExitProc:
	ret
endp OpenBmpFile


proc CloseBmpFile near
	mov ah,3Eh
	mov bx, [FileHandle]
	int 21h
	ret
endp CloseBmpFile


; Read 54 bytes the Header
proc ReadBmpHeader	near
	push cx
	push dx

	mov ah,3fh
	mov bx, [FileHandle]
	mov cx,54
	mov dx,offset Header
	int 21h

	pop dx
	pop cx
	ret
endp ReadBmpHeader


proc ReadBmpPalette near ; Read BMP file color palette, 256 colors * 4 bytes (400h)
						 ; 4 bytes for each color BGR + null)
	push cx
	push dx

	mov ah,3fh
	mov cx,400h
	mov dx,offset Palette
	int 21h

	pop dx
	pop cx

	ret
endp ReadBmpPalette
;===============================================
; Will move out to screen memory the colors
; video ports are 3C8h for number of first color
; and 3C9h for all rest
;==============================================
proc CopyBmpPalette		near

	push cx
	push dx

	mov si,offset Palette
	mov cx,256
	mov dx,3C8h
	mov al,0  ; black first
	out dx,al ;3C8h
	inc dx	  ;3C9h
CopyNextColor:
	mov al,[si+2] 		; Red
	shr al,2 			; divide by 4 Max (cos max is 63 and we have here max 255 ) (loosing color resolution).
	out dx,al
	mov al,[si+1] 		; Green.
	shr al,2
	out dx,al
	mov al,[si] 		; Blue.
	shr al,2
	out dx,al
	add si,4 			; Point to next color.  (4 bytes for each color BGR + null)

	loop CopyNextColor

	pop dx
	pop cx

	ret
endp CopyBmpPalette

;================================================
; BMP graphics are saved upside-down.
; Read the graphic line by line (BmpRowSize lines in VGA format),
; PacmanDispalying the lines from bottom to top.
;================================================
proc ShowBMP

	push cx

	mov ax, 0A000h
	mov es, ax

	mov cx,[BmpRowSize]


	mov ax,[BmpColSize] ; row size must dived by 4 so if it less we must calculate the extra padding bytes
	xor dx,dx
	mov si,4
	div si
	cmp dx,0
	mov bp,0
	jz @@row_ok
	mov bp,4
	sub bp,dx

@@row_ok:
	mov dx,[BmpLeft]

@@NextLine:
	push cx
	push dx

	mov di,cx  ; Current Row at the small bmp (each time -1)
	add di,[BmpTop] ; add the Y on entire screen


	; next 5 lines  di will be  = cx*320 + dx , point to the correct screen line
	dec di
	mov cx,di
	shl cx,6
	shl di,8
	add di,cx
	add di,dx

	; small Read one line
	mov ah,3fh
	mov cx,[BmpColSize]
	add cx,bp  ; extra  bytes to each row must be divided by 4
	mov dx,offset ScrLine
	int 21h
	; Copy one line into video memory
	cld ; Clear direction flag, for movsb
	mov cx,[BmpColSize]
	mov si,offset ScrLine
	rep movsb ; Copy line to the screen

	pop dx
	pop cx

	loop @@NextLine

	pop cx
	ret
endp ShowBMP


;================================================
; Description: Graphic Mode
; INPUT: None
; OUTPUT: Screen
; Register Usage: AX
;================================================

proc stratGraphicMode

	 mov ax, 13h
	 int 10h

	 ret

endp stratGraphicMode

;================================================
; Description: End Graphic Mode
; INPUT: None
; OUTPUT: Screen
; Register Usage: AX
;================================================

proc finishGraphicMode

	 mov al, 3
	 mov ah, 0
	 int 10h

	 ret

endp finishGraphicMode

;------------------------------------------------------------------------
; PrintSecondsElapse -   Interrupt Service Routine (ISR)
;------------------------------------------------------------------------
;	Input:  none
;	Output: print counter every ticks elapsed
;       Registers:  none
;------------------------------------------------------------------------
inProgress	db	0
difference	dw	0
lastTimer   dw  0
fixDrift    db  5
counter     dw  0

PROC	PrintSecondsElapse
	cmp	[byte cs:inProgress],0
	jne	@@99
	inc	[byte cs:inProgress]
	; this needs for the processor to be able
    ; recognizing again an external interrupt signals
    sti
	push	ax
	push	ds
	push	dx

    ; this needs to tell the 8259 chip tp pass the
    ; interrupts it receives along to the processor
	mov	al,EOI
	out	PIC8259,al
	mov	ax,BIOSData
	mov	ds,ax


	;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ; Y O U R    C O D E
	;;;;;;;;;;;;;;;;;;;;;;;;;;;
    mov	dx, [word LowTimer]     ; read the timmer
    push dx                     ; save the timmer
	sub	dx,[cs:lastTimer]
	cmp	dx, [cs:difference]
	pop dx
    jb	@@20
    dec [cs:fixDrift]
    jnz @@10
    mov [cs:fixDrift], 5
    inc dx
@@10:
    mov	[cs:lastTimer], dx
    mov ax, [cs:counter]
    call printAxDec
    inc [cs:counter]




@@20:
	cli
	dec	[byte cs:inProgress]
	pop	dx
	pop	ds
	pop	ax
@@99:
	iret
ENDP	PrintSecondsElapse

;-----------------------------------------------------------------------
;  KeyWaiting - checks if a key press is available
;-----------------------------------------------------------------------
;	Input:  none
;	Output:	zf = 0 : (JNZ) Character is waiting to be read
;		zf = 1 : (JZ) No character is waiting
;	Registers: none (flags only)
;-----------------------------------------------------------------------

PROC printAxDec
	   push ax
       push bx
	   push dx
	   push cx

		 push ax
		 mov  dl, 35   ;Column
		 mov  dh, 18   ;Row
		 mov  bh, 0    ;Display page
		 mov  ah, 02h  ;SetCursorPosition
		 int  10h
		 pop ax
     mov cx,0   ; will count how many time we did push
     mov bx,10  ; the divider

put_next_to_stack:

       xor dx,dx
       div bx
       add dl,30h
	   ; dl is the current LSB digit
	   ; we cant push only dl so we push all dx
       push dx
       inc cx
       cmp ax,9   ; check if it is the last time to div
       jg put_next_to_stack

	   cmp ax,0
	   jz pop_next_from_stack  ; jump if ax was totally 0

       add al,30h
	   mov dl, al
  	   mov ah, 2h
	   int 21h        ; show first digit MSB



pop_next_from_stack:

       pop ax    ; remove all rest LIFO (reverse) (MSB to LSB)
	   mov dl, al
     mov ah, 2h
	   int 21h        ; show all rest digits
       loop pop_next_from_stack

	   pop cx
	   pop dx
	   pop bx
	   pop ax
       ret
endp printAxDec

;================================================
; in dx how many cols
; in cx how many rows
; in matrix - the bytes
; in di start byte in screen (0 64000 -1)
;================================================
; Description: Put Matrix on Screen
; INPUT: DX [COL], CX [ROWS], Matrix offset, DI[Adress]
; OUTPUT: Screen
; Register Usage: AX, CX, DX, SI
;================================================


proc putMatrixInScreen
	push es
	push ax
	push si

	mov ax, 0A000h
	mov es, ax
	cld

	push dx
	mov ax,cx
	mul dx
	mov bp,ax
	pop dx


	mov si,[matrix]

NextRow:
	push cx

	mov cx, dx
	rep movsb ; Copy line to the screen
	sub di,dx
	add di, 320


	pop cx
	loop NextRow


endProc:

	pop si
	pop ax
	pop es
    ret
endp putMatrixInScreen


END start
