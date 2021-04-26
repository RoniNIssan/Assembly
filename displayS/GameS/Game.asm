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

NEXT_POS_ADDED_PIXELS_Y = 9
NEXT_POS_ADDED_PIXELS_X = 9

PACMAN_MIDDLE_X_PIXLE_WEST = (FILE_COLS_PACMAN - 1) / 2 + 1
PACMAN_MIDDLE_Y_PIXLE_WEST = (FILE_ROWS_PACMAN - 1) / 2 + 1

;Needed when turn:
DISTANCE_FROM_BOUNDARY_X = 2; when moving on Y - distance between ghost and boundary	
DISTANCE_FROM_BOUNDARY_Y = 2; when moving on X - distance between ghost and boundary


DATASEG


		Filename_Maze db FILENAME_MAZE_DISPLAY, 0
		Filename_PacmanNorth db FILENAME_PACMAN_NORTH, 0
		Filename_PacmanSouth db FILENAME_PACMAN_SOUTH, 0
		Filename_PacmanEast db FILENAME_PACMAN_EAST, 0
		Filename_PacmanWest db FILENAME_PACMAN_WEST, 0
		ScrLine db FILE_COLS_MAZE dup (0)  ; One Color line read buffer

		FileHandle	dw ?
		Header 	    db 54 dup(0)
		Palette 	db 400h dup (0)	
		
		BmpFileErrorMsg    	db 'Error At Opening Bmp File ',FILE_COLS_PACMAN, 0dh, 0ah,'$'
		ErrorFile           db 0
	
		BmpLeft dw ?
		BmpTop dw ?
		BmpColSize dw ?
		BmpRowSize dw ?
	
		matrix dw ?

        ;Current Position 
;		 pacmanX dw 84
;        pacmanY dw 146
 		 pacmanX dw 126
         pacmanY dw 164

        currentPoint dw ?
		;pacmanCurrentDirection dw 'A'
		pacmanCurrentDirection dw 'S'

		;Boolean
		Bool db 0 
		
		isDirectionChanged dw 0
		

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
	 mov ax, @data
	 mov ds,ax


;========TEXT MODE========
;		Openning Scrren

	 call stratGraphicMode
     call StratScreen

     call setPacmanCurrentPoint
	 
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

     mov ah, 0
     int 16h

	 call finishGraphicMode
	 mov ax, 4C00h
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
	jne @@ExitProc

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
    ja @@Defualt

@@CountedSteps:
     
	sub cx, FILE_COLS_PACMAN
	mov currentX, cx ;cx value is next X value
	jmp @@ExitProc

@@Defualt:

	sub nextX, FILE_COLS_PACMAN
	mov cx, nextX
	mov currentX, cx

@@ExitProc:

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
	add normalizedY, NEXT_POS_ADDED_PIXELS_Y

	mov ax, normalizedY
	mov nextY, ax
	add nextY, NEXT_POS_ADDED_PIXELS_Y

	mov cx, normalizedX
	mov dx, nextY
	mov ah,0Dh
	int 10h
	
	cmp al, 0FCh
	jne @@ExitProc

	mov dx, currentY
	mov ah,0Dh

@@FindClosestNextY:

	int 10h

	cmp al, 0FCh
	jne @@ReturnClosestNextY

	inc dx

	jmp @@FindClosestNextY

@@ReturnClosestNextY:

	;dec dx
	cmp dx, nextY
	jb @@CountSmaller

@@CountSmaller:

	mov nextY, dx

	jmp @@ExitProc

@@DefualtSmaller:

	add nextY, NEXT_POS_ADDED_PIXELS_Y

@@ExitProc:

	sub nextY, NEXT_POS_ADDED_PIXELS_Y
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
nextY equ [bp - 6]

proc FindNextAddedY_North

	push bp
	mov bp, sp

	push ax
	push dx
	push cx

	sub sp, 4

	mov ax,  currentX
	mov normalizedX, ax
	add normalizedX, PACMAN_MIDDLE_X_PIXLE_WEST

	mov ax, currentY
	mov nextY, ax
	sub nextY, NEXT_POS_ADDED_PIXELS_Y

	mov cx, normalizedX
	mov dx, nextY
	mov ah,0Dh
	int 10h
	
	cmp al, 0FCh
	jne @@ExitProc

	mov dx, currentY
	mov ah,0Dh

@@FindClosestNextY:

	int 10h

	cmp al, 0FCh
	jne @@ReturnClosestNextY

	dec dx

	jmp @@FindClosestNextY

@@ReturnClosestNextY:

	;inc dx
	cmp dx, nextY
	ja @@CountSmaller

@@CountSmaller:

	mov nextY, dx

	jmp @@ExitProc

@@DefualtSmaller:

	sub nextY, NEXT_POS_ADDED_PIXELS_Y

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


;====================================
;Proc is_turnRight
;Input: 
;-current point (gets from super where calculated)
;Output
;-able or unable to turn [Boolean varible]
;====================================
proc is_turnRight

    push ax
    push cx
    push dx

	mov [Bool], 0

    mov cx, [pacmanX]
    mov dx, [pacmanY]
    mov ah, 0Dh

    int 10h

	cmp al, 1
	je @@ExitProc

@@DoubleCheck:

    add dx, 9
    mov ah, 0Dh

    int 10h

 	cmp al, 1
	je @@ExitProc 

@@True:

	mov [Bool], 1

	
@@ExitProc:

    pop dx
    pop cx
    pop ax
	ret


endp is_turnRight

;====================================
;Proc is_turnLeft
;Input: 
;-current point (gets from super where calculated)
;Output
;-able or unable to turn [Boolean varible]
;====================================
;currentPos equ [bp + 4] ;pushed

proc is_turnLeft

    push ax
    push cx
    push dx

	mov [Bool], 0

    mov cx, [pacmanX]
    mov dx, [pacmanY]
    mov ah, 0Dh

    int 10h

	cmp al, 1
	je @@ExitProc

@@DoubleCheck:

    add cx, 9
    mov ah, 0Dh

    int 10h

 	cmp al, 1
	je @@ExitProc 

@@True:

	mov [Bool], 1

	
@@ExitProc:

    pop dx
    pop cx
    pop ax
	ret


endp is_turnLeft

;====================================
;Proc is_turnBack
;Input: 
;-current point (gets from super where calculated)
;Output
;-able or unable to turn [Boolean varible]
;====================================
   ; currentPos equ [bp + 4] 

proc is_turnBack
    push ax
    push cx
    push dx

	mov [Bool], 0

    mov cx, [pacmanX]
    mov dx, [pacmanY]
    add dx, 9
    mov ah, 0Dh

    int 10h

	cmp al, 1
	je @@ExitProc

@@DoubleCheck:

    add cx, 9
    mov ah, 0Dh

    int 10h

 	cmp al, 1
	je @@ExitProc 

@@True:

	mov [Bool], 1

	
@@ExitProc:

    pop dx
    pop cx
    pop ax
	ret


endp is_turnBack

;====================================
;Proc is_turnFront
;Input: 
;-current point (gets from super where calculated)
;Output
;-able or unable to turn [Boolean varible]
;====================================
;currentPos equ [bp + 4] ;pushed

proc is_turnFront

    push ax
    push cx
    push dx

	mov [Bool], 0

    mov cx, [pacmanX]
    mov dx, [pacmanY]
    mov ah, 0Dh

    int 10h

	cmp al, 1
	je @@ExitProc

@@DoubleCheck:

    add cx, 9
    mov ah, 0Dh

    int 10h

 	cmp al, 1
	je @@ExitProc 

@@True:

	mov [Bool], 1

	
@@ExitProc:

    pop dx
    pop cx
    pop ax
	ret


endp is_turnFront

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

;================================================
; Description: Waits
; INPUT: None
; OUTPUT: Screen 
; Register Usage: CX, DX, AX  
;================================================
	 
proc ToWait

	 mov cx, 0Fh
	 mov dx, 4240
	 mov ah, 86h
	 int 15h
	 ret
	
endp ToWait	

;================================================
; Description: Draw a Horizontal line
; INPUT: DX [Y], CX [X], SI [WIDTH]
; OUTPUT: Screen 
; Register Usage: AX, CX, DX, SI
;================================================
	
proc DrawHorizontalLine	near
	push si
	push cx
DrawLine:
	cmp si,0
	jz ExitDrawLine	
	 
    mov ah,0ch	
	int 10h    ; put pixel
		
	
	inc cx
	dec si
	jmp DrawLine
	
	
ExitDrawLine:
	pop cx
    pop si
	ret
endp DrawHorizontalLine


;================================================
; Description: Draw a Vertical line
; INPUT: DX [Y], CX [X], SI [LENGHT]
; OUTPUT: Screen 
; Register Usage: AX, CX, DX, SI
;================================================
	
proc DrawVerticalLine	near
	push si
	push dx
 
DrawVertical:
	cmp si,0
	jz ExitDrawLine	
	 
    mov ah,0ch	
	int 10h    ; put pixel
	
	 
	
	inc dx
	dec si
	jmp DrawVertical
	
	
@@ExitDrawLine:
	pop dx
    pop si
	ret
endp DrawVerticalLine	

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