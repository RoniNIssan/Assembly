	IDEAL
MODEL small 
STACK 100h	



FILENAME_MAZE equ 'Maze.bmp'
FILENAME_PACMAN_NORTH equ 'PN.bmp'
FILENAME_PACMAN_SOUTH equ 'PS.bmp'
FILENAME_PACMAN_EAST equ 'PE.bmp'
FILENAME_PACMAN_WEST equ 'PW.bmp'

;Maze - background
FILE_ROWS_SCREEN = 200
FILE_COLS_SCREEN = 320 

;Pacman figure 
FILE_ROWS_PACMAN = 9
FILE_COLS_PACMAN = 9 

;Quit Banner values
QUIT_RIGHT_COL = 34
QUIT_LEFT_COL = 10
QUIT_TOP_ROW = 6
QUIT_BOTTOM_ROW = 31

DATASEG
	 


	
		Filename_Maze db FILENAME_MAZE, 0
		Filename_PacmanNorth db FILENAME_PACMAN_NORTH, 0
		Filename_PacmanSouth db FILENAME_PACMAN_SOUTH, 0
		Filename_PacmanEast db FILENAME_PACMAN_EAST, 0
		Filename_PacmanWest db FILENAME_PACMAN_WEST, 0
		ScrLine 	db FILE_COLS_SCREEN dup (0)  ; One Color line read buffer

		FileHandle	dw ?
		Header 	    db 54 dup(0)
		Palette 	db 400h dup (0)	
		
		BmpFileErrorMsg    	db 'Error At Opening Bmp File ',FILENAME_MAZE, 0dh, 0ah,'$'
		ErrorFile           db 0
	
		BmpLeft dw ?
		BmpTop dw ?
		BmpColSize dw ?
		BmpRowSize dw ?
	
		matrix dw ?

        ;Current Position 
        pacmanX dw ?
        pacmanY dw ?

		;Boolean
		Bool db 0 




CODESEG

    ORG 100h
	
start:
	 mov ax, @data
	 mov ds,ax


;========TEXT MODE========
;		Openning Scrren

	 call stratGraphicMode
    ; call StratScreen

     mov dx, offset Filename_Maze
     mov [BmpLeft],0 ;start point
     mov [BmpTop],0
     mov [BmpColSize], FILE_COLS_SCREEN
     mov [BmpRowSize] ,FILE_ROWS_SCREEN
     call OpenShowBmp

     
EXIT:

     call ToWait
     mov ah, 0
     int 16h

	 call finishGraphicMode
	 mov ax, 4C00h ; returns control to dos
  	 int 21h
  


;======================
;start screen dispaly
;=====================
proc StratScreen

     mov dx, offset Filename_Maze
     mov [BmpLeft],0 ;start point
     mov [BmpTop],0
     mov [BmpColSize], FILE_COLS_SCREEN
     mov [BmpRowSize] ,FILE_ROWS_SCREEN
     call OpenShowBmp

     ret

endp StratScreen

;=============================================
;Shows pacman on scren
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

proc PacmanFigureDisplay_North

     push bp
     mov bp, sp

     push dx
     push ax


North:

     cmp dirction, 'N'
     jne South

     mov dx, offset Filename_PacmanNorth
     jmp Dispaly

South:

     cmp dirction, 'S'
     jne East

     mov dx, offset Filename_PacmanSouth
     jmp Dispaly

East:

     cmp dirction, 'E'
     jne West

     mov dx, offset Filename_PacmanEast
     jmp Dispaly

West:

     mov dx, offset Filename_PacmanWest


Display:

     mov ax, yPos
     mov [BmpLeft],ax
     mov ax, xPos
     mov [BmpTop],ax
     mov [BmpColSize], FILE_COLS_PACMAN
     mov [BmpRowSize] ,FILE_ROWS_PACMAN
     call OpenShowBmp

     pop ax
     pop dx
     pop bp

     ret 2

endp PacmanFigureDisplay


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
; displaying the lines from bottom to top. 
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



END start	