IDEAL
MODEL small
STACK 100h



FILENAME_SCREEN equ 'Screen.bmp'
FILENAME_PLAY equ 'Play.bmp'
FILENAME_LB equ 'lb.bmp'
FILENAME_INST equ 'Inst.bmp'
FILENAME_NA equ 'NA.bmp'
FILENAME_INST_1 equ '1.bmp'
FILENAME_INST_2 equ '2.bmp'
FILENAME_INST_3 equ '3.bmp'
FILENAME_INST_4 equ '4.bmp'

FILE_ROWS = 200
FILE_COLS = 320

;Play Banner values
PLAY_RIGHT_COL = 187
PLAY_LEFT_COL = 137
PLAY_TOP_ROW = 50
PLAY_BOTTOM_ROW = 71

;Leaderboard Banner values
LB_RIGHT_COL = 239
LB_LEFT_COL = 92
LB_TOP_ROW = 78
LB_BOTTOM_ROW = 89

;Game instructions Banner values
INST_RIGHT_COL = 270
INST_LEFT_COL = 51
INST_TOP_ROW = 96
INST_BOTTOM_ROW = 107

;Quit Banner values
QUIT_RIGHT_COL_OPEN = 34
QUIT_LEFT_COL_OPEN = 10
QUIT_TOP_ROW_OPEN = 6
QUIT_BOTTOM_ROW_OPEN = 31

;Arrow forward
ARROW_FORWARD_RIGHT_COL = 195
ARROW_FORWARD_LEFT_COL = 177
ARROW_FORWARD_TOP_ROW = 183
ARROW_FORWARD_BOTTOM_ROW = 198

;Arrow backward
ARROW_BACKWARD_RIGHT_COL = 160
ARROW_BACKWARD_LEFT_COL = 144
ARROW_BACKWARD_TOP_ROW = 183
ARROW_BACKWARD_BOTTOM_ROW = 193

DATASEG




	Filename_StartScreen db FILENAME_SCREEN, 0
	Filename_PlayButton db FILENAME_PLAY, 0
	Filename_LbButton db FILENAME_LB, 0
	Filename_Inst_button db FILENAME_INST, 0
	Filename_NA_Dis db FILENAME_NA, 0
	Filename_FirstInst db FILENAME_INST_1, 0
	Filename_SecondInst db FILENAME_INST_2, 0
	Filename_ThirdInst db FILENAME_INST_3, 0
	Filename_FourthInst db FILENAME_INST_4, 0

	ScrLine 	db FILE_COLS dup (0)  ; One Color line read buffer

	FileHandle	dw ?
	Header 	    db 54 dup(0)
	Palette 	db 400h dup (0)

	BmpFileErrorMsg    	db 'Error At Opening Bmp File ',FILENAME_SCREEN, 0dh, 0ah,'$'
	ErrorFile           db 0

	BmpLeft dw ?
	BmpTop dw ?
	BmpColSize dw ?
	BmpRowSize dw ?

	matrix dw ?

			;Mouse Varibles
			MouseX dw ?
			MouseY dw ?

	;Boolean
	Bool db 0
	isButtonOn db 0

	;Buttons on
	play db 0
	Lb db 0
	inst db 0


CODESEG

	ORG 100h

start:
 mov ax, @data
 mov ds,ax


;========TEXT MODE========
;		Openning Scrren

 call stratGraphicMode
 call StratScreen_OPEN

 call OpenScreen

EXIT:

 call finishGraphicMode
 mov ax, 4C00h ; returns control to dos
	 int 21h

;=============================================
;Open screen graphics
;--------------------------------------------
;Output:
;via global bool varibles about turned buttons
;=============================================
proc OpenScreen

mov ax,0h
int 33h

mov ax,1h
int 33h

CheckStatus:


mov ax, 3h
int 33h

shr cx, 1
mov [MouseX], cx
mov [MouseY], dx

push QUIT_LEFT_COL_OPEN
push QUIT_RIGHT_COL_OPEN
push QUIT_TOP_ROW_OPEN
push QUIT_BOTTOM_ROW_OPEN
call isInRange

cmp [Bool], 1
jne PlayBanner

cmp bx, 1
je ExitShourtcut


PlayBanner:

push PLAY_LEFT_COL
push PLAY_RIGHT_COL
push PLAY_TOP_ROW
push PLAY_BOTTOM_ROW
call isInRange

cmp [Bool], 1
jne LeaderBoardBanner

cmp bx, 1
je PlayClick

cmp [isButtonOn], 1
je CheckStatusShourtcut

mov ax, 2
int 33h

mov [isButtonOn], 1
call PlayButtonDisplay

mov ax,1h
int 33h

jmp CheckStatus

PlayClick:
mov [play], 1

ExitShourtcut:
jmp @@ExitProc


LeaderBoardBanner:

push LB_LEFT_COL
push LB_RIGHT_COL
push LB_TOP_ROW
push LB_BOTTOM_ROW
call isInRange

cmp [Bool], 1
jne GameInstructionsBanner

cmp bx, 1
je LbClick

cmp [isButtonOn], 1
je CheckStatusShourtcut

mov ax, 2
int 33h

mov [isButtonOn], 1
call LbButtonDisplay

mov ax,1h
int 33h

jmp CheckStatus

CheckStatusShourtcut:
jmp CheckStatus

LbClick:
mov ax, 2
int 33h

call NADisplay
call ToWait

mov ax, 1
int 33h
call LbButtonDisplay
jmp CheckStatusShourtcut

GameInstructionsBanner:

push INST_LEFT_COL
push INST_RIGHT_COL
push INST_TOP_ROW
push INST_BOTTOM_ROW
call isInRange


cmp [Bool], 1
jne @@CleanScreen

cmp bx, 1
je InstClick

cmp [isButtonOn], 1
je CheckStatusShourtcut

mov ax, 2
int 33h

mov [isButtonOn], 1
call InstButtonDisplay

mov ax,1h
int 33h

jmp CheckStatus


InstClick:

call GameInstructionsPages
call InstButtonDisplay
jmp CheckStatusShourtcut

@@CleanScreen:

cmp [isButtonOn], 1
jne CheckStatusShourtcut

mov ax, 2
int 33h

mov [isButtonOn], 0
call StratScreen_OPEN

mov ax,1h
int 33h

jmp CheckStatus
@@ExitProc:

ret

endp OpenScreen




count equ [word bp - 2]
proc GameInstructionsPages

	push bp
	mov bp, sp

	sub sp, 2

	mov count, 0
	jmp FirstPage

InstructionsLoop:

	;call ToWait

	mov ax, 3h
	int 33h

	shr cx, 1
	mov [MouseX], cx
	mov [MouseY], dx

	push QUIT_LEFT_COL_OPEN
	push QUIT_RIGHT_COL_OPEN
	push QUIT_TOP_ROW_OPEN
	push QUIT_BOTTOM_ROW_OPEN
	call isInRange

	cmp [Bool], 1
	jne NextPage

	cmp bx, 1
	je @@ExitShourtcut

NextPage:

	push ARROW_FORWARD_LEFT_COL
	push ARROW_FORWARD_RIGHT_COL
	push ARROW_FORWARD_TOP_ROW
	push ARROW_FORWARD_BOTTOM_ROW
	call isInRange

	cmp [Bool], 1
	jne BackPage

	cmp bx, 1
	jne InstructionsLoop

	cmp count, 4
	je InstructionsLoopShortcut
	inc count
	jmp CheckCount

@@ExitShourtcut:
	jmp @@ExitProc
BackPage:
	push ARROW_BACKWARD_LEFT_COL
	push ARROW_BACKWARD_RIGHT_COL
	push ARROW_BACKWARD_TOP_ROW
	push ARROW_BACKWARD_BOTTOM_ROW
	call isInRange

	cmp [Bool], 1
	jne InstructionsLoopShortcut

	cmp bx, 1
	jne InstructionsLoopShortcut

	cmp count, 0
	je InstructionsLoopShortcut

	dec count

	jmp CheckCount

InstructionsLoopShortcut:
	jmp InstructionsLoop

CheckCount:
	cmp count, 1
	je FirstPage
	cmp count, 2
	je SecondPage
	cmp count, 3
	je ThirdPage
	cmp count, 4
	je FourthPage
	jne InstructionsLoopShortcut

FirstPage:

	mov ax, 2h
	int 33h

	mov dx, offset Filename_FirstInst
	call ShortBmp

	mov ax, 1h
	int 33h
	jmp InstructionsLoopShortcut

SecondPage:
	mov ax, 2h
	int 33h

	mov dx, offset Filename_SecondInst
	call ShortBmp

	mov ax, 1h
	int 33h

	jmp InstructionsLoopShortcut

ThirdPage:

	mov ax, 2h
	int 33h

	mov dx, offset Filename_ThirdInst
	call ShortBmp


	mov ax, 1h
	int 33h

	jmp InstructionsLoopShortcut

FourthPage:
	mov ax, 2h
	int 33h

	mov dx, offset Filename_FourthInst
	call ShortBmp

	mov ax, 1h
	int 33h

	jmp InstructionsLoopShortcut


@@ExitProc:

		add sp, 2
		pop bp

		ret

endp GameInstructionsPages

proc ShortBmp

	mov [BmpLeft],0 ;start point
	mov [BmpTop],0
	mov [BmpColSize], FILE_COLS
	mov [BmpRowSize] ,FILE_ROWS
	call OpenShowBmp
	ret

	endp ShortBmp


;=============================================
;Check if quit button is pressed
;--------------------------------------------
;Output:
;varible Bool 1 true/ 0 false
;=============================================
proc isQuitPressed

 push QUIT_LEFT_COL_OPEN
 push QUIT_RIGHT_COL_OPEN
 push QUIT_TOP_ROW_OPEN
 push QUIT_BOTTOM_ROW_OPEN
 call isInRange

 ret

endp isQuitPressed

;=============================================
;Check mouse position on buttons
;--------------------------------------------
;Input:
;1- MouseX -> cx (shr cx, 1)
;2- MouseY -> dx
;Stack inputs:
;left column, right column
;top row, bottom row
;--------------------------------------------
;Registers:
; ax, bp
;--------------------------------------------
;Output:
;varible Bool 1 true/ 0 false
;=============================================

;Button values
leftCol equ [bp + 10]
rightCol equ [bp + 8]
topRow equ [bp + 6]
bottomRow equ [bp + 4]

proc isInRange

 push bp
 mov bp, sp

 push ax

 mov [Bool], 0

Rows_Check:

	 ;mouse pos bigger than button edge
 mov ax, [MouseX]

	 cmp ax, rightCol
 ja @@ExitProc

	 cmp ax, leftCol
 jb @@ExitProc

Col_Check:

 mov ax, [MouseY]

	 cmp ax, topRow
 jb @@ExitProc

 cmp ax, bottomRow
 ja @@ExitProc

 mov [Bool], 1


@@ExitProc:

 pop ax
	 pop bp

 ret 8

endp isInRange


;======================
;start screen dispaly
;=====================
proc StratScreen_OPEN

	 mov dx, offset Filename_StartScreen
	 mov [BmpLeft],0 ;start point
	 mov [BmpTop],0
	 mov [BmpColSize], FILE_COLS
	 mov [BmpRowSize] ,FILE_ROWS
	 call OpenShowBmp

	 ret

endp StratScreen_OPEN

;==================================
;play button colored screen dispaly
;==================================
proc PlayButtonDisplay

	 mov dx, offset Filename_PlayButton
	 mov [BmpLeft],0 ;start point
	 mov [BmpTop],0
	 mov [BmpColSize], FILE_COLS
	 mov [BmpRowSize] ,FILE_ROWS
	 call OpenShowBmp

	 ret

endp PlayButtonDisplay

;==========================================
;leaderboard button colored screen dispaly
;=========================================
proc LbButtonDisplay

	 mov dx, offset Filename_LbButton
	 mov [BmpLeft],0 ;start point
	 mov [BmpTop],0
	 mov [BmpColSize], FILE_COLS
	 mov [BmpRowSize] ,FILE_ROWS
	 call OpenShowBmp

 ret

endp LbButtonDisplay

;===============================================
;Game instructions button colored screen dispaly
;===============================================
proc InstButtonDisplay

	 mov dx, offset Filename_Inst_button
	 mov [BmpLeft],0 ;start point
	 mov [BmpTop],0
	 mov [BmpColSize], FILE_COLS
	 mov [BmpRowSize] ,FILE_ROWS
	 call OpenShowBmp

 ret

endp InstButtonDisplay
;===============================================
;Game instructions button colored screen dispaly
;===============================================
proc NADisplay

	 mov dx, offset Filename_NA_Dis
	 mov [BmpLeft],0 ;start point
	 mov [BmpTop],0
	 mov [BmpColSize], FILE_COLS
	 mov [BmpRowSize] ,FILE_ROWS
	 call OpenShowBmp

 ret

endp NADisplay
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
