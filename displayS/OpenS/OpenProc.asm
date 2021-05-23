;=============================================
;Open screen graphics
;--------------------------------------------
;Output:
;via global bool varibles about turned buttons
;=============================================
proc OpenScreen

mov ax,0h ;initilaize mouse
int 33h
call Delay
@@MainLoop:
call StratScreen_OPEN

call ShowMouse

CheckStatus: ;maon loop

call GetMousePos

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

call HideMouse
mov [isButtonOn], 1
call PlayButtonDisplay
call ShowMouse

jmp CheckStatus

PlayClick:
mov [play], 1

ExitShourtcut:
jmp @@ExitProc
@@MainLoopShortcut:
jmp @@MainLoop

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

call HideMouse
mov [isButtonOn], 1
call LbButtonDisplay
call ShowMouse

jmp CheckStatus

CheckStatusShourtcut:
jmp CheckStatus

LbClick:
mov [play], 0
call HideMouse
call NADisplay
call ToWait
call ShowMouse
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

call HideMouse
mov [isButtonOn], 1
call InstButtonDisplay
call ShowMouse
jmp CheckStatus


InstClick:
mov [play], 0
call GameInstructionsPages
call InstButtonDisplay
jmp CheckStatusShourtcut

@@CleanScreen:

cmp [isButtonOn], 1
jne CheckStatusShourtcut

call HideMouse

mov [isButtonOn], 0
call StratScreen_OPEN

call ShowMouse
jmp CheckStatus

@@ExitProc:
call Delay
ret

endp OpenScreen



;=============================================
;Displays Game manual
;--------------------------------------------
;Output:
;Screen
;=============================================
count equ [word bp - 2]
proc GameInstructionsPages

 push bp
 mov bp, sp
 sub sp, 2


 mov count, 1
 jmp FirstPage

InstructionsLoop:

 call GetMousePos
 cmp bx, 1
 jne InstructionsLoop

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
 je @@ExitShourtcut

NextPage:

 push ARROW_FORWARD_LEFT_COL
 push ARROW_FORWARD_RIGHT_COL
 push ARROW_FORWARD_TOP_ROW
 push ARROW_FORWARD_BOTTOM_ROW
 call isInRange

 cmp [Bool], 1
 jne BackPage

 ;cmp bx, 1
 ;jne InstructionsLoop

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

;cmp bx, 1
 ;jne InstructionsLoopShortcut

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

 call HideMouse
 mov dx, offset Filename_FirstInst
 call ShortBmp
 call ShowMouse
 call Delay
 jmp InstructionsLoopShortcut


SecondPage:
 call HideMouse
 mov dx, offset Filename_SecondInst
 call ShortBmp
 call ShowMouse
 call Delay
 jmp InstructionsLoopShortcut

ThirdPage:

 call HideMouse
 mov dx, offset Filename_ThirdInst
 call ShortBmp
 call ShowMouse
 call Delay
 call Delay
 jmp InstructionsLoopShortcut

FourthPage:

 call HideMouse
 mov dx, offset Filename_FourthInst
 call ShortBmp
 call ShowMouse
 call Delay
 jmp InstructionsLoopShortcut


@@ExitProc:
call Delay

	 add sp, 2
	 pop bp

	 ret

endp GameInstructionsPages


proc Delay

	pusha
	mov cx, 3h
	mov dx, 0D40h
	mov al, 0
	mov ah, 86h
	int 15h

	popa

	ret
endp Delay
;================
; 	Show Mouse
;===============
proc ShowMouse

mov ax, 1h
int 33h

ret

endp ShowMouse
;================
; 	Hide Mouse
;===============
proc HideMouse

mov ax, 2h
int 33h
ret

endp HideMouse
;================
; 	GetMousePos
;===============
proc GetMousePos

mov ax, 3h
int 33h
ret

endp GetMousePos
;=============================================
;Open Bmp file
;--------------------------------------------
;Input:
;filename offset in dx
;=============================================
proc ShortBmp

	mov [BmpLeft],0 ;start point
	mov [BmpTop],0
	mov [BmpColSize], FILE_COLS
	mov [BmpRowSize] ,FILE_ROWS
	call OpenShowBmp
	ret

	endp ShortBmp

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
	 call ShortBmp

	 ret

endp PlayButtonDisplay

;==========================================
;leaderboard button colored screen dispaly
;=========================================
proc LbButtonDisplay

	 mov dx, offset Filename_LbButton
	 call ShortBmp

 ret

endp LbButtonDisplay

;===============================================
;Game instructions button colored screen dispaly
;===============================================
proc InstButtonDisplay

	 mov dx, offset Filename_Inst_button
	 call ShortBmp

 ret

endp InstButtonDisplay
;===============================================
;Game instructions button colored screen dispaly
;===============================================
proc NADisplay

	 mov dx, offset Filename_NA_Dis
	 call ShortBmp

 ret

endp NADisplay

proc ToWait

 mov cx, 0Fh
 mov dx, 4240
 mov ah, 86h
 int 15h
 ret

endp ToWait
