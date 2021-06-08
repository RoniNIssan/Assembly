;=============================================
;Open screen graphics
;=============================================
proc OpenScreen

call Delay
@@MainLoop:
call StratScreen_OPEN

call ShowMouse

CheckStatus: ;maon loop

call GetMousePos

shr cx, 1
mov [MouseX], cx
mov [MouseY], dx

;check if quit button available
push [MouseX]
push [MouseY]
push QUIT_LEFT_COL_OPEN
push QUIT_RIGHT_COL_OPEN
push QUIT_TOP_ROW_OPEN
push QUIT_BOTTOM_ROW_OPEN
call isInRange

cmp [Bool], 1 ;if not, continue
jne PlayBanner

cmp bx, 1 ;check quit pressed
je ExitShourtcut

PlayBanner:
;check if play button available
push [MouseX]
push [MouseY]
push PLAY_LEFT_COL
push PLAY_RIGHT_COL
push PLAY_TOP_ROW
push PLAY_BOTTOM_ROW
call isInRange

cmp [Bool], 1;if not, continue
jne LeaderBoardBanner

cmp bx, 1;check play pressed, if so jump
je PlayClick

cmp [isButtonOn], 1 ;if button is not pressed but on, keep it so
je CheckStatusShourtcut

call HideMouse ;if button is not pressed and off, turn it on
mov [isButtonOn], 1
call PlayButtonDisplay
call ShowMouse

jmp CheckStatus

PlayClick:
mov [play], 1 ;bool varible explins exit reason

ExitShourtcut:
jmp @@ExitProc


LeaderBoardBanner:
push [MouseX]
push [MouseY]
push LB_LEFT_COL
push LB_RIGHT_COL
push LB_TOP_ROW
push LB_BOTTOM_ROW
call isInRange ;check if leaderboard

cmp [Bool], 1 ;if not, continue
jne GameInstructionsBanner

cmp bx, 1 ;if clicked, displays
je LbClick

cmp [isButtonOn], 1;if button is not pressed but on, keep it so
je CheckStatusShourtcut

call HideMouse;if button is not pressed and off, turn it on
mov [isButtonOn], 1
call LbButtonDisplay
call ShowMouse

jmp CheckStatus

CheckStatusShourtcut:
jmp CheckStatus

LbClick:
;Display "NOT available" and continue to main loop
mov [play], 0
call HideMouse
call NADisplay
call Delay
call LbButtonDisplay
call ShowMouse
jmp CheckStatusShourtcut

GameInstructionsBanner:
push [MouseX]
push [MouseY]
push INST_LEFT_COL
push INST_RIGHT_COL
push INST_TOP_ROW
push INST_BOTTOM_ROW
call isInRange ;check if game instructions


cmp [Bool], 1
jne @@CleanScreen

cmp bx, 1 ;i was clicked, display
je InstClick

cmp [isButtonOn], 1 ;if button is not pressed but on, keep it so
je CheckStatusShourtcut

call HideMouse;if button is not pressed and off, turn it on
mov [isButtonOn], 1
call InstButtonDisplay
call ShowMouse
jmp CheckStatus


InstClick:
mov [play], 0
call GameInstructionsPages ;display rules
call InstButtonDisplay ;return to last preview
jmp CheckStatusShourtcut

@@CleanScreen:
;if none of the keys are available restore screen to default
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
 cmp bx, 1 ;if anything pressed -> check what was pressed.
          ;otherwise, wait for input
 jne InstructionsLoop

 shr cx, 1
 mov [MouseX], cx
 mov [MouseY], dx

 push [MouseX]
 push [MouseY]
 push QUIT_LEFT_COL_OPEN
 push QUIT_RIGHT_COL_OPEN
 push QUIT_TOP_ROW_OPEN
 push QUIT_BOTTOM_ROW_OPEN
 call isInRange ;check quit button

 cmp [Bool], 1
 jne NextPage ;if not available, continue
 je @@ExitShourtcut

NextPage:
 push [MouseX]
 push [MouseY]
 push ARROW_FORWARD_LEFT_COL
 push ARROW_FORWARD_RIGHT_COL
 push ARROW_FORWARD_TOP_ROW
 push ARROW_FORWARD_BOTTOM_ROW
 call isInRange ;check forward arrow

 cmp [Bool], 1
 jne BackPage;if not available, continue

 cmp count, 4 ;top limit -> page cant be bigger than 4
 je InstructionsLoop
 inc count
 jmp CheckCount

@@ExitShourtcut:
 jmp @@ExitProc

BackPage:
 push [MouseX]
 push [MouseY]
 push ARROW_BACKWARD_LEFT_COL
 push ARROW_BACKWARD_RIGHT_COL
 push ARROW_BACKWARD_TOP_ROW
 push ARROW_BACKWARD_BOTTOM_ROW
 call isInRange;check backwards arrow

 cmp [Bool], 1
 jne InstructionsLoop;if not available, continue

 cmp count, 0;top limit -> page cant be smaller than 4
 je InstructionsLoop
 dec count
 jmp CheckCount


CheckCount:
;print page display by counter
 cmp count, 1
 je FirstPage
 cmp count, 2
 je SecondPage
 cmp count, 3
 je ThirdPage
 cmp count, 4
 je FourthPage
 jne InstructionsLoop

FirstPage:

 call HideMouse
 mov dx, offset Filename_FirstInst
 call ShortBmp
 call ShowMouse
 call Delay
 jmp InstructionsLoop


SecondPage:
 call HideMouse
 mov dx, offset Filename_SecondInst
 call ShortBmp
 call ShowMouse
 call Delay
 jmp InstructionsLoop

ThirdPage:

 call HideMouse
 mov dx, offset Filename_ThirdInst
 call ShortBmp
 call ShowMouse
 call Delay
 call Delay
 jmp InstructionsLoop

FourthPage:

 call HideMouse
 mov dx, offset Filename_FourthInst
 call ShortBmp
 call ShowMouse
 call Delay
 jmp InstructionsLoop


@@ExitProc:
call Delay

	 add sp, 2
	 pop bp

	 ret

endp GameInstructionsPages

;================
; 	Delay - wait 0.2 s
;===============
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
;Proc fits BMP's with the size of 320*200
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
;Check given point position on buttons
;--------------------------------------------
;Input:
;1- Current X [MouseX -> cx (shr cx, 1)]
;2- Current Y [MouseY -> dx]
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
;current point checked [mouse values etc]
currentX equ [bp + 14]
currentY equ [bp + 12]
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

@@Rows_Check:

 ;current pos bigger than button edge
 mov ax, currentX
 ;check if currentX checked is in given row range
 cmp ax, rightCol
 ja @@ExitProc
 cmp ax, leftCol
 jb @@ExitProc

@@Col_Check:

 mov ax, currentY
 ;check if currentY checked is in given col range
 cmp ax, topRow
 jb @@ExitProc
 cmp ax, bottomRow
 ja @@ExitProc

 mov [Bool], 1


@@ExitProc:

 pop ax
 pop bp

 ret 12

endp isInRange

;======================
;start screen dispaly
;=====================
proc StratScreen_OPEN

	 mov dx, offset Filename_StartScreen
   call ShortBmp

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
