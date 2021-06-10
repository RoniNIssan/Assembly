;============
;	game
;===========
proc Game

  call Delay
  call hideMouse
  call restoreGameDis
  call StratScreen_Game
  call Timer
  ;Show pacman figure according tp direction and Ticurrent Position
  push [pacmanCurrentDirection]
  push [pacmanX]
  push [pacmanY]
  call PacmanFigureDisplay

 call showMouse
 cmp [isMouseOn],1

;update mouse start positiob
 call GetMousePos
 shr cx, 1
 mov [MouseX], cx
 mov [MouseY], dx


MainLoop:

  call ScoreDisplay ;present score on screen

  ;time exit check
  ;if time is up -> exit
  cmp [cs:isTimeUp], 1
  je @@TimesUp


  ;Absorb mouse position
  call GetMousePos
	shr cx, 1
  ;if mouse current pos changed, show mouse
  ;cmp with previous values
  cmp cx, [MouseX]
  jne ShowMouseWhenMoved
  cmp dx, [MouseY]
  jne ShowMouseWhenMoved
  cmp [isMouseOn],1
  jne MouseContinue

  ;wait a while till turning off mouse
  call Delay
  call hideMouse
  mov [isMouseOn], 0
  jmp MouseContinue

ShowMouseWhenMoved:
  ;if mouse posiition had changed., present it
  call showMouse
	mov [MouseX], cx
	mov [MouseY], dx
  mov [isMouseOn], 1

MouseContinue:
  ;check if mouse is on quit button
  push [MouseX]
  push [MouseY]
	push QUIT_LEFT_COL_GAME
	push QUIT_RIGHT_COL_GAME
	push QUIT_TOP_ROW_GAME
	push QUIT_BOTTOM_ROW_GAME
	call isInRange ;returns value in bool
	cmp [Bool], 1
	jne continue

	cmp bx, 1 ;if mouse was pressed
  jne continue
  jmp @@Gameover

continue:

  ;if maze is empty from dots -> display game over
	push [pacmanY]
	push [pacmanX]
	call CheckFinish
	cmp [isScoreExits],0
	je @@Win

  ;check if keyboard key available
 	 mov ah, 1
 	 int 16h
	 jz MainLoop ;not available -> check mouse new pos

   ;get key value present new direction
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
	 je East
	 cmp al, 'd'
	 je East

	 cmp al, 'A'
	 je West
	 cmp al, 'a'
	 je West

	 jne MainLoop
North:

;proc color pacman in black
 push [pacmanX]
 push [pacmanY]
 call removePacman

 mov [pacmanCurrentDirection], 'W' ;update direction

;calculate score according to next eaten spot
 push [pacmanY]
 push [pacmanX]
 call AddScore_North

;find next pos
 push [pacmanY]
 push [pacmanX]
 call FindNextAddedY_North
 pop [pacmanY] ;new value tranformed into varible

;display figure in next position
 push [pacmanCurrentDirection]
 push [pacmanX]
 push [pacmanY]
 call PacmanFigureDisplay

 jmp MainLoop


South:
;proc color pacman in black
 push [pacmanX]
 push [pacmanY]
 call removePacman

 mov [pacmanCurrentDirection], 'S';update direction

 ;calculate score according to next eaten spot
 push [pacmanY]
 push [pacmanX]
 call AddScore_South

 ;find next pos
 push [pacmanY]
 push [pacmanX]
 call FindNextAddedY_South
 pop [pacmanY] ;next value tranformed into varible

;present figure in new position
 push [pacmanCurrentDirection]
 push [pacmanX]
 push [pacmanY]
 call PacmanFigureDisplay

 jmp MainLoop

East:
;proc color pacman in black
 push [pacmanX]
 push [pacmanY]
 call removePacman

 mov [pacmanCurrentDirection], 'D' ;update direction

 ;calculate score according to next eaten spot
 push [pacmanX]
 push [pacmanY]
 call AddScore_East

 ;find next pos
 push [pacmanX]
 push [pacmanY]
 call FindNextAddedX_East
 pop [pacmanX] ;next value tranformed into vvatible

;present figure in new position
 push [pacmanCurrentDirection]
 push [pacmanX]
 push [pacmanY]
 call PacmanFigureDisplay

 jmp MainLoop

West:

;proc color pacman in black
 push [pacmanX]
 push [pacmanY]
 call removePacman

 mov [pacmanCurrentDirection], 'A' ;update direction

 ;calculate score according to next eaten spot
 push [pacmanX]
 push [pacmanY]
 call AddScore_West

 ;find next pos
 push [pacmanX]
 push [pacmanY]
 call FindNextAddedX_West
 pop [pacmanX] ;next value tranformed into varible

;display figure in new position
 push [pacmanCurrentDirection]
 push [pacmanX]
 push [pacmanY]
 call PacmanFigureDisplay

 jmp MainLoop

;if time is up- present screen and exit
@@TimesUp:
  call Delay
  call Delay
  call Delay
  call Delay
  call hideMouse
  call EndTimer
	call TimesUpDisplay
  call ShowMouse
  call Delay
  jmp @@ExitProc

;if win- present screen and exit
@@Win:
  call EndTimer
  call Delay
  call Delay
  call Delay
  call Delay
  call hideMouse
	call WinDisplay
  call ShowMouse
  call Delay
  jmp @@ExitProc

;if exit button pressed- present screen and exit
@@Gameover:
  call EndTimer
  call Delay
  call hideMouse
	call GameoverDisplay
  call ShowMouse
  call Delay
  ;jmp @@ExitProc

@@ExitProc:
  call showMouse
  mov [play],0 ;update -> no longer in game
  call EndTimer
	ret

endp Game
;=============================================
;Check if win or loose
;--------------------------------------------
;Input:
;1- Pacman's current X
;2- Pacman's current Y
;Stack inputs:
;--------------------------------------------
;Registers:
; ax, bx, cx, dx, si, di, bp
;--------------------------------------------
;Output:
;varible isScoreExits 1 true [loose]/ 0 false
;=============================================
curX equ [word bp + 4] ;left col
curY equ [word bp + 6] ;top row
rightCol equ [word bp - 2]
bottomRow equ [word bp - 4]

proc CheckFinish

  push bp
  mov bp, sp
  sub sp, 4

  mov [isScoreExits],0 ;varible present wether score left

;pacman edges values:
mov ax, curX
mov rightCol, FILE_COLS_PACMAN
add rightCol, ax

mov ax, curY
mov bottomRow, FILE_ROWS_PACMAN
add bottomRow, ax

mov di, MAZE_RIGHT_EDGE_X - MAZE_LEFT_EDGE_X ;cols counter
mov si, 200 ;max Y value
mov dx, 0 ;min Y value


Rows:
  mov cx, MAZE_LEFT_EDGE_X ;min X value
	mov di, MAZE_RIGHT_BOUNDARY_X ;max X value
Cols:
	; ◄■■ Get pixel color of X&Y
	mov ah,0Dh
	int 10H ; AL = COLOR
	cmp al, YELLOW_DOTS_COLOR_1
	je writeYellow
	cmp al, YELLOW_DOTS_COLOR_2
	je writeYellow

cont:

	inc cx ;x pointer
	dec di ;loop counter

	cmp di, 0;cols check finished
	jne Cols

	inc dx ;increase y counter
	dec si ;decrese row loop counter
	cmp si, 0; row check finished
	jne Rows

	jmp done

writeYellow:
    ;find if current check refers to pacman -if so skip
    ;is the yellow in pacman's range
    cmp cx, curX
    jb yesYellow
    cmp cx, rightCol
    ja yesYellow
    cmp dx, curY
    jb yesYellow
    cmp dx, bottomRow
    ja yesYellow

SkipPacman:
		jmp cont

yesYellow:
    mov [isScoreExits], 1 ;score appears in maze- no win
    jmp done

done:

    add sp, 4
    pop bp
    ret 4
endp CheckFinish

;======================
;start screen dispaly
;=====================
proc StratScreen_Game

	 mov dx, offset Filename_Maze
   call ShortBmp
	 ret

endp StratScreen_Game

;======================
;time's up screen dispaly
;=====================
proc TimesUpDisplay

	 mov dx, offset Filename_Lose_Dis
   call ShortBmp
	 ret

endp TimesUpDisplay
;======================
;game over screen dispaly
;=====================
proc GameoverDisplay

	 mov dx, offset Filename_GameOver_Dis
   call ShortBmp
	 ret

endp GameoverDisplay
;======================
;win screen dispaly
;=====================
proc WinDisplay

	 mov dx, offset Filename_Win_Screen
   call ShortBmp
	 ret

endp WinDisplay
;============================================
;restore pacman values before restarting game
;============================================
proc restoreGameDis

  mov [pacmanX], START_POS_X
  mov [pacmanY], START_POS_Y
  mov [pacmanCurrentDirection], DEFAULT_DIRECTION
  mov [score], 0
  mov [isMouseOn], 1

  ;clear keyboard buffer
  mov ah,0ch
  mov al,0
  int 21h

  ret
endp restoreGameDis

;======================
;Timer initalizing
;=====================
proc Timer
	 mov	ax,@data
   mov	es,ax
   mov	[word cs:difference],ticks

	 ; save the current interrupt verctor.
	 ; the timer interrupt number is 1C
 	 push	es
 	 mov	ax, 351Ch
 	 int	21h
 	 mov	[timerSeg],es
 	 mov	[timerOfs],bx
 	 pop	es

	 ;set the new inerrupt vector with our function
	 push	ds
	 mov	ax,251Ch
	 push	cs
	 pop	ds
	 mov	dx, offset PrintSecondsElapse
	 int	21h
	 pop	ds
	 ret

endp Timer
;============
;	End Timer
;===========
proc EndTimer

;restore time values
mov [cs: inProgress], 0
mov [cs:difference], 0
mov [cs:lastTimer], 0
mov [cs:fixDrift], 5
mov [cs:counter], 0
mov [cs:isTimeUp], 0

;restore interrupt
push	ds
mov	ax,251Ch
mov	dx,[timerOfs]
mov	ds,[timerSeg]
int	21h
pop	ds

ret

endp EndTimer
;======================
;Score Display
;=====================
proc ScoreDisplay

	 push SCORE_ROW ;cursor position
	 push SCORE_COL
	 mov ax, [score]
	 call printAxDec
	 ret

endp ScoreDisplay
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
currentX equ [word bp + 6]
currentY equ [word bp + 4]
nextX equ [word bp - 8]
saveX equ [word bp - 10]

proc FindNextAddedX_West

push bp
mov bp, sp

push ax
push dx
push cx

sub sp, 4

;nextX is the defualt value of currentX in the next step
mov ax, currentX
mov nextX, ax
sub nextX, NEXT_POS_ADDED_PIXELS_X

;saveX value which changes according to boundary check
mov cx, currentX
mov saveX, cx

@@IsTouchingBoundray:
  ;===============================================
  ; loop checks when pacman meets boundary by:
  ;1. one added pixel forward
  ;2. pacman pront col
  ;when color is equal to boundary -> return smallest X
  ;==============================================

  mov dx, currentY
  mov cx, FILE_COLS_PACMAN ;check of front col

  @@CheckByYs:
  push cx ;save counter value

  ;dx value is initilaized before inner loop
  mov cx, saveX
  mov ah,0Dh
	int 10h ;absorb color
	cmp al, BLUE_BOUNDARY_COLOR
	je @@PopStack

  pop cx
  inc dx ;raise col pointer

  loop @@CheckByYs

  ;stop counting if next value smaller than next defualt value
  mov cx, saveX
	cmp cx, nextX
	jbe @@FindMinSteps

	dec saveX ;decrease nextX counter
	jmp @@IsTouchingBoundray

@@PopStack:
  pop cx

@@FindMinSteps:
  ;find the minimum steps forward
  mov cx, saveX
	inc cx
	cmp cx, nextX
	jb @@CheckAddedSteps

@@CountedSteps:

mov nextX, cx ;cx value is next X value

@@CheckAddedSteps:

;calc the distance from boundary
mov cx, currentX
sub cx, nextX

;Check if the difference between current pos to next sticks pacman to boundary
;if so, move the pacman to the proper distance from boundary.
cmp cx, DISTANCE_FROM_BOUNDARY_X
jnae @@ExitProc

add nextX, DISTANCE_FROM_BOUNDARY_X

@@CheckEdge:
  ;lanuch pacman to the other side
	cmp nextX, MAZE_LEFT_EDGE_X
	jnbe @@ExitProc

	mov nextX, MAZE_RIGHT_EDGE_X - FILE_COLS_PACMAN

@@ExitProc:

;save nextX in top Stack cell
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
currentX equ [word bp + 6]
currentY equ [word bp + 4]
nextX equ [word bp - 8]
saveX equ [word bp - 10]
normalizedX equ [word bp - 12]

proc FindNextAddedX_East

push bp
mov bp, sp

push ax
push dx
push cx

sub sp, 6

;Containing next defualt value
;CurrentX is top left pacman point -> dosen't present the east muserments properly
;normalizedX is the top left pacman X value
mov ax, currentX
mov normalizedX, ax
add normalizedX, FILE_COLS_PACMAN

;nextX is the defualt value of currentX in the next step
mov ax, normalizedX
mov nextX, ax
add nextX, NEXT_POS_ADDED_PIXELS_X

;saveX value which changes according to boundary check
mov cx, normalizedX
mov saveX, cx

@@IsTouchingBoundray:
;===============================================
; loop checks when pacman meets boundary by:
;1. one added pixel forward
;2. pacman pront col
;when color is equal to boundary -> return smallest X
;==============================================

  mov dx, currentY
  mov cx, FILE_COLS_PACMAN;check of front col

  @@CheckByYs:
  push cx ;save counter value

  ;dx value is initilaized before inner loop
  mov cx, saveX
  mov ah,0Dh
	int 10h ;absorb color
  cmp al, BLUE_BOUNDARY_COLOR
  je @@PopStack

  pop cx
  inc dx ;decrease col pointer

  loop @@CheckByYs

  ;stop counting if next value bigger than next defualt value
  mov cx, saveX
	cmp cx, nextX
	jae @@FindMinSteps

	inc saveX;decrease nextX counter
	jmp @@IsTouchingBoundray

@@PopStack:
  pop cx
@@FindMinSteps:
;find the minimum steps forward
  mov cx, saveX ;restore x value
	dec cx
	cmp cx, nextX
	ja @@CheckAddedSteps

@@CountedSteps:

	mov nextX, cx ;cx value is next X value

@@CheckAddedSteps:
;calc the distance from boundary
mov cx, nextX
sub cx, normalizedX

;Check if the difference between current pos to next sticks pacman to boundary
;if so, move the pacman to the proper distance from boundary.
cmp cx, DISTANCE_FROM_BOUNDARY_X
jnae @@ExitProc

sub nextX, DISTANCE_FROM_BOUNDARY_X

@@CheckEdge:
;lanuch pacman to the other side
	cmp nextX, MAZE_RIGHT_EDGE_X
	jnae @@ExitProc

	mov nextX, MAZE_LEFT_EDGE_X + FILE_COLS_PACMAN

@@ExitProc:
;restore normalized value
sub nextX, FILE_COLS_PACMAN
;save nextX in top Stack cell
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
currentY equ [word bp + 6]
currentX equ [word bp + 4]
nextY equ [word bp - 8]
saveX equ [word bp - 10]
normalizedY equ [word bp - 12]

proc FindNextAddedY_South

push bp
mov bp, sp

push ax
push dx
push cx

sub sp, 6

;presents bottom row of pacman [instead of top]
mov ax, currentY
mov normalizedY, ax
add normalizedY, FILE_ROWS_PACMAN

;defualt next Y value
mov ax, normalizedY
mov nextY, ax
add nextY, NEXT_POS_ADDED_PIXELS_Y


;initalizing dx value
mov dx, normalizedY

@@IsTouchingBoundray:
;===============================================
; loop checks when pacman meets boundary by:
;1. one added pixel forward
;2. pacman pront row
;when color is equal to boundary -> return smallest Y
;==============================================

;default checked X value
mov cx, currentX
mov saveX, cx

mov cx, FILE_COLS_PACMAN;check of front col
@@CheckByXs:
push cx ;save counter value

;dx y value already saved
mov cx, saveX
mov ah,0Dh
int 10h;absorb color
cmp al, BLUE_BOUNDARY_COLOR
je @@PopStack

inc saveX  ;raise row pointer
pop cx

loop @@CheckByXs

;stop counting if next value bigger than next defualt value
cmp dx, nextY
jae @@FindMinSteps

inc dx
jmp @@IsTouchingBoundray

@@PopStack:
pop cx

@@FindMinSteps:
  ;find minimum steps
		dec dx
		cmp dx, nextY
		ja @@CheckAddedSteps

@@CountedSteps:

		mov nextY, dx ;dx value is next Y value

@@CheckAddedSteps:
;Check if the difference between current pos to next sticks pacman to boundary
;if so, move the pacman to the proper distance from boundary.
	mov dx, nextY
	sub dx, normalizedY

	cmp dx, DISTANCE_FROM_BOUNDARY_Y
	jnae @@ExitProc

	sub nextY, DISTANCE_FROM_BOUNDARY_Y

@@ExitProc:
  ;restore not normalized pacman values
	sub nextY, FILE_COLS_PACMAN
  ;save next Y value in top stack cell
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
currentY equ [word bp + 6]
currentX equ [word bp + 4]
nextY equ [word bp - 8]
saveX equ [word bp - 10]

proc FindNextAddedY_North

  push bp
  mov bp, sp

  push ax
  push dx
  push cx

  sub sp, 4

  ;defualt next Y value
  mov ax, currentY
  mov nextY, ax
  sub nextY, NEXT_POS_ADDED_PIXELS_Y

  ;initalizing dx value
  mov dx, currentY

@@IsTouchingBoundray:
;===============================================
; loop checks when pacman meets boundary by:
;1. one added pixel forward
;2. pacman pront row
;when color is equal to boundary -> return smallest Y
;==============================================
  ;default checked X value
  mov cx, currentX
  mov saveX, cx

  mov cx, FILE_COLS_PACMAN ;check of front col
  @@CheckByXs:
  push cx ;save counter value

  ;dx y value already saved
  mov cx, saveX
  mov ah,0Dh
  int 10h ;absorb color
  cmp al, BLUE_BOUNDARY_COLOR
  je @@PopStack

  inc saveX ;raise row pointer
  pop cx

  loop @@CheckByXs

  ;stop counting if next value smaller than next defualt value
  cmp dx, nextY
  jbe @@FindMinSteps

  dec dx ;decrease Y value
  jmp @@IsTouchingBoundray

@@PopStack:
  pop cx

@@FindMinSteps:
;find minimum steps
		inc dx
		cmp dx, nextY
		jb @@CheckAddedSteps

@@CountedSteps:

		mov nextY, dx ;dx value is next Y value

@@CheckAddedSteps:
;Check if the difference between current pos to next sticks pacman to boundary
;if so, move the pacman to the proper distance from boundary.
	mov dx, currentY
	sub dx, nextY

	cmp dx, DISTANCE_FROM_BOUNDARY_Y
	jnae @@ExitProc

	add nextY, DISTANCE_FROM_BOUNDARY_Y

@@ExitProc:

	mov dx, nextY
	mov currentY, dx
  ;save next Y value in top stack cell

  add sp, 4

  pop cx
  pop dx
  pop ax
  pop bp

  ret 2

endp FindNextAddedY_North

;=============================================
;Changes score.
;--------------------------------------------
;Input:
;1- CurrentXPos
;2- CurrentYPos
;--------------------------------------------
;Registers:
;bp, cx, dx, ax
;--------------------------------------------
;Output:
;score - global varible
;=============================================
currentX equ [word bp + 6]
currentY equ [word bp + 4]
nextX equ [word bp - 8]
saveX equ [word bp - 10]

proc AddScore_West

	push bp
	mov bp, sp

	push ax
	push dx
	push cx

	sub sp, 4

	push currentX
	push currentY
	call FindNextAddedX_West
	pop nextX

  mov cx, currentX
  mov saveX, cx

@@FindNextDot:

  mov dx, currentY
  mov cx, FILE_COLS_PACMAN ;check of front col

  @@CheckByYs:
  push cx ;save counter value

  ;dx value is initilaized before inner loop
  mov cx, saveX
  mov ah,0Dh
  int 10h ;absorb color
  ;each meeting point increases points
	cmp al, YELLOW_DOTS_COLOR_1
  je @@PopStack
	cmp al, YELLOW_DOTS_COLOR_2
  je @@PopStack

  pop cx
  inc dx ;raise col pointer

  loop @@CheckByYs

  ;stop counting if next value smaller than next defualt value
  mov cx, saveX
  cmp cx, nextX
  jbe @@ExitProc

  dec saveX ;decrease nextX counter
  jmp @@FindNextDot

@@PopStack:
  pop cx
@@IncScore:
  ;inc score by defualt val
	add [score], SCORE_ADDED_POINTS

@@ExitProc:

	add sp, 4

	pop cx
	pop dx
	pop ax

	pop bp

	ret 4

endp AddScore_West
;=============================================
;Changes score.
;--------------------------------------------
;Input:
;1- CurrentXPos
;2- CurrentYPos
;--------------------------------------------
;Registers:
;bp, cx, dx, ax
;--------------------------------------------
;Output:
;score - global varible
;=============================================
currentX equ [word bp + 6]
currentY equ [word bp + 4]
nextX equ [word bp - 8]
saveX equ [word bp - 10]
normalizedX equ [word bp - 12]

proc AddScore_East

  push bp
  mov bp, sp

  push ax
  push dx
  push cx

  sub sp, 6

  ;Containing next defualt value
  ;CurrentX is top left pacman point -> dosen't present the east muserments properly
  mov ax, currentX
  mov normalizedX, ax
  add normalizedX, FILE_COLS_PACMAN

  push currentX
  push currentY
  call FindNextAddedX_East
  pop nextX
  add nextX, FILE_COLS_PACMAN

  mov cx, normalizedX
  mov saveX, cx

  @@FindNextDot:

  mov dx, currentY
  mov cx, FILE_COLS_PACMAN ;check of front col

  @@CheckByYs:
  push cx ;save counter value

  ;dx value is initilaized before inner loop
  mov cx, saveX
  mov ah,0Dh
  int 10h ;absorb color
  ;each meeting point increases points
  cmp al, YELLOW_DOTS_COLOR_1
  je @@PopStack
  cmp al, YELLOW_DOTS_COLOR_2
  je @@PopStack

  pop cx
  inc dx ;raise col pointer

  loop @@CheckByYs

  ;stop counting if next value smaller than next defualt value
  mov cx, saveX
  cmp cx, nextX
  jae @@ExitProc

  inc saveX ;increase nextX counter
  jmp @@FindNextDot

  @@PopStack:
  pop cx

  @@IncScore:

  add [score], SCORE_ADDED_POINTS

  @@ExitProc:

  add sp, 6

  pop cx
  pop dx
  pop ax
  pop bp

  ret 4

endp AddScore_East
;=============================================
;Changes score.
;--------------------------------------------
;Input:
;1- CurrentXPos
;2- CurrentYPos
;--------------------------------------------
;Registers:
;bp, cx, dx, ax
;--------------------------------------------
;Output:
;score - global varible
;=============================================
currentY equ [word bp + 6]
currentX equ [word bp + 4]
nextY equ [word bp - 8]
saveX equ [word bp - 10]
normalizedY equ [word bp - 12]

proc AddScore_South

	push bp
	mov bp, sp

	push ax
	push dx
	push cx

	sub sp, 6

	mov ax, currentY
	mov normalizedY, ax
	add normalizedY, FILE_ROWS_PACMAN
  ;for calculation of dots betweeen cur pos to next
	push currentY
	push currentX
	call FindNextAddedY_South
	pop nextY
	add nextY, FILE_ROWS_PACMAN

  mov dx, normalizedY
@@FindNextDot:

  ;default checked X value
  mov cx, currentX
  mov saveX, cx

  mov cx, FILE_COLS_PACMAN;check of front col
  @@CheckByXs:
  push cx ;save counter value

  ;dx y value already saved
  mov cx, saveX
  mov ah,0Dh
  int 10h ;absorb color
  ;each meeting point increases points
	cmp al, YELLOW_DOTS_COLOR_1
  je @@PopStack
	cmp al, YELLOW_DOTS_COLOR_2
  je @@PopStack

  inc saveX  ;raise row pointer
  pop cx

  loop @@CheckByXs

	cmp dx, nextY
	jae @@ExitProc

	inc dx
	jmp @@FindNextDot

@@PopStack:
  pop cx

@@IncScore:
	add [score], SCORE_ADDED_POINTS

@@ExitProc:

	add sp, 6

	pop cx
	pop dx
	pop ax
	pop bp

	ret 4

endp AddScore_South
;=============================================
;Changes score.
;--------------------------------------------
;Input:
;1- CurrentXPos
;2- CurrentYPos
;--------------------------------------------
;Registers:
;bp, cx, dx, ax
;--------------------------------------------
;Output:
;score - global varible
;=============================================
currentY equ [word bp + 6]
currentX equ [word bp + 4]
nextY equ [word bp - 8]
saveX equ [word bp - 10]

proc AddScore_North

	push bp
	mov bp, sp

	push ax
	push dx
	push cx

	sub sp, 4

  ;for calculation of dots betweeen cur pos to next
	push currentY
	push currentX
	call FindNextAddedY_North
	pop nextY

  ;initalizing dx value
  mov dx, currentY

@@FindNextDot:
  ;default checked X value
  mov cx, currentX
  mov saveX, cx

  mov cx, FILE_COLS_PACMAN ;check of front col
  @@CheckByXs:
  push cx ;save counter value

  ;dx y value already saved
  mov cx, saveX
  mov ah,0Dh
  int 10h ;absorb color
  ;each meeting point increases points
	cmp al, YELLOW_DOTS_COLOR_1
  je @@PopStack
	cmp al, YELLOW_DOTS_COLOR_2
  je @@PopStack

  inc saveX ;raise row pointer
  pop cx

  loop @@CheckByXs

	cmp dx, nextY
	jbe @@ExitProc

	dec dx ;continue loop
	jmp @@FindNextDot

@@PopStack:
  pop cx
@@IncScore:

	add [score], SCORE_ADDED_POINTS

@@ExitProc:

	add sp, 4

	pop cx
	pop dx
	pop ax
	pop bp

	ret 4

endp AddScore_North
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
currentX equ [word bp + 6]
currentY equ [word bp + 4]

proc removePacman

	 push bp
	 mov bp, sp

	 push cx ; save cx
   lea cx, [pacmanBlank] ;save matrix in varible
   mov [matrix] , cx

	 push dx; save dx
   mov dx, FILE_COLS_PACMAN ;define matrix size
   mov cx, FILE_ROWS_PACMAN

	 push di
	 push ax

   ;calculate coloring position
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

	dirction equ [word bp + 8]
	xPos equ [word bp + 6]
	yPos equ [word bp + 4]

proc PacmanFigureDisplay

	 push bp
	 mov bp, sp

	 push dx
	 push ax

;print pacman according to dirction and X,Y pos

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
isTimeUp    db  0

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
		cmp ax, TIMEOUT
		ja @@TimeoutEnd

		push TIME_ROW
		push TIME_COL
    call printAxDec
    inc [cs:counter]
		jmp @@20

@@TimeoutEnd:
	mov [cs:isTimeUp], 1
  ;TODO: display Time is over

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
	;cursor pos values
	col equ [bp + 4]
	row equ [bp + 6]

PROC printAxDec
		 push bp
		 mov bp, sp
	   push ax
     push bx
	   push dx
	   push cx

		 push ax

		 mov ax, col
		 mov  dl, al   ;Column
		 mov ax, row
		 mov  dh, al   ;Row
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
		 pop bp
     ret 4
endp printAxDec
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
