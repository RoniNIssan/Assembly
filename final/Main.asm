IDEAL

MODEL small
STACK 256h
jumps
p186

include "OpenEqu.asm"
include "GameEqu.asm"

DATASEG

include "OpenData.asm"
include "GameData.asm"


CODESEG

  ORG 100h
Start:
 mov ax, @data
 mov ds,ax


 call stratGraphicMode

Main:
 call OpenScreen ;open screen display
 cmp [Play], 1 ;check- exit by Play button?
 jne EXIT_END

 mov	es,ax
 mov	[word cs:difference],ticks

 call Game ;game play display
 call hideMouse
 call GameOverDisplay
 call Delay
 call Delay
 call ShowMouse
 jmp Main

EXIT:

;for future development of timer exit
call EndTimer
;call Delay
jmp Main
EXIT_END:

  call finishGraphicMode
  mov ax, 4C00h ; returns control to dos
  int 21h

include "OpenProc.asm"
include "GameProc.asm"

END Start
