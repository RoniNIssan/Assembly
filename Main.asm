IDEAL

MODEL small
STACK 256h
jumps
p186

include "C:\TASM\Proj\displayS\OpenS\OpenEqu.asm"
include "C:\TASM\Proj\displayS\GameS\GameEqu.asm"

DATASEG

include "C:\TASM\Proj\displayS\OpenS\OpenData.asm"
include "C:\TASM\Proj\displayS\GameS\GameData.asm"


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
 jmp Main

 EXIT:

jmp Main

EXIT_END:

  call finishGraphicMode
  mov ax, 4C00h ; returns control to dos
  int 21h

include "C:\TASM\Proj\displayS\OpenS\OpenProc.asm"
include "C:\TASM\Proj\displayS\GameS\GameProc.asm"

END Start
