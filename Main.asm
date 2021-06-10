IDEAL

MODEL small
STACK 256h
jumps
p186

include "C:\TASM\Proj\code\OpenEqu.asm"
include "C:\TASM\Proj\code\GameEqu.asm"

DATASEG

include "C:\TASM\Proj\code\OpenData.asm"
include "C:\TASM\Proj\code\GameData.asm"


CODESEG

  ORG 100h
Start:
 mov ax, @data
 mov ds,ax


 call stratGraphicMode

 mov ax,0h ;initilaize mouse
 int 33h

Main:

 call OpenScreen ;open screen display
 cmp [Play], 1 ;check- exit by Play button?
 jne EXIT

 call Game ;game play display
 call hideMouse
 call Delay
 call Delay
 jmp Main

EXIT:

  call finishGraphicMode
  mov ax, 4C00h ; returns control to dos
  int 21h

include "C:\TASM\Proj\code\OpenProc.asm"
include "C:\TASM\Proj\code\GameProc.asm"

END Start
