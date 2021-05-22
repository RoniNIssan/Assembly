IDEAL

MODEL small
STACK 100h


DATASEG

  include "C:\TASM\Proj\displayS\OpenS\Open.asm"

  include "C:\TASM\Proj\displayS\GameS\Game.asm"


CODESEG

  ORG 100h
Main:
 mov ax, @data
 mov ds,ax

 call stratGraphicMode
 call StratScreen_OPEN

 call OpenScreen
 cmp [Play], 1
 jne EXIT
 call StratScreen
 call Game


EXIT:

mov ax, 4C00h ; returns control to dos
int 21h

include "C:\TASM\Proj\displayS\OpenS\Open.asm"
include "C:\TASM\Proj\displayS\GameS\Game.asm"













END Main
