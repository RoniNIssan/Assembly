FILENAME_GAME_DISPLAY equ 'C:\TASM\Proj\files\Game.bmp'
FILENAME_PACMAN_NORTH equ 'C:\TASM\Proj\files\PN.bmp'
FILENAME_PACMAN_SOUTH equ 'C:\TASM\Proj\files\PS.bmp'
FILENAME_PACMAN_EAST equ 'C:\TASM\Proj\files\PE.bmp'
FILENAME_PACMAN_WEST equ 'C:\TASM\Proj\files\PW.bmp'
FILENAME_WIN equ 'C:\TASM\Proj\files\win.bmp'
FILENAME_LOSE equ 'C:\TASM\Proj\files\Lose.bmp'
FILENAME_GAMEOVER equ 'C:\TASM\Proj\files\GameOver.bmp'

;Pacman maze
START_POS_X = 86
START_POS_Y = 146
DEFAULT_DIRECTION = 'A'

;Pacman figure
FILE_ROWS_PACMAN = 9
FILE_COLS_PACMAN = 9

;Maze
FILE_ROWS_MAZE = 200
FILE_COLS_MAZE = 320

;Maze colors
BLUE_BOUNDARY_COLOR = 0FCh
YELLOW_DOTS_COLOR_1 = 07Fh
YELLOW_DOTS_COLOR_2 = 0FBh

;Quit Banner values
QUIT_RIGHT_COL_GAME = 313
QUIT_LEFT_COL_GAME = 289
QUIT_TOP_ROW_GAME = 9
QUIT_BOTTOM_ROW_GAME = 33

;Maze values
MAZE_RIGHT_BOUNDARY_X = 167
MAZE_LEFT_BOUNDARY_X = 13
MAZE_RIGHT_EDGE_X = 172
MAZE_LEFT_EDGE_X = 8

NEXT_POS_ADDED_PIXELS_Y = 7
NEXT_POS_ADDED_PIXELS_X = 7

;Needed when turn:
DISTANCE_FROM_BOUNDARY_X = 2; when moving on Y - distanc1e between pacman and boundary
DISTANCE_FROM_BOUNDARY_Y = 2; when moving on X - distance between  pacman and boundary


;----- Equates Timeer
ticks		EQU	18
BIOSData	EQU	040h
LowTimer	EQU	006Ch
PIC8259		EQU	0020h
EOI		    EQU	0020h

TIMEOUT = 90
;cursor pos
TIME_ROW = 46
TIME_COL = 20


;Score
SCORE_ADDED_POINTS = 10
;cursor pos
SCORE_ROW = 36
SCORE_COL = 20
