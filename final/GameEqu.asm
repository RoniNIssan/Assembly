FILENAME_GAME_DISPLAY equ 'Game.bmp'
FILENAME_PACMAN_NORTH equ 'PN.bmp'
FILENAME_PACMAN_SOUTH equ 'PS.bmp'
FILENAME_PACMAN_EAST equ 'PE.bmp'
FILENAME_PACMAN_WEST equ 'PW.bmp'
FILENAME_WIN equ 'win.bmp'
FILENAME_LOOSE equ 'loose.bmp'

;Paman
START_POS_X = 86
START_POS_Y = 146
DEFAULT_DIRECTION = 'D'

;Maze
FILE_ROWS_MAZE = 200
FILE_COLS_MAZE = 320

;Maze colors
BLUE_BOUNDARY_COLOR = 0FCh
YELLOW_DOTS_COLOR_1 = 07Fh
YELLOW_DOTS_COLOR_2 = 0FBh

;Pacman figure
FILE_ROWS_PACMAN = 9
FILE_COLS_PACMAN = 9

;Quit Banner values
QUIT_RIGHT_COL_GAME = 313
QUIT_LEFT_COL_GAME = 289
QUIT_TOP_ROW_GAME = 9
QUIT_BOTTOM_ROW_GAME = 33

;Pacman values
MAZE_RIGHT_BOUNDARY_X = 167
MAZE_LEFT_BOUNDARY_X = 13

NEXT_POS_ADDED_PIXELS_Y = 7
NEXT_POS_ADDED_PIXELS_X = 7

PACMAN_MIDDLE_X_PIXLE = (FILE_COLS_PACMAN - 1) / 2 + 1
PACMAN_MIDDLE_Y_PIXLE_WEST = (FILE_ROWS_PACMAN - 1) / 2 + 1

;Needed when turn:
DISTANCE_FROM_BOUNDARY_X = 2; when moving on Y - distanc1e between pacman and boundary
DISTANCE_FROM_BOUNDARY_Y = 2; when moving on X - distance between  pacman and boundary
MAZE_RIGHT_EDGE_X = 172
MAZE_LEFT_EDGE_X = 8


;----- Equates Timeer
ticks		EQU	18
BIOSData	EQU	040h
LowTimer	EQU	006Ch
PIC8259		EQU	0020h
EOI		    EQU	0020h

TIMEOUT = 70
TIME_ROW = 46
TIME_COL = 20


;Score
SCORE_ROW = 36
SCORE_COL = 20
SCORE_ADDED_POINTS = 10