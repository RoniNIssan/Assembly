	Filename_Maze db FILENAME_GAME_DISPLAY, 0
	Filename_PacmanNorth db FILENAME_PACMAN_NORTH, 0
	Filename_PacmanSouth db FILENAME_PACMAN_SOUTH, 0
	Filename_PacmanEast db FILENAME_PACMAN_EAST, 0
	Filename_PacmanWest db FILENAME_PACMAN_WEST, 0
	Filename_Lose_Dis db FILENAME_LOSE, 0
	Filename_Win_Screen db FILENAME_WIN, 0
	ScrLine db FILE_COLS_MAZE dup (0)  ; One Color line read buffer

	FileHandle	dw ?
	Header 	    db 54 dup(0)
	Palette 	db 400h dup (0)

	BmpFileErrorMsg    	db 'Error At Opening Bmp File ',FILE_COLS_PACMAN, 0dh, 0ah,'$'
	ErrorFile           db 0

	BmpLeft dw ?
	BmpTop dw ?
	BmpColSize dw ?
	BmpRowSize dw ?

	matrix dw ?

	;Current Position
	pacmanX dw START_POS_X
	pacmanY dw START_POS_Y


	pacmanCurrentDirection dw DEFAULT_DIRECTION

	;Boolean
	;TimeIsUp db 0
	isScoreExits db 0
	;Timer
	exitCode1	db	0
	timerSeg	dw	?
	timerOfs	dw	?

	;Score
	score dw 0
	countYellow dw 0

	pacmanBlank db	0,0,0,0,0,0,0
							db	0,0,0,0,0,0,0
							db	0,0,0,0,0,0,0
							db	0,0,0,0,0,0,0
							db	0,0,0,0,0,0,0
							db	0,0,0,0,0,0,0
							db	0,0,0,0,0,0,0
