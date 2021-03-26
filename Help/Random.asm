IDEAL
p286
MACRO NEW_LINE
	mov dl,13   ; CR = Caridge Return - go to row start position
	mov ah,2   
	int 21h
	mov dl,10   ;  LF = Line Feed - go down to the next line
	int 21h
ENDM LINE


MACRO RND
    mov ax, 40h
    mov es, ax
    mov ax, [es:6Ch]
    and ax, 7
ENDM


MODEL small
STACK 100h

DATASEG
	
	 RndCurrentPos dw start

		
CODESEG
    
start:		 
	 mov ax,@data
	 mov ds,ax
	 	 
; print 400 random numbers in range of 5-8
	 mov cx,400
ag:
	 mov bl,5
	 mov bh,8	 
	 call RandomByCs
	 
	 mov ah,0
	 call printAxDec
	  
	 mov dl,','
	 mov ah,2
	 int 21h
	  
	 loop ag

	 NEW_LINE


; print 100 random numbers in range of 1-1550
	mov cx,100
ag2:
	
	 mov bx,1
	 mov dx,1550 
	 call RandomByCsWord
	 
	  
	 call printAxDec
	  
	 mov dl,','
	 mov ah,2
	 int 21h
	 
     push 200
     call sleep_ms
    
	 loop ag2
	 
	
exit:
 	mov ax, 4c00h
	int 21h
	
	; put some data in Code segment in order to have enough bytes to xor with 
	SomeRNDData	    db 227	,111	,105	,1		,127
					db 234	,6		,116	,101	,220
					db 92	,60		,21		,228	,22
					db 222	,63		,216	,208	,146
					db 60	,172	,60		,80		,30
					db 23	,85		,67		,157	,131
					db 120	,111	,105	,49		,107
					db 148	,15		,141	,32		,225
					db 113	,163	,174	,23		,19
					db 143	,28		,234	,56		,74
					db 223	,88		,214	,122	,138
					db 100	,214	,161	,41		,230
					db 8	,93		,125	,132	,129
					db 175	,235	,228	,6		,226
					db 202	,223	,2		,6		,143
					db 8	,147	,214	,39		,88
					db 130	,253	,106	,153	,147
					db 73	,140	,251	,32		,59
					db 92	,224	,138	,118	,200
					db 244	,4		,45		,181	,62
					


; Description  : get RND between any bl and bh includs (max 0 -255)
; Input        : 1. Bl = min (from 0) , BH , Max (till 255)
; 			     2. RndCurrentPos a  word variable,   help to get good rnd number
; 				 	Declre it at DATASEG :  RndCurrentPos dw ,0
;				 3. EndOfCsLbl: is label at the end of the program one line above END start		
; Output:        Al - rnd num from bl to bh  (example 50 - 150)
; More Info:
; 	Bl must be less than Bh 
; 	in order to get good random value again and agin the Code segment size should be 
; 	at least the number of times the procedure called at the same second ... 
; 	for example - if you call to this proc 50 times at the same second  - 
; 	Make sure the cs size is 50 bytes or more 
; 	(if not, make it to be more) 
proc RandomByCs
    push es
	push si
	push di
	
	mov ax, 40h
	mov	es, ax
	
	sub bh,bl  ; we will make rnd number between 0 to the delta between bl and bh
			   ; Now bh holds only the delta
	cmp bh,0
	jz @@ExitP
 
	mov di, [word RndCurrentPos]
	call MakeMask ; will put in si the right mask according the delta (bh) (example for 28 will put 31)
	
RandLoop: ;  generate random number 
	mov ax, [es:06ch] ; read timer counter
	mov ah, [byte cs:di] ; read one byte from memory (from semi random byte at cs)
	xor al, ah ; xor memory and counter
	
	; Now inc di in order to get a different number next time
	inc di
	cmp di,(EndOfCsLbl - start - 1)
	jb @@Continue
	mov di, offset start
@@Continue:
	mov [word RndCurrentPos], di
	
	and ax, si ; filter result between 0 and si (the mask)
	cmp al,bh    ;do again if  above the delta
	ja RandLoop
	
	add al,bl  ; add the lower limit to the rnd num
		 
@@ExitP:	
	pop di
	pop si
	pop es
	ret
endp RandomByCs


; Description  : get RND between any bl and bh includs (max 0 -255)
; Input        : 1. BX = min (from 0) , DX, Max (till 64k -1)
; 			     2. RndCurrentPos a  word variable,   help to get good rnd number
; 				 	Declre it at DATASEG :  RndCurrentPos dw ,0
;				 3. EndOfCsLbl: is label at the end of the program one line above END start		
; Output:        AX - rnd num from bx to dx  (example 50 - 1550)
; More Info:
; 	BX  must be less than DX 
; 	in order to get good random value again and again the Code segment size should be 
; 	at least the number of times the procedure called at the same second ... 
; 	for example - if you call to this proc 50 times at the same second  - 
; 	Make sure the cs size is 50 bytes or more 
; 	(if not, make it to be more) 
proc RandomByCsWord
    push es
	push si
	push di
 
	
	mov ax, 40h
	mov	es, ax
	
	sub dx,bx  ; we will make rnd number between 0 to the delta between bl and bh
			   ; Now bh holds only the delta
	cmp dx,0
	jz @@ExitP
	
	push bx
	
	mov di, [word RndCurrentPos]
	call MakeMaskWord ; will put in si the right mask according the delta (bh) (example for 28 will put 31)
	
@@RandLoop: ;  generate random number 
	mov bx, [es:06ch] ; read timer counter
	
	mov ax, [word cs:di] ; read one word from memory (from semi random bytes at cs)
	xor ax, bx ; xor memory and counter
	
	; Now inc di in order to get a different number next time
	inc di
	inc di
	cmp di,(EndOfCsLbl - start - 2)
	jb @@Continue
	mov di, offset start
@@Continue:
	mov [word RndCurrentPos], di
	
	and ax, si ; filter result between 0 and si (the nask)
	
	cmp ax,dx    ;do again if  above the delta
	ja @@RandLoop
	pop bx
	add ax,bx  ; add the lower limit to the rnd num
		 
@@ExitP:
	
	pop di
	pop si
	pop es
	ret
endp RandomByCsWord

; make mask acording to bh size 
; output Si = mask put 1 in all bh range
; example  if bh 4 or 5 or 6 or 7 si will be 7
; 		   if Bh 64 till 127 si will be 127
Proc MakeMask    
    push bx

	mov si,1
    
@@again:
	shr bh,1
	cmp bh,0
	jz @@EndProc
	
	shl si,1 ; add 1 to si at right
	inc si
	
	jmp @@again
	
@@EndProc:
    pop bx
	ret
endp  MakeMask


Proc MakeMaskWord    
    push dx
	
	mov si,1
    
@@again:
	shr dx,1
	cmp dx,0
	jz @@EndProc
	
	shl si,1 ; add 1 to si at right
	inc si
	
	jmp @@again
	
@@EndProc:
    pop dx
	ret
endp  MakeMaskWord



; get RND between bl and bh includs
; output al - rnd num from bl to bh
; the distance between bl and bh  can't be greater than 100 
; Bl must be less than Bh 
proc RndBlToBh  ; by Dos  with delay
	push  cx
	push dx
	push si 


	mov     cx, 1h
	mov     dx, 0C350h
	mov     ah, 86h
	int     15h   ; Delay of 50k micro sec
	
	sub bh,bl
	cmp bh,0
	jz @@EndProc
	
	call MakeMask ; will put in si the right mask (example for 28 will put 31)
RndAgain:
	mov ah, 2ch   
	int 21h      ; get time from MS-DOS
	mov ax, dx   ; DH=seconds, DL=hundredths of second
	and ax, si  ;  Mask for Highst num in range  
	cmp al,bh    ; we deal only with al (0  to 100 )
	ja RndAgain
 	
	add al,bl

@@EndProc:
	pop si
	pop dx
	pop cx
	
	ret
endp RndBlToBh



	 
proc printAxDec  
	   
       push bx
	   push dx
	   push cx
	           	   
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
	   
       ret
endp printAxDec    


proc sleep_ms
delay_in_ms equ [bp+4]
	push bp
	mov bp, sp
    push ax
    push cx
    push dx

    mov ax, 1025
    mov cx, delay_in_ms
    mul cx
    mov cx, dx
    mov dx, ax
    mov ah, 86h
    int 15h
	
	pop dx
	pop cx	
	pop ax	

	pop bp
    ret	2
endp sleep_ms





EndOfCsLbl:
END start