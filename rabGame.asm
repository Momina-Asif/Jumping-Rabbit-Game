[org 0x0100]

jmp start


pcb: times 32*16 dw 0 ; space for 32 PCBs

stack: times 32*256 dw 0 ; space for 32 512 byte stacks

nextpcb: dw 1 ; index of next free pcb

current: dw 0 ; index of current pcb


buffer: times 40*320 dw 0


clrscr:
		mov ax, 0xb800 ; load video base in ax 
		mov es, ax ; point es to video base 
		mov di, 0 ; point di to top left column 
nextchar1: 
		mov word [es:di], 0x0720 ; clear next char on screen 
		add di, 2 ; move to next screen location 
		cmp di, 80*25*2 ; has the whole screen cleared 
		jne nextchar1
		ret
		 

delay:      push cx
			mov cx, 0xFFFF
			loop1: loop loop1
			mov cx, 0xFFFF
			loop2: loop loop2
			mov cx, 0xFFFF
			loop3: loop loop3
			mov cx, 0xFFFF
			loop4: loop loop4
			mov cx, 0xFFFF
			loop5: loop loop5
			mov cx, 0xFFFF
			loop6: loop loop6
			mov cx, 0xFFFF
			loop7: loop loop7
			mov cx, 0xFFFF
			loop8: loop loop8
			mov cx, 0xFFFF
			loop9: loop loop9
			mov cx, 0xFFFF
			loop10: loop loop10
			mov cx, 0xFFFF
			loop11: loop loop11
			mov cx, 0xFFFF
			loop12: loop loop12
			mov cx, 0xFFFF
			pop cx
			ret

delay2:     
		push cx
		mov cx, 0xFFFF
		loop13: loop loop13
		; mov cx, 0xFFFF
		; loop14: loop loop14

		pop cx
		ret
		

mdelay:     
		push bp
		mov bp, sp
		push cx
		
		
loop15:		
		mov cx, 0xFFFF
		loop14: loop loop14
		sub word[bp + 4], 1
		jnz loop15
		; mov cx, 0xFFFF
		; loop14: loop loop14

		pop cx
		pop bp
		ret	2
; ;--------------------------------------------------------------------
; ;;; KBISR
; ;--------------------------------------------------------------------
 kbisr:	
			
			push ds
			push ax
			push es
			push cs
			pop ds
			in al, 0x60 ; read a char from keyboard port
			
			cmp word[quitFlag], 1
			jz enterCheck
			cmp al, 0x01 ;esc scancode
			jne enterCheck

			call saveScreen
			mov al, 0
			out 61h, al	
			call PrintExitScreen
			
			mov word[quitFlag], 1

enterCheck:	

			cmp word [quitFlag], 1
			jne nextcmp
			cmp al, 49;28 ;enter scancode
			jne nextcmp1
			mov al, 0
			out 61h, al	
			call restoreScreen
			mov word [quitFlag], 0
			jmp exit;nomatch

nextcmp1:	
			cmp al, 21
			jne nextcmp 

			mov word[exitFlag], 1 ; esc pressed twice game exit
			jmp exit;nomatch
			
			mov word [quitFlag], 1
			jmp exit;nomatch
			
nextcmp:	
			cmp word[quitFlag], 1
			jz exit
			cmp al, 0x48; has the up  pressed
			jne nomatch;exit ; no, try next comparison
			mov al, 0
			out 61h, al	
			call Scrollup
			call CheckCordinates
			call checkCarrotCordinates
			add word[carrotCordinates], 25

			call delay
			call scrolldown
			call SettingCordinates
			call PrintNextBrick
			call PrintCarrot
			
			jmp exit;nomatch ; leave interrupt routine

nomatch:	
			
			pop es
			pop ax
			pop ds
			jmp far [cs:oldisr] ; call the original ISR
	
exit:		mov al, 0x20
			out 0x20, al ; send EOI to PIC
			pop es
			pop ax
			pop ds
			iret ; return from interrupt
; ;--------------------------------------------------------------------
; ;;; TIMER
; ;--------------------------------------------------------------------			


timer: 

		push ax
			push bx
			push dx
			push ds
			push cs
			pop ds
			cmp word [randNum], 0xFFFF
			jnz timerCheck2
			mov word [randNum], 0
timerCheck2:
			cmp word[quitFlag], 1
			jz timerExit
			mov word[showtime], 0
			; increment tick count
			inc word [randNum]
			
			cmp word[brickColor3], 3
			jnz timerReset
			inc word [tickcount]
			mov word[showtime], 1
			
			cmp  word [tickcount], 18
			jnz timerExit
			inc word [tickcount1]
			mov word [tickcount], 0
			
			cmp  word [tickcount1], 10
			jnz timerExit
			mov word[exitFlag], 1

timerReset:			
			mov word [tickcount1], 0
timerExit:	mov al, 0x20
			out 0x20, al ; end of interrupt
			pop ds
			pop dx
			pop bx
			pop ax
			

		push ds
		push bx
		push cs
		pop ds ; initialize ds to data segment
		
		
		
		
		mov bx, [current] ; read index of current in bx
		shl bx, 1
		shl bx, 1
		shl bx, 1
		shl bx, 1
		shl bx, 1 ; multiply by 32 for pcb start
		mov [pcb+bx+0], ax ; save ax in current pcb
		mov [pcb+bx+4], cx ; save cx in current pcb
		mov [pcb+bx+6], dx ; save dx in current pcb
		mov [pcb+bx+8], si ; save si in current pcb
		mov [pcb+bx+10], di ; save di in current pcb
		mov [pcb+bx+12], bp ; save bp in current pcb
		mov [pcb+bx+24], es ; save es in current pcb
		pop ax ; read original bx from stack
		mov [pcb+bx+2], ax ; save bx in current pcb
		pop ax ; read original ds from stack
		mov [pcb+bx+20], ax ; save ds in current pcb
		pop ax ; read original ip from stack
		mov [pcb+bx+16], ax ; save ip in current pcb
		pop ax ; read original cs from stack
		mov [pcb+bx+18], ax ; save cs in current pcb
		pop ax ; read original flags from stack
		mov [pcb+bx+26], ax ; save cs in current pcb
		mov [pcb+bx+22], ss ; save ss in current pcb
		mov [pcb+bx+14], sp ; save sp in current pcb
		mov bx, [pcb+bx+28] ; read next pcb of this pcb
		mov [current], bx ; update current to new pcb
		mov cl, 5
		shl bx, cl ; multiply by 32 for pcb start
		mov cx, [pcb+bx+4] ; read cx of new process
		mov dx, [pcb+bx+6] ; read dx of new process
		mov si, [pcb+bx+8] ; read si of new process
		mov di, [pcb+bx+10] ; read diof new process

		mov bp, [pcb+bx+12] ; read bp of new process
		mov es, [pcb+bx+24] ; read es of new process
		mov ss, [pcb+bx+22] ; read ss of new process
		mov sp, [pcb+bx+14] ; read sp of new process
		push word [pcb+bx+26] ; push flags of new process
		push word [pcb+bx+18] ; push cs of new process
		push word [pcb+bx+16] ; push ip of new process
		push word [pcb+bx+20] ; push ds of new process
		mov al, 0x20
		out 0x20, al ; send EOI to PIC
		mov ax, [pcb+bx+0] ; read ax of new process
		mov bx, [pcb+bx+2] ; read bx of new process
		pop ds ; read ds of new process
		iret






	
; timer: push ds
; push bx
; push cs
; pop ds ; initialize ds to data segment
; mov bx, [current] ; read index of current in bx
; shl bx, 1
; shl bx, 1
; shl bx, 1
; shl bx, 1
; shl bx, 1 ; multiply by 32 for pcb start
; mov [pcb+bx+0], ax ; save ax in current pcb
; mov [pcb+bx+4], cx ; save cx in current pcb
; mov [pcb+bx+6], dx ; save dx in current pcb
; mov [pcb+bx+8], si ; save si in current pcb
; mov [pcb+bx+10], di ; save di in current pcb
; mov [pcb+bx+12], bp ; save bp in current pcb
; mov [pcb+bx+24], es ; save es in current pcb
; pop ax ; read original bx from stack
; mov [pcb+bx+2], ax ; save bx in current pcb
; pop ax ; read original ds from stack
; mov [pcb+bx+20], ax ; save ds in current pcb
; pop ax ; read original ip from stack
; mov [pcb+bx+16], ax ; save ip in current pcb
; pop ax ; read original cs from stack
; mov [pcb+bx+18], ax ; save cs in current pcb
; pop ax ; read original flags from stack
; mov [pcb+bx+26], ax ; save cs in current pcb
; mov [pcb+bx+22], ss ; save ss in current pcb
; mov [pcb+bx+14], sp ; save sp in current pcb
; mov bx, [pcb+bx+28] ; read next pcb of this pcb
; mov [current], bx ; update current to new pcb
; mov cl, 5
; shl bx, cl ; multiply by 32 for pcb start
; mov cx, [pcb+bx+4] ; read cx of new process
; mov dx, [pcb+bx+6] ; read dx of new process
; mov si, [pcb+bx+8] ; read si of new process
; mov di, [pcb+bx+10] ; read diof new process


; mov bp, [pcb+bx+12] ; read bp of new process
; mov es, [pcb+bx+24] ; read es of new process
; mov ss, [pcb+bx+22] ; read ss of new process
; mov sp, [pcb+bx+14] ; read sp of new process
; push word [pcb+bx+26] ; push flags of new process
; push word [pcb+bx+18] ; push cs of new process
; push word [pcb+bx+16] ; push ip of new process
; push word [pcb+bx+20] ; push ds of new process
; mov al, 0x20
; out 0x20, al ; send EOI to PIC
; mov ax, [pcb+bx+0] ; read ax of new process
; mov bx, [pcb+bx+2] ; read bx of new process
; pop ds ; read ds of new process
; iret			
; ; timer:		push ax
			; ; push bx
			; ; push dx
			; ; push ds
			; ; push cs
			; ; pop ds
			; ; cmp word [randNum], 0xFFFF
			; ; jnz timerCheck2
			; ; mov word [randNum], 0
; ; timerCheck2:
			; ; cmp word[quitFlag], 1
			; ; jz timerExit
			; ; mov word[showtime], 0
			; ; ; increment tick count
			; ; inc word [randNum]
			
			; ; cmp word[brickColor3], 3
			; ; jnz timerReset
			; ; inc word [tickcount]
			; ; mov word[showtime], 1
			
			; ; cmp  word [tickcount], 18
			; ; jnz timerExit
			; ; inc word [tickcount1]
			; ; mov word [tickcount], 0
			
			; ; cmp  word [tickcount1], 10
			; ; jnz timerExit
			; ; jmp gameExit

; ; timerReset:			
			; ; mov word [tickcount1], 0
; ; timerExit:	mov al, 0x20
			; ; out 0x20, al ; end of interrupt
			; ; pop ds
			; ; pop dx
			; ; pop bx
			; ; pop ax
			; ; iret ; return from interrupt

saveScreen:

		push ax
		push es
		push ds
		push si
		push di
		push cx
		
		mov di,buffer
		push cs
		pop es
		; mov ax,0x19f5
		; mov es,ax
		mov ax,0x0A000
		mov ds,ax
		mov si,320*50  ; sr

		mov cx,320*40   ; nr
		cld
	rep movsw
		
		pop cx
		pop di
		pop si
		pop ds
		pop es
		pop ax
		ret

restoreScreen:
		push ax
		push es
		push ds
		push si
		push di
		push cx
		

		mov si,buffer
		mov ax,0x0A000
		mov es,ax

		mov di, 320*50
		mov cx,320*40
		cld
		rep movsw
		
		pop cx
		pop di
		pop si
		pop ds
		pop es
		pop ax
		ret
			
; ;--------------------------------------------------------------------
; ;;; RANDOM NUMBER GENERATOR
; ;--------------------------------------------------------------------	
			
RANDGEN:  
push bp
mov bp, sp 
push ax
push cx
push dx
      ; generate a rand no using the system time
RANDSTART:

    mov  al, [randNum]
    xor  dx, dx
    mov  cx, [bp + 4]   ;range 
    div  cx       ; here dx contains the remainder of the division - from 0 to 9
	add dl,  [bp + 6]    
	mov dh, 0
	mov word[bp + 8], dx  ;return value  
	pop dx
	pop cx
	pop ax
	pop bp
RET  4


;;--------------------------------------------------------------------
;;;; CHECKING CO ORDINATES
;;--------------------------------------------------------------------

checkCarrotCordinates2:

		push dx
		
		cmp word[carrot3], 186 ;THIRD BRICK (y axis)
		;cmp word[carrotCordinates], 186 ;THIRD BRICK (y axis)
		jb checkCarrotExit2
		
		mov dx, [RabbitCordinates]
		sub dx, 2
		cmp [carrot3 + 2], dx
		;cmp [carrotCordinates + 2], dx
		jb checkCarrotExit2
		
		mov dx, [RabbitCordinates + 2]
		add dx, 2
		cmp [carrot3 + 2], dx
		;cmp [carrotCordinates + 2], dx
		ja checkCarrotExit2
		add word[points], 1
		
		push 4 ; height.....[bp + 12]
		push 320 ; width......[bp + 10]
		push  167; color = blue.....[bp + 8]
		push 186 ; starting row.....[bp + 6]
		push 0 ; starting col.....[bp + 4]
		call PrintBackground
		
		push 6 ; height.....[bp + 12]
		push 8 ; width......[bp + 10]
		push 31 ; color = blue.....[bp + 8]
		push 186 ; starting row.....[bp + 6]
		push word [RabbitCordinates]; starting col.....[bp + 4]
		call PrintBackground
		
		mov word[carrot3], 0
		
		;mov word[carrotCordinates], 0
checkCarrotExit2:
		pop dx
ret			
		
checkCarrotCordinates:

		push dx
		cmp word[carrot2], 161
		;cmp word[carrotCordinates], 161
		jb checkCarrotExit
		
		mov dx, [RabbitCordinates]
		sub dx, 2		
		cmp [carrot2 + 2], dx
		;cmp [carrotCordinates + 2], dx
		jb checkCarrotExit
		
		mov dx, [RabbitCordinates + 2]
		add dx, 2
		cmp [carrot2 + 2], dx
		; cmp [carrotCordinates + 2], dx
		ja checkCarrotExit
		add word[points], 1
		
		
		
		push 4 ; height.....[bp + 12]
		push 320 ; width......[bp + 10]
		push  167; color = blue.....[bp + 8]
		push 161 ; starting row.....[bp + 6]
		push 0 ; starting col.....[bp + 4]
		call PrintBackground
		
		push 8 ; height.....[bp + 12]
		push 8 ; width......[bp + 10]
		push 31 ; color = blue.....[bp + 8]
		push 159 ; starting row.....[bp + 6]
		push word [RabbitCordinates]; starting col.....[bp + 4]
		call PrintBackground
		
		;mov word[carrotCordinates], 0
		mov word[carrot2], 0
		mov word[carrot2 + 2], 0
		
checkCarrotExit:
		pop dx
ret		

CheckCordinates:
			push dx
			
			mov word[exitFlag], 0
		
			mov dx, [BrickCordinates2]
			sub dx, 4
			cmp dx, [RabbitCordinates]
			jbe check2
			mov word[exitFlag], 1
						
			jmp checkExit
check2:		
			mov dx, [BrickCordinates2 + 2]
			add dx, 4
			cmp dx, [RabbitCordinates + 2]
			jae checkExit
			mov word[exitFlag], 1
			jmp checkExit
			
checkExit:
			pop dx
			ret 

; ;--------------------------------------------------------------------
; ;;; Printing Background
; ;--------------------------------------------------------------------			
PrintBackground:
		push bp
		mov bp, sp;

		push es
		push ax
		push cx
		push si
		push bx
		push dx

		mov cx, [bp + 4]; ; starting col
		mov dx,  [bp + 6]; ; starting row
		mov bh, [bp + 12] ; height
		mov ah, 0x0C;

printh:
		mov si, [bp + 10] ; width
printrow:
		mov al, [bp + 8] ; color
		int 10h;

		add cx, 1;
		dec si;
		jnz printrow;

		mov cx, [bp + 4]
		add dx, 1;
		dec bh;
		jnz printh;

		pop dx
		pop bx
		pop si
		pop cx
		pop ax
		pop es
		pop bp 
		ret 10
		
		

; ;--------------------------------------------------------------------
; ;;; Printing Mountains
; ;--------------------------------------------------------------------
PrintMountain:
		push bp
		mov bp, sp

		push es
		push ax
		push cx
		push si
		push di
		push bx

		push word[bp + 12]
		push word[bp + 10]
		push word[bp + 8]
		push word[bp + 6]
		push word[bp + 4]
		call PrintTriangle
		
		push word 10
		push word[bp + 10]
		push word 90
		push word[bp + 6]
		push word[bp + 4]
		call PrintTriangle
		
		pop bx
		pop di
		pop si
		pop cx
		pop ax
		pop es
		pop bp
		ret 10
		
; ;--------------------------------------------------------------------
; ;;; Printing Trees
; ;--------------------------------------------------------------------


PrintTree:
		push bp
		mov bp, sp;
		
		sub sp, 2
		
		push es
		push ax
		push cx
		push si
		push bx
		push dx
		push di
		sub word[bp + 12], 4
		push word[bp + 12]			; height.....[bp + 12]
		push word[bp + 10]		; width......[bp + 10]
		push word [bp + 8]	; color = green.....[bp + 8]
		push word[bp + 6]		; starting row.....[bp + 6]
		push word[bp + 4]		; starting col.....[bp + 4]
		call PrintTriangle
		
		add word[bp + 12], 4
		
		add word[bp + 6], 7
		
		push word[bp + 12]			; height.....[bp + 12]
		push word[bp + 10]		; width......[bp + 10]
		push word [bp + 8]	; color = green.....[bp + 8]
		push word[bp + 6]		; starting row.....[bp + 6]
		push word[bp + 4]		; starting col.....[bp + 4]
		call PrintTriangle

;;printing bark
		sub word[bp + 4], 2
		add word[bp + 6], 17
		push 15			; height.....[bp + 12]
		push 5			; width......[bp + 10]
		push 114			; color = blue.....[bp + 8]
		push word[bp + 6]	; starting row.....[bp + 6]
		push word[bp + 4]		; starting col.....[bp + 4]
		call PrintBackground
			
		pop di
		pop dx
		pop bx
		pop si
		pop cx
		pop ax
		pop es
		pop bp
		add sp, 2
		ret 10
;-------------------------------------------------------------------
; subroutine to find string lenght
;-------------------------------------------------------------------
strlen: push bp
		mov bp,sp
		push es
		push cx
		push di
		push ds
	
		les di, [bp+4] ;***** point es:di to string di=[bp+4], es = [bp+6]
		mov cx, 0xffff ; load maximum number in cx
		mov al, 0 ; load a zero in al
		repne scasb ; find zero in the string
		mov ax, 0xffff ; load maximum number in ax
		sub ax, cx ; find change in cx
		dec ax ; exclude null from length

		pop ds
		pop di
		pop cx
		pop es
		pop bp
		ret 4 
;-------------------------------------------------------------------
; subroutine to print number
;-------------------------------------------------------------------
printnum: 
				push bp
				mov bp, sp
				push es
				push ax
				push bx
				push cx
				push dx
				push di
				push si

				mov cx, ax
				mov si, [bp+4]		; load number in ax= 4529
				mov ax, si
				
				mov bx, 10			; use base 10 for division
				mov cx, 0

nextdigit:		mov dx, 0			; zero upper half of dividend
				div bx				; divide by 10 AX/BX --> Quotient --> AX, Remainder --> DX ..... 
				add dx, 0x30		; convert digit into ascii value
				push dx				; save ascii value on stack

				inc cx				; increment count of values
				cmp ax, 0			; is the quotient zero
				jnz nextdigit		; if no divide it again

				mov bh, 00
				mov bl, [bp + 6]	;color
				mov dl, [bp + 10]	;col
				mov dh, [bp + 8]	  ;row
				mov ah, 02h    ; BIOS.SetCursorPosition - Point to the location of dx and page no. of bh
				int 10h
				
				
nextpos:		pop ax; remove a digit from the stack
				mov ah,  0x0E
				int 10h
				inc dl
				add si, 1
				loop nextpos		; repeat for all digits on stack
				
			

			
				
				pop si
				pop di
				pop dx
				pop cx
				pop bx
				pop ax
				pop es
				pop bp
				ret 8		
		
;-------------------------------------------------------------------
; print null terminated string using strlen
;-------------------------------------------------------------------
printstr:	push bp
			mov bp, sp
			push es
			push ax
			push cx
			push si
			push di
			push ds
			push bx
			push dx
push cs
pop ds
			push cs				; push segment of string........[bp+6]
			mov ax, [bp+4]
			push ax				; push offset of string.........[bp+4]
			call strlen			; calculate string length 

			cmp ax, 0			; is the string empty
			jz strExit				; no printing if string is empty
			mov cx, ax			; save length in cx

			mov si, [bp+4]		; point si to string
			mov bh, 00
			mov bl, [bp+6]		; Page=0
			mov dl, [bp + 10]
			mov dh, [bp+8]	  ;row
			mov ah, 02h    ; BIOS.SetCursorPosition - Point to the location of dx and page no. of bh
			int 10h
			cld ; auto increment mode
nextchar: 	

;call delay2
			lodsb ; load next char in  al
			inc dl
			mov ah,  0x0E
			int 10h
		
			loop nextchar ; repeat for the whole string

strExit: 	
			pop dx
			pop bx
			pop ds
			pop di
			pop si
			pop cx
			pop ax
			pop es
			pop bp
			ret 8 


		
; ;--------------------------------------------------------------------
; ;;; Printing Triangle
; ;--------------------------------------------------------------------
PrintTriangle:

		push bp
		mov bp, sp
		push es
		push ax
		push cx
		push si
		push di
		push bx
		push dx

		mov cx, [bp + 4]; ; starting col
		mov dx,  [bp + 6];;starting row
		mov bh, [bp + 12] ; height
		mov ah, 0x0C;
		mov al, [bp + 8]  ; color
		mov di, [bp + 10]    ; width  
PrintTriH:
		mov si, di ; width
PrintTriR:
		int 10h;
		add cx, 1;  
		dec si;
		jnz PrintTriR;

		add di, 2
		sub word[bp + 4], 1   ;mov to prev column3
		mov cx, [bp + 4]
		add dx, 1			; next row
		dec bh;				; height count
		jnz PrintTriH;

		pop dx
		pop bx
		pop di
		pop si
		pop cx
		pop ax
		pop es
		pop bp
		ret 10
; ;--------------------------------------------------------------------
; ;;; Printing Brick
; ;--------------------------------------------------------------------
PrintBrick:
		push bp
		mov bp, sp
		
		push 2 ; height.....[bp + 12]
		push 40 ; width......[bp + 10]
		push word[bp + 8] ; color = blue.....[bp + 8]
		push word[bp + 6] ; starting row.....[bp + 6]
		push word[bp + 4] ; starting col.....[bp + 4]
		call PrintBackground 
		
		add word[bp + 6], 2 ; starting row.....[bp + 6]
		
		push 5 ; height.....[bp + 12]
		push 40 ; width......[bp + 10]
		push 6 ; color = blue.....[bp + 8]
		push word [bp + 6]; starting row.....[bp + 6]
		push word[bp + 4] ; starting col.....[bp + 4]
		call PrintBackground 
		
		pop bp
		ret 6
; ;--------------------------------------------------------------------
; ;;; Printing Car
; ;--------------------------------------------------------------------
PrintCar:

		push bp
		mov bp, sp
		push es
		push ax
		push cx
		push si
		push di
		push bx

		push word[bp + 12]
		push word[bp + 10]
		push word 77
		push word[bp + 6]
		push word[bp + 4]
		call PrintTriangle
		
		sub word[bp + 4], 15
		mov di, [bp + 12]
		add word[bp + 6], di 
		push  6
		push word 55
		push word [bp + 8]
		push word [bp + 6]
		push word [bp + 4]
		call PrintTriangle
		
		sub word[bp + 4], 5
		add word[bp + 6], 6
		push 7 ; height.....[bp + 12]
		push 65 ; width......[bp + 10]
		push word[bp + 8] ; color = blue.....[bp + 8]
		push word [bp + 6]; starting row.....[bp + 6]
		push  word[bp + 4]; starting col.....[bp + 4]
		call PrintBackground

		mov bx, 0024		; Page=0
		mov dl, [bp + 14]
		mov dh,  [bp + 16]  ;row
		mov ah, 02h    ; BIOS.SetCursorPosition - Point to the location of dx and page no. of bh
		int 10h
		mov ah,  0x0E
		mov al, '@'
		int 10h
		
		add dl, 5
		mov ah, 02h    ; BIOS.SetCursorPosition - Point to the location of dx and page no. of bh
		int 10h
		mov ah,  0x0E
		mov al, '@'
		int 10h

		pop bx
		pop di
		pop si
		pop cx
		pop ax
		pop es
		pop bp
		ret 14
		
		
; ;--------------------------------------------------------------------
; ;;; Printing Main Screen
; ;--------------------------------------------------------------------
PrintMainScreen:


		push 110 ; height.....[bp + 12]
		push 320 ; width......[bp + 10]
		push 0 ; color = blue.....[bp + 8]
		push 30 ; starting row.....[bp + 6]
		push 0 ; starting col.....[bp + 4]
		call PrintBackground

		push 50 ; height.....[bp + 12]
		push 320 ; width......[bp + 10]
		push 3 ; color = blue.....[bp + 8]
		push 0 ; starting row.....[bp + 6]
		push 0 ; starting col.....[bp + 4]
		call PrintBackground

		push 20 ; height.....[bp + 12]
		push 320 ; width......[bp + 10]
		push 119 ; color = green.....[bp + 8]
		push 50 ; starting row.....[bp + 6]
		push 0 ; starting col.....[bp + 4]
		call PrintBackground
		
		push 49 ; height.....[bp + 12]
		push 1 ; width......[bp + 10]
		push 114 ; color = brown.....[bp + 8]
		push 10 ; starting row.....[bp + 6]
		push 100 ; starting col.....[bp + 4]
		call PrintMountain

		push 40 ; height.....[bp + 12]
		push 1 ; width......[bp + 10]
		push 6 ; color = brown.....[bp + 8]
		push 23 ; starting row.....[bp + 6]
		push 140 ; starting col.....[bp + 4]
		call PrintMountain

		push 17 ; height.....[bp + 12]
		push 1 ; width......[bp + 10]
		push 191 ; color = green.....[bp + 8]
		push 23 ; starting row.....[bp + 6]
		push 182 ; starting col.....[bp + 4]
		call PrintTree

		push 17 ; height.....[bp + 12]
		push 1 ; width......[bp + 10]
		push 191 ; color = green.....[bp + 8]
		push 22 ; starting row.....[bp + 6]
		push 17 ; starting col.....[bp + 4]
		call PrintTree

		push 17 ; height.....[bp + 12]
		push 1 ; width......[bp + 10]
		push 191 ; color = green.....[bp + 8]
		push 27 ; starting row.....[bp + 6]
		push 46 ; starting col.....[bp + 4]
		call PrintTree

		push 40 ; height.....[bp + 12]
		push 1 ; width......[bp + 10]
		push 114 ; color = brown.....[bp + 8]
		push 18 ; starting row.....[bp + 6]
		push 228 ; starting col.....[bp + 4]
		call PrintMountain

		push 17 ; height.....[bp + 12]
		push 1 ; width......[bp + 10]
		push 191 ; color = green.....[bp + 8]
		push 27 ; starting row.....[bp + 6]
		push 303 ; starting col.....[bp + 4]
		call PrintTree

		push 17 ; height.....[bp + 12]
		push 1 ; width......[bp + 10]
		push 191 ; color = green.....[bp + 8]
		push 30 ; starting row.....[bp + 6]
		push 276 ; starting col.....[bp + 4]
		call PrintTree

		push 4 ; height.....[bp + 12]
		push 320 ; width......[bp + 10]
		push 14 ; color = blue.....[bp + 8]
		push 69 ; starting row.....[bp + 6]
		push 0 ; starting col.....[bp + 4]
		call PrintBackground

		push 4 ; height.....[bp + 12]
		push 320 ; width......[bp + 10]
		push 14 ; color = blue.....[bp + 8]
		push 131 ; starting row.....[bp + 6]
		push 0 ; starting col.....[bp + 4]
		call PrintBackground

		push 3 ; height.....[bp + 12]
		push 32 ; width......[bp + 10]
		push 90 ; color = blue.....[bp + 8]
		push 100 ; starting row.....[bp + 6]
		push 4 ; starting col.....[bp + 4]
		call PrintBackground

		push 3 ; height.....[bp + 12]
		push 32 ; width......[bp + 10]
		push 90 ; color = blue.....[bp + 8]
		push 100 ; starting row.....[bp + 6]
		push 64 ; starting col.....[bp + 4]
		call PrintBackground
		push 3 ; height.....[bp + 12]
		push 32 ; width......[bp + 10]
		push 90 ; color = blue.....[bp + 8]
		push 100 ; starting row.....[bp + 6]
		push 128 ; starting col.....[bp + 4]
		call PrintBackground
		push 3 ; height.....[bp + 12]
		push 32 ; width......[bp + 10]
		push 90 ; color = blue.....[bp + 8]
		push 100 ; starting row.....[bp + 6]
		push 192 ; starting col.....[bp + 4]
		call PrintBackground
		push 3 ; height.....[bp + 12]
		push 32 ; width......[bp + 10]
		push 90 ; color = blue.....[bp + 8]
		push 100 ; starting row.....[bp + 6]
		push 256 ; starting col.....[bp + 4]
		call PrintBackground

		push 41 ; tyre width.....[bp + 16]
		push 23 ; tyre height.....[bp + 14]
		push 8 ; height.....[bp + 12]
		push 26 ; width......[bp + 10]
		push 41 ; color = blue.....[bp + 8]
		push 103 ; starting row.....[bp + 6]
		push 256 ; starting col.....[bp + 4]
		call PrintCar
		
		push 37 ; tyre width.....[bp + 16]
		push 3 ; tyre height.....[bp + 14]
		push 8 ; height.....[bp + 12]
		push 26 ; width......[bp + 10]
		push 21 ; color = blue.....[bp + 8]
		push 71 ; starting row.....[bp + 6]
		push 100 ; starting col.....[bp + 4]
		call PrintCar
	
	
		push 65 ; height.....[bp + 12]
		push 320 ; width......[bp + 10]
		push  167; color = blue.....[bp + 8]
		push 135 ; starting row.....[bp + 6]
		push 0 ; starting col.....[bp + 4]
		call PrintBackground
		
		push 14 ; color = blue.....[bp + 8]
		push 142 ; starting row.....[bp + 6]
		push 140 ; starting col.....[bp + 4]
		call PrintBrick
		
		push 49 ; color = blue.....[bp + 8]
		push 167 ; starting row.....[bp + 6]
		push 140 ; starting col.....[bp + 4]
		call PrintBrick
		
		push 42 ; color = blue.....[bp + 8]
		push 192 ; starting row.....[bp + 6]
		push 140 ; starting col.....[bp + 4]
		call PrintBrick
		
		push 4 ; height.....[bp + 12]
		push 1 ; width......[bp + 10]
		push 43 ; color = blue.....[bp + 8]
		push 136 ; starting row.....[bp + 6]
		push 160 ; starting col.....[bp + 4]
		call PrintTriangle
		
		; push 5 ; height.....[bp + 12]
		; push 10 ; width......[bp + 10]
		; push 43 ; color = blue.....[bp + 8]
		; push 110 ; starting row.....[bp + 6]
		; push 160 ; starting col.....[bp + 4]
		; call PrintInvertedTriangle
		
		push 8 ; height.....[bp + 12]
		push 8 ; width......[bp + 10]
		push 31 ; color = blue.....[bp + 8]
		push 184 ; starting row.....[bp + 6]
		push 156 ; starting col.....[bp + 4]
		call PrintBackground
		
		ret

RotateLeft:
			push bp
			mov bp,sp
			push ax
			push cx
			push si
			push di
			push es
			push ds
			push dx
			push bx
			
			push cs
			pop ds
			
			mov ax, 0x0A000;
			mov ds, ax ; point ds to video base
			mov es, ax;
			mov bx, [bp + 6]; staring row
			mov dx,  [bp + 10] ; starting col
			;mov cx,  [bp + 8] ; ending col
			 cli
Rotateleft:	
			push dx 
			mov ax, 320 ; load chars per row in ax
			mul bx; calculate source position
			pop dx
			add ax, dx
			mov di, ax ; load source position in di
			mov cx,  [bp + 8] ; ending col
			sub cx,  [bp + 10]
			dec cx
			;mov cx, 319 ; number of screen locations
			mov si, di
			add si, 1
			mov ax, [es:di]
			cld
			rep movsb ; mov [es:di], [ds: si]
			mov [es: di], al
			
			add bx, 1
			cmp bx, [bp + 4] ;ending row
			jnz Rotateleft
		sti
			pop bx
			pop dx
			pop ds
			pop es
			pop di
			pop si
			pop cx
			pop ax
			pop bp
			ret 8	
			
RotateRight:
			push bp
			mov bp,sp
			push ax
			push cx
			push si
			push di
			push es
			push ds
			push dx
			push bx
			
			push cs
			pop ds
			
			
			mov ax, 0x0A000;
			mov ds, ax ; point ds to video base
			mov es, ax;
			mov bx, [bp + 6]
			cli
Rotateright:	
			mov ax, 320 ; load chars per row in ax
			mul bx; calculate source position
			add ax, [bp + 8]
			mov di, ax ; load source position in si
			mov cx,  [bp + 8] ; ending col
			sub cx,  [bp + 10]
			dec cx
			;mov cx, 319 ; number of screen locations
			;add di, 319	
	
			mov ax, [es:di]
			mov si, di
			sub si, 1
			std
			rep movsb 
			mov [es: di], al
			
			add bx, 1
			cmp bx, [bp + 4]     
			jnz Rotateright
sti
			pop bx
			pop dx
			pop ds
			pop es
			pop di
			pop si
			pop cx
			pop ax
			pop bp
			ret 8

Animate:			

Rotate:	
		cmp word [exitFlag], 0
		jnz gameExit

		mov dx, 0
		; mov ax, [points]
		
		; shl ax, 2

r1:
		push word 8
		push word 0
		push word 3
		push word [points] 
		call printnum
		
		
		cmp word[showtime], 1
		jnz showNoTime
		push word 39
		push word 0
		push word 3
		push word [tickcount1] 
		call printnum
		jmp r_5
		
showNoTime:
		push word 39
		push word 0
		push word 3
		push word 0
		call printnum
		
		
r_5:	
		push cs
		pop ds
		
quitloop:
		cmp word [exitFlag], 0
		jnz gameExit
		cmp word [quitFlag], 1
		jz quitloop
		
		push word 0
		push word 320
		push word 10
		push word 98
		call RotateLeft
quitloop1:
		cmp word [exitFlag], 0
		jnz gameExit
		cmp word [quitFlag], 1
		jz quitloop1
		
		push word 0
		push word 320
		push word 103
		push word 130
		call RotateRight

quitloop3:
		cmp word [exitFlag], 0
		jnz gameExit
		cmp word [quitFlag], 1
		jz quitloop
		
				
		; cmp  word [movingBrick], 200  
		; ja r1

		; cmp  word [movingBrick], 192 ; last brick
		; jnz r4
		;sub word [movingBrick], 8; to rotate the rabbit
r4: 	
		mov cx, 30
		add cx, ax
		cmp dx, cx;60 ; first 30 times to left
		jb r3
		mov cx, 89
		shl ax, 1
		add cx, ax
		cmp dx, cx;179 ; next 60 to right
		ja r3
		
		cmp  word [movingBrick1 + 2], 149
		;cmp  word [movingBrick + 2], 149
		jnz r7
		
		push ax
		mov ax, word[BrickCordinates1]
		dec ax
		push ax
		mov ax, word[BrickCordinates1 + 2]
		;dec ax
		push ax	
		
		push word [movingBrick1]
		push word [movingBrick1 + 2]
		
		call RotateRight
		pop ax
		inc word[BrickCordinates1]
		inc word[BrickCordinates1 + 2]

r7: 	

		cmp  word [movingBrick2 + 2], 174
		jnz r6
		
		push ax
		mov ax, word[BrickCordinates2]
		dec ax
		push ax
		mov ax, word[BrickCordinates2 + 2]
		;dec ax
		push ax	
		push word [movingBrick2]
		push word [movingBrick2 + 2]
		call RotateLeft
		pop ax
		
		dec word[BrickCordinates2]
		dec word[BrickCordinates2 + 2]



r6: 
		cmp  word [movingBrick3 + 2], 199
		jnz r5
		push ax
		mov ax, word[BrickCordinates3]
		dec ax
		push ax
		mov ax, word[BrickCordinates3 + 2]
		;dec ax
		push ax		
		push word [movingBrick3]
		push word [movingBrick3 + 2]
		call RotateRight
		pop ax
		
		push ax
		mov ax, word[RabbitCordinates]
		dec ax
		push ax
		push word[RabbitCordinates + 2]
		push word 180;[RabbitCordinatesY]
		push word 192;[RabbitCordinatesY + 2]
		call RotateRight
		pop ax
		
		inc word[RabbitCordinates]
		inc word[RabbitCordinates + 2]
		inc word[BrickCordinates3]
		inc word[BrickCordinates3 + 2]
		call checkCarrotCordinates2

r5:
		inc dx
		JMP r1
r3: 

		cmp  word [movingBrick1 + 2], 149
		jnz r_8
		
		push ax
		mov ax, word[BrickCordinates1]
		dec ax
		push ax
		mov ax, word[BrickCordinates1 + 2]
		;dec ax
		push ax	
		push word [movingBrick1]
		push word [movingBrick1 + 2]
		call RotateLeft
		pop ax
		
		dec word[BrickCordinates1]
		dec word[BrickCordinates1 + 2]

r_8: 

		cmp  word [movingBrick2 + 2], 174
		jnz r_9
		
		push ax
		mov ax, word[BrickCordinates2]
		dec ax
		push ax
		mov ax, word[BrickCordinates2 + 2]
		;dec ax
		push ax	
		push word [movingBrick2]
		push word [movingBrick2 + 2]
		
		call RotateRight
		pop ax
		inc word[BrickCordinates2]
		inc word[BrickCordinates2 + 2]



		
r_9: 
		cmp  word [movingBrick3 + 2], 199
		jnz r_6
		
		push ax
		mov ax, word[BrickCordinates3]
		dec ax
		push ax
		mov ax, word[BrickCordinates3 + 2]
		;dec ax
		push ax	
		
		push word [movingBrick3]
		push word [movingBrick3 + 2]
		call RotateLeft
		pop ax
		
		push ax
		mov ax, word[RabbitCordinates]
		dec ax
		push ax
		push word[RabbitCordinates + 2]
		push word 184;[RabbitCordinatesY]
		push word 192;[RabbitCordinatesY + 2]
		call RotateLeft
		pop ax
		
		dec word[RabbitCordinates]
		dec word[RabbitCordinates + 2]
		dec word[BrickCordinates3]
		dec word[BrickCordinates3 + 2]
		call checkCarrotCordinates2

r_6: 	inc dx	
		mov cx, 120;240
		shl ax, 1
		add cx, ax
		cmp dx, cx;240
		jnz r1
		JMP Rotate
		ret

;;;;;------------------------------------------------------------------------------
;;;;;					SCROLL UP
;;;;;------------------------------------------------------------------------------		
Scrollup:

		push ax
		push dx
		push bx
		push cx
		mov dx, [RabbitCordinatesY]
		mov cx, 27
		mov ax, 0

		cmp word[brickColor2], 0 
		jnz checkColor1
		mov bx, 49
		jmp ScrollUp
		
checkColor1:	

		cmp word[brickColor2], 1
		jnz checkColor2
		mov bx, 14
		jmp ScrollUp
		
checkColor2:	

		cmp word[brickColor2], 2
		jnz checkColor3
		mov bx, 42
		jmp ScrollUp
		
checkColor3:	

		cmp word[brickColor2], 3
		jnz ScrollUp
		mov bx, 52
		
ScrollUp:

		; push word 0
		; push word 320
		; push word 10
		; push word 98
		; call RotateLeft

		
		; push word 0
		; push word 320
		; push word 103
		; push word 130
		; call RotateRight

		sub dx, 1
		call delay2
		push 8 ; height.....[bp + 12]
		push 8 ; width......[bp + 10]
		push 31 ; color = blue.....[bp + 8]
		push dx ; starting row.....[bp + 6]
		push word [RabbitCordinates]; starting col.....[bp + 4]
		call PrintBackground
		

		add dx, 9
		cmp ax, 3
		jb ScrollUp2
		push 1 ; height.....[bp + 12]
		push 320 ; width......[bp + 10]
		push 167 ; color = blue.....[bp + 8]
		push dx ; starting row.....[bp + 6]
		push 0;word[RabbitCordinates]; starting col.....[bp + 4]
		call PrintBackground
		
ScrollUp2:


		push bx;49;1 ; color = blue.....[bp + 8]
		push 142 + 25 ; starting row.....[bp + 6]
		push word[BrickCordinates2] ; starting col.....[bp + 4]
		call PrintBrick

		sub dx, 9
		inc ax
loop 	ScrollUp	
		pop cx
		pop bx
		pop dx
		pop ax
		ret
		
scrolldown:	

			push bp
			mov bp,sp
			push ax
			push cx
			push si
			push di
			push es
			push dx
			push bx
			push ds
			
			mov ax, 0x0A000;
			mov ds, ax ; point ds to video base
			mov es, ax;
			mov bx, 150
			mov dx, 175
			mov cx, 2

scroll:		
			push cx
			push dx
			mov ax, 320 ; load chars per row in ax
			mul bx; calculate source position
			mov di, ax ; load source position in si
			mov cx, 320 ; number of screen locations
			add	di, 320*25		
			
			mov ax, 320 ; load chars per row in ax
			mul bx; calculate source position
			mov si, ax ; load source position in si
	
			std
			rep movsb 
			
			pop dx
			pop cx
			add bx, 1
			cmp bx, dx;160;175
			jnz scroll
			mov bx, 136
			mov dx, 150
			dec cx
			jnz scroll
			
			push 3 ; height.....[bp + 12]
			push 320 ; width......[bp + 10]
			push  167; color = blue.....[bp + 8]
			push 136 ; starting row.....[bp + 6]
			push 0 ; starting col.....[bp + 4]
			call PrintBackground
			
			
			pop ds
			pop bx
			pop dx
			pop es
			pop di
			pop si
			pop cx
			pop ax
			pop bp
			ret 
;;;---------------------------------------------------
			
SettingCordinates:
			push dx
			
			mov dx, [brickColor2]  
			mov [brickColor3], dx
			
			mov dx, [brickColor1]  
			mov [brickColor2], dx
			
			;;setting cordinates of bricks
			mov dx, [BrickCordinates2]  
			mov [BrickCordinates3], dx
			mov dx, [BrickCordinates2 + 2]
			mov [BrickCordinates3 + 2], dx
			
			mov dx, [BrickCordinates1]
			mov [BrickCordinates2], dx
			mov dx, [BrickCordinates1 + 2]
			mov [BrickCordinates2 + 2], dx

			add word[movingBrick], 25
			add word[movingBrick + 2], 25
			pop dx
			ret
			
PrintNextBrick:		

			push dx			
			push bx
			push cx
			push 0xCCCC	
			
			push word 0 ; starting number
			push word 4 ; range
			call RANDGEN
			pop dx
			
			
			mov bx , word[movingBrick2]
			add bx, 25
			mov word[movingBrick3], bx
			
			mov bx, word[movingBrick2 + 2]
			add bx, 25
			mov word[movingBrick3 + 2], bx
			
			mov bx , word[movingBrick1]
			add bx, 25
			mov word[movingBrick2], bx
			
			mov bx, word[movingBrick1 + 2]
			add bx, 25
			mov word[movingBrick2 + 2], bx
			
			
			mov word[movingBrick1], 0
			mov word[movingBrick1 + 2], 0


			cmp word[brick], 3
			jnz brickExit2
			; mov word[movingBrick], 142
			; mov word[movingBrick + 2], 149
			mov word[movingBrick1], 142
			mov word[movingBrick1 + 2], 149
			mov word[brick], 0

brickExit2:	

			push 0xCCCC			
			push word 125 ; starting number
			push word 35 ; range
			call RANDGEN
			pop cx

			cmp dx, 1
			jnz b2			
			push 25 ; height.....[bp + 12]
			push 320 ; width......[bp + 10]
			push  167; color = blue.....[bp + 8]
			push 136 ; starting row.....[bp + 6]
			push 150 ; starting col.....[bp + 4]
			call PrintBackground
			push 14;49;1 ; color = blue.....[bp + 8]
			push 142 ; starting row.....[bp + 6]
			push cx ; starting col.....[bp + 4]
			call PrintBrick
			mov word[brickColor1], 1

			;add  word[brick], 1
			jmp brickExit
			
b2:	
			cmp dx, 0
			jnz b3
									
			push 25 ; height.....[bp + 12]
			push 320 ; width......[bp + 10]
			push  167; color = blue.....[bp + 8]
			push 136 ; starting row.....[bp + 6]
			push 150 ; starting col.....[bp + 4]
			call PrintBackground
			
			push 49 ; color = blue.....[bp + 8]
			push 142 ; starting row.....[bp + 6]
			push cx ; starting col.....[bp + 4]
			call PrintBrick
			mov word[brickColor1], 0

			
			;add  word[brick], 1
			
			jmp brickExit
b3:
			cmp dx, 2
			jnz b4

			push 25 ; height.....[bp + 12]
			push 320 ; width......[bp + 10]
			push  167; color = blue.....[bp + 8]
			push 136 ; starting row.....[bp + 6]
			push 150 ; starting col.....[bp + 4]
			call PrintBackground
			push 42 ; color = blue.....[bp + 8]
			push 142 ; starting row.....[bp + 6]
			push cx ; starting col.....[bp + 4]
			call PrintBrick
			mov word[brickColor1], 2
			
			;mov word[brick], 0
			; ; mov word[movingBrick], 157
			; ; mov word[movingBrick + 2], 175
			; mov word[movingBrick], 10
			; mov word[movingBrick + 2], 10
b4:			cmp dx, 3
			jnz brickExit			
			push 25 ; height.....[bp + 12]
			push 320 ; width......[bp + 10]
			push 167; color = blue.....[bp + 8]
			push 136 ; starting row.....[bp + 6]
			push 150 ; starting col.....[bp + 4]
			call PrintBackground
			push 52;49;1 ; color = blue.....[bp + 8]
			push 142 ; starting row.....[bp + 6]
			push cx ; starting col.....[bp + 4]
			call PrintBrick
			;mov word[blueBrick], 0
			mov word[brickColor1], 3
			;add  word[brick], 1
			jmp brickExit			
brickExit:	
			mov word[BrickCordinates1], cx
			add cx, 40
			mov word[BrickCordinates1 + 2], cx

			add  word[brick], 1
			;add word[blueBrick], 1
			
			pop cx
			pop bx
			pop dx	

			ret

PrintCarrot:
			push dx
			push bx
			push 0xCCCC			
			push word 120 ; starting number
			push word 80 ; range
			call RANDGEN
			pop dx
			

			cmp word[carrot], 3
			jnz nocarrot
			push 4 ; height.....[bp + 12]
			push 1 ; width......[bp + 10]
			push 43 ; color = blue.....[bp + 8]
			push 136 ; starting row.....[bp + 6]
			push dx;160;dx ; starting col.....[bp +  4]
			call PrintTriangle	
			
			mov word[carrot], 0
			mov word[carrotCordinates], 136; row
			mov word[carrotCordinates + 2], dx;160;dx;col
			
			mov bx, word[carrot2 + 2]
			mov word[carrot3 + 2], bx
			
			
			mov bx, word[carrot2]
			add bx, 25
			mov word[carrot3], bx;186
			
			mov bx, word[carrot1 + 2]
			mov word[carrot2 + 2], bx
			
			
			mov bx, word[carrot1]
			add bx, 25
			mov word[carrot2], bx; 161
			
			mov word[carrot1 + 2], dx
			mov word[carrot1], 136
			
			jmp carrotExit
nocarrot:			
			
	
			
			mov bx, word[carrot2 + 2]
			mov word[carrot3 + 2], bx
			
			
			mov bx, word[carrot2]
			add bx, 25
			mov word[carrot3], bx;186
			
			mov bx, word[carrot1 + 2]
			mov word[carrot2 + 2], bx
			
			
			mov bx, word[carrot1]
			add bx, 25
			mov word[carrot2], bx; 161
			
			mov word[carrot1 + 2], 0
			mov word[carrot1], 0
			
carrotExit:
			
			add word[carrot], 1
			pop bx
			pop dx
			ret 	
			
PrintBackground1:	
		push bp
		mov bp, sp;

		push es
		push ax
		push cx
		push si
		push bx
		push dx
		mov ax, 0xb800
		mov es, ax
		mov cx, [bp + 4]; ; starting col
		mov bh, [bp + 12] ; height
		

printh1:
		mov ax, 160
		mul word[bp + 6]
		add ax, [bp + 4]
		mov di, ax
		mov cx, [bp + 10] ; width
		mov ax, [bp + 8]
printrow1:
		mov [es:di], ax
		add di, 2
		loop printrow1

		mov cx, [bp + 10]
		inc word[bp + 6]
		dec bh;
		jnz printh1;

		pop dx
		pop bx
		pop si
		pop cx
		pop ax
		pop es
		pop bp 
		ret 10
				


PrintStartingScreen:

		mov ah, 0xA7
		mov al, '*'
		push word 15; height.....[bp + 12]
		push word 62 ; width......[bp + 10]
		push ax; color = blue.....[bp + 8]
		push word 4 ; starting row.....[bp + 6]
		push word 18; starting col.....[bp + 4]
		call PrintBackground1

		push word 13; height.....[bp + 12]
		push word 60 ; width......[bp + 10]
		push word 0x3520; color = blue.....[bp + 8]
		push word 5 ; starting row.....[bp + 6]
		push word 20; starting col.....[bp + 4]
		call PrintBackground1


		push word 33; column
		push word 7 ; row
		push word 0x30 ;blue
		push word str11
		call printstr1

		push word 33; column
		push word 6 ; row
		push word 0x30 ;blue
		push word str1
		call printstr1
		
		push word 20; column
		push word 8 ; row
		push word 0x30 ;blue
		push word str2
		call printstr1
		
		push word 22; column
		push word 10 ; row
		push word 0x30 ;blue
		push word str4
		call printstr1
		
		push word 22; column
		push word 12 ; row
		push word 0x30 ;blue
		push word str5
		call printstr1
		
		push word 18; column
		push word 14 ; row
		push word 0x30 ;blue
		push word str3
		call printstr1
		
		push word 24; column
		push word 16 ; row
		push word 0x30 ;blue
		push word NameStr
		call printstr1
		
		push word 25; column
		push word 17 ; row
		push word 0x30 ;blue
		push word RollNoStr
		call printstr1
		ret
		
printstr1:	push bp
			mov bp, sp
			push es
			push ax
			push cx
			push si
			push di

			push ds				; push segment of string........[bp+6]
			mov ax, [bp+4]
			push ax				; push offset of string.........[bp+4]
			call strlen			; calculate string length 

			cmp ax, 0			; is the string empty
			jz strexit1			; no printing if string is empty
			mov cx, ax			; save length in cx

			mov ax, 0xb800
			mov es, ax			; point es to video base
			mov al, 80			; load al with columns per row
			mul byte [bp+8]		; multiply with y position
			add ax, [bp+10]		; add x position
			shl ax, 1			; turn into byte offset
			mov di,ax			; point di to required location

			mov si, [bp+4]		; point si to string
			mov ah, [bp+6]		; load attribute in ah

			cld ; auto increment mode
strnextchar: 	lodsb ; load next char in al
			stosw ; print char/attribute pair
			loop strnextchar ; repeat for the whole string

strexit1: 		pop di
			pop si
			pop cx
			pop ax
			pop es
			pop bp
			ret 8 		
		

PrintExitScreen:

		push 60 ; height.....[bp + 12]
		push 200 ; width......[bp + 10]
		push 72 ; color = blue.....[bp + 8]
		push 70 ; starting row.....[bp + 6]
		push 50 ; starting col.....[bp + 4]
		call PrintBackground

		push word 9; column
		push word 10 ; row
		push word 15 ;blue
		push word str6
		call printstr
		
		push word 11; column
		push word 12 ; row
		push word 15 ;blue
		push word str7
		call printstr
		
		push word 11; column
		push word 14 ; row
		push word 15 ;blue
		push word str8
		call printstr
		ret


Music:

push ax

mov al, 0b6h
out 43h, al

infSound:


mov ax, 3619
out 42h, al
mov al, ah
out 42h, al


in al, 61h
mov ah,al
or al, 3h
out 61h, al
push word 3
call mdelay
mov al, ah
out 61h, al


push word 2
call mdelay





;load the counter 2 value for D5
mov ax, 3619;2032
out 42h, al
mov al, ah
out 42h, al


;turn the speaker on
in al, 61h
mov ah,al
or al, 3h
out 61h, al
push word 3
call mdelay

mov al, ah
out 61h, al


push word 12
call mdelay

	
mov ax, 3619;1708
out 42h, al
mov al, ah
out 42h, al

	
;turn the speaker on
in al, 61h
mov ah,al
or al, 3h
out 61h, al
push word 5
call mdelay
mov al, ah
out 61h, al


push word 12
call mdelay







;load the counter 2 value for F5
mov ax, 4560;1708
out 42h, al
mov al, ah
out 42h, al


;turn the speaker on
in al, 61h
mov ah,al
or al, 3h
out 61h, al
push word 3
call mdelay
mov al, ah
out 61h, al


push word 2
call mdelay


;load the counter 2 value for e5
mov ax, 3619;1810
out 42h, al
mov al, ah
out 42h, al

	
;turn the speaker on
in al, 61h
mov ah,al
or al, 3h
out 61h, al
push word 5
call mdelay
mov al, ah
out 61h, al


push word 17
call mdelay






;load the counter 2 value for d5
mov ax, 3034;6087;2032
out 42h, al
mov al, ah
out 42h, al
	

;turn the speaker on
in al, 61h
mov ah,al
or al, 3h
out 61h, al
push word 5
call mdelay
mov al, ah
out 61h, al

push word 4
call mdelay

;load the counter 2 value for d5
mov ax, 4063;6087;2032
out 42h, al
mov al, ah
out 42h, al
	

;turn the speaker on
in al, 61h
mov ah,al
or al, 3h
out 61h, al
push word 5
call mdelay
mov al, ah
out 61h, al


push word 15
call mdelay







jmp infSound

pop ax

ret






PrintEndingScreen:

		push 200 ; height.....[bp + 12]
		push 320 ; width......[bp + 10]
		push 72 ; color = blue.....[bp + 8]
		push 0 ; starting row.....[bp + 6]
		push 0 ; starting col.....[bp + 4]
		call PrintBackground

		push word 15; column
		push word 10 ; row
		push word 15 ;blue
		push word str9
		call printstr
		
		
		push word 15; column
		push word 14 ; row
		push word 15 ;blue
		push word str10
		call printstr
		
		push word 15; column
		push word 12 ; row
		push word 15 ;blue
		push word PointString
		call printstr
		
		push word 22 ; coloumn
		push word 12 ; row
		push word 3 ; blue
		push word [points] 
		call printnum
		
		ret


initpcb:
			push bp
			mov bp, sp
			push ax
			push bx
			push cx
			push si

			mov bx, [nextpcb] ; read next available pcb index
			cmp bx, 32 ; are all PCBs used
			je pcbExit ; yes, exit

			mov cl, 5
			shl bx, cl ; multiply by 32 for pcb start

			mov ax, [bp+6] ; read segment parameter
			mov [pcb+bx+18], ax ; save in pcb space for cs

			mov ax, [bp+4] ; read offset parameter
			mov [pcb+bx+16], ax ; save in pcb space for ip

			mov [pcb+bx+22], ds ; set stack to our segment

			mov si, [nextpcb] ; read this pcb index
			mov cl, 9
			shl si, cl ; multiply by 512
			add si, 256*2+stack ; end of stack for this thread

			;mov ax, [bp+4] ; read parameter for subroutine
			;sub si, 2 ; decrement thread stack pointer
			;mov [si], ax ; pushing param on thread stack

			sub si, 2 ; space for return address
			mov [pcb+bx+14], si ; save si in pcb space for sp
			mov word [pcb+bx+26], 0x0200 ; initialize thread flags
			mov ax, [pcb+28] ; read next of 0th thread in ax
			mov [pcb+bx+28], ax ; set as next of new thread
			mov ax, [nextpcb] ; read new thread index
			mov [pcb+28], ax ; set as next of 0th thread
			inc word [nextpcb] ; this pcb is now used

			pcbExit: 

			pop si
			pop cx
			pop bx
			pop ax
			pop bp
			ret 4


loadingScreen:
		push bp
		mov bp, sp
		sub sp, 2
		mov word[bp-2], 0
		mov ax, 0x0A000
		mov es, ax
		
		push 3 ; height.....[bp + 12]
		push 206 ; width......[bp + 10]
		push 72 ; color = blue.....[bp + 8]
		push 160 ; starting row.....[bp + 6]
		push 60 ; starting col.....[bp + 4]
		call PrintBackground
		
		
		push 3 ; height.....[bp + 12]
		push 206 ; width......[bp + 10]
		push 72 ; color = blue.....[bp + 8]
		push 135 ; starting row.....[bp + 6]
		push 60 ; starting col.....[bp + 4]
		call PrintBackground
		
		push 25 ; height.....[bp + 12]
		push 3 ; width......[bp + 10]
		push 72 ; color = blue.....[bp + 8]
		push 135 ; starting row.....[bp + 6]
		push 60 ; starting col.....[bp + 4]
		call PrintBackground
		
		push 25 ; height.....[bp + 12]
		push 3 ; width......[bp + 10]
		push 72 ; color = blue.....[bp + 8]
		push 135 ; starting row.....[bp + 6]
		push 263 ; starting col.....[bp + 4]
		call PrintBackground
		
		push word 15 ; column
		push word 15 ; row
		push word 3 ;blue
		push word loadingstr 
		call printstr
		
		mov bx, 138 ;row
		mov si, 63
		
		mov ax, 0x0A000
		mov es, ax
		;mov dx, 20
		;push dx
		mov dx, 0
		push dx
mov cx, 20
loadloop:	
push cx
		mov cx, 10
loadloop11:		
		push cx
		mov ax, 320
		mul bx
		add ax, si
		mov di, ax
		mov cx, 22
loadloop1: 		
		mov byte[es:di], 0x57
		add di, 320
		loop loadloop1
		inc si
		pop cx
		loop loadloop11
		
		push word 22 ; column
		push word 15 ; row
		push word 3 ;blue
		push word dotstr 
		call printstr
		
		;pop word[bp-2]
		add word[bp-2], 5
		
		push word 98 ; coloumn
		push word 146 ; row
		push word 3 ; blue
		push word[bp-2]
		call printnum
		
		push word 101 ; column
		push word 146 ; row
		push word 3 ;blue
		push word pcastr 
		call printstr
		;push word[bp-2]
		
		call delay
pop cx
dec cx
		jnz loadloop


call delay
call delay

		add sp, 2
		pop bp
ret

start:	

		call clrscr


		
		call PrintStartingScreen
		
		mov ah, 0								; service 0 – get keystroke			
		int 0x16
		
				
		mov ah, 00h;  graphic mode interrupt	
		mov al, 13h;
		int 10h	; video mode

		call clrscr

call loadingScreen

		
		
		call PrintMainScreen
		
		push word 32 ; column
		push word 0 ; row
		push word 3 ;blue
		push word TimerString 
		call printstr
		
		push word 0 ; column
		push word 0 ; row
		push word 3 ;blue
		push word PointString 
		call printstr
		
		push word 8 ; coloumn
		push word 0 ; row
		push word 3 ; blue
		push word [points] 
		call printnum
		
		; saving old isr	
		xor ax, ax
		mov es, ax ; point es to IVT base
		mov ax, [es:9*4]
		mov [oldisr], ax ; save offset of old routine
		mov ax, [es:9*4+2]
		mov [oldisr+2], ax ; save segment of old routine
		
		
				; saving old isr	
		xor ax, ax
		mov es, ax ; point es to IVT base
		mov ax, [es:8*4]
		mov [oldtimer], ax ; save offset of old routine
		mov ax, [es:8*4+2]
		mov [oldtimer+2], ax ; save segment of old routine


		;hooking
		cli ; disable interrupts
		mov word [es:9*4], kbisr ; store offset at n*4
		mov [es:9*4+2], cs ; store segment at n*4+2
		sti ; enable interrupts
		
		xor ax, ax
		mov es, ax ; point es to IVT base

		cli ; disable interrupts
		mov word [es:8*4], timer; store offset at n*4
		mov [es:8*4+2], cs ; store segment at n*4+2
		sti ; enable interrupts

			
push cs ; use current code segment
mov ax, Music
push ax ; use mytask as offset
call initpcb

		call Animate


	
gameExit:




mov al, 0
out 61h, al	

mov ax, 0
mov es, ax
		mov  ax, [oldisr] ; save offset of old routine
		mov  bx, [oldisr+2] ; save offset of old routine
		cli ; disable interrupts
		mov word [es:9*4], ax ; store offset at n*4
		mov [es:9*4+2], bx; store segment at n*4+2
		sti ; enable interrupts
		
		
		mov  ax, [oldtimer] ; save offset of old routine
		mov  bx, [oldtimer+2] ; save offset of old routine
		cli ; disable interrupts
		mov word [es:8*4], ax ; store offset at n*4
		mov [es:8*4+2], bx; store segment at n*4+2
		sti ; enable interrupts
		
		
		
		call PrintEndingScreen
		
			mov ah, 0								; service 0 – get keystroke			
		int 0x16

		cmp al, '1'
		jne gameExit
			
			
					mov ax, 0x0003 
		int 0x10 

mov ax, 0x3100 ; terminate and stay resident
int 0x21
; terminate
		mov ah, 4ch;
		int 21h;
;;;;;----------------------VARIABLES--------------------------------------


carrot1: dw 136, 160
carrot2: dw 0, 0
carrot3:dw 0, 0
quitFlag: dw 0 
showtime: dw 0 
brickColor1: dw 1 ; brick ka colour 1= yellow 0 = green 2=orange 3=blue
brickColor2: dw 0
brickColor3: dw 2
tickcount1: dw 0  ;counter for bluebrick 0-9
randNum: dw 0  
tickcount: dw 0 ; 18 times timer interrupt call then 1 increase
brick: dw 3
carrot: dw 0
movingBrick: dw 142, 149;sr, er
movingBrick1: dw 0, 0;sr, er
movingBrick2: dw 0, 0;sr, er
movingBrick3: dw 0, 0;sr, er
exitFlag: dw 0
BrickCordinates1: dw 140, 180
BrickCordinates2: dw 140, 180
BrickCordinates3: dw 140, 180
carrotCordinates: dw  0, 0;136, 160;;r, c
RabbitCordinates: dw 156, 164  ;c
RabbitCordinatesY: dw 186, 194 ;r
points: dw 0  
TimerString: db ' Timer:', 0
PointString: db 'Points: ', 0
oldisr: dd 0 
oldtimer: dd 0


dotstr: db '.', 0
pcastr: db '%', 0
loadflag: dw 0
loadingstr: db 'LOADING', 0
str6: db 'DO YOU WANT TO EXIT', 0
str7: db 'Yes[y]', 0 
str8: db 'No[n]', 0
NameStr: db 'Momina Asif     Imtisal Waqas', 0
str10: db 'Press 1 to Exit', 0
str9: db 'GAME OVER   ', 0
RollNoStr: db '22L-6834          22L-6695', 0
str1: db 'JUMPING RABBIT', 0
str11: db '---------------', 0
str2: db '[PRESS UP KEY TO JUMP ON NEXT BRICK]', 0 
str3: db '[BLUE BRICK HAVE A TIMER OF 10 SECONDS]', 0
str4: db '[CATCH CARROT TO INCREASE POINT]', 0
str5: db '[PRESS ESC KEY TO EXIT THE GAME]', 0
