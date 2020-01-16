[org 0x100]
jmp start
snake: times 240 dw 0;stores the location in terms of row and columns for each character of the snake
asterisk:db '*'
space: db ' '
head:db 'x'
left: db 0
right:db 0
up: db 0
down: db 0
fruit: dw 0
food:db 'o','$','*'
poison:db '?'
array: dw 0,0,0,0
foodindex: dw 0
size:dw 21
lives: db 3
random: db 4
boundary:db '='
oldkbisr:dd 0
livesRemaining: db 3
string: db'Time Remaining: '
str2: db'Lives:'
initialMins:db 78
initialSecs: db 59
MinsRemaining: db 03
timeOver: dw 0
tickCount:db 0
winner: db 'YOU WON!'
gameOver: db 'GAME OVER!'
points: db 'Score:'
score: dw 0
seconds: db 0
tickCount2: db 0
speed: db 6
flag: db 0;indicates no poison
welcome: db'LETS PLAY!!!!'
displayPoison:push cx
	push dx
	push si
	push ax
	mov bx,snake
	mov si,0
	mov ax,[array+si]
	mov cx,[size]
l13:	cmp ax,[bx]
	jne continue
	add si,2
	mov ax,[array+si]
	mov bx,snake
	cmp si,3
	jg skipPoison
continue:add bx,2
	loop l13
	mov dx,0x1
	push dx
	push ax
	mov bx,poison
	push bx
	call print
	mov byte[flag],1;indiactes poison is there
skipPoison:pop cx
	pop dx
	pop si
	pop ax
	ret
setPoison:push ax
	push bx
	mov bx,0
	mov ah,04
	mov al,07
	mov [array+bx],ax
	mov ah,10
	add bx,2
	mov [array+bx],ax
	mov ah,17
	mov al,56
	add bx,2
	mov [array+bx],ax
	mov ah,20
	mov al,75
	add bx,2
	mov [array+bx],ax
	pop bx
	pop ax
	ret
printNum: push bp
	mov bp, sp
	push es
	push ax
	push bx
	push cx
	push dx
	push di
	mov ax, 0xb800
	mov es, ax ; point es to video base
	mov ax, [bp+4] ; load number in ax
	mov bx, 10 ; use base 10 for division
	mov cx, 0 ; initialize count of digits
	nextdigit: mov dx, 0 ; zero upper half of dividend
	div bx ; divide by 10
	add dl, 0x30 ; convert digit into ascii value
	push dx ; save ascii value on stack
	inc cx ; increment count of values
	cmp ax, 0 ; is the quotient zero
	jnz nextdigit ; if no divide it again
	mov di, 3872 ;
	nextpos: pop dx ; remove a digit from the stack
	mov dh, 0x07 ; use normal attribute
	mov [es:di], dx ; print char on screen
	add di, 2 ; move to next screen location
	loop nextpos ; repeat for all digits on stack
	pop di
	pop dx
	pop cx
	pop bx
	pop ax
	pop es
	pop bp
	ret 2
timeDisplay:push ax
	push es
	push di
	push bx
	push cx
	
	mov ax,0xb800
	mov es,ax
	mov di,3982;
	
hours:	mov al,0;remaining hour's always gonna be 0
	add al,0x30
	mov ah,0x7
	mov word[es:di],ax;displaying higher digit of hour
	add di,2
	mov word[es:di],ax
	add di,2
	call colon
	add di,2
mins:	mov dl,[cs:MinsRemaining]
	add dl,0x30
	mov dh,0x7
	mov al,0
	add al,0x30
	mov ah,0x7
	mov word[es:di],ax;displaying higher digit
	add di,2
	mov word[es:di],dx;displaying lower digit
	add di,2
	call colon
	add di,2
secs:	cmp byte[cs:initialSecs],0
	jne decrement
	mov byte[cs:initialSecs],60
	dec byte[cs:MinsRemaining]
decrement:dec byte[cs:initialSecs]
	mov dl,[cs:initialSecs]
noChange:mov al,dl
	mov ah,0
	mov dl,10
	div dl;divides remaining secs by 10
	mov dl,al;dl contains quotient
	mov dh,0x7
	add dl,0x30
	mov [es:di],dx
	add di,2
	mov al,ah;al contains remainder
	add al,0x30
	mov ah,0x7
	mov [es:di],ax
	add di,2
	jmp endDisplay
colon:  push ax
	push di
	mov al,':'
	mov ah,0x7
	mov word[es:di],ax
	add di,2
	pop di
	pop ax
	ret
victory:call clrscr
	mov ax,0xb800
	mov es,ax
	mov cx,8
	mov di,1828
	mov bx,winner
msg:	mov al,[bx]
	mov ah,0x7
	mov word[es:di],ax
	add bx,1
	add di,2
	loop msg
	mov ax,0x4c00
	int 0x21
quit:	push ax
	push es
	push cx
	push di
	push bx
	
	cmp word[cs:size],240
	jne loser
	call victory
loser:	cmp byte[livesRemaining],0
	je over
	dec byte[livesRemaining]
	call displayLives
	jmp nextChance
over:	call clrscr
	mov ax,0xb800
	mov es,ax
	mov cx,10
	mov di,1828
	mov bx,gameOver
msg2:	mov al,[bx]
	mov ah,0x7
	mov word[es:di],ax
	add bx,1
	add di,2
	loop msg2
	mov ax,0x4c00
	int 0x21
nextChance:mov byte[cs:MinsRemaining],3
	mov byte[cs:initialSecs],59
	pop bx
	pop di
	pop cx
	pop es
	pop ax
	ret
endDisplay:	pop cx
	pop bx
	pop di
	pop es
	pop ax
	ret 
timer:	
	
	push ax
	mov ax,[cs:speed]
	cmp [cs:tickCount],ax
	jne alreadySet
	call printing
alreadySet:cmp byte[cs:tickCount],18
	jne endTimer
	call printing
	inc byte[cs:seconds]
	mov byte[cs:tickCount],0
	call timeDisplay
	cmp byte[cs:initialSecs],0
	jne endTimer
	cmp byte[cs:MinsRemaining],0
	jne l9
	call quit
l9:	cmp byte[cs:seconds],20
	jne endTimer
	mov byte[cs:seconds],0
	sub byte[cs:speed],2
endTimer:inc byte[cs:tickCount]	
	mov al,0x20
	out 0x20,al
	
	pop ax
	iret

border:	push ax ;this subroutine sets up the border of the game 
	push es
	push dx
	push cx
	mov dx,0
	mov cx,79
	mov ah,0
	mov al,0x22
firstRow:push ax
	push dx
	push boundary
	call print
	inc dl
	loop firstRow
	inc dh;move to next row
	mov dl,0
	mov cx,21
l6:	push ax
	push dx
	push boundary
	call print
	add dl,78
	push ax
	push dx
	push boundary
	call print
	inc dh
	mov dl,0
	loop l6
	mov cx,79
	mov dl,0
lastRow:push ax
	push dx
	push boundary
	call print
	inc dl
	loop lastRow
	mov cx,6
	mov ax,0xb800
	mov es,ax
	mov bx,str2
	mov di,3840
dis:	mov al,[bx]
	mov ah,0x7
	mov word[es:di],ax
	add di,2
	inc bx
	loop dis
	call displayLives
	call displayScore
	mov di,3952
	mov bx,string
	mov cx,15
time:	mov al,[bx]
	mov ah,0x7
	mov word[es:di],ax
	add di,2
	inc bx
	loop time
exitBorder:	pop cx
	pop dx
	pop es
	pop ax
	ret
displayLives:
	push di
	push ax
	push es
	mov di,3852
	mov ax,0xb800
	mov es,ax
	mov al,[livesRemaining]	
	add al,0x30
	mov ah,0x4
	mov word[es:di],ax
	pop es
	pop ax
	pop di
	ret
displayScore:
	push di
	push ax
	push es
	push bx
	mov bx,points
	mov di,3860
	mov ax,0xb800
	mov es,ax
	mov cx,6
printScore:	mov al,[bx]
	mov ah,0x7
	mov word[es:di],ax
	inc bx
	add di,2
	loop printScore
	push word[score]
	call printNum
	
	pop bx
	pop es
	pop ax
	pop di
	ret

printSnake:
	push bx
	push cx
	push di
	push dx
	mov cx,0x5
	push cx
	push word[snake]
	mov bx,head
	push bx
	call print;prints head
	mov di,2
	mov cx,[size]
	sub cx,2;1 for head and 1 for space at the end
body:	mov dx,0x11
	push dx
	push word[snake+di];location of character to be printed pushed
	mov bx,asterisk
	push bx
	call print
	add di,2
	loop body;characters of the remaining body of snake printed
	mov dx,0x7
	push dx
	push word[snake+di];clearing the previous location of last character of snake's body  
	mov bx,space
	push bx
	call print
	pop dx
	pop di
	pop cx
	pop bx
	ret


sound:
	push ax
	push bx
	push cx
	mov al,182
	out 43h,al
	mov ax,4560
	
	out 42h,al
	mov al,ah
	out 42h,al
	in al,61h
	
	or al,00000011b
	out 61h,al
	mov bx,25
pause1:
	mov cx,65535
pause2:
	dec cx
	jne pause2
	dec bx
	jne pause1
	in al,61h
	
	and al,11111100b
	out 61h,al

	pop cx
	pop bx
	pop ax
	ret 


initialize:push si
	push cx
	push ax 
	mov ax,0x0A0A;location of snake's head
	mov si,0
	mov cx,[size]
set:	mov [snake+si],ax
	inc al;column number incremented
	add si,2
	loop set
	mov byte[left],0
	mov byte[right],0
	mov byte[up],0
	mov byte[down],0
	pop ax
	pop cx
	pop si
	ret
clrPoison:push ax
	push word[array+bx]
	push space
	call print
	ret
movement:push si
	push cx
	push ax
	push di
skip:	mov si,[size];si set to last index of snake
	shl si,1
	sub si,2
	mov cx,[size]
	dec cx
l3:	mov ax,[snake+si-2];
	mov [snake+si],ax
	sub si,2
	loop l3
	pop ax
	pop di
	pop cx
	pop si
	ret
temp:push dx
	push cx
	push bx
	push ax
	call sound
	dec byte[livesRemaining]
	mov bx,snake
	mov cx,[size]
remove2:	mov ax,0x7
	push ax
	push word[bx]
	add bx,2
	mov dx,space
	push dx
	call print
	loop remove2
	mov word[size],21
	call initialize
	call displayLives
	
	cmp byte[livesRemaining],0
	jne popTemp
	call quit
popTemp:pop ax
	pop bx
	pop cx
	pop dx
	ret
coordinates:push bp
	mov bp,sp;takes coordinates of head as parameter
	push si
	push cx
	push ax
	push bx
	mov ax,[bp+4];new location of snake's head
	cmp ah,00
	je borderTouched
	cmp ah,22
	je borderTouched
	cmp al,00
	je borderTouched
	cmp al,78
	je borderTouched	
	mov bx,snake
	mov cx,[size]
	dec cx
checkHead:	cmp ax,[bx];checks whether the snake touches itself
	je borderTouched
	add bx,2
	loop checkHead
	call movement
	mov ax,[bp+4];setting head to the argument passed
	mov cx,4
	mov bx,0
	cmp byte[flag],1
	jne noCheckForPoison
p:	cmp ax,[array+bx];compares new location of head with all possible locations of poison
	jne next2
	mov byte[flag],0;poison removed
	mov ax,0x7
	call clrPoison
	jmp borderTouched
next2:	add bx,2
	loop p
noCheckForPoison:
	mov [snake],ax	
	mov si,ax;location of head
	mov bx,[fruit];location of fruit
	cmp si,bx
	jne popAll
	call increaseSize
	call fruitGen
	add byte[score],4
	mov al,[score]
	mov dl,16
	mov ah,0
	div dl
	cmp ah,0
	jne noPoison
	call displayPoison
noPoison:	call displayScore
	jmp popAll
	
borderTouched:
	call sound
	dec byte[livesRemaining]
	mov bx,snake
	mov cx,[size]
remove:	mov ax,0x7
	push ax
	push word[bx]
	add bx,2
	mov dx,space
	push dx
	call print
	loop remove 
	call initialize
	call displayLives
	cmp byte[livesRemaining],0
	jne popAll
	call quit
popAll:	pop bx
	pop ax
	pop cx
	pop si
	pop bp
	ret 2
increaseSize:
	push ax
	push bx
	push cx
	mov cx,4
	mov dx,2
shifting:mov ah,[snake+1];row number
	mov al,[snake];column number
	cmp ah,[snake+2+1]
	jne vertical
	cmp al,[snake+2]
	jl decreasingCol
	inc al
	jmp save
decreasingCol:dec al
	jmp save
vertical:cmp ah,[snake+2]
	jl decreasingRow
	inc ah
	jmp save
decreasingRow:dec ah
save:	push ax;location of new character saved
	mov dx,[size]
	shl dx,1
	mov si,0
check:	cmp ax,[snake+si]
	jne noTouch
	call temp
	pop ax
	jmp pop30
noTouch:add si,2
	cmp si,dx
	jnz check

	mov bx,[size]
	shl bx,1
	sub bx,2
l7:	mov ax,[snake+bx]
	mov [snake+bx+2],ax
	sub bx,2
	cmp bx,0
	jge l7
	pop ax
	mov [snake],ax
	inc word[size]
	mov dx,240
	cmp word[size],dx
	jne nvm
	call victory
nvm:	call printSnake
	loop shifting
	mov ax,0x7
	push ax
	push word[fruit]
	
	mov bx,space
	push bx
	call print
pop30:	pop cx
	pop bx
	pop ax
	ret

kbisr:	push ax
	in al,0x60
	cmp al,75
	jne notA
	cmp byte[left],1
	je end
	mov byte[left],1
	mov byte[right],0
	mov byte[up],0
	mov byte[down],0
	jmp end
notA:  	cmp al,77
	jne notAD
	cmp byte[right],1
	je end
	mov byte[right],1
	mov byte[left],0
	mov byte[up],0
	mov byte[down],0
	jmp end
notAD:  cmp al,72
	jne d
	cmp byte[up],1
	je end
	mov byte[up],1
	mov byte[left],0
	mov byte[right],0
	mov byte[down],0
	jmp end
d:	cmp al,80
	jne end
	cmp byte[down],1
	je end
	mov byte[down],1
	mov byte[left],0
	mov byte[right],0
	mov byte[up],0
end:	pop ax
	jmp far[cs:oldkbisr]
	
print:	push bp
	mov bp,sp
	push bx
	push cx
	push dx
	push es
	push ax
	mov ah, 0x13 ; service 13 - print string
	mov al, 1 ; subservice 01 – update cursor
	mov bh, 0 ; output on page 0
	mov bl, [bp+8] ; normal attrib
	mov dx,[bp+6]; row 10 column 3
	mov cx, 1 ; length of string
	push cs
	pop es ; segment of string
	mov bp, [bp+4]; offset of string
	int 0x10 ; call BIOS video service
	pop ax
	pop es
	pop dx
	pop cx
	pop bx
	pop bp
	ret 6
fruitGen:
	push bp
	mov bp,sp
	push ax
	push bx
	push cx
	push dx
	push si
 	push di

	
	mov cx,[size]
	shl cx,1
	mov bx,0
	
	mov al,1
	mov ah,1
	
reset:
	add byte[random],2
	jmp again
init:
	mov byte[random],8

	
again:	
	add al,[random]
	add al,4
	cmp al,79
	ja init
	je reset
	add ah,[random]
	sub ah,2
	cmp ah,23
	ja init
	je reset

	cmp al,0
	je again
	
	cmp ah,0
	je again
	
l5:
	cmp ax, [snake+bx]
	je again
	add bx,2
	cmp bx,cx
	jne l5

	mov dx,0x4
	push dx
	push ax
	mov [fruit],ax
	mov bx,food
	cmp word[foodindex],3
	jl goAhead
	mov word[foodindex],0
goAhead:add bx,[foodindex]
	inc word[foodindex]
	push bx
	call print
	

	pop di
	pop si
	pop dx
	pop cx
	pop bx
	pop ax
	pop bp
	ret
clrscr:	push dx
	push cx
	push bx
	push ax
	mov cx,2000;clears screen using BIOS service
	mov dx,0x7
	
	mov al,00
	mov ah,00
l1:	push dx
	push ax
	mov bx,space
	push bx
	call print
	cmp al,79
	je clear
	inc al
	jmp next
clear:  mov al,0
	inc ah
next:   loop l1
	pop ax
	pop bx
	pop cx
	pop dx
	ret
printing:
	push ax
	cmp byte[cs:right],0
	je checkleft
	mov al,[cs:snake];column number of snake's head
	cmp al,79
	je exit;snake needs to remain there at the last index if end of screen reached
	inc al;column number incremented
	mov ah,[cs:snake+1];previous row of snake retained
	push ax;new location of snake's head pushed
	call coordinates
	call printSnake
	jmp exit
checkleft:
	cmp byte[cs:left],0
	je checkUp
	mov al,[cs:snake];column number of snake's head
	cmp al,0
	je exit
	dec al;column number decremented
	mov ah,[cs:snake+1];previous row of snake retained
	push ax;new location of snake's head pushed
	call coordinates
	call printSnake
	jmp exit
checkUp:cmp byte[cs:up],0
	je checkDown
	mov ah,[cs:snake+1];ah contains row number of head
	cmp ah,0
	je exit
	dec ah
	mov al,[cs:snake];previous column of snake retained
		
	push ax;new location of snake's head pushed
	call coordinates
	call printSnake
	jmp exit
checkDown:cmp byte[cs:down],0
	je exit
	mov ah,[cs:snake+1];ah contains row number of head
	cmp ah,23
	je exit
	inc ah;row number incremented
	mov al,[cs:snake]
	push ax
	call coordinates
	call printSnake
	jmp exit
exit:	pop ax
	ret

start: call clrscr
	mov cx,11
	mov ax,0xb800
	mov es,ax
	mov ah,0x5
	mov bx,welcome
	mov di,1828
l90:	mov al,[bx]
	add bx,1
	mov word[es:di],ax
	add  di,2
	loop l90
	mov ah,0
	int 0x16
	call clrscr
	call border
	xor ax,ax
	mov es,ax
	mov ax,[es:9*4]
	mov [oldkbisr],ax
	mov ax,[es:9*4+2]
	mov [oldkbisr+2],ax
	cli
	mov word[es:9*4],kbisr
	mov word[es:9*4+2],cs
	sti
	mov byte[cs:MinsRemaining],3
	xor ax, ax
	mov es, ax ; point es to IVT base
	cli ; disable interrupts
	mov word [es:8*4], timer; store offset at n*4
	mov [es:8*4+2], cs ; store segment at n*4+2
	sti ; enable interrupts
	call initialize
	call setPoison
	call printSnake
	call fruitGen
delay: jmp delay
	