.286
.model small
.stack 64
.data

;--------------------------user welcome------------------------------------
        box        dB 219,"                                                                              " ,219,"$"
        welcome    db 219,"                           welcome to Game of Pawns                           ",219
                   dB 219,"                            Are you ready to Rook?                            ",219,'$'
        Hello      db 10,13,219,"                         Please Enter Your UserName:"  ,'$'
        hello2     db 10,13,219,"           Hello ",219,'$'
        validation db 10,13,219,"                                Invalid Username",'$'
        
        Username   db 15,?,15 dup('$')
        enter1     db 10,13,'$'
;--------------------------------------------------------------

Esource         db      ?
Edest           db      ?

chosenSquare    db     3H ; To Whom is moving variables. Please note to move these variables togetherrrrrrrrrrrrrrrrr.
TimerFlag db    0h; 
chosenSquareColor   DB  ? ; 
numOfDirections         db      0h ; Procedure Rules sets those variables according to the piece
ArrayOfDirections       db      8 dup(0h), 0h ; Permissible moves for a piece
ArrayOfRepetitions      db      8 dup(0h), 0h ;
sourceSquare    db      0FFH ; square to be moved 
destSquare      db      0FFH ; destination after movment 
FlickeringTime  db      1h
SelectionKey    db      0dh
SELECTED        db      0h
entered        db      5 dup(0ffh)
sizeEntered           db      0ffh
EnemySourceSquare   db  0h
Currentcolor    DW      5Ah 
sourceSquareColor   dw      ?
rowX            DW      ? ; coordinates SrcSquare 
rowY            DW      ?
Flicker         dw      08h
ColorCheck      dw      2h

chosenSquareBlack    db     3bH ; 
TimerFlagBlack db    0h; 
chosenSquareColorBlack   DB  ? ; 
numOfDirectionsBlack         db      0h ; Procedure Rules sets those variables according to the piece
ArrayOfDirectionsBlack       db      8 dup(0h), 0h ; Permissible moves for a piece
ArrayOfRepetitionsBlack      db      8 dup(0h), 0h ;
sourceSquareBlack    db      0FFH ; square to be moved 
destSquareBlack      db      0FFH ; destination after movment 
FlickeringTimeBlack  db      1h
SelectionKeyBlack    db      0dh
SELECTEDBlack        db      0h
enteredBlack        db      5 dup(0ffh)
sizeEnteredBlack           db      0ffh
EnemySourceSquareBlack    db      0h
CurrentcolorBlack    DW      5Ah 
sourceSquareColorBlack   dw      ?
rowXBlack            DW      ? ; coordinates SrcSquare 
rowYBlack            DW      ?
FlickerBlack         dw      08h
ColorCheckBlak      dw      2h

ArrayOfWhiteDead             db      16 DUP(0h)
numOfWhiteDead          dw      0h
ArrayOfBlackDead             db      16 DUP(0h)
numOfBlackDead          dw      0h
ArrayOfMoves            db      55h DUP(?)
isItWhite               db      1h ; 0h ->white ; 1h -> black;  
RepetionCounter         db      1h
GoToNext                db      1h
NewSourceSquare         db      ?
DirectionCounter        dw      ?
Enemy                   dw      ? ; To Be deleted
DESELECT                db      0h
WhichTurn               dw      ? ; Source
WhichToExchange         dw      ? ; Destination
WhichIsEnemy            dw      ? ; Enemy
FirstTime               db      1h
OrganizationTrigger     db      0h
OrganizationSelector    db      0h

boardFile   db   'chess.bin', 0h; chess board 
firstState  db   'board.txt', 0h; file contain all names of the pieces names 
MovesFile   db   'moves.txt', 0h; file that contains moves
DIRECTORY       DB      'D:\Pieces',0h ; 
filehandle dw ?

Squares     DB     07h, 05h, 03h, 01h, 02h, 04h, 06h, 08h, 09h, 0ah, 0bh, 0ch, 0dh, 0eh, 0fh, 10h ; pieces order in the .txt files 
            DB     32 DUP(0h)
            DB     19h, 1ah, 1bh, 1ch, 1dh, 1eh, 1fh, 20h, 17h, 15h, 13h, 11h, 12h, 14h, 16h, 18h 

Pieces      DB    8h DUP(0), 0ffh DUP(0), 20h  ; empty 8 bytes , total 32* 8 -1 because each name have 8 bytes except the last one has 7 , 20h =32d ->number of pieace

countX      DW    ?
countY      DW    ?
SelectColor db    35h

destX           dw        ? ; coordinates of destSquare 
destY           dw        ?
sourceLocationInES     DW      ?

TokenFile       db     'outtkn1.bin', 0h

m       db      64d
a       db      25d
b       dw      8d
xo      db      30d
rand    db      ?
numberofmoves       db          ?
freezingtime        db      3h

exitCondition       db      0h

;message1        db       'To Start Chatting Press F1','$'
;message2        db       'To Start the game Press F2','$'
;message3        db       'To End The Program Press ESC','$'
message1        db       '                To Start Chatting Press F1',10,13
message2        db       '                          To Start the game Press F2',10,13
message3        db       '                          To End The Program Press ESC','$'
chatmessage     db       'Chat mode','$'
waitchatmes     db       'There is a chat invitation. If you want to accept it click f1','$'
waitplaymes     db       'There is a game invitation. If you want to accept it click f2','$'   
endmessage             db       'The Program has been ended','$'
winnerb         DB        'Black Player is the winner','$'
winnerw         DB        'White Player is the winner','$'
indicator        DB         0

inliney     db          0H
inliney1    db          0dh
inlinex     db          20H
inlinex1    dB          20h

KingW          db 0h  
KingB          db 0h 
BlackKingCheckMate db 0h ;
WhiteKingCheckMate db 0h ;
CurrentKingCheckMate    db      0h
KingTobeChecked    db 0h ; 
BlackWhiteFlag     db 1h ; 
KnightCheckMate   db  ? ; 
CHECKmsg          db  'Check' , '$'; 


boardWidth      dw     320d
boardHeight     dw     200d
ColorToDraw     Db     05AH
StartDraw       dw     44d
;--------------------------timer------------------------------------
time_left dB 64 dup(0)
ready_to_move dB  64 dup(1)
locked_squared DB   64 dup(0);
time_penalty   dB 99h
ChosenSquareTimer db 0 ; 
one    db 'one.bin', 0H
two    dB   'two.bin', 0H
three    dB   'three.bin', 0H

value db ?, "$"
cursorWrite  dw 0h
cursorRead   dw 29h
valueNotSent  db    0h, '$'

imagedata db 625 dup(?);

chessData db  9C40h dup(?); all pixels in the grid in the start 
; ------------------------------------ Problematic ----------------------------------------------------------

showmain Macro message1,message2,message3
    pusha
    mov ax,0003h
    int 10h    
    
    mov ah,2h
    mov dl,10
    mov dh,5
    int 10h  
    
    mov ah,9h
    mov dx,offset message1
    int 21h
    
    mov ah,2h
    mov dl,10
    mov dh,7
    int 10h
    
    mov ah,9h
    mov dx,offset message2
    int 21h
    
    mov ah,2h
    mov dl,10
    mov dh,9
    int 10h
    
    mov ah,9h
    mov dx,offset message3
    int 21h
    popa
ENDM

printnotification MACRO message123
    mov ah,2h
    mov dl,2d
    mov dh,23d
    int 10h  
    
    mov ah,9h
    mov dx,offset message123
    int 21h
ENDM

PlacePowerup1    Macro   ;;this macro will be executed when the number of correct movements is equal to specific number
    ;; it is call will be inside the movement process and it will have a value in the encoding to make the movement avaialble
    Incorrect12:
    call random
    cmp [rand],40H
    JAE Incorrect12
    mov bl,rand
    mov bh,0h        
    cmp Squares[bx],0H
    jnz Incorrect12
    
    mov si,offset TokenFile
    mov cx,271H
    mov di,offset chessData
    push si
    push cx
    push di
    call HandleFile
    add sp, 6H
    push bx

    mov al,[rand]
    mov ah,0h
    push ax
    call SquaresCalculation 
    pop ax
    ADD SP, 2H 
    MOV dx, [rowX]
    PUSH dx
    mov dx, [rowY]
    push dx
    Call DrawPiece
    pop dx
    pop dx
ENDM

printline MACRO
    local middle
    pusha 
    mov ah,2                    ;; print the line of the notify bar
    mov dl,0d
    mov dh,22d
    int 10h   
    mov cx,80    
    middle:         
    mov ah,2 
    mov dl,'-'
    int 21h
    loop middle
    popa
ENDM

RevertFlickering    MACRO
LOCAL FINISHLBL
mov di, [Flicker]
cmp [Currentcolor] , di ; 
                          jnz FINISHLBL ;
                          push AX; 
                          mov cx , [Currentcolor] 
                          push cx ;
                          mov cl , [chosenSquareColor]
                          mov [Currentcolor], cx
                          mov ch , 0ch ; 
                          push cx ; 
                          CALL DrawSquare
                          add sp , 4h ;
                          pop AX;
FINISHLBL:  
ENDM

; draw intial state of pieces and grid  
DrawInitialState     MACRO
    MOV DX, OFFSET firstState ; loading the board.txt file that contain all pieces name in specific order 
    MOV CX, 9C40H ; total number if pixels 
    MOV BX, OFFSET Pieces + 8 ; pieces will be holding all names of all 32 pieces (16 whites and 16 black ) from the board.txt file 
    PUSH DX  
    PUSH CX ; 
    PUSH BX ; ofsett of pieces 
    CALL HandleFile
    
    ADD SP, 6H ; emoty the stack  

    mov bx, 0ffffh ; settting bx to -1 ; to begin incrementing in loop from zero 
    DrawPiecesLoop:     inc bx; 
                        mov cl, Squares[bx] ; accessing number of the piece in the file from Square Variable
                        cmp cl, 0H ; compare to zero because if cl is zero then no piece will be drawn 
                        jz DrawPiecesLoop
                    mov al, 08h ; mov al 8 , because as said before each has 8 bytes , so i need reach the begining of the file name in the [Pieces] variable  
                    mul cl ; 8 * cl to know exaclty how many bytes I should move 
                    mov si, offset Pieces  
                    add si, ax ; si know points the the head name 
                    mov byte ptr [si + 7], 0h ; adding zero at the end of the file name to be able to read the name by the interrupt 
                    ;;;MOV DX, OFFSET firstState
                    MOV CX, 271H  ; 25d * 25d --> total number of pixel in each square 
                    MOV DI, OFFSET chessData ; will be using this to load the pixel colors from the .bin file to it; 
                    PUSH SI ; file name 
                    PUSH CX ; number of bytes 
                    PUSH DI ; place to write to the data in the memory 
                    CALL HandleFile ; explained below 
                    ADD SP, 6H ; free stack 
                    mov byte ptr [si + 7], 20h ; m4 faker yasta kona 3amleeeno leeh XD , bs seebo talma 46al 
                    
                    PUSH BX
                    CALL SquaresCalculation ; start to calculate the X and Y for the first  Square; and put in the rowX and rowY variable 
                    ADD SP, 2H 
                    MOV dx, [rowX]
                    PUSH dx
                    mov dx, [rowY]
                    push dx
                    CALL DrawPiece ; start to draw piece reading from chessdata variable a 
                    add sp, 4h

                    cmp bx, 03fh ; each the last square 
                    Jnz DrawPiecesLoop

    
    mov ax, OFFSET MovesFile
    mov bx, 0a5h ; 165 byte to be read
    mov cx, OFFSET chessData
    PUSH AX
    PUSH BX
    PUSH CX
    CALL HandleFile
    add sp, 6h

    mov si, 0h
    mov bx, 0h
    mov cl, 0ah
    mov di, 05h

    ParseMoves:     mov dl, [chessData+si]
                    sub dl, 30h
                    mov [ArrayOfMoves+bx], dl
                    mov ch, 0h
                    inc bx
                    inc si
    MovesLoop:      sub [chessData+si], 30h
                    sub [chessData+si+1], 30h
                    mov al, [chessData+si+1]
                    mul cl
                    add al, [chessData+si+2]
                    sub al, 30h
                    mov ah, [chessData+si]
                    cmp [chessData+si+3], 'A'
                    jnz CONTINUELooping
                    NEG al
                    CONTINUELooping:    mov WORD PTR [ArrayOfMoves+bx], ax
                    add bx, 2h
                    add si, 4h
                    inc ch
                    cmp ch, 8h
                    jnz MovesLoop
                    dec di
                    jnz ParseMoves

mov bx, OFFSET chessData+9D00h
mov cx, 0h
mov dl, 8h
ColorsLoop:     cmp cx, 0h
                jnz DARK
                mov [bx], 5ah
                mov cx, 1h
                jmp CONTLOOP
                DARK:   mov [bx], 06h
                mov cx, 0h
                CONTLOOP: inc bx
                          cmp bx, OFFSET chessData+9D40h
                          jz FINISHinsertion
                          dec dl
                          jnz ColorsLoop
                          XOR cx, 1h
                          mov dl, 8h
                          jmp ColorsLoop

FINISHinsertion:    
ENDM
.code
main PROC far
mov ax , @data ;
mov ds , ax ;

; initinalize COM
    ;Set Divisor Latch Access Bit
    mov dx,3fbh 			; Line Control Register
    mov al,10000000b		;Set Divisor Latch Access Bit
    out dx,al				;Out it
    ;Set LSB byte of the Baud Rate Divisor Latch register.
    mov dx,3f8h			
    mov al,0ch			
    out dx,al

    ;Set MSB byte of the Baud Rate Divisor Latch register.
    mov dx,3f9h
    mov al,00h
    out dx,al

    ;Set port configuration
    mov dx,3fbh
    mov al,00011011b
    out dx,al

call welcome_box   
call clear_screen
    mov al,0
    mov ah,2
                
    mov dx,0A0Ah
    int 0Ah ; Eh daaaaaaaaaaaaaaaaaa

ClearScreem:     
        
again:
call clear_screen ;mov [second], 0h
mov al, 3h
mov ah, 0h
int 10h
    
;showmain message1,message2,message3
mov dl,10
    mov dh,5
call set_cursor

 mov ah,9h
    mov dx,offset message1
    int 21h

printline    
    
incorrect:

;cmp [second], 1h
;jnz Second1
;mov [second], 0h
;
Second1: mov ah,0h    
int 16h
push ax
pop ax

;mov dx, 3FDh
;in al, dx
;AND al, 1h
;jz CheckStart
;
;mov dx, 3F8H
;in ah, dx
;mov [second], 1h
;

CheckStart: cmp ah, 01h                  ;; end program
jz finish
    
cmp ah,3bH                  ;; check chat
jz Chat
    
cmp ah,3ch                  ;; check game                                        
jz play

;cmp [second], 1h
;jz CheckStart
;
;jmp incorrect               

Chat:
printnotification waitchatmes    
mov ah,0h
int 16h
cmp ah,3bh
jnz again    
mov ax,0003h
int 10h
mov ah,9
mov dx, offset chatmessage
int 21h
printline
waitchat:
mov ax,0600h
mov bh,07
mov cx,0
mov dx,184FH
int 10h

mov dx, 27h
DrawSeparation: mov ah,2
mov bh, 0h
int 10h 

mov ah,9 ;Display
mov bh,0h ;Page 0
mov al,'|' ;Letter |
mov cx,2h ;5 times
mov bl, 0fh ;Green (A) on white(F) background
int 10h

add dx, 100h
cmp dx, 1927h
jnz DrawSeparation

mov dx, 0h
mov ah, 2h
mov bh, 0h
int 10h 


CHECK: mov ah,1h
int 16h
jz ReadSerial
mov ah, 0h
int 16h

mov [value], al

cmp ah, 3dh
jz SendEnd


mov ah,2
mov bh, 0h
mov dx, [cursorWrite]
int 10h

mov ah,2
mov dl, [value]
int 21h

cmp dl, 0dh ; Check Enter Case
jnz ADD2
add [cursorWrite], 100h
mov BYTE PTR [cursorWrite], 0h
jmp SCROLL2
ADD2:   add [cursorWrite], 1h

SCROLL2:    cmp BYTE PTR [cursorWrite], 27h
            JNZ CheckSC2
            add [cursorWrite], 100h
            mov BYTE PTR [cursorWrite], 0h
            CheckSC2:   cmp [cursorWrite], 1900h
            jnz WriteToUART

               impsc2: mov ah,6       ; function 6
               mov al,1        ; scroll by 1 line    
               mov bh,7       ; normal video attribute         
               mov ch,0       ; upper left Y
               mov cl,0h        ; upper left X
               mov dh,18h     ; lower right Y
               mov dl,26h      ; lower right X 
               int 10h
               sub [cursorWrite], 100h

;Check that Transmitter Holding Register is Empty
            WriteToUART:    mov dx , 3FDH		; Line Status Register

            In al , dx 			;Read Line Status
            AND al , 00100000b
            jnz SEND
            mov al, [value]
            mov [valueNotSent], al
            jmp ReadSerial

    ;If empty put the VALUE in Transmit data register
            SEND:   mov dx , 3F8H		; Transmit data register
            mov al, [value]
            out dx ,  al 
            jmp ReadSerial

            SendEnd:    mov dx, 3f8h
            mov al, 3dh
            out dx, al
            jmp exitChat

            SENDNOTSENT:    mov dx , 3F8H		; Transmit data register
            mov al, [valueNotSent]
            out dx ,  al 
            mov [valueNotSent], 0h
            jmp CHECK

ReadSerial:     mov dx , 3FDH		; Line Status Register
            	in al , dx 
                AND al , 1
                JZ CHECK

                mov dx , 03F8H
                in al , dx 
                mov [value] , al

                cmp al, 3dh
                jz exitChat

                mov ah,2
                mov bh, 0h
                mov dx, [cursorRead]
                int 10h


                mov ah,2
                mov dl, [value]
                int 21h
                
                cmp dl, 0dh ; Check Enter Case
                jnz ADD1
                add [cursorRead], 100h
                mov BYTE PTR [cursorRead], 29h
                jmp SCROLL
                ADD1:   add [cursorRead], 1h
               SCROLL:  cmp BYTE PTR [cursorRead], 4fh
                        JNZ CheckSC
                        add [cursorRead], 100h
                        mov BYTE PTR [cursorRead], 29h
                        jmp impsc
                        CheckSC:    cmp [cursorRead], 1929h
                        jnz checkNotSent

               impsc: mov ah,6       ; function 6
               mov al,1        ; scroll by 1 line    
               mov bh,7       ; normal video attribute         
               mov ch,0       ; upper left Y
               mov cl,29h        ; upper left X
               mov dh,18h     ; lower right Y
               mov dl,79      ; lower right X 
               int 10h
               sub [cursorRead], 100h

                checkNotSent: cmp [valueNotSent], 0h
                jnz SENDNOTSENT
                jmp CHECK           


exitChat: 
jz again
        
play:
printnotification waitplaymes
;cmp [second], 1h
;jnz WaitforResponse
mov ah,0h
int 16h
;jmp 

;WaitforResponse:  mov dx, 3fdh
;in al, dx
;AND al, 1h
;jz WaitforResponse



cmp ah,3ch
jnz again
jmp letsstart



;;;;;;;;;;;;;;;;;
letsstart:

; setting directory to read files 
MOV AH, 3BH
MOV DX, OFFSET DIRECTORY
INT 21H
;enter graphics mode 
mov ah,0;
mov al,13h;
int 10h;

;   This handles the rest of the background


; set the handleFile for drawing empty  grid  
MOV DX, OFFSET boardFile
MOV CX, 9C40H ; pixel number 200d  * 200d 
MOV BX, OFFSET chessData
;pushing these data because HandleFile procedure will need it;  
PUSH DX 
PUSH CX
PUSH BX
CALL HandleFile
ADD SP, 6H ; poping all pushed data to empty the stack 
; after load pixels colors to chess data 
; we need to loop over the chessdata and draw each pixel using int 10h; 
LEA bx , chessData
mov cx , 30h ; the shif in the horizontal 
mov dx , 0c8h ; total number of rows 
mov ah ,0ch ;
drawingloop :
mov al ,[Bx] ; loading pixel color in al and call int 10h ;
int 10h;
inc cx; increment counter 
inc bx; increment the bx to move to the next pixel 
cmp cx , 0f8h; C8h + 30h --> 200d + 48d shift 
JNE drawingloop ;
mov cx , 30H ; return cx to its origin value 
dec dx ; decrement to start drawing the second row 
cmp dx, 0h;
JNE drawingloop;
mov [StartDraw] , 0h 
mov[boardHeight], 200d; 
mov [boardWidth], 48d;
mov [ColorToDraw], 5ah ;
call DrawIntialBoard;

mov [StartDraw] , 248d  
mov[boardHeight], 200d; 
mov [boardWidth], 72d;
call DrawIntialBoard;

mov[boardHeight], 200d; 
mov [boardWidth], 3h; 
mov [ColorToDraw] ,08h;
mov [StartDraw], 45d 
call DrawIntialBoard;
mov[boardHeight], 200d; 
mov [boardWidth], 3h; 
mov [ColorToDraw] ,08h;
mov [StartDraw], 248d 
call DrawIntialBoard;
;waiting to press ENTER Key to display pieces 
mov ah , 0h ;
int 16h ;

;DrawingIntialState of the board with pieces 
DrawInitialState
;Waiting to Press ENTER Key to show flickering background 
mov ah , 0h ;
int 16h ;

mov cl, [chosenSquare] ; it place where the flickers begins ; black king ;
mov ch, 0h
PUSH CX
CALL SquaresCalculation
add sp, 2h

CALL GetSquareColor ; get color in current rowx and rowy 
mov dl, [chosenSquareColor] ; color in the dx ; 
mov dh, 0h
mov [Currentcolor], dx
jmp SkipFirstTime

GAMELBLBlack:
mov [isItWhite], 1h
SkipFirstTime: 
CALL GAME
CALL RecieveData

;;;;;;;;;;;;;;;;;;;;;; arrows Movement , this part is responsible for moving the flickering square in the gird 
                    Arrows:  mov ah,01h; 
                    int 16h; check if any key is pressed 
                    jz SWITCHBLACK
            HandlearrowsBlack: ; is just checking if it is Currentcolor of the square is the same as flickering color ; Before moving i need to reset it to the original backgrnd color 
                          ; so to make it short is just get the origin color from [chosenSquareColor]  and push it to the stack , so that DrawSquare procedure use it and draw the origin color square again  
                           RevertFlickering
                          COMP:
                          cmp ah, 3dh
                          jz ClearScreem
                          cmp ah , 48h ; ascii for up
                          jz UP 
                          cmp ah , 50h ; ascii for down 
                          jz DOWN 
                          cmp ah , 4Dh ;  ascii for right 
                          jz RIGHT 
                          cmp ah , 4Bh;  ascii for left 
                          jz LEFT;
                          ;jmp inlinechat
                        exit:
                          jmp MoveSquareBlack;   
            ; in this part , i just move rowX and rowY variables according to the key pressed , also i make sure that the flickering color is not getting out of the grid 
            ; also i change the  chosenSquare number becuause it will be used again ;  
            UP:
            cmp [rowX], 1h ; check if it is out of the grid
            jz exit ; 
            sub [rowX],19h; 25 pixel up 
            sub [chosenSquare], 8h ; the number decrease by 8h; 
            jmp MoveSquareBlack;
            DOWN:
            cmp [rowX] , 0B0h;
            jz exit 
            add [rowX],19h;
            add [chosenSquare], 8h; the number of square increase by 8h;  
            jmp MoveSquareBlack;
            LEFT :
            cmp [rowY] ,00h ; 
            jz exit ;
            sub [rowY],19h;
            sub [chosenSquare], 1h ;the number of square decrement by 1h;  
            jmp MoveSquareBlack;
            RIGHT:
            cmp [rowY],  0AFh; 
            jz exit
            add [rowY],19h;
            add [chosenSquare], 1h ;  the number of square increase by 1h;  
            MoveSquareBlack:         ; this will the program main Loop    
            CALL GetSquareColor ; get color in current rowx and rowy 
            mov dl, [chosenSquareColor] ; color in the dx ; 
            mov dh, 0h
            mov [Currentcolor], dx
            mov ah,0h  ; empty the buffer if the key is pressed 
            int 16h
            jmp SWITCHBLACK

SWITCHBLACK:    jmp GAMELBLBlack
;inlinechat:
;    push dx  
;    mov bx, 0H
;
;    beforeStart: 
;    cmp [inlinex+bx],28H
;    jz increasey
;    newline:
;    mov ah,2                    
;    mov dl, [inlinex+bx] 
;    mov dh, [inliney+bx]
;    int 10h
;    mov ah,2 
;    mov dl,al
;    int 21h
;
;    mov dx, 3f8h
;    out dx, al    
;    ReadNotWrite:   in al, dx
;
;    inc [inlinex+bx]
;    inc bx
;    cmp bx, 2h
;    jb beforeStart
;    pop dx
;    jmp exit
;    increasey:
;    inc [inliney+bx]
;    mov [inlinex+bx],20H
;    jmp newline
;mov ah , 0h ;
;mov al , 3h ;
;int 10h ;
; call interrup to terminate the program the return to OS 
prefinish:
Call showwinner
jmp ClearScreem

finish:  mov ah , 4ch ;
int 21h;


main ENDP

; In order for this procedure to work you have to put in DX the OFFSET of your file name variable
OpenFile PROC
mov ah , 3dh ;
mov al ,0h ;
;mov dx, offset boardFile
MOV BP, SP
mov dx, [BP]+2 ; It was a trial to make it a macro but changed
int 21h ;   
mov [filehandle], ax;

RET
OpenFile ENDP

; In order for this procedure to work you have to put in DX the OFFSET of your DataLocation
ReadData PROC ; DataLoc
mov ah , 3fh ;
mov bx , [filehandle];
MOV BP, SP
mov cx , [BP+6];
mov dx, [BP+4]; It was a trial to make it a macro but changed
int 21h;
;mov ah , 3fh;

RET
ReadData ENDP;

; The Procedure HandleFile wants by pushing OFFset of Filename then number of bytes, then address of writing to the memory
HandleFile      PROC
    MOV BP, SP ; setting bp to the stack pointer to access pushed data 

    mov dx, [BP+6] ; access file name 
    PUSH DX  ; pushing the offset of the file name to handle the file
    CALL OpenFile
    add sp, 2h; empty stack from pushed data  

    MOV BP, SP
    mov dx, [BP+2] ; adress of writing to the memory
    mov cx, [BP+4] ; number of bytes 
    push cx
    PUSH DX
    push bx ; 
    CALL ReadData
    CALL closeFile
    pop bx;

    add sp, 4h ; emtpy stack 
    RET
HandleFile ENDP
; this procedure is only when drawing the pieces in the intial state 
; it is just take the paramaters by pushing it to the stack as mentioned above ; 
; loop over pixels   and draw ; it read the pixes from readData after loading it in the main above  
DrawPiece     PROC  FAR ; To be noticed. May cause an error due to Jump Far
    mov bp, sp
    LEA si , chessData
    mov cx , 30h ; 
    ADD CX, [BP + 4] ;
    mov dx , [BP + 6] ;
    mov ah, 0ch ;

    ADD CX, 19H
    MOV [countX], CX
    SUB CX, 19H

    add dx, 19h
    MOV [countY], dx
    sub dx, 19h
    
    piecesloop :
                    mov al ,[si] ;
                    cmp al, 06h
                    jz NODRAW
                    int 10h;
                    NODRAW: inc cx;
                    inc si;
                    cmp cx , [countX];
                    JNE piecesloop ;
                    mov cx , [bp+4] ;
                    add cx, 30h
                    inc dx ;
                    cmp dx, [countY];
                    JNE piecesloop;

RET
DrawPiece   ENDP
;
DrawSquare      PROC
    mov bp, sp
    mov si, [bp+4] ; old background color 
    mov di, [bp+2] ; new color that will be drawn

    mov cx, [rowY] ; colomn 
    mov dx, [rowX] ; row 
    add cx, 48H ; 30h + 18h; 
    add dx, 18h ; 
    mov bp, [rowY]
    ADD BP, 2FH

    StartDrawing:
    ; we will start by getting the color of the pixel 
    mov ah, 0dh
    mov bh, 0h
    int 10h 
    ; if pixel have the same color of the old background , we will change it to the color in di ; 
    mov ah, 0h
    cmp si, ax
    jnz proceed
    mov ax, di
    int 10h ; 
    ; just setting decrementing X and Y after each loop  
    proceed: dec cx
             cmp cx, bp
             jnz StartDrawing
             mov cx, bp
             add cx, 19h
             dec dx
             mov ax, [rowX]
             dec ax
             cmp dx, ax
             jnz StartDrawing

    RET
DrawSquare  ENDP
;this procedures calculate the the coordinates of the Square by knowing the number of it 
; here is the way  
; grid is numbered from 0 to 63 
; rows are numbered from 0 to 7 as well as the columns  
; lets say i want to know the X [row ]and Y[column ] for square number 15 ; 
; first i will divide by 8 --> 15/8 = 1  --> row =1 ; then the x value will 25d * 1 ;
; second we need to get the remainder 15 % 8 = 7 ; the y value will 25 * 7 = 175 pixel ; in addtion to the 30h shift horizontaly ;
; Know to reach the pixel we will have to move [rowX] pixel down and [rowY] pixel right      
SquaresCalculation  PROC
    mov bp, sp

    mov dx, 0h
    mov ax, [bp+2] ; getting number of the square 
    mov cx, 8h
    div cx
    mov si, dx ; so now  contain the remainder of the division  
    mov dx, 0h
    mov cx, 19h
    mul cx
    inc ax ; adding one because there is one pixel shif in drawing 
    mov [rowX], ax ; Down or Up 
    mov ax, si
    mov cx, 19h ; 
    mul cx ; 
    ;inc ax
    mov [rowY], ax ; Left or Right

    RET
SquaresCalculation  ENDP
; get the color of the Square by getting the color of one of the pixels and load it in the [chosenSquarecolor]
GetSquareColor      PROC
    ; int 10 
    mov ah, 0dh
    mov bh, 0h
    mov cx, [rowY]
    add cx, 32h
    mov dx, [rowX]
    add dx, 2h
    int 10h
    MOV [chosenSquareColor], al

    RET
GetSquareColor ENDP

closeFile proc;
mov ah , 3eh;
mov bx , [filehandle];
int 21h;
RET ;
closeFile ENDP;

ColorSelected       PROC

mov di, [rowX]
push di
mov di, [rowY]
push di

mov [RepetionCounter], 1h ; Solves the issue of a crazy pieces
mov cl, [numOfDirections]
mov bx, 0h
mov [DirectionCounter], 0h
CALL ConHundred

BeginColoring:      mov ch, [ArrayOfDirections+bx]
                    add ch, [sourceSquare]
INDIRECTION:        mov al, ch
                    mov ah, 0h
                    mov di, ax
                    mov al, [Squares+di]
                    dec al
                    mov dl, 10h
                    div dl
                    mov ah, 0h
                    XOR al, [isItWhite]
                    cmp ax, 1h
                    jnz CONTINUEdrawing
                    cmp [ArrayOfDirections+bx], 0h
                    jnz CONFIGURE

                    CONTINUEdrawing:    push ax
                    push cx
                    mov ax, di
                    sub al, [sourceSquare] ; Can be Optimized
                    mov ah, [ArrayOfDirections+bx]
                    push ax
                    CALL OUTOFBORDER
                    add sp, 2h

                    cmp [GoToNext], 0h
                    jz RESCONFIGURE
                    push di ; Problematic. Very Problematic.
                    CALL SquaresCalculation
                    add sp, 2h
                    pop cx
                    mov bl, ch
                    mov bh, 0h
                    mov bl, [chessData+9D00h+bx]
                    mov di, bx
                    push cx
                    cmp [DESELECT], 1h
                    jz DESELECTLBL
                    cmp [OrganizationTrigger], 1h
                    jz DESELECTLBL

                    mov ax, di
                    mov bx, 0c35h
                    jmp DRAW

                    DESELECTLBL: mov ax, 35h
                              mov bx, di
                              mov bh, 0ch
                              cmp ch, [chosenSquare]
                              jnz DRAW
                              mov [chosenSquareColor], bl

                    DRAW: push ax
                    push bx
                    CALL DrawSquare
                    add sp, 4h

                    NODRAWHere: pop cx
                    mov bx, [DirectionCounter]
                    pop bp
                    mov al, [ArrayOfRepetitions+bx]
                    cmp al, 0h
                    jz CONFIGURE
                    cmp bp, 0h
                    jnz CONTADD
                    cmp [OrganizationSelector], 1h
                    jnz CONFIGURE
                    mov [OrganizationTrigger], 1h
                    CONTADD:    add ch, [ArrayOfDirections+bx]
                    inc [RepetionCounter]
                    JMP INDIRECTION

RESCONFIGURE:       pop cx
                    pop bp

CONFIGURE:          mov [OrganizationTrigger], 0h
                    inc [DirectionCounter]
                    mov bx, [DirectionCounter]
                    mov ch, 0h
                    mov [RepetionCounter], 1h
                    cmp bx, cx
                    jb BeginColoring

cmp [DESELECT], 1h
jz UNSELECT
mov [SELECTED], 1h
mov [chosenSquareColor], 35h
jmp RESETLBL
UNSELECT:   mov [SELECTED], 0h
RESETLBL:  pop di
mov [rowY], di
pop di
mov [rowX], di
RET
ColorSelected ENDP

OUTOFBORDER     PROC
mov bp, sp

mov dl, [NewSourceSquare]
mov dh, 0h

mov cl, [RepetionCounter]
mov al, 2h
mul cl
mov cl, al

mov ax, [bp + 2] ; First Two Hexadecimal is Indicator. The Second are the actual move
cmp ah, 0h
jz YES
cmp ah , 0fh;   34an lao 2ayz 27rk el +16 to move down or up PAWN
jl skip;
add al ,2h; 
skip:
cmp ah ,0f1h  ; -16 ; 
jg skip2  
sub al , 2h; 
skip2:
cmp ah, 1h
jz NOINC
cmp ah, 0ffh
jz NOINC
cmp al, 0h
jg increment
sub al, cl
jmp NOINC
increment: add al, cl
NOINC:      mov ah, 0h
            add dx, ax
            mov dh, 0h
            cmp dx, 0bh
            jb NO
            cmp dx, 58h ; 88 decimal
            ja NO
            mov ax, dx
            mov dx, 0h
            mov bp, 0ah
            div bp
            cmp dx, 0h
            jz NO
            cmp dx, 09h
            jz NO

YES:   mov [GoToNext], 1h
RET

NO: mov [GoToNext], 0h
RET
OUTOFBORDER ENDP

ConHundred      PROC

mov al, [sourceSquare]
mov ah, 0h
mov dh, al
mov dl, 08h
div dl
mov dl, 02h
mul dl
add dh, 0bh
add dh, al
mov [NewSourceSquare], dh

RET
ConHundred  ENDP

RULES proc
; ah ; Up/Down +8 ; al ; Up/Down + 16 ;
; ch ; Diagonal ; +7 , -7h; 
; cl ; Diagonal ; +9 , -9h; 
mov [isItWhite], 0h;  
mov bl , [chosenSquare] ; 
mov bh , 0h; 
cmp Squares[bx], 0h ;
jz Quit 
mov ah , 8h ;
mov al , 10h;  
mov ch , 7h;
mov cl , 9h;   
mov [numOfDirections] , 0h  ; 
mov dl , Squares[bx]; 
cmp dl , 9h; 
Jb checkBlackOrNotWhitePawn
cmp dl , 10h;  
jA checkBlackOrNotWhitePawn; 
jmp SetTheArrays; 
checkBlackOrNotWhitePawn: cmp dl, 9h
                          jb NotPawn
cmp dl , 19h ; 
jB NotPawnButBlack; 
cmp dl , 20h; 
jA NotPawnButBlack;
mov [isItWhite] , 1h; 
neg al;
neg ah;
neg ch;
neg cl;
SetTheArrays:
push bx ;
add bl , ah; 8h ;   
cmp Squares[Bx] , 0h ;
pop  bx;
jnz setMoves;
mov dl, [numOfDirections]; 
mov dh ,0h ;
mov di , dx;  
mov ArrayOfDirections[di], ah; 
mov ArrayOfRepetitions[di], 0h ; 
inc [numOfDirections]; 
;pop bx; 
cmp [isItWhite] ,0h;
jz White 
cmp bl , 30h; 
jb SetMoves
cmp bl , 37h; 
ja SetMoves; 
jmp setInitial
White:
cmp bl , 8h ;  
jb setMoves; 
cmp bl , 0fh; 
ja setMoves
setInitial:
;sub bl, ah ; 
add bl, al ;
cmp Squares[bx], 0h ; 
jnz setMoves; 
push bx ;
mov bl, [numOfDirections]; 
mov bh ,0h ; 
mov ArrayOfDirections[bx], al; 
mov ArrayOfRepetitions[bx], 0h ; 
inc [numOfDirections]; 
pop bx; 
SetMoves:   
pushA 
mov ch , 0h ;  
push cx ; 
call CheckOpponent; 
add sp ,2h ;
popA
pushA 
xchg cl , ch ;
mov ch , 0h ; 
push cx ; 
call CheckOpponent ; 
add sp ,2h ;
popA
jmp SELECT
;dec [numOfDirections]; 
NotPawnButBlack:    sub dl, 10h
                    mov [isItWhite], 1h
NotPawn:        mov al, dl
                mov ah, 0h
                mov cl, 2h
                div cl
                cmp al, 0h
                jz KING
                add al, ah
                KING:   mov cl, 11h; 17 bytes. One for count. Rest is for moves.
                mul cl
                mov bx, ax
                mov al, [ArrayOfMoves+bx]
                mov [numOfDirections], al
                inc bx
mov di, ax
dec di
LOADMOVES:      mov cx, WORD PTR [ArrayOfMoves+bx]
                mov [ArrayOfDirections+di], cl
                mov [ArrayOfRepetitions+di], ch
                add bx, 2h
                dec di
                jns LOADMOVES
   
SELECT:     mov bl, [numOfDirections]
            mov bh, 0h
            mov [ArrayOfDirections+bx], 0h
            mov [ArrayOfRepetitions+bx], 0h
            xor [isItWhite], 1h
            MOV [DESELECT], 0H
            inc [numOfDirections]
            CALL ColorSelected
Quit: 
RET
RULES ENDP

CheckOpponent proc ;; push the direction diagonal   
mov bp , sp ; 
mov cx , [bp+2]; direction 
mov bl , [chosenSquare]; 
mov bh, 0h ; 
add bx , cx ; bx contain the new direction 
mov bh , 0h ;
cmp [isItWhite] , 0h ;  
jnz Blackcheck ;
mov dl , Squares[bx];  
cmp dl , 11h;  
jb NoMove
cmp dl , 20h; 
jA NoMove; 
mov dl, [numOfDirections];
mov dh ,0h;
mov si , dx ; 
mov ArrayOfDirections[si] , cl; 
mov ArrayOfRepetitions[si],  0h ;
inc [numOfDirections]; 
Blackcheck:
mov dl , Squares[bx];  
cmp dl , 10h;  
jA NoMove
cmp dl , 1h ;
jb NoMove
mov dl, [numOfDirections];
mov dh ,0h;
mov si , dx ;  
mov ArrayOfDirections[si] , cl; 
mov ArrayOfRepetitions[si],  0h ;
inc [numOfDirections]; 
NoMove:
RET 
CheckOpponent endp 

GAME    PROC
          ;CALL BatchModeFunc
          ;cmp [sizeEntered], 0h
          ;jge FINISHGAME
          cmp [Enemy], 1h
          jz EnmeyTurn
          cmp [EnemySourceSquare], 0h
          jnz Chosen
          cmp [ColorCheck], 1h
          jnz NothingChosen
          Chosen:   mov bl, [chosenSquareColor]
          push bx
          CALL GetSquareColor
          pop ax
          mov bl, [chosenSquareColor]
          mov bh, 0h
          cmp bx, [Flicker]
          jnz BeforeNothingChosen
          mov [chosenSquareColor], al
          jmp NothingChosen
          BeforeNothingChosen:  mov [Currentcolor], bx
          NothingChosen:    inc [ColorCheck]
          mov dx, [Currentcolor]
          cmp dx, [Flicker] ; 15h is the color of the  flickering  
          jnz flashColor ; if chosenSquarecolor not equal the flickering color ; 
          mov al, [chosenSquareColor] ; 
          mov ah, 0ch ;
          jmp flashing;
          flashColor:   mov ax, [Flicker]
          add ax, 0c00h
          flashing: dec [FlickeringTime]
                    cmp [FlickeringTime], 0h
                    jnz SourceCheck
                    PUSH DX ; color of the current backgrnd;
                    PUSH AX ; color that will be drawn; 
                    CALL DrawSquare ; draw square 
                    ; wait 1 second to flicker again 
                    MOV CX, 2H
                    MOV DX, 0000H
                    MOV AH, 86H
                    INT 15H       

                    mov [FlickeringTime], 9h

                    pop dx ; color that is drawn during last call of DrawSquare , will be used in the next loop    
                    sub dx, 0c00h ; sub 0c because ax was 0c15 ;
                    mov [Currentcolor], dx
                    add sp, 2h ; free stack because i pushed dx and ax above ;

;;;;;;;;;;;;;;;;;;;;;; Moving Pieces
        ; selection begins when we press ENTER kay 
        SourceCheck:    cmp [SELECTED], 1h
                        jnz SeeKeys
                        NoProblem:  cmp [EnemySourceSquare], 0h
                        jz SeeKeys
                        mov al, [sourceSquare]
                        inc al
                        cmp [EnemySourceSquare], al
                        jnz ConTHERE
                        mov [DESELECT], 1h
                        CALL ColorSelected
                        jmp SeeKeys
                        ConTHERE:   mov [DESELECT], 0h
                        mov [OrganizationSelector], 1h
                        CALL ColorSelected
                        mov [OrganizationSelector], 0h
        SeeKeys:    mov [EnemySourceSquare], 0h
        mov ah, 1H ;
        int 16h
        cmp al, [SelectionKey] ; ENTER KEY Ascii Code ; 
        jnz FINISHGAME ; if its is not clicked  jmp to FINISHGAME (label below )
        mov ah, 0H ;
        int 16h
        
        
        ;starting to locate sourceSquare and DestSquare 
        mov bl, [chosenSquare] ; 
        mov bh, 0h ; Move Current Square to BX
        ;cmp Squares[bx], 0h ; Check if it is empty. Don't select.
        ;jz FINISHGAME
        cmp [sourceSquare], 0ffh ; if sourceSquare is 0ffh then it is not defined  yet  
        jnz Dest
        MOV AL, [Squares+bx]
        dec al
        mov ah, 0h
        shl ax, 4H
        XOR ah, [isItWhite]
        jz FINISHGAME
        ChangeSelected: mov bl, [chosenSquare] ; chosend Square will be changed every arrow move (also it appears below ) 
        mov [sourceSquare], bl ; make sourceSquare equal to chosenSquare ,so when enter is pressed sourceSquare will be the chosenSquare 
        mov bh, 0h
        mov [EnemySourceSquare], bl
        inc [EnemySourceSquare]
        mov bl, [chessData+9D00h+bx]
        mov [sourceSquareColor], bx
        RevertFlickering

        mov bh,0 
        mov bl,[chosenSquare]
        lea si, locked_squared
        mov ah,2ch ; dh second ;
        int 21h
        mov al ,0h;
        cmp  al,locked_squared[bx]
        jnz Donotmove

        CALL RULES
        mov cl, [chosenSquareColor]
        mov ch, 0h
        mov [Currentcolor], cx
        jmp FINISHGAME

        Dest:   
        
        ; CALL RULES -------------------------------------------
        mov [DESELECT], 1H
        mov al, 1h
        add al, [sourceSquare]
        mov [EnemySourceSquare], al
        cmp [chosenSquareColor], 35h
        jz GoToDest
        cmp [chosenSquareColor],00h
        jz GoToDest
        ;mov al, [chosenSquare]
        ;cmp al, [rand]
        ;jnz ContComparison
        ;mov 
        ContComparison: mov bl, [chosenSquare]
        mov bh, 0h
        cmp [Squares+bx], 0h
        jz IMPDES
        MOV AL, [Squares+bx]
        mov ah, 0h
        shl ax, 4H
        XOR ah, [isItWhite]
        jnz selectNewPiece
        IMPDES: CALL ColorSelected
                MOV [sourceSquare], 0FFH
                JMP FINISHGAME
        selectNewPiece: CALL ColorSelected
                        jmp ChangeSelected
        GoToDest:   mov bl, [chosenSquare]  
        mov [destSquare], bl ; setting desSquare after pressing the second ENTERKEY; 
        cmp bl, [sourceSquare]; make sure SourceSquare not equal to DesSquare because the piece will be deleted if the ENTER is pressed twice on the same Square
        jz FINISHGAME; jmp FINISHGAME if srcsqare == destsquare
            ;Update Square ;
        CALL ColorSelected
        mov bl, [destSquare]
        mov bh, 0h
        cmp Squares[bx], 0h
        jz NOKILL
        
        cmp Squares[bx],1H
        jz prefinish1
        cmp Squares[bx],11H
        jz prefinish2

        mov al, Squares[bx]
        cmp [isItWhite], 0h ; IsitWhite currently is the enemy not always.
        jz WhiteEnemy
        mov di, [numOfBlackDead]
        mov [ArrayOfBlackDead+di], al
        inc [numOfBlackDead]
        jmp NOKILL
        WhiteEnemy: mov di, [numOfWhiteDead]
        mov [ArrayOfWhiteDead+di], al
        inc [numOfWhiteDead]
        NOKILL:         ;; where the execution of powerup will occur
        mov al,[rand]
        mov ah,[chosenSquare]
        cmp al,ah
        jz checktokin
        jnz continue
    
        checktokin: 
        cmp [freezingtime],1
        JA executetokin
        jz continue

        executetokin: 
        dec freezingtime
        mov al,[freezingtime]
        jmp continue

        prefinish1:
        mov indicator,1
        jmp continue
        prefinish2:
        mov indicator,2
        
        continue:
        mov al, [destSquare]
        inc al
        mov [EnemySourceSquare], al
        add [EnemySourceSquare], 40h
        EnmeyTurn:  mov [ColorCheck], 1h
        mov ch , 0h ;
        mov bh , 0h ;
        mov bl , [sourceSquare] ;
        mov cl , Squares[bx]
        mov Squares[bx] , 0h;
        mov bl ,[destSquare]
        mov Squares[bx] , cl ;   

        ;;increment total number of moves and make a comparison with a constant number to draw the powerup after it
        inc numberofmoves;
        cmp [numberofmoves],05H        
        jz power
        jnz completeee

        power:
        pusha
        mov cx,[rowX]
        push cx
        mov cx,[rowY]
        push cx
        PlacePowerup1    
        mov numberofmoves,0
        pop cx
        mov [rowY],cx
        pop cx
        mov [rowX],cX
        popa

        ; we are going to use Extra segment to to write to screen directly withtout using interupt 
                ;--------------------------timer call------------------------------------
        completeee:     cmp [Enemy], 1h
        jz DrawMove
        pusha
        
        mov bh,0
        mov bl,[chosenSquare]
        mov ah,2ch
        int 21h; dh contain the time in seconds

        mov locked_squared[bx],dh
        mov[chosenSquareTimer], bl; 
        popa
        ;--------------------------------------------------------------

        ; we are going to use Extra segment to to write to screen directly withtout using interupt 
        DrawMove:   mov ax, 0a000h ;  adress of graphics part in extra segment 
        mov es, ax ;
        mov ax, [rowX] ; 
        mov [destX], ax ; setting the destX to the X of the currently selected square 
        mov bx, [rowY] ; setting the destX to the Y of the currently selected square
        add bx, 30h ; adding 30h grid shift ;
        mov [destY], bx ;

        PUSH AX ; destX;
        push bx ; destY;
        mov cl, [sourceSquare] ; 
        mov ch, 0h
        PUSH CX ; number of the source square ; 
        CALL SquaresCalculation
        add sp, 2h ; 
        add [rowY], 30h ; shift 
        pop bx
        pop ax
; this part we are trying to locate the offset of the pixel in the extra segment (rows* 320 + col ) ; each row contain 140h or 320d pixel 
        mov cx, 140h 
        mul cx ; multiplying cx * ax ; ax know is the destX ; 
        add ax, bx ; adding to the colomn part ; bx know is the destY ; 
        mov di, ax ; di know have the offset of the DestSquare pixel ;
        ;rowX and rowY know holding the coordinates of the SourceSquare ;
; also in this part we try to calcuate the offset if sourceSquare pixel in extra segment in the same way of destSquare  
        mov ax, [rowX]
        mov bx, [rowY]
        mov cx, 140h
        mul cx
        add ax, bx
        mov [sourceLocationInES], ax ; sourceLocationInES know holds the offset of the Sourcesquare pixel 
        
        mov cx, [sourceSquareColor] ; color of the background 
        mov ch, 0dh
        mov si, cx ; si color of the background  
        
        mov bl, 0h ; counter for the coloumns 
        mov bp, [rowX]
        add bp, 19h ; will be used in the nex loop just to compare if we reached the last row , loop will exit 
        MOVEPIECE:      ;get color of the pixels in source Square  
                        mov bh, 0h
                        mov cx, [rowY]
                        mov dx, [rowX]
                        mov ah, 0dh
                        int 10h
                       
                        cmp ax, si; if the pixel contain the same color of the background No copy will occure 
                        jz NOCOPY
                        STOSB ; store  es:di [location if the pixel of destination Square in extra segement] = AL [color of the pixel ]
                        mov cx, di
                        mov di, [sourceLocationInES] ; move the source Square pixel offset to start deleting the source square and move the piece  
                        mov ax, si; si contain background color ; 
                        STOSB ; es:di = al;
                        mov di, cx ; return back the offest of di 
                        JMP COPY
; NOTE : AFTER STOSB di incremented automaticaly  
                        NOCOPY: mov al, [chosenSquareColor]
                        STOSB
                        COPY: inc bl ; increment coloumn counter ;
                        inc [sourceLocationInES] ; increment to move to the next pixel 
                        inc [rowY] 
                        cmp bl, 19h ; after 25d colomn i need to move to the next row ; 
                        jnz MOVEPIECE
                        mov bl, 0h
                        ;navigate to the next row in es 
                        sub DI, 19H; sub 25d --> number of colomns ; 
                        add di, 140h ; add 320d to move to the next row in es ; NOTE : ES number the pixel in row as shown below  ; 
                        ; p1 p2 p3 
                        ; p4 p5 p6 
                        ; operation that is done to DI is done to [sourceLocationInES]
                        sub [sourceLocationInES], 19H ; 
                        add [sourceLocationInES], 140h ;  
                        sub [rowY], 19h ; decrement rowY ;  
                        add [rowX], 1h ;  
                        cmp [rowX], bp ; to check if we reached the the last Row or not 
                        jz Reset 
                        jmp MOVEPIECE


; this part resets every thing for the next loop 
Reset:  cmp indicator,1
jz prefinish
cmp indicator,2
jz prefinish
CALL CheckMate
mov ax, [destX]
mov [rowX], ax
mov ax, [destY]
sub ax, 30h
mov [rowY], ax
mov [sourceSquare], 0ffh
mov [destSquare], 0ffh

FINISHGAME: 
    mov bx , 0h ;
    mov ax , [rowX]; 
    mov cx , [rowY]; 
    mov dl , [chosenSquare];
    mov dh, [chosenSquareColor] ; 
    push ax; 
    push cx ;  
    push dx ; 

    LoopLockedSquared:    


        pushA 
        mov [chosenSquare] , bl; 
        push bx ; 
        call SquaresCalculation; 
        add sp ,2h;
        call GetSquareColor;  


        cmp locked_squared[bx], 0h ;
        jz  FinishTimer 

        mov ah , 2ch ; 
        int 21h ; 
        cmp [freezingtime],1H
        jB comp3
        mov cl , locked_squared[bx] ; 
        sub dh , cl ;
        cmp [TimerFlag] , 1h; 
        jB comp4
        cmp dh , 1h ; 
        jb FinishTimer
        cmp [freezingtime],1H
        jz comp3
        jmp comp1



        comp4:
        mov [TimerFlag] , 1h ; 

        pushA
        mov si , offset one ;
        push si ;
        call DrawNumbers; 
        pop si 
        popA ; 

        jmp FinishTimer
        comp1:

        mov ah , 2ch ; 
        int 21h ; 
        cmp [freezingtime],2H
        jb comp3
        mov cl , locked_squared[bx] ; 
        sub dh , cl ; 
        cmp [TimerFlag] , 2h; 
        jb comp5
        cmp dh , 2h ; 
        jb FinishTimer
        cmp [freezingtime],2H
        jz comp3
        jmp comp2


        comp5:
        mov [TimerFlag] , 2h ; 


        pushA 
        call DeleteNumber;
        mov si , offset two ; 
        push si
        call DrawNumbers; 
        pop si
        popA 

        jmp FinishTimer
        comp2:
        mov ah , 2ch ; 
        int 21h ; 
        cmp [freezingtime],3H
        jb comp3
        mov cl , locked_squared[bx] ; 
        sub dh , cl ; 
        cmp [TimerFlag] , 3h; 
        jB comp6
        cmp dh , 3h ; 
        jb FinishTimer
        jmp comp3


        comp6:
        mov [TimerFlag] , 3h ;

        pushA 
        call DeleteNumber;
        mov si , offset three ; 
        push si 
        call DrawNumbers; 
        pop si 
        popA 

        jmp FinishTimer
        comp3:
        mov locked_squared[bx], 0h;
        call DeleteNumber; 
        mov [TimerFlag],0h ;
        FinishTimer: 
        popA
        inc bx ; 
        cmp bx , 64d ;
        jnz LoopLockedSquared
        
        pop dx ;
        pop cx ; 
        pop ax;         
         mov [chosenSquare] , dl ;
         mov [chosenSquareColor], dh; 
        mov [rowX] , ax ;
        mov [rowY], cx; 

        Donotmove:;
RET
GAME    ENDP

SwitchTurns     PROC

mov di, [WhichTurn]
mov bx, [WhichToExchange]
mov cx, bx
add cx, 21h

TurnsLoopByte:  mov al, BYTE PTR[di]
                mov BYTE PTR [bx], al
                inc di
                inc bx
                cmp bx, cx
                jnz TurnsLoopByte

mov al, [di]
mov si, [WhichIsEnemy]
add si, 21h
mov [si], al
inc di
inc bx
add cx, 0dh

TurnsLoopWord:  mov ax, WORD PTR[di]
                mov WORD PTR [bx], ax
                add di, 2h
                add bx, 2h
                cmp bx, cx
                jnz TurnsLoopWord

RET
SwitchTurns ENDP

CheckMate proc  
; NOTE THIS FUNCTION search the positions of two  kings and put  them in  KingB and KingW positions  
mov [BlackKingCheckMate] , 0h ;  
mov [WhiteKingCheckMate] , 0h ;
mov [BlackWhiteFlag] , 1h;  
mov [RepetionCounter] , 1h ; 
call PawnCheckMate ;
mov al, [NewSourceSquare] ; 
mov ah , [sourceSquare] ; 

push ax;  
cmp [BlackKingCheckMate] , 1h ; 
jnz StartOtherPieces  
WhiteKing:
cmp [WhiteKingCheckMate] , 1h ;
jz KingDead
;KINGB pos blackKing 

StartOtherPieces:

cmp [WhiteKingCheckMate] , 1h ; 
jz BlackKingTurn;
mov bl , [Kingw]
mov [sourceSquare], bl; 
mov [KingTobeChecked]  , bl ;
call ConHundred 
; newsquare grid 
StartQueen:
mov bx , 12h ;
mov [KnightCheckMate] ,22h;

KingAll : mov [RepetionCounter] , 1h ;
          mov dx  , Word PTR ArrayOfMoves[bx] ;  dl  mov , dh  repition
          mov dh , dl;
          InnerLoop:
                push dx ; 
                call OUTOFBORDER
                pop dx ;
                cmp [GoToNext] , 0h ; 
                jz ProceedLoop
                add dl , [KingTobeChecked];
                mov cx ,dx ;   
                mov ch , 0h ; 
                mov di ,cx;
                mov cl  , Squares[di];
                cmp cl , 0h ; 
                jz ProceedInnerLoop;
                dec cl ; 
                shr cl , 4 ;
                Xor cl ,[BlackWhiteFlag] ;
                Jz EnemyLBL ; 0h -> Enemy  
                jmp ProceedLoop

                EnemyLBL: 
                sub dl ,[KingTobeChecked] ;
                mov al , Squares[di] ; 
                cmp al , 10h; 
                jB SkipSub 
                sub al , 10h ; 
                SkipSub: 
                mov cl ,2h ;
                div cl ;
                cmp al , 0h ; 
                jz EnemyKing ; 
                add al, ah   ;
                EnemyKing:
                cmp al , 4h ; 
                jA ProceedLoop; 
                mov ah , 0h  ;
                mov cl , 11h ;
                mul cl ; 
                mov si ,ax ;
                inc si ;
                mov cx , si ; 
                add cx , 10h ; 
                        CanAttack: 
                        mov ax  , word ptr ArrayOfMoves[si]; al ->mov , ah -> repeation
                        cmp ax,0h ;  
                        jz ProceedLoop
                        cmp dh , al ;
                        jnz ProceedCanAttack
                        cmp dh , dl ; 
                        jz CheckMateLBL 
                        cmp ah , 1h ; 
                        jz CheckMateLBL 
                        jmp ProceedLoop   
                        
                        ProceedCanAttack:
                        add si , 2h ; 
                        cmp si , cx  ; 
                        jnz CanAttack ; 
                        jmp ProceedLoop;  
                ProceedInnerLoop:
                sub dl , [KingTobeChecked]; 
                add dl , dh ;
                inc [RepetionCounter];
                cmp  Byte PTR ArrayOfMoves[bx+1] , 0h  ;  dl  mov , dh  repition
                jz ProceedLoop
                jmp InnerLoop
          
          ProceedLoop:   
          add bx , 2h; 
          cmp bl ,[KnightCheckMate]; 
          jnz KingAll; 
          cmp bx ,22h;
          jnz KnightCheck
          mov bx ,34h ;
          mov [KnightCheckMate] , 44h; 
          jmp KingAll

          KnightCheck:  
          cmp [BlackWhiteFlag] , 1h; 
          jz BlackKingTurn ; 
          jmp KingDead
          
          CheckMateLBL: 
          cmp [BlackWhiteFlag] , 1h ; 
          jz WhiteCheckMate
          mov [BlackKingCheckMate] ,1h ;
          jmp KingDead ;; TEMP
          WhiteCheckMate: 
          mov [WhiteKingCheckMate] , 1h ;
          BlackKingTurn:
          cmp [BlackKingCheckMate] ,1h ;
          jz KingDead
          
          mov[BlackWhiteFlag] , 0h ;  
          mov al , [KingB];
          mov[KingTobeChecked], al ;
          mov bl ,[sourceSquare] ; 
          mov bh , 0h ; 
          push bx ; 
          mov [sourceSquare] , al;
          call ConHundred ; 
          pop bx  ;
          mov [sourceSquare], bl ; 
          jmp StartQueen;   

KingDead: 
pop ax  ; 
mov [NewSourceSquare] , al ; 
mov [sourceSquare], ah ;

MesTEST :     mov al, [CurrentKingCheckMate]
              cmp [isItWhite], 1h
              jnz BlackMessage
              XOR al, [WhiteKingCheckMate]
              jz ENDTEST
              mov bl, [WhiteKingCheckMate]
              mov [CurrentKingCheckMate], bl
              mov ah, 2H
              mov dx, 0100h
              int 10h
              cmp [WhiteKingCheckMate], 1h
              jz PrintWarning
              jmp Remove

BlackMessage:   XOR al, [BlackKingCheckMate]
                jz ENDTEST
                mov bl, [BlackKingCheckMate]
                mov [CurrentKingCheckMate], bl
                mov ah, 2H
                mov dx, 1700h
                int 10h
                cmp [BlackKingCheckMate], 1h
                jz PrintWarning
                jmp Remove

PrintWarning:   mov ah, 9h
                mov dx, offset CHECKmsg
                int 21h
                jmp ENDTEST

Remove:         mov ah, 9h
                mov bh, 0h
                mov al, 20h
                mov cx, 5h
                mov bl, 0fah
                int 10h


ENDTEST: 
RET 
CheckMate ENDP

PawnCheckMate Proc
mov bx , 0ffffh ; -1   
SearchForTwoKings: 
    inc bx ; 
    mov dl , Squares[bx] ;
    cmp dl , 11h ; 
    jnz CheckWhiteKing; 
    mov [KingB] , bl; 
    CheckWhiteKing: 
    cmp dl , 1h ; 
    jnz CheckEnd; 
    mov [KingW] , bl;
    CheckEnd: 
    cmp bx , 3fh ; 63 ;
jnz SearchForTwoKings;
; checkForPwn First for BlackKing ; 
; -9 and -7 
mov dl , [sourceSquare] ;
mov dh , [NewSourceSquare] ; 
push dx;

mov ah , [KingB];  
mov [sourceSquare],ah; 
call ConHundred; 
mov ax ,0F9F9h; -7 , 01  
push ax ;  
call OUTOFBORDER;
add sp, 2h ; 
cmp GoToNext, 0h ; 
jz CheckSecondDiagonalB
mov  bl , [KingB]; 
add bl , 0F9h;
mov bh, 0h ; 
cmp Squares[bx] , 10h; 
jA CheckSecondDiagonalB ;
cmp Squares[bx]  , 9h; 
jB CheckSecondDiagonalB ; 
mov [BlackKingCheckMate] , 1h ; 
jmp OtherPiecesCheck ; 

CheckSecondDiagonalB: 
mov ax ,0F7F7h; 
push ax ;  
call OUTOFBORDER;
add sp, 2h ; 
cmp GoToNext, 0h;
jz OtherPiecesCheck;
mov  bl , [KingB]; 
add bl , 0F7h; -9 
mov bh, 0h ; 
cmp Squares[bx] , 10h; 
jA OtherPiecesCheck ;
cmp Squares[bx]  , 9h; 
jB OtherPiecesCheck ; 
mov [BlackKingCheckMate] , 1h ; 
;;;;;;;;;;;ENd of BlackKingPawnCheck 
; whitePawnCheck
OtherPiecesCheck:
mov ah , [Kingw];  
mov [sourceSquare],ah; 
call ConHundred; 
mov ax ,0707h; 
push ax ;  
call OUTOFBORDER;
add sp, 2h ; 
cmp GoToNext, 0h  ; may cause a probelm , remember to return it the the original value 
jz CheckSecondDiagonalW
mov  bl , [Kingw]; 
add bl , 07h;
mov bh, 0h ; 
cmp Squares[bx] , 19h; 
jB CheckSecondDiagonalW ;
cmp Squares[bx]  , 20h; 
jA CheckSecondDiagonalW ; 
mov [WhiteKingCheckMate] , 1h ; 
jmp PawnCheckEND ; 
CheckSecondDiagonalW: 
mov ax ,0909h; 
push ax ;  
call OUTOFBORDER;
add sp, 2h ; 
cmp GoToNext, 0h  ; may cause a probelm , remember to return it the the original value 
jz PawnCheckEND;
mov  bl , [KingW]; 
add bl , 09h;
mov bh, 0h ; 
cmp Squares[bx] , 19h; 
jB PawnCheckEND ;
cmp Squares[bx]  , 20h; 
jA PawnCheckEND ; 
mov [WhiteKingCheckMate] , 1h ; 

PawnCheckEND: 
    pop dx
    mov [sourceSquare], dl ;
    mov [NewSourceSquare] ,dh ;   
    RET 
PawnCheckMate ENDP 


BatchModeFunc      PROC

    mov [si], offset entered

    cmp [sizeEntered], 0h
    jge batchmode    

    mov ah, 1h    ;;take the first char
    int 16h
    jz NoBatch
    
    cmp ah, 0bH      ;;compare if zero
    jnz Black
    mov al, 0h
    jmp XORING
    Black:  cmp ah, 52h
            jnz CheckAbove    
            mov al, 1h

    XORING: xor [isItWhite], al
    jnz FirstTimeBatch
    jmp NoBatch

    CheckAbove: cmp al, 0dh
                jz NoBatch
    cmp ah, 10h
    jb EmptyBuffer
    cmp ah, 32h
    jbe NoBatch
    cmp ah, 48h
    jb EmptyBuffer
    cmp ah, 4BH
    jbe NoBatch
    EmptyBuffer:    mov ah, 0h    ;;empty the buffer
    int 16h
    jmp NoBatch

;; case if the start is the end???    
;; first check on the numbers that the value is inside the grid
;; second eheck is that the start one is a self playerpiece 
;; third check is that the target is a self player piece
;; fourth check is that the start one is not empty
;; fifth check is that the target one is an empty
;; sixth check is that the target one is attack
    
    
    ;;start of batch mode

    FirstTimeBatch: mov ax, 0H   ;; Empty Buffer
                    int 16h 
                    inc [sizeEntered]    
                    
    push ax
    batchmode: mov ah, 0h       ;;take char 
    int 16h                              
    ;jz NoBatch
    cmp al, 30H     ;;sompare if zero
    jz ResetBatch        ;;end batch mode

    cmp ah, 0bH      ;;compare if zero
    ja Black2
    mov al, 0h
    jmp XORING2
    Black2:  cmp ah, 52h
            ja CheckAbove
            mov al, 1h

    XORING2: xor [isItWhite], al
    jz NoBatch
    pop ax

    ;mov ah, 0h
    ;int 16h

    mov ah, 2
    mov dx, 17F0h
    int 10h
    mov dl, al
    mov ah, 2
    int 21h

    sub al, 31h       ;;if else will subtract 31h ;; To be checked
    mov bl, [sizeEntered]
    mov bh, 0h
    mov [si+bx], al    ;;put value in the array
    inc [sizeEntered]
    cmp [sizeEntered], 4H    ;;compare array sizeEntered to 4 since we need 4 numbers to execute
    jz execute 
    jmp NoBatch
    
    
    execute:      ;; before execution reset every variable to be ready for the next iteration
    mov dx,0
    mov si,dx    
    Call HandleBatchExecution
    jmp NoBatch

    ResetBatch: mov [sizeEntered], 0ffh
    
NoBatch:  
    RET
BatchModeFunc ENDP


HandleBatchExecution    PROC
;;first check
    mov bx,0 
    cmp entered[bx], 8
    jZ forcedkill
    
    inc bx
    cmp entered[bx],8
    jZ forcedkill
    
    inc bx
    cmp entered[bx],8
    jZ forcedkill2
    
    inc bx
    cmp entered[bx],8
    jZ forcedkill2        
    
    ;;if first check is false
    ;;calculate the square number 
    mov bl,0
    mov bh,0
    mov si,0
    mov al, entered[si]
    mov bl, 8H
    mul bl
    inc si
    mov ch,0
    mov cl, entered[si]   
    add ax,cx
    ; add ax, entered[si] ; Not known
    
    mov [sourceSquare], al
    inc si
    
    mov al, entered[si]
    mov bl, 8H
    mul bl
    inc si
    mov ch,0
    mov cl, entered[si]
    add ax, cx    
    mov [destSquare], al
    
    mov cx, 0h
    mov ax, 0h
    mov bx, 0h
    
    ;;if source is empty kill piece 
    mov bl, [sourceSquare]
    mov al, [destSquare] ; No use
    cmp Squares[bx], 0H
    jz forcedkill
    
    ;;if source is the destination no effect
    cmp ax, bx
    jz goback
    
    ;;if same player or not
    cmp [isItWhite], 1h
    jz TryBlack
    cmp squares[bx], 10H     ;;in this line I compared with the maximum piece value for the black or white (i don't know :] ) player
    JBE forcedkill

    TryBlack:   cmp squares[bx], 11H
                JAE forcedkill

    mov [sizeEntered], 0feh

    mov cl, [chessData+9D00h+bx]
    mov [chosenSquareColor], cl
    mov ch, 0h
    mov [Currentcolor], cx
    mov [chosenSquare], bl
    push ax
    CALL SquaresCalculation
    CALL GAME

    pop bx
    mov cl, [chessData+9D00h+bx]
    mov [chosenSquareColor], cl
    mov ch, 0h
    mov [Currentcolor], cx
    mov [chosenSquare], bl
    push ax
    CALL SquaresCalculation
    CALL Game

    inc [sizeEntered]

    jmp goback


forcedkill:     mov di, 0h
   Incorrect1111:
    call random

    CONTRAND:   mov bl, [rand]
    mov bh,0h     

    cmp Squares[bx], 0h
    jz Incorrect1111

    mov cl, Squares[bx]
    dec cl
    shr cl, 4h
    XOR cl, [isItWhite]
    jz Incorrect1111
    

    cmp Squares[bx], 1h
    jz NOKINGWhite

    cmp Squares[bx], 11h
    jnz forcedkill2
    jmp NOKINGBlack

    NOKINGWhite:          cmp [numOfWhiteDead], 1fh
                          jz forcedkill2
                          jmp Incorrect1111

    NOKINGBlack:          cmp [numOfBlackDead], 1fh
                          jz forcedkill2
                          jmp Incorrect1111



forcedkill2:
;;remove selected piece
RET 

goback:
RET
HandleBatchExecution    ENDP

LoadSquareDate proc 
pushA

mov bl , [chosenSquare]; 
mov bh  , 0h ; 
mov cl, Squares[bx] ; accessing number of the piece in the file from Square Variable
cmp cl, 0H ; compare to zero because if cl is zero then no piece will be drawn 
jz LeaveSquare
mov al, 08h ; mov al 8 , because as said before each has 8 bytes , so i need reach the begining of the file name in the [Pieces] variable  
mul cl ; 8 * cl to know exaclty how many bytes I should move 
mov si, offset Pieces  
add si, ax ; si know points the the head name 
mov byte ptr [si + 7], 0h ; adding zero at the end of the file name to be able to read the name by the interrupt 
;;;MOV DX, OFFSET firstState
MOV CX, 271H  ; 25d * 25d --> total number of pixel in each square 
MOV DI, OFFSET chessData ; will be using this to load the pixel colors from the .bin file to it; 
PUSH SI ; file name 
PUSH CX ; number of bytes 
PUSH DI ; place to write to the data in the memory 
CALL HandleFile ; explained below 
ADD SP, 6H ; free stack 
mov byte ptr [si + 7], 20h ; m4 faker yasta kona 3amleeeno leeh XD , bs seebo talma 46al 

popA
LeaveSquare:
RET
LoadSquareDate ENDP 

DrawNumbers proc ; Make SI filehandle
 ;lea si, [filename]
   
    mov bp , sp ;  
    mov si ,[bp+2];
    mov cx,271H
    mov di,offset chessData;
    push si
    push cx
    push di
    call HandleFile
    add sp, 6H
    
    MOV dx,[rowX]
    PUSH dx
    mov dx,[rowY]
    push dx
    call DrawPiece
    add sp ,4h; 

    RET
DrawNumbers endp

DeleteNumber proc 
     
 mov bx, 04h
    mov al, [chosenSquareColor]
    mov ah, 0ch
    push bx 
    push ax
    CALL DrawSquare
    add sp, 4h
    mov al, [chosenSquareColor]
    mov ah,0
    ;mov [Currentcolor], ax
    

    call LoadSquareDate; 

    MOV dx,[rowX]
    PUSH dx
    mov dx,[rowY]
    push dx
    call DrawPiece
    add sp ,4h; 
    
RET 
DeleteNumber endp 

random proc 
   mov al,xo
   mul a
   add ax,b
   div [m]
   mov [rand],ah
   mov xo,ah
RET;
random ENDP

RecieveData         PROC
    mov al, [EnemySourceSquare]
    mov dx , 3F8H		; Transmit data register
    out dx ,  al

    cmp al, 0h
    jz NoWait
    mov ah, 0h
    int 16h

    noWait: mov [EnemySourceSquare], 0h

    CONTTTTT: cmp al, 0h
    jz nostop

    nostop: mov dx , 3FDH		; Line Status Register
    In al , dx 			;Read Line Status
    AND AL, 1h
    jz NothingToDo

    mov dx, 3f8h
    in al, dx

    cmp al, 40h
    jbe SaveBeforeDoingNothing
    sub al, 41h
    mov [EnemySourceSquare], al
    
    mov [WhichTurn], OFFSET chosenSquare ; Source
    mov [WhichToExchange], OFFSET chosenSquareBlack ; Destination
    mov [WhichIsEnemy], OFFSET chosenSquareBlack    ; Copy My Move to Enemy to detect
    CALL SwitchTurns
   
    mov al, [Esource]
    mov [chosenSquare], al
    mov [sourceSquare], al ; Done
    mov [Enemy], 1h ; Remember to move 0 to it .................................................
    mov ah, 0h
    push ax
    CALL SquaresCalculation
    add sp, 2h
    CALL GetSquareColor
    
    mov al, [chosenSquareColor]
    mov ah, 0h
    mov [sourceSquareColor], ax ; source color Done
    mov al, [EnemySourceSquare] 
    mov [chosenSquare], al ; chosen square Done
    mov [destSquare], al

    mov bl, al
    mov bh, 0h
    mov al, Squares[bx]
    cmp al, 0h
    jz GoOnyourWay
    mov Squares[bx], 0h
    cmp [isItWhite], 1H
    jz BlackDead
    mov bx, [numOfWhiteDead]
    mov ArrayOfWhiteDead[bx], al
    jmp GoOnyourWay
    BlackDead:  mov bx, [numOfBlackDead]
                mov ArrayOfBlackDead[bx], al

    GoOnyourWay:    mov bl, [chosenSquare] ; Chosen Square Color Done
                    mov bh, 0h
                    push bx
                    CALL SquaresCalculation ; rowX and rowY aheh
                    add sp, 2h
                    mov bl, [chosenSquare]
                    mov bh, 0h
                    mov al, [chessData+9D00h+bx]
                    mov [chosenSquareColor], al

    CALL GAME
    mov [Enemy], 0h

    mov [WhichTurn], OFFSET chosenSquareBlack ; Source
    mov [WhichToExchange], OFFSET chosenSquare ; Destination
    mov [WhichIsEnemy], OFFSET chosenSquare    ; Copy My Move to Enemy to detect
    CALL SwitchTurns
    mov [Esource], 0h
    jmp NothingToDo

    SaveBeforeDoingNothing:    cmp [Esource], 0h
                                jnz NothingToDo
                                mov [Esource], al
                                cmp al, 0h
                                jz NothingToDo
                                dec [Esource]
    NothingToDo:    RET
RecieveData     ENDP

showwinner Proc
mov ah,2ch
int 21H
add dh,5h
mov [freezingtime],dh
cmp indicator,2
jz whitewinner
jnz blackwinner
blackwinner:
mov ah,9
mov dx, offset winnerb
int 21h
jmp winnernotification
whitewinner:
mov ah,9
mov dx, offset winnerw
int 21h
winnernotification:
mov ah,2ch
int 21H
cmp dh,freezingtime
jnz winnernotification

Ret;
showwinner ENDP

DrawIntialBoard proc
mov dx, [StartDraw];
add [boardWidth] ,dx;

MOV CX, [StartDraw]
MOV DX, 0H
MOV AH, 0CH

DRAWROW:    MOV AL, [ColorToDraw];
            INT 10H
            INC CX
            CMP CX, [boardWidth]
JNZ DRAWROW

MOV CX, [StartDraw]
INC DX
CMP DX, [boardHeight]

JNZ DRAWROW
RET 
DrawIntialBoard ENDP

welcome_box proc 

                mov al,0
                mov ah,2
                
                mov dx,0A0Ah
                int 0Ah
         
          
            
            
           
            call clear_screen

            

                
       

             call draw_line

             mov dl,0h
                mov dh,018h
                
                call set_cursor

                call draw_header



                
                
             mov dl,0h
                mov dh,0h
                
                call set_cursor
                call draw_header
            

                mov dl,5
                mov dh,5h
                
                call set_cursor
                
            
            call new_line
                
             
                mov ah,9
                mov dx,offset welcome
                int 21h

                call new_line
            
            
              
  
        User:   mov ah,9
                mov dx,offset hello
                int 21h

                mov ah,0Ah
                mov dx,offset username
                int 21h

                mov BH,username+2
                CMP BH,41H
                JB  invalid
                cmp BH,7AH
                JA  Invalid
                CMP BH,5BH
                JB  VALID
                CMP BH,61H
                JB  INVALID

        VALID:  mov ah,9
                mov dx,offset hello2
                int 21h

                mov ah,9
                mov dx,offset username+2
                int 21h

                mov ah,9
                mov dx,offset enter1
                int 21h
                jmp EXIT_welcome
                

           

        Again1:  mov AH,01
                INT 16H
                JZ  NEXT_welcome
                MOV AH,0
                INT 16H

                CMP AL,1BH
                JE  EXIT_welcome
                MOV AH,1
                MOV DX,01
                INT 14h

        NEXT_welcome:   MOV AH,03
                MOV DX,01
                INT 14h
                AND AH,01
                CMP AH,01
                JNE Again1
                MOV AH,02
                MOV DX,01
                INT 14h
                MOV DL,AL
                MOV AH,02
                INT 21h
                JMP Again1

       

        invalid:
                mov dh,8h 
                mov dl,0H

                call set_cursor

               
                mov ah,9
               mov dx,offset validation
              int 21h
                jmp user
EXIT_welcome:
ret
welcome_box endp
 ;--------------------------draw box------------------------------------
         draw_line    proc
         mov cx,24
             .draw_line:
              mov ah,9
                mov dx,offset box
                int 21h 
            loop .draw_line
            RET
          draw_line  endp

          draw_header proc
            mov ah, 02h ; Set AH to 02h to write a character to the screen
                mov dl, 219 ; Set DL to the ASCII code for the character to write
                mov cx, 80 ; Set CX to the number of times to write the character

                write_char:
                int 21h  ; Call the interrupt to write the character
                loop write_char  ; Decrement CX and go back to write_char if CX is not 0
            ret
          draw_header endp

        


            clear_screen proc
                 ;--------------------------clear screen------------------------------------
            mov ax,0600h
            mov bh,07
            mov cx,0
            mov dx,184FH
            int 10h
               ;--------------------------------------------------------------
               ret
               clear_screen endp


        set_cursor proc
        
        mov ah, 02h ; Set AH to 02h to set the cursor position
        mov bh, 0   ; Set BH to 0 to specify the active page
        ;mov dh, set_cursorX  ; Set DH to the row number (0-based)
        ;mov dl, set_cursorY ; Set DL to the column number (0-based)
      

        int 10h     ; Call the BIOS interrupt


        ret
        set_cursor endp

        new_line proc
           mov ah,9
                mov dx,offset enter1
                int 21h
        
        ret
new_line endp


End main