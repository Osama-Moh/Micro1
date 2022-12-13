.286
.model Small
.stack 64
.data

sourceSquare    db      0FFH ; square to be moved 
destSquare      db      0FFH ; destination after movment 
destX           dw        ? ; coordinates of destSquare 
destY           dw        ?
rowX            DW      ? ; coordinates SrcSquare 
rowY            DW      ?
sourceLocationInES     DW      ?

Currentcolor    DW      ? 
boardFile   db   'chess.bin', 0h; chess board 
firstState  db   'board.txt', 0h; file contain all names of the pieces names 
DIRECTORY       DB      'D:\Pieces',0h ; 
filehandle dw ?

Squares     DB     07h, 05h, 03h, 02h, 01h, 04h, 06h, 08h, 09h, 0ah, 0bh, 0ch, 0dh, 0eh, 0fh, 10h ; pieces order in the .txt files 
            DB     32 DUP(0h)
            DB     19h, 1ah, 1bh, 1ch, 1dh, 1eh, 1fh, 20h, 17h, 15h, 13h, 12h, 11h, 14h, 16h, 18h 

Pieces      DB    8h DUP(0), 0ffh DUP(0), 20h  ; empty 8 bytes , total 32* 8 -1 because each name have 8 bytes except the last one has 7 , 20h =32d ->number of pieace

countX      DW    ?
countY      DW    ?

chosenSquare    db     3cH ; 
chosenSquareColor   DB  ? ; 



chessData db  9C40h dup(?); all pixels in the grid in the start 

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

ENDM
.code
main PROC far
mov ax , @data ;
mov ds , ax ;
; setting directory to read files 
MOV AH, 3BH
MOV DX, OFFSET DIRECTORY
INT 21H
;enter graphics mode 
mov ah,0;
mov al,13h;
int 10h;
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
; closing file after loading data from it 
CALL CloseFile
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

MoveSquare:         ; this will the program main Loop    
CALL GetSquareColor ; get color in current rowx and rowy 
mov dl, [chosenSquareColor] ; color in the dx ; 
mov dh, 0h

GAME:     cmp dx, 08h ; 15h is the color of the  flickering  
          jnz flashColor ; if chosenSquarecolor not equal the flickering color ; 
          mov al, [chosenSquareColor] ; 
          mov ah, 0ch ;
          jmp flashing;
          flashColor:   mov ax, 0c08h
          flashing: PUSH DX ; color of the current backgrnd;
                    PUSH AX ; color that will be drawn; 
                    CALL DrawSquare ; draw square 
                    ; wait 1 second to flicker again 
                    MOV CX, 3H
                    MOV DX, 0F000H
                    MOV AH, 86H
                    INT 15H                    

                    pop dx ; color that is drawn during last call of DrawSquare , will be used in the next loop    
                    sub dx, 0c00h ; sub 0c because ax was 0c15 ;
                    add sp, 2h ; free stack because i pushed dx and ax above ;

;;;;;;;;;;;;;;;;;;;;;; Moving Pieces
        ; selection begins when we press ENTER kay 
        mov ah, 1H ;
        int 16h
        cmp al, 0dh ; ENTER KEY Ascii Code ; 
        jnz Arrows ; if its is not clicked  jmp to Arrows (label below )
        ;starting to locate sourceSquare and DestSquare 
        mov al, [sourceSquare] ; 
        cmp al, 0ffh ; if sourceSquare is 0ffh then it is not defined  yet  
        jnz Dest
        mov al, [chosenSquare] ; chosend Square will be changed every arrow move (also it appears below ) 
        mov [sourceSquare], al ; make sourceSquare equal to chosenSquare ,so when enter is pressed sourceSquare will be the chosenSquare 
        MOV CL, [chosenSquareColor] ; the color if the current rowX and rowY , it is changing also every arrow press
        MOV CH, 0H ;
        PUSH CX ; so it is contain the color of the background "before" the last drawSquare call 
        jmp Arrows

        Dest:   
        mov al, [chosenSquare]  
        mov [destSquare], al ; setting desSquare after pressing the second ENTERKEY; 
        cmp al, [sourceSquare]; make sure SourceSquare not equal to DesSquare because the piece will be deleted if the ENTER is pressed twice on the same Square
        jz Arrows; jmp arrows if srcsqare == destsquare
        ; we are going to use Extra segment to to write to screen directly withtout using interupt 
        push dx ; color that is drawn in the last drawSquare call ; 
        mov ax, 0a000h ;  adress of graphics part in extra segment 
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
        
        pop dx ; number of sourceSquare ;
        pop cx ; color of the background 
        push dx ; ; 
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

                        mov ah, 0h
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
                        NOCOPY: inc di ; if no copy done I need to move di to the next pixel 
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
Reset:  mov ax, [destX]
mov [rowX], ax
mov ax, [destY]
sub ax, 30h
mov [rowY], ax
mov [sourceSquare], 0ffh
mov [destSquare], 0ffh
POP DX

;;;;;;;;;;;;;;;;;;;;;; arrows Movement , this part is responsible for moving the flickering square in the gird 
                    Arrows: pushA  ; pushing all before starting 
                    mov ah,01h; 
                    int 16h; check if any key is pressed 
                    jz Nopress
                    mov ah,0h  ; empty the buffer if the key is pressed 
                    int 16h
                    jmp handlearrows
                    Nopress: POPA   
                    ;;;;
                    JMP GAME
            Handlearrows: ; is just checking if it is Currentcolor of the square is the same as flickering color ; Before moving i need to reset it to the original backgrnd color 
                          ; so to make it short is just get the origin color from [chosenSquareColor]  and push it to the stack , so that DrawSquare procedure use it and draw the origin color square again  
                          cmp [Currentcolor] , 0c08h ; 
                          jnz COMP ;
                          push AX; 
                          mov cx , [Currentcolor] 
                          sub cx , 0c00h;
                          push cx ;
                          mov cl , [chosenSquareColor]
                          mov ch , 0ch ; 
                          push cx ; 
                          CALL DrawSquare
                          add sp , 4h ;
                          pop AX; 
                          COMP:
                          cmp ah , 48h ; ascii for up
                          jz UP 
                          cmp ah , 50h ; ascii for down 
                          jz DOWN 
                          cmp ah , 4Dh ;  ascii for right 
                          jz RIGHT 
                          cmp ah , 4Bh;  ascii for left 
                          jz LEFT;
                        exit:
                          popA; 
                          jmp MoveSquare;   
            ; in this part , i just move rowX and rowY variables according to the key pressed , also i make sure that the flickering color is not getting out of the grid 
            ; also i change the  chosenSquare number becuause it will be used again ;  
            UP:
            cmp [rowX], 1h ; check if it is out of the grid
            jz exit ; 
            sub [rowX],19h; 25 pixel up 
            sub [chosenSquare], 8h ; the number decrease by 8h; 
            popA
            jmp MoveSquare;
            DOWN:
            cmp [rowX] , 0B0h;
            jz exit 
            add [rowX],19h;
            add [chosenSquare], 8h; the number of square increase by 8h;  
            popA
            jmp MoveSquare;
            LEFT :
            cmp [rowY] ,00h ; 
            jz exit ;
            sub [rowY],19h;
            sub [chosenSquare], 1h ;the number of square decrement by 1h;  
            popA
            jmp MoveSquare;
            RIGHT:
            cmp[rowY] ,  0AFh; 
            jz exit
            add [rowY],19h;
            add [chosenSquare], 1h ;  the number of square increase by 1h;  
            popA
            jmp MoveSquare;

            
            


;mov ah , 0h ;
;mov al , 3h ;
;int 10h ;
; call interrup to terminate the program the return to OS 
mov ah , 4ch ;
int 21h;


hlt
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
    CALL CloseFile
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
    mov [Currentcolor], di 

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
End main