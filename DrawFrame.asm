.model tiny                         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.code                               ;                         Программа вывода цветной рамочки                         ;
org 100h                            ;                            (например, как эта рамка)                             ;
                                    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Start: jmp MAIN
;---------------------------------------------------------
MESSAGE: db "wqds$"

SYMBOLS: db 0c9h, 03h, 0bbh, 03h, 0h, 03h, 0c8h, 03h, 0bch

STYLES: 
        db 0c9h, 03h,  0bbh
        db 03h,  0h,   03h
        db 0c8h, 03h,  0bch

        db 0dah, 0c2h, 0bfh
        db 0c3h, 0h,   0b4h
        db 0c0h, 0c1h, 0d9h

        db 0c9h, 0cbh, 0bbh
        db 0cch, 0h,   0b9h
        db 0c8h, 0cah, 0bch

        db 0d6h, 0d2h, 0b7h
        db 0c7h, 0h,   0b6h
        db 0d3h, 0d0h, 0bdh

        db 0d5h, 0d1h, 0b8h
        db 0c6h, 0h,   0b5h
        db 0d4h, 0cfh, 0beh
;---------------------------------------------------------
    MAIN:
            call GET_X_Y
            call GET_COLOR
            push di

            call GET_BIAS
            call GET_EVEN
            pop di

            mov dx, bx              ;\
            mov bx, cx              ;| <=> mov ah, cl
            mov ah, bl              ;|
            mov bx, dx              ;/
            call GET_STYLE_FRAME

            call CTOR_DRAW_FRAMES
            call DRAW_FRAMES
            ;call MY_STRLEN
            ;call PRINT_MESSAGE

            call MY__END
;---------------------------------------------------------
    GET_X_Y proc
; 
; ENTRY:   None
; EXIT:    AX - frame width  (X)
;          BX - frame height (Y)
; DESTROY: AX, BX, CX, DX, SI, DI

            mov si, 082h
            call MY_ATOI
            mov cx, bx

            mov di, si
            call SKIP_SPACE

            mov si, di
            call MY_ATOI

            mov di, si
            call SKIP_SPACE
            
            mov ax, cx
            ret
    endp
;---------------------------------------------------------
    GET_COLOR proc
;
;
;           
           mov dx, ax       ;| - сохранить значение ax в dx (ненужный регистр)
           mov si, di
           call MY_ATOHEX

           mov di, si
           call SKIP_SPACE
           mov ax, dx       ;| - вернуть прошлое значение ax
           ret
    endp
;---------------------------------------------------------
        MY_ATOI proc
; Converts a number consisting of ascii codes into a hex number
; ENTRY:   None
; EXIT:    BX - result
; DESTROY: AX, BX, DX, SI 

           mov ax, [si]         ;\
           cmp al, 0h           ;| - если по адресу ds:si = 0 (нет аргументов командной строки) => завершаем программу
           je OUT__             ;/

           xor ax, ax
           xor bx, bx
           lodsb
           
        CONDITION_ATOI:
           cmp al, '9'
           ja NOT_NUMBER

           cmp al, '0'
           jb NOT_NUMBER

           sub al, '0'
           mov dx, bx   ;\
           shl bx, 3    ;|  <=> bx = bx * 10   
           shl dx, 1    ;|
           add bx, dx   ;/
           add bx, ax
           lodsb
           jmp CONDITION_ATOI

        NOT_NUMBER:
           ret

        OUT__: call MY__END
   endp
;---------------------------------------------------------
        SKIP_SPACE proc

; Skips whitespace characters until it reaches the first non-whitespace character. 
; ENTRY:   AL - current symbol in string
;          DI - address of current symbol
; EXIT:    DI - address of the first non-space character encountered
; DESTROY: AL, SI, DI

           mov si, ds
           mov es, si
           mov al, byte ptr es:[di] 
           cmp al, ' '
           jne OUT_SKIP_SPACE

           mov al, ' '
           repe scasb
           dec di

        OUT_SKIP_SPACE: 
           ret
    endp
;---------------------------------------------------------
        MY_ATOHEX proc

; Converts a number consisting of ascii codes into a hex number
; ENTRY:   None
; EXIT:    CX - result
; DESTROY: AX, CX, DX, SI 

           mov ax, [si]         ;\
           cmp al, 0h           ;| - если по адресу ds:si = 0 (нет аргументов командной строки) => завершаем программу
           je OUT__             ;/

           xor ax, ax
           xor cx, cx
           lodsb
           
        CONDITION_ATOHEX:
           cmp al, '0'
           jb MY_NOT_NUMBER

           cmp al, '9'
           ja CompareBigLetters
           
           sub al, '0'
           jmp ConvertingToHex
        
        CompareBigLetters:
           cmp al, 'A'
           jb MY_NOT_NUMBER

           cmp al, 'F'
           ja CompareSmallLetters
           
           sub al, 'A'
           add al, 10
           jmp ConvertingToHex

        CompareSmallLetters:
           cmp al, 'a'
           jb MY_NOT_NUMBER

           cmp al, 'f'
           ja MY_NOT_NUMBER
           
           sub al, 'a'
           add al, 10

        ConvertingToHex:
           shl cx, 4
           add cx, ax
           lodsb
           jmp CONDITION_ATOHEX

        MY_NOT_NUMBER:
           ret

        MY_OUT__: call MY__END
   endp
;---------------------------------------------------------
    GET_BIAS proc

; ENTRY: AX - x 
;        BX - y
; EXIT: DI - result
; DESTROY: DI, SI
            mov di, ax
            sub di, 80     ; bias_x = (160 - 2x)/2 = 80 - x
            neg di                  

            mov si, bx
            sub si, 25     ; bias_y = (25 - y)/2
            neg si
            shr si, 1

            mov dx, si
            shl si, 6      ; bias_common = (bias_y * 160) + bias_x
            shl dx, 4
            add si, dx
            shl si, 1
            add si, di
            ret
    endp
;---------------------------------------------------------
    GET_EVEN proc
; Rounding function to an even number
; ENTRY  : AX
; EXIT   : SI - result 
; DESTROY: SI

        and si, 0fffeh
        ret
   endp
;---------------------------------------------------------
    GET_STYLE_FRAME proc
;
; ENTRY:   DI - ASCII symbol of style frame
; EXIT:    BH - hex digit frame style
; DESTROY: BH

           mov bh, byte ptr ds:[di]
           sub bh, '0'
           inc di
           push ax
           push si
           call SKIP_SPACE
           pop si
           pop ax
           ret
    endp
;---------------------------------------------------------
    CTOR_DRAW_FRAMES proc
;
;
;
;          
           mov cx, ax       ;\
           and cx, 0ffh     ;| <=> mov ah, cl &&  подготовка регистра cx как счётчика для DRAW_LINE
           sub cx, 2        ;/

           push di          ;\
           and ax, 0ff00h   ;| - сохранение значений
           push ax          ;/

           mov dx, 0b800h   ;\
           mov es, dx       ;/ <=> es = 0b800h + si (bias)

           mov di, si
           xor ax, ax

           mov al, bh       ;\
           cmp al, 0h       ;| - сравниваем стиль рамки с "0". Если да => переход по метке 
           je ADD_STYLE     ;/
           
           dec ax           ;|
           mov dx, ax       ;| 
           shl ax, 3        ;|
           add ax, dx       ;| <=> mov si, offset STYLES + (bh - 1)*9
           lea dx, STYLES   ;|
           add ax, dx       ;|
           mov si, ax       ;/

           xor dx, dx       ;\
           mov dl, bl       ;| - подготовка регистра dx как счётчика для DRAW_FRAME_CYCLE
           sub dx, 2        ;/
           ret
    endp
;---------------------------------------------------------
    DRAW_FRAMES proc
;
;
;
;
           pop ax
           push cx 
           push di          ;\
           call DRAW_LINE   ;|
           pop di           ;| - print first line
           pop cx           ;|
           add di, 160      ;/
        
        DRAW_FRAME_CYCLE:
           push cx
           push di
           call DRAW_LINE
           pop di
           add di, 160
           pop cx
           sub si, 3
           dec dx
           cmp dx, 0h
           jne DRAW_FRAME_CYCLE

           add si, 3
           call DRAW_LINE  ;| - print last line

           pop di
           ret
    endp
;---------------------------------------------------------
    DRAW_LINE proc
;
;
;
           lodsb            ;| <=> mov al, ds:[si]  ;\ - print first symbol
           stosw                                    ;/

        DRAW_LINE_CYCLE:
           lodsb            ;| <=>  mov es[di], ax   add di, 2
           stosw
           dec si
           loop DRAW_LINE_CYCLE

           inc si
           lodsb            ;\ - print last symbol
           stosw            ;/

           ret
    endp
;---------------------------------------------------------

    MY_STRLEN proc

; MY_STRLEN counts the number of characters in the string until it reaches the '$' character
; ENTRY: None
; EXIT:  CX - result
; DESTR: AL, DI, CX

            mov al, '$'
            mov di, ds
            mov es, di
            lea di, MESSAGE
            xor cx, cx
            dec cx
            repne scasb
            neg cx
            sub cx, 2
            ret
    endp
;---------------------------------------------------------
    PRINT_MESSAGE proc
; Print message in center of frame
; ENTRY  : 
; DESTROY: AX, BX

            call GET_BIAS_MESSAGE
            call GET_EVEN
            call PRINT
            ret
    endp
;---------------------------------------------------------
    GET_BIAS_MESSAGE proc
;
;
;
            mov si, cx
            sub si, 2000   ; bias_common = bias_x + bias_y = (25 - 1)/2 * 160 + (160 - 2x)/2 = 2000 - x
            neg si
            ret
    endp
;--------------------------------------------------------
    PRINT proc
; Function for printing text at a calculated address
; ENTRY  : SI - bias
;          DX - color of text
; DESTROY: 
            mov bx, 0b800h
            mov es, bx
            mov bx, offset MESSAGE
            xor ax, ax

    BELOW_PRINT:
            mov di, [bx]
            mov es: [si], di
            mov es: [si+1], dh
            inc bx
            inc ax
            add si, 2

    CONDITION_PRINT:
            cmp ax, cx
            jb BELOW_PRINT
            ret
    endp
;---------------------------------------------------------
    MY__END proc

    MY_END_Label:
            mov ah, 4ch
            int 21h
            ret
    endp
;---------------------------------------------------------
end     Start
;---------------------------------------------------------
