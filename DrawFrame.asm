.model tiny                         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.code                               ;                                                                                  ;
    locals @@                       ;                         Программа вывода цветной рамочки                         ;
    org 100h                        ;                            (например, как эта рамка)                             ;
                                    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    Start: jmp MAIN_DRAW_FRAME
;---------------------------------------------------------

    START_COMMAND_LINE      equ     082h        ;| - адрес начала команднонй строки
    VIDEOMEM_SEGMENT        equ     0b800h      ;| - адрес начала видеопамяти
    ASCII_CARRIAGE_RETURN   equ     0dh         ;| - ASCII-код символа возврата каретки
    WIDTH_DISPLAY           equ     80          ;| - ширина дисплея (в знакоместах)
    LENGTH_DISPLAY          equ     25          ;| - высота дисплея (в знакоместах)
    BYTE_WIDTH_DISPLAY      equ     160         ;| - ширина дисплея (в байтах)
    BYTE_MASK_EVEN          equ     0fffeh      ;| - битовая маска числа   1 1 1 1 1 1 1 1 1 1  1  1  1  1  1  1  0 b
                                                ;|                       № 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 15 16

    OFFSET_FROM_STACK       equ     8           ;| - в ф-ции ADD_FRAME_STYLE я обращаюсь к стеку через sp и смещение относительно него.
                                                ;| - Значение 8 может быть другим если в код программы перед данной ф-цией добавить push'ей/ pop'ов,
                                                ;| - поэтому нужно быть аккуратнее с этой константой !

;---------------------------------------------------------
STYLES: 
        db 0dah, 0c4h, 0bfh
        db 0b3h, 0h,   0b3h     ; First style
        db 0c0h, 0c4h, 0d9h

        db 0c9h, 0cdh, 0bbh
        db 0bah, 0h,   0bah     ; Second style
        db 0c8h, 0cdh, 0bch

        db 0c9h, 03h,  0bbh
        db 03h,  0h,   03h      ; Third style - "Valentine's Day" style
        db 0c8h, 03h,  0bch

        db 0dah, 0c2h, 0bfh
        db 0c3h, 0h,   0b4h     ; Fourth style
        db 0c0h, 0c1h, 0d9h

        db 0c9h, 0cbh, 0bbh
        db 0cch, 0h,   0b9h     ; Fifth style
        db 0c8h, 0cah, 0bch

        db 0d6h, 0d2h, 0b7h
        db 0c7h, 0h,   0b6h     ; Sixth style
        db 0d3h, 0d0h, 0bdh

        db 0d5h, 0d1h, 0b8h
        db 0c6h, 0h,   0b5h     ; Seventh style
        db 0d4h, 0cfh, 0beh

        db 0dbh, 0dfh, 0dbh
        db 0dbh, 0h,   0dbh     ; Eighth style
        db 0dfh, 0dfh, 0dfh
;---------------------------------------------------------
    MAIN_DRAW_FRAME: 
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

        push di
        call CTOR_DRAW_FRAMES
        call DRAW_FRAMES
        pop di

        call CHECK_NEW_STYLE

        dec cx                  ;| - чтобы CX стал равным ffffh и repe scasb в ф-ции SKIP_SPACE работал корректно
        call SKIP_SPACE
        call MY_STRLEN
        call PRINT_MESSAGE

        call TERMINATE_PROGRAMM
;---------------------------------------------------------
    GET_X_Y proc
; 
; ENTRY:   None
; EXIT:    AX - frame width  (X)
;          BX - frame height (Y)
; DESTROY: AX, BX, CX, DX, SI, DI

        mov si, START_COMMAND_LINE
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

        mov ax, [si]                 ;\
        cmp al, 0h                   ;| - если по адресу ds:si = 0 (нет аргументов командной строки) => завершаем программу
        je @@TERMINATE               ;/

        xor ax, ax
        xor bx, bx
        lodsb
        
    @@CONDITION:
        cmp al, '9'
        ja @@NOT_NUMBER

        cmp al, '0'
        jb @@NOT_NUMBER

        sub al, '0'
        mov dx, bx   ;\
        shl bx, 3    ;|  <=> bx = bx * 10   
        shl dx, 1    ;|
        add bx, dx   ;/
        add bx, ax
        lodsb
        jmp @@CONDITION

    @@NOT_NUMBER:
        ret

    @@TERMINATE: call TERMINATE_PROGRAMM
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
        jne @@OUT

        repe scasb
        dec di

    @@OUT: 
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
           je @@TERMINATE       ;/

           xor ax, ax
           xor cx, cx
           lodsb
           
        @@CONDITION:
           cmp al, '0'
           jb @@NOT_NUMBER

           cmp al, '9'
           ja @@CompareBigLetters
           
           sub al, '0'
           jmp @@ConvertingToHex
        
        @@CompareBigLetters:
           cmp al, 'A'
           jb @@NOT_NUMBER

           cmp al, 'F'
           ja @@CompareSmallLetters
           
           sub al, 'A'
           add al, 10
           jmp @@ConvertingToHex

        @@CompareSmallLetters:
           cmp al, 'a'
           jb @@NOT_NUMBER

           cmp al, 'f'
           ja @@NOT_NUMBER
           
           sub al, 'a'
           add al, 10

        @@ConvertingToHex:
           shl cx, 4
           add cx, ax
           lodsb
           jmp @@CONDITION

        @@NOT_NUMBER:
           ret

        @@TERMINATE : call TERMINATE_PROGRAMM
   endp
;---------------------------------------------------------
    GET_BIAS proc

; ENTRY: AX - x 
;        BX - y
; EXIT: DI - result
; DESTROY: DI, SI
        mov di, ax
        sub di, WIDTH_DISPLAY     ; bias_x = (160 - 2x)/2 = 80 - x
        neg di                  

        mov si, bx
        sub si, LENGTH_DISPLAY    ; bias_y = (25 - y)/2
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

        and si, BYTE_MASK_EVEN
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
;===========================================================================================================================
; Итого перед рисованием рамки в регистрах лежат:
;          AH - код цвета
;          AL - ширина рамки (X)
;          BH - стиль рамки
;          BL - высота рамки (Y)
;          SI - смещение начала рамки относитеьно левого верхнего угла дисплея (всего 80 x 25 знакомест)
;          DI - текущее смещение в памяти, где лежит коммандная строка (команданая строка лежит а сегменте DS)
;          
;          В остальных регистрах не лежит ничего важного !
;          В стеке за'push'ено содержимое  регистра DI, т.к. этот регистр будет использован в ф-ции CTOR_DRAW_FRAMES
;============================================================================================================================
;---------------------------------------------------------
    CTOR_DRAW_FRAMES proc
    
        mov cx, ax       ;\
        and cx, 0ffh     ;| <=> mov ah, cl &&  подготовка регистра cx как счётчика для DRAW_LINE
        sub cx, 2        ;/

        and ax, 0ff00h   ;| - сохранение значений
        push ax          ;/

        mov dx, 0b800h   ;\
        mov es, dx       ;/ <=> es = 0b800h + si (bias)
        mov di, si

        xor ax, ax

        mov al, bh       ;\
        cmp al, 0h       ;| - сравниваем стиль рамки с "0". Если да => переход по метке 
        je @@ADD_STYLE   ;/
        
        call CALCULATE_BIAS_STYLE
        jmp @@NEXT

    @@ADD_STYLE: 
        call ADD_FRAME_STYLE
    
    @@NEXT:
        pop ax

        xor dx, dx       ;\
        mov dl, bl       ;| - подготовка регистра dx как счётчика для DRAW_FRAME_CYCLE
        sub dx, 2        ;/
        ret
    endp
;---------------------------------------------------------
    DRAW_FRAMES proc

        push di          ;\
        push cx          ;|
        call DRAW_LINE   ;| - print first line
        pop cx           ;|
        pop di           ;|
        add di, BYTE_WIDTH_DISPLAY      ;/
    
    @@DRAW_FRAME_CYCLE:
        push cx
        push di
        call DRAW_LINE
        pop di
        add di, BYTE_WIDTH_DISPLAY
        pop cx
        sub si, 3
        dec dx
        cmp dx, 0h
        jne @@DRAW_FRAME_CYCLE

        add si, 3       ;\ - print last line
        call DRAW_LINE  ;/

        ret
    endp
;---------------------------------------------------------
    DRAW_LINE proc

        lodsb            ;| <=> mov al, ds:[si]  ;\ - print first symbol
        stosw                                    ;/

    @@DRAW_LINE_CYCLE:
        lodsb            ;| <=>  mov es[di], ax   add di, 2
        stosw
        dec si
        loop @@DRAW_LINE_CYCLE

        inc si
        lodsb            ;\ - print last symbol
        stosw            ;/

        ret
    endp
;---------------------------------------------------------
    CALCULATE_BIAS_STYLE proc

        dec ax           ;\
        mov dx, ax       ;| 
        shl ax, 3        ;|
        add ax, dx       ;| <=> mov si, offset STYLES + (bh - 1)*9
        lea dx, STYLES   ;|
        add ax, dx       ;|
        mov si, ax       ;/
        
        ret
    endp
;---------------------------------------------------------
    ADD_FRAME_STYLE proc

        push bp
        mov bp, sp
        mov si, [bp + OFFSET_FROM_STACK] ;| - кладём в di адрес последнего прочитанного символа из командной строки 
        pop bp
        ret
    endp
;---------------------------------------------------------
    CHECK_NEW_STYLE proc

        cmp bh, 0h
        jne @@OUT

        mov di, si
        
    @@OUT:
        ret
    endp
;---------------------------------------------------------

    MY_STRLEN proc

; MY_STRLEN counts the number of characters in the string until it reaches the '$' character
; ENTRY: None
; EXIT:  CX - result
; DESTR: AL, DI, CX

        mov al, ASCII_CARRIAGE_RETURN     ;| ASCII код возврата каретки
        mov bx, ds
        mov es, bx
        xor cx, cx
        dec cx
        mov dx, di
        repne scasb
        mov di, dx
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

        mov si, cx
        sub si, 2000   ; bias_common = bias_x + bias_y = (25 - 1)/2 * 160 + (160 - 2x)/2 = 2000 - x
        neg si
        ret
    endp
;--------------------------------------------------------
    PRINT proc
; Function for printing text at a calculated address
; ENTRY:   DI - bias message
;          AH - color of text
;          CX - number of symbols
;          SI - current place in command line
; DESTROY: 

        mov bx, VIDEOMEM_SEGMENT
        mov es, bx

        mov bx, di
        mov di, si
        mov si, bx

    @@PRINT_MESSAGE_CYCLE:
        lodsb            ;| <=>  mov es[di], ax   add di, 2
        stosw
        loop @@PRINT_MESSAGE_CYCLE
        ret
    endp
;---------------------------------------------------------
    TERMINATE_PROGRAMM proc

    MY_END_Label:
        mov ah, 4ch
        int 21h
        ret
    endp
;---------------------------------------------------------
    end     Start
;---------------------------------------------------------
