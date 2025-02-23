 .model tiny
 .code
  locals @@
  org 100h

 Start: jmp MAIN_RESIDENT_FRAME
;--------------------------------------------------------------------------------------------------------
;////////------CONSTANTS-------//////////////////////////////////////////////////////////////////////////
;--------------------------------------------------------------------------------------------------------
    VIDEOMEM_SEGMENT        equ     0b800h      ;| - адрес начала видеопамяти
    WIDTH_FRAME             equ     014d        ;| - ширина рамки
    LENGTH_FRAME            equ     015d        ;| - высота рамки
    COLOR_FRAME             equ     0f2h        ;| - цвет рамки
    CODE_JUMP               equ     0eah        ;| - команда jmp в hex формате
    ASCII_CARRIAGE_RETURN   equ     0dh         ;| - ASCII-код символа возврата каретки
    WIDTH_DISPLAY           equ     080d        ;| - ширина дисплея (в знакоместах)
    LENGTH_DISPLAY          equ     025d        ;| - высота дисплея (в знакоместах)
    BYTE_WIDTH_DISPLAY      equ     0160d       ;| - ширина дисплея (в байтах)
    BIAS_FRAME              equ     0438d       ;| - смещение относительно начала видеопамяти (в байтах)
    OFFSET_INT_08h          equ     08h*4h      ;| - смещение в таблице прерываний, по которому  лежит адрес функции-обработчика 8 прерывния
    OFFSET_INT_09h          equ     09h*4h      ;| - смещение в таблице прерываний, по которому  лежит адрес функции-обработчика 9 прерывния
    PUSH_KEY_FOR_SHOW_REG   equ     32h         ;| - скан-код нажатия клавиши, который рисует/убирает рамку с регистрами
    PULL_KEY_FOR_SHOW_REG   equ     0b2h        ;| - скан-код отпускания этой же клавиши 
    EMPTY_SYMBOL            equ     0h          ;| - чёрный фон для затирания рамки

;--------------------------------------------------------------------------------------------------------
;//////////------DATA-------////////////////////////////////////////////////////////////////////////
;--------------------------------------------------------------------------------------------------------
    STYLES:
        db 0c9h, 0cdh, 0bbh
        db 0bah, 0h,   0bah                     ;| - Double frame
        db 0c8h, 0cdh, 0bch

    BUFFER__DISPLAY_SYMBOLS:
        dw WIDTH_FRAME*LENGTH_FRAME dup(0h)     ;| -инициализирую массив для загрузки туда символов которые находились до рисования рамки
;--------------------------------------------------------------------------------------------------------
;////////------MACRO-------//////////////////////////////////////////////////////////////////////////////
;--------------------------------------------------------------------------------------------------------
    FINISHED_PROCESSING_SYMBOL macro
        in   al,  61h           ;\ -  al = 61h
        mov  ah,  al            ;| -  ah = al (save al)
        or   al,  80h           ;|
        out  61h, al            ;|
        xchg al,  ah            ;|
        out  61h, al            ;/ - мигнули старшим битом 61h

        mov al, 20h             ;\ - сигнал контроллеру прерываний
        out 20h, al             ;/
    endm
;--------------------------------------------------------------------------------------------------------
    SAVE_ALL_REGISTERS macro
        push ax
        push bx
        push cx
        push dx
        push si 
        push di 
        push es
        push ds
        push bp 
    endm
;--------------------------------------------------------------------------------------------------------
    RET_ALL_REGISTERS macro
        pop bp 
        pop ds
        pop es
        pop di
        pop si
        pop dx
        pop cx
        pop bx
        pop ax
    endm
;--------------------------------------------------------------------------------------------------------
    SAVE_DISPLAY macro

        mov ax, VIDEOMEM_SEGMENT
        mov ds, ax
        mov si, BIAS_FRAME

        push cs
        pop es
        lea di, BUFFER__DISPLAY_SYMBOLS

        mov dx, LENGTH_FRAME
        mov cx, WIDTH_FRAME

     @@CYCLE_SAVE_DISPLAY:
        push si
        push cx
        rep movsw
        pop cx
        pop si
        add si, BYTE_WIDTH_DISPLAY
        dec dx
        cmp dx, 0h 
        jne @@CYCLE_SAVE_DISPLAY
    endm
;--------------------------------------------------------------------------------------------------------
     FILL_VIDEO_MEMORY macro

        mov ax, VIDEOMEM_SEGMENT
        mov es, ax
        mov di, BIAS_FRAME

        push cs
        pop ds
        lea si, BUFFER__DISPLAY_SYMBOLS

        mov dx, LENGTH_FRAME
        mov cx, WIDTH_FRAME

     @@CYCLE_FILL_VIDEOMEMORY:
        push di
        push cx
        rep movsw
        pop cx
        pop di
        add di, BYTE_WIDTH_DISPLAY
        dec dx
        cmp dx, 0h 
        jne @@CYCLE_FILL_VIDEOMEMORY
    endm
;--------------------------------------------------------------------------------------------------------
;////////////////////////////////////////////////////////////////////////////////////////////////////////
;--------------------------------------------------------------------------------------------------------
    START_MY_HANDLE_KEYBOARD_INT proc

        SAVE_ALL_REGISTERS                      ;\
        in al, 60h                              ;| - вызываем функцию считывания символа из буфера клавиатуры и сравниваем полученное значение со скан кодом нужной клавиши
        cmp al, PULL_KEY_FOR_SHOW_REG           ;|
        je @@NEXT
        
        cmp al, PUSH_KEY_FOR_SHOW_REG           ;|
        je @@Enable_or_Disable_Frame            ;/

        ;cmp al, PUSH_KEY_ENTER
        ;je @@CASE_ENTER

    @@CALL_OLD_HANDLE:                          
        RET_ALL_REGISTERS                      ;\ - возвращаем регистр ax в прежнее состояние и переходим к старому обработчику
        
        db CODE_JUMP                            ;/
        ORIGINAL_OFFSET_INT_09h:  dw 0          ;\ - задаёт сегмент и смещение для прыжка 
        ORIGINAL_SEGMENT_INT_09h: dw 0          ;/
        ;jmp @@EXIT

    ;@@CASE_ENTER
          ;SAVE_DISPLAY
          ;jmp @@CALL_OLD_HANDLE

    @@Enable_or_Disable_Frame:
        mov bl, byte ptr cs:[ACTIVE]            ;\
        not bl                                  ;| - инвертируем биты в переменной ACTIVE
        mov byte ptr cs:[ACTIVE], bl            ;/

        cmp byte ptr cs:[ACTIVE], 0h 
        jne @@NEXT

        FILL_VIDEO_MEMORY

    @@NEXT:
        FINISHED_PROCESSING_SYMBOL
        RET_ALL_REGISTERS
    
    @@EXIT:
        iret                                    ;|

        ACTIVE: db 0h                           ;|
 endp 
;--------------------------------------------------------------------------------------------------------xx 
;////////////////////////////////////////////////////////////////////////////////////////////////////////
;--------------------------------------------------------------------------------------------------------
    START_MY_HANDLE_TIMER_INT proc

        SAVE_ALL_REGISTERS

        cmp byte ptr cs:[ACTIVE], 0ffh
        jne  @@CLEAR_FRAME

        call DRAW_RESIDENT_FRAME
        jmp @@CALL_OLD_HANDLE

    @@CLEAR_FRAME:
        SAVE_DISPLAY                            ;| - сохраняю ту часть экрана, которая будет затёрта рамкой
        FILL_VIDEO_MEMORY

    @@CALL_OLD_HANDLE:                          ;\                 
        RET_ALL_REGISTERS                       ;| - 
        db CODE_JUMP                            ;/

        ORIGINAL_OFFSET_INT_08h:  dw 0          ;\
        ORIGINAL_SEGMENT_INT_08h: dw 0          ;/

        iret                                    ;|

 endp 
;--------------------------------------------------------------------------------------------------------
;////////////////////////////////////////////////////////////////////////////////////////////////////////
;--------------------------------------------------------------------------------------------------------
    DRAW_RESIDENT_FRAME proc 

    mov dx, VIDEOMEM_SEGMENT    ;\ <=> es = 0b800h
    mov es, dx                  ;/

    mov ax, cs                  ;\
    push ax                     ;| - кладу в регистр ds содержимое регистра cs, чтобы данные сохранённые в этом сегменте не потерялись 
    pop ds                      ;/

    mov ax, 1003h               ;\
    xor bl, bl                  ;| - вызываю прерывание, отключающее моргание дисплея, задающееся в аттрибуте цвета
    int 10h                     ;/

    mov ah, COLOR_FRAME         ;\ - цвет рамки           \   
    mov cx, WIDTH_FRAME         ;| - длина рмки           |
    mov dx, LENGTH_FRAME        ;| - высота рамки         | -  подготавливаю регистры для рисования рамки
    mov di, BIAS_FRAME          ;| - записываю смещение   |
                                ;/   для распечатки рамки /

    sub cx, 2                   ;| - подготовка регистра cx как счётчика для DRAW_LINE
    sub dx, 2                   ;| - подготовка регистра dx как счётчика для DRAW_FRAME_CYCLE
    lea si, STYLES              ;| - подготовка si для ф-ци draw line

    call DRAW_FRAMES
    ret
 endp
;--------------------------------------------------------------------------------------------------------
;////////////////////////////////////////////////////////////////////////////////////////////////////////
;--------------------------------------------------------------------------------------------------------
    DRAW_FRAMES proc

           push di                           ;\
           push cx                           ;|
           call DRAW_LINE                    ;| - print first line
           pop cx                            ;|
           pop di                            ;|
           add di, BYTE_WIDTH_DISPLAY        ;/
        
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
;--------------------------------------------------------------------------------------------------------
;////////////////////////////////////////////////////////////////////////////////////////////////////////
;--------------------------------------------------------------------------------------------------------
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
;--------------------------------------------------------------------------------------------------------
;////////////////////////////////////////////////////////////////////////////////////////////////////////
;--------------------------------------------------------------------------------------------------------
    ;sinclude fndrfram.asm

    END_MY_HANDLS:
;--------------------------------------------------------------------------------------------------------
;////////////////////////////////////////////////////////////////////////////////////////////////////////
;--------------------------------------------------------------------------------------------------------
    MAIN_RESIDENT_FRAME:
        xor ax, ax                                          ;\
        mov es, ax                                          ;| - 
        mov bx, OFFSET_INT_08h                              ;|
        mov di, OFFSET_INT_09h                              ;/

        mov ax, es:[bx]                                     ;\
        mov word ptr ORIGINAL_OFFSET_INT_08h, ax            ;| - 
        mov ax, es:[bx + 2]                                 ;|
        mov word ptr ORIGINAL_SEGMENT_INT_08h, ax           ;/

        mov dx, es:[di]                                     ;\
        mov word ptr ORIGINAL_OFFSET_INT_09h, dx            ;| - 
        mov dx, es:[di + 2]                                 ;|
        mov word ptr ORIGINAL_SEGMENT_INT_09h, dx           ;/

        cli                                                 ;\
        push cs                                             ;|
        pop ax                                              ;| -
        mov es:[di], offset START_MY_HANDLE_KEYBOARD_INT    ;| 
        mov es:[di + 2], ax                                 ;/
        
        mov es:[bx], offset START_MY_HANDLE_TIMER_INT       ;\
        mov es:[bx + 2], ax                                 ;|
        sti                                                 ;/

        ;int 09h                         ; - TEST!!!
        ;int 08h                         ; - TEST!!!
        ;cli                             ; - TEST!!!
        ;call DRAW_RESIDENT_FRAME        ; - TEST!!!
        ;sti                             ; - TEST!!!
        mov ah, 31h                                         ;\
        mov dx, offset END_MY_HANDLS                        ;|
        shr dx, 4                                           ;| - 
        inc dx                                              ;|
        int 21h                                             ;/

;--------------------------------------------------------------------------------------------------------
;////////////////////////////////////////////////////////////////////////////////////////////////////////
;--------------------------------------------------------------------------------------------------------
 end     Start
;--------------------------------------------------------------------------------------------------------
;////////////////////////////////////////////////////////////////////////////////////////////////////////
;--------------------------------------------------------------------------------------------------------
