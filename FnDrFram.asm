;---------------------------------------------------------
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
    MY_STRLEN proc

; MY_STRLEN counts the number of characters in the string until it reaches the '$' character
; ENTRY: None
; EXIT:  CX - result
; DESTR: AL, DI, CX

            mov al, 0dh     ;| ASCII код возврата каретки
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
;--------------------------------------------------------
