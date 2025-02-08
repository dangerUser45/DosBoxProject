                                    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.model tiny                         ;                         Программа вывода цветной рамочки                         ;
.code                               ;                            (например, как эта рамка)                             ;
org 100h                            ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

Start: 
    mov ax, 40  ; x                                          
    mov bl, 20  ; y

    call MY_STRLEN
    call GET_BIAS
    call GET_EVEN

    call DRAW_FRAMES
    call PRINT_MESSAGE

    call MY__END
;---------------------------------------------------------
    MY_STRLEN proc

; MY_STRLEN counts the number of characters in the string until it reaches the '$' character
; ENTRY: None
; EXIT:  CX - result
; DESTR: SI, DX, CX

            mov si, offset MESSAGE
            mov di, si
            mov cl, '$'

    CONDITION_STRLEN:
                                ;move si offset si
            cmp [si], cl        ;scasb ds:[si] c al b si++
            jne NOT_EQUAL       ;repne scasb -  repeat while not equal
            jmp EQUAL           ;sub
                                ;xor cx cx 
                                ;dec cx 
                                                   
    NOT_EQUAL:
            inc si
            jmp CONDITION_STRLEN
    EQUAL:                                       
            sub si, di
            mov cx, si
            ret
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
        db " $"
   endp
;---------------------------------------------------------
    DRAW_FRAMES proc

; ENTRY:  AX - x
;         BL - y
;         DI - current location in symbols array
;         CX - counter of cycle
;         SI - current line start address
;EXIT   : None
;DESTROY: BH
            xor bh, bh 
            ;mov si, 1368
            mov cx, 0b800h
            mov es, cx
            mov dh, 01110100b
            xor di, di
            sub bl, 2
            call DRAW_LINES
            add si, 160
            inc di

    CONDITION_FRAMES:
            cmp bh, bl
            jb BELOW_FRAMES
            add di, 3
            call DRAW_LINES
            ret

    BELOW_FRAMES:
            call DRAW_LINES
            sub di, 2
            inc bh
            add si, 160
            jmp CONDITION_FRAMES
    endp
;---------------------------------------------------------
    DRAW_LINES proc

; Draws one line to video memory
; ENTRY  : AX - length
;          BL - width
;          DH - color of frame
;          DL - current symbol
;          DI - counter of current symbol
; EXIT   : None
; DESTROY: SI

    FILL_VDO_MEM macro
            mov dl, [offset SYMBOLS + di]
            mov es: [si], dx
    endm
            push si
            FILL_VDO_MEM
            inc di
            sub ax, 2
            xor cx, cx

    CONDITION_DRAW:
            cmp cx, ax
            jb BELOW

            add ax, 2
            add si, 2
            inc di
            FILL_VDO_MEM
            pop si
            ret 

    BELOW:
            add si, 2
            FILL_VDO_MEM
            inc cx
            jmp CONDITION_DRAW

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
            sub ax, 800   ; bias_common = bias_x = bias_y = (25 - y=1)/2 * 160 + (160 - 2x)/2 = 800 - x
            neg ax
            ret
    endp
;--------------------------------------------------------
    PRINT proc
; Function for printing text at a calculated address
; ENTRY  : SI - bias
;          DH - color of text
; DESTROY: 
            mov bx, 0b800h
            mov es, bx
            mov bx,  [offset MESSAGE]
            xor ax, ax

    CONDITION_PRINT:
            cmp ax, cx
            jb BELOW_PRINT
            ret

    BELOW_PRINT:
            mov  es: [si], bx
            mov  es: [si+1], dh
            inc bx
            inc ax
    endp
;---------------------------------------------------------
    MY__END proc
            mov ah, 4ch
            int 21h
            ret
    endp
;---------------------------------------------------------
MESSAGE: db "I love you$"

SYMBOLS: db 0c9h, 03h, 0bbh, 03h, 0h, 03h, 0c8h, 03h, 0bch

end     Start
;---------------------------------------------------------
