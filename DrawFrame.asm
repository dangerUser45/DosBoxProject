                                    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.model tiny                         ;                         Программа вывода цветной рамочки                         ;
.code                               ;                            (например, как эта рамка)                             ;
org 100h                            ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


Start: jmp MAIN
;---------------------------------------------------------
MESSAGE: db "Hello world!$"

COLOR: dw 0111010000000000b

SYMBOLS: db 0c9h, 03h, 0bbh, 03h, 0h, 03h, 0c8h, 03h, 0bch
;---------------------------------------------------------
    MAIN:
            mov ax, 40  ; x                                          
            mov bl, 20   ; y

            call GET_BIAS
            call GET_EVEN

            call DRAW_FRAMES
            call MY_STRLEN
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
        ret  
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
            mov cx, 0b800h
            mov es, cx
            mov di, [offset COLOR]
            mov dx, [di]
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
;          SI - current bias of begining video memory
; EXIT   : None
; DESTROY: SI

    FILL_VIDEO_MEMORY macro
            mov dl, [offset SYMBOLS + di]
            mov es: [si], dx
    endm
            FILL_VIDEO_MEMORY
            inc di
            sub ax, 2
            xor cx, cx

    CONDITION_DRAW:
            cmp cx, ax
            jb BELOW

            add ax, 2
            add si, 2
            inc di
            FILL_VIDEO_MEMORY
            shl ax,  1
            sub si, ax
            add si, 2
            shr ax, 1
            ret 

    BELOW:
            add si, 2
            FILL_VIDEO_MEMORY
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
            mov si, cx
            sub si, 2000   ; bias_common = bias_x + bias_y = (25 - 1)/2 * 160 + (160 - 2x)/2 = 2000 - x
            neg si
            ret
    endp
;--------------------------------------------------------
    PRINT proc
; Function for printing text at a calculated address
; ENTRY  : SI - bias
;          DH - color of text
; DESTROY: 
            mov di, [offset COLOR]
            mov dx, [di]
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
            mov ah, 4ch
            int 21h
            ret
    endp
;---------------------------------------------------------
end     Start
;---------------------------------------------------------
