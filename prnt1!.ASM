.model tiny
.code

org 100h

Start:
	mov ah, 02h

	mov dx, offset MESSAGE
	int 21h

	mov ax, 4c00h
	int 21h

	db 21h - 0ch dup (0)	; Добавляет смещение к адресу 
							; строки таким образом,
							; что последний байт адреса
							; имеет ASCII код '!' (21)

MESSAGE: db 'Hello World !$'

end 	Start
