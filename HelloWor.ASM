.model tiny
.code

org 100h

Start:

	mov ah, 09h

	mov dx, offset MESSAGE
	int 21h

	mov ax, 4c00h
	int 21h

MESSAGE: db 'Hello World !$'

end 	Start
