.model tiny
.code

org 100h

Start:
	mov ah, 02h

	mov dx, offset MESSAGE
	int 21h

	mov ax, 4c00h
	int 21h

	db 21h - 0ch dup (0)	; �������� ᬥ饭�� � ����� 
							; ��ப� ⠪�� ��ࠧ��,
							; �� ��᫥���� ���� ����
							; ����� ASCII ��� '!' (21)

MESSAGE: db 'Hello World !$'

end 	Start
