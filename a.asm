BITS 64
segment .text
global _start
_start:
    mov rdx, len
    mov rcx, msg
    mov rbx, 1
    mov rax, 1
section        .data             
    msg        db "Hello world!", 0xa 
    len        equ $ -msg
