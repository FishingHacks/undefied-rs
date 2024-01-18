BITS 64
segment .text
global _start
_start:
    mov rax, ret_stack_end
    mov [ret_stack_rsp], rax
    ;; -- call main --
    mov rax, rsp
    mov rsp, [ret_stack_rsp]
    call addr_20
    mov [ret_stack_rsp], rsp
    mov rsp, rax
    ;; -- exit --
    mov rax, 60
    mov rdi, 0
    syscall
    ;; -- fn_skip(main) --
    jmp addr_54
    ;; -- fn(main) --
addr_20:
    sub rsp, 0
    mov [ret_stack_rsp], rsp
    mov rsp, rax
    ;; end/ret
    mov rax, rsp
    mov rsp, [ret_stack_rsp]
    add rsp, 0
    ret
    addr_54:
    mov rax, 60
    mov rdi, 0
    syscall

segment .data
    str_0: db 10
    str_1: db 45
    str_2: db 80, 65, 78, 73, 67, 58, 32
    str_3: db 69, 82, 82, 79, 82, 58, 32, 103, 101, 116, 95, 101, 108, 101, 109, 101, 110, 116, 32, 99, 97, 108, 108, 101, 100, 32, 119, 105, 116, 104, 32, 105, 110, 100, 101, 120, 32, 62, 61, 32, 108, 101, 110
    str_4: db 69, 82, 82, 79, 82, 58, 32, 114, 101, 109, 111, 118, 101, 95, 101, 108, 101, 109, 101, 110, 116, 32, 99, 97, 108, 108, 101, 100, 32, 119, 105, 116, 104, 32, 110, 111, 32, 101, 108, 101, 109, 101, 110, 116, 115

segment .bss
    ret_stack_rsp: resq 1
    ret_stack: resb 4096
    ret_stack_end:
