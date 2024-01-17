BITS 64
segment .text
print:
    mov     r9, -3689348814741910323
    sub     rsp, 40
    mov     BYTE [rsp+31], 10
    lea     rcx, [rsp+30]
.L2:
    mov     rax, rdi
    lea     r8, [rsp+32]
    mul     r9
    mov     rax, rdi
    sub     r8, rcx
    shr     rdx, 3
    lea     rsi, [rdx+rdx*4]
    add     rsi, rsi
    sub     rax, rsi
    add     eax, 48
    mov     BYTE [rcx], al
    mov     rax, rdi
    mov     rdi, rdx
    mov     rdx, rcx
    sub     rcx, 1
    cmp     rax, 9
    ja      .L2
    lea     rax, [rsp+32]
    mov     edi, 1
    sub     rdx, rax
    xor     eax, eax
    lea     rsi, [rsp+32+rdx]
    mov     rdx, r8
    mov     rax, 1
    syscall
    add     rsp, 40
    ret
global _start
_start:
    mov rax, ret_stack_end
    mov [ret_stack_rsp], rax
    ;; -- call main --
    mov rax, rsp
    mov rsp, [ret_stack_rsp]
    call addr_11
    mov [ret_stack_rsp], rsp
    mov rsp, rax
    ;; -- exit --
    mov rax, 60
    mov rdi, 0
    syscall
    ;; -- fn_skip(fputs) --
    jmp addr_12
    ;; -- fn(fputs) --
addr_0:
    sub rsp, 0
    mov [ret_stack_rsp], rsp
    mov rsp, rax
    ;; -- int --
    mov rax, 1
    push rax
    ;; -- syscall3 --
    pop rax
    pop rdi
    pop rsi
    pop rdx
    syscall
    push rax
    ;; end/ret
    mov rax, rsp
    mov rsp, [ret_stack_rsp]
    add rsp, 0
    ret
    addr_12:
    ;; -- fn_skip(puts) --
    jmp addr_13
    ;; -- fn(puts) --
addr_1:
    sub rsp, 0
    mov [ret_stack_rsp], rsp
    mov rsp, rax
    ;; -- constant --
    mov rax, 1
    push rax
    ;; -- call fn --
    mov rax, rsp
    mov rsp, [ret_stack_rsp]
    call addr_0
    mov [ret_stack_rsp], rsp
    mov rsp, rax
    ;; -- drop --
    pop rax
    ;; end/ret
    mov rax, rsp
    mov rsp, [ret_stack_rsp]
    add rsp, 0
    ret
    addr_13:
    ;; -- fn_skip(main) --
    jmp addr_29
    ;; -- fn(main) --
addr_11:
    sub rsp, 0
    mov [ret_stack_rsp], rsp
    mov rsp, rax
    ;; -- int --
    mov rax, 18446744073709551612
    push rax
    ;; -- dup --
    pop rax
    push rax
    push rax
    ;; -- print --
    pop rdi
    call print
    ;; -- int --
    mov rax, 0
    push rax
    ;; -- < --
    mov rcx, 0
    mov rdx, 1
    pop rbx
    pop rax
    cmp rax, rbx
    cmovl rcx, rdx
    push rcx
    ;; -- if --
    pop rax
    cmp rax, 0
    je addr_27__
    ;; -- str --
    mov rax, 4
    push rax
    push str_2
    ;; -- call fn --
    mov rax, rsp
    mov rsp, [ret_stack_rsp]
    call addr_1
    mov [ret_stack_rsp], rsp
    mov rsp, rax
    ;; -- else --
addr_27:
    jmp addr_28
addr_27__:
    ;; -- str --
    mov rax, 5
    push rax
    push str_3
    ;; -- call fn --
    mov rax, rsp
    mov rsp, [ret_stack_rsp]
    call addr_1
    mov [ret_stack_rsp], rsp
    mov rsp, rax
    ;; -- end --
addr_28:
    ;; end/ret
    mov rax, rsp
    mov rsp, [ret_stack_rsp]
    add rsp, 0
    ret
    addr_29:
    mov rax, 60
    mov rdi, 0
    syscall

segment .data
    str_0: db 10
    str_1: db 45
    str_2: db 116, 114, 117, 101
    str_3: db 102, 97, 108, 115, 101

segment .bss
    ret_stack_rsp: resq 1
    ret_stack: resb 4096
    ret_stack_end: