BITS 64
segment .text
global _start
_start:
    mov rax, ret_stack_end
    mov [ret_stack_rsp], rax
    ;; -- call main --
    mov rax, rsp
    mov rsp, [ret_stack_rsp]
    call addr_8
    mov [ret_stack_rsp], rsp
    mov rsp, rax
    ;; -- exit --
    mov rax, 60
    mov rdi, 0
    syscall
    ;; -- fn_skip(fputs) --
    jmp addr_9
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
    addr_9:
    ;; -- fn_skip(puts) --
    jmp addr_10
    ;; -- fn(puts) --
addr_1:
    sub rsp, 0
    mov [ret_stack_rsp], rsp
    mov rsp, rax
    ;; -- int --
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
    addr_10:
    ;; -- fn_skip(!bool) --
    jmp addr_13
    ;; -- fn(!bool) --
addr_4:
    sub rsp, 0
    mov [ret_stack_rsp], rsp
    mov rsp, rax

    ;; -- !8 --
    ;; !
    pop rax
    pop rbx
    mov [rax], bl
    ;; end/ret
    mov rax, rsp
    mov rsp, [ret_stack_rsp]
    add rsp, 0
    ret
    addr_13:
    ;; -- fn_skip(@bool) --
    jmp addr_14
    ;; -- fn(@bool) --
addr_5:
    sub rsp, 0
    mov [ret_stack_rsp], rsp
    mov rsp, rax
    ;; -- @8 --
    ;; @
    pop rax
    xor rbx, rbx
    mov bl, [rax]
    push rbx
    ;; -- cast(bool) --
    ;; end/ret
    mov rax, rsp
    mov rsp, [ret_stack_rsp]
    add rsp, 0
    ret
    addr_14:
    ;; -- fn_skip(Bool::to_string) --
    jmp addr_22
    ;; -- fn(Bool::to_string) --
addr_7:
    sub rsp, 0
    mov [ret_stack_rsp], rsp
    mov rsp, rax
    ;; -- if --
    pop rax
    cmp rax, 0
    je addr_20__
    ;; -- str --
    mov rax, 4
    push rax
    push str_3
    ;; -- else --
addr_20:
    jmp addr_21
addr_20__:
    ;; -- str --
    mov rax, 5
    push rax
    push str_4
    ;; -- end --
addr_21:
    ;; end/ret
    mov rax, rsp
    mov rsp, [ret_stack_rsp]
    add rsp, 0
    ret
    addr_22:
    ;; -- fn_skip(main) --
    jmp addr_23
    ;; -- fn(main) --
addr_8:
    sub rsp, 1
    mov [ret_stack_rsp], rsp
    mov rsp, rax
    ;; -- constant --
    mov rax, 1
    push rax
    ;; -- push_local_mem --
    mov rax, [ret_stack_rsp]
    add rax, 0
    push rax
    ;; -- call fn --
    mov rax, rsp
    mov rsp, [ret_stack_rsp]
    call addr_4
    mov [ret_stack_rsp], rsp
    mov rsp, rax
    ;; -- push_local_mem --
    mov rax, [ret_stack_rsp]
    add rax, 0
    push rax
    ;; -- call fn --
    mov rax, rsp
    mov rsp, [ret_stack_rsp]
    call addr_5
    mov [ret_stack_rsp], rsp
    mov rsp, rax
    ;; -- call fn --
    mov rax, rsp
    mov rsp, [ret_stack_rsp]
    call addr_7
    mov [ret_stack_rsp], rsp
    mov rsp, rax
    ;; -- call fn --
    mov rax, rsp
    mov rsp, [ret_stack_rsp]
    call addr_1
    mov [ret_stack_rsp], rsp
    mov rsp, rax
    ;; end/ret
    mov rax, rsp
    mov rsp, [ret_stack_rsp]
    add rsp, 1
    ret
    addr_23:
    mov rax, 60
    mov rdi, 0
    syscall

segment .data
    str_0: db 10
    str_1: db 116, 114, 117, 101, 10
    str_2: db 102, 97, 108, 115, 101, 10
    str_3: db 116, 114, 117, 101
    str_4: db 102, 97, 108, 115, 101

segment .bss
    ret_stack_rsp: resq 1
    ret_stack: resb 4096
    ret_stack_end:
