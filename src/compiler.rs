use crate::{
    error::{err, err_generic},
    parser::{
        CallProc, Intrinsic, Keyword, OpIntrinsic, OpKeyword, Operation, Proc, Program,
        PushAssembly, PushConst, PushInt, PushLocalMem, PushMem, PushStr, Ret,
    },
    typecheck::Type,
};

const START: &str = r#"BITS 64
segment .text
"#;
const PRINT_FN: &str = r#"print:
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
"#;
const START_BODY: &str = r#"global _start
_start:
    mov rax, ret_stack_end
    mov [ret_stack_rsp], rax
"#;

const SYSCALL_REGS: &[&str] = &["rdi", "rsi", "rdx", "r10", "r8", "r9"];

#[inline]
fn syscall(mut num: u8) -> String {
    if num > 6 {
        num = 6;
    }

    let mut str = "    pop rax\n".to_string();

    for i in 0..num {
        str += &format!("    pop {}\n", SYSCALL_REGS[i as usize]);
    }

    str += "    syscall\n    push rax\n";

    str
}

pub fn compile(mut program: Program) -> String {
    let mut str: String = String::with_capacity(1000);
    str.push_str(START);

    for op in &program.ops {
        if let Operation::Intrinsic(OpIntrinsic {
            op: Intrinsic::Print,
            ..
        }) = op
        {
            str.push_str(PRINT_FN);

            break;
        }
    }

    str.push_str(START_BODY);

    if let Some(id) = program.main_fn {
        let main_fn_contract = program.contracts.get(id).unwrap_or_else(|| {
            err_generic("Expected the main function to have a contract, but found nothing")
        });

        if main_fn_contract.in_types.len() > 0 {
            err(
                &main_fn_contract.loc,
                "Expected 0 inputs for the main function",
            );
        }
        if main_fn_contract.out_types.len() > 1 {
            err(
                &main_fn_contract.loc,
                "Expected the return type of the main function to be int or nothing",
            );
        }
        if main_fn_contract
            .out_types
            .get(0)
            .and_then(|t| Some(Type::Int.equal_to(t)))
            .unwrap_or(false)
        {
            err(
                &main_fn_contract.loc,
                "Expected the return type of the main function to be int or nothing",
            );
        }

        str += "    ;; -- call main --\n";
        str += "    mov rax, rsp\n";
        str += "    mov rsp, [ret_stack_rsp]\n";
        str += &format!("    call addr_{}\n", id);
        str += "    mov [ret_stack_rsp], rsp\n";
        str += "    mov rsp, rax\n";
        str += "    ;; -- exit --\n";

        str += "    mov rax, 60\n"; // exit syscall number
        if main_fn_contract.out_types.len() < 1 {
            str += "    mov rdi, 0\n"; // exit code
        } else {
            str += "    pop rdi\n"; // get exit code as main return
        }
        str += "    syscall\n";
    } else {
        // print the no main fn to stdout
        str += "    mov rax, 1\n"; // write syscall number
        str += "    mov rdi, 1\n"; // stdout fd
        str += "    mov rsi, __str_no_main\n"; // str ptr
        str += "    mov rdx, __str_no_main_len\n"; // str len
        str += "    syscall\n";
        str += "    ;; -- exit --\n";
        str += "    mov rax, 60\n"; // exit syscall number
        str += "    mov rdi, 0\n"; // exit code
        str += "    syscall\n";
    }

    let mut ip: usize = 0;
    program.reverse_refs();
    let reversed = program.reversed_refs;

    while ip < program.ops.len() {
        let op = &program.ops[ip];
        match op {
            Operation::Intrinsic(OpIntrinsic { op, loc }) => {
                str += &format!("    ;; -- {} --\n", op.to_str());
                match op {
                    Intrinsic::Print => {
                        str += "    pop rdi\n";
                        str += "    call print\n";
                    }
                    Intrinsic::Here => {
                        let _str = format!("{}", loc);
                        let len = _str.len();
                        let i = program.strings.len();
                        program.strings.push(_str);
                        str += &format!("    mov rax, {}\n", len);
                        str += "    push rax\n";
                        str += &format!("    push str_{i}\n");
                    }
                    Intrinsic::Plus => {
                        str += "    pop rax\n";
                        str += "    pop rbx\n";
                        str += "    add rax, rbx\n";
                        str += "    push rax\n";
                    }
                    Intrinsic::Minus => {
                        str += "    pop rax\n";
                        str += "    pop rbx\n";
                        str += "    sub rbx, rax\n";
                        str += "    push rbx\n";
                    }
                    Intrinsic::Mul => {
                        str += "    pop rax\n";
                        str += "    pop rbx\n";
                        str += "    mul rbx\n";
                        str += "    push rax\n";
                    }
                    Intrinsic::DivMod => {
                        str += "    xor rdx, rdx\n";
                        str += "    pop rbx\n";
                        str += "    pop rax\n";
                        str += "    div rbx\n";
                        str += "    push rax\n";
                        str += "    push rdx\n";
                    }

                    Intrinsic::LessThan => {
                        str += "    mov rcx, 0\n";
                        str += "    mov rdx, 1\n";
                        str += "    pop rbx\n";
                        str += "    pop rax\n";
                        str += "    cmp rax, rbx\n";
                        str += "    cmovl rcx, rdx\n";
                        str += "    push rcx\n";
                    }
                    Intrinsic::LessThanEqual => {
                        str += "    mov rcx, 0\n";
                        str += "    mov rdx, 1\n";
                        str += "    pop rbx\n";
                        str += "    pop rax\n";
                        str += "    cmp rax, rbx\n";
                        str += "    cmovle rcx, rdx\n";
                        str += "    push rcx\n";
                    }
                    Intrinsic::GreaterThan => {
                        str += "    mov rcx, 0\n";
                        str += "    mov rdx, 1\n";
                        str += "    pop rbx\n";
                        str += "    pop rax\n";
                        str += "    cmp rax, rbx\n";
                        str += "    cmovg rcx, rdx\n";
                        str += "    push rcx\n";
                    }
                    Intrinsic::GreaterThanEqual => {
                        str += "    mov rcx, 0\n";
                        str += "    mov rdx, 1\n";
                        str += "    pop rbx\n";
                        str += "    pop rax\n";
                        str += "    cmp rax, rbx\n";
                        str += "    cmovge rcx, rdx\n";
                        str += "    push rcx\n";
                    }
                    Intrinsic::Equal => {
                        str += "    pop rax\n";
                        str += "    pop rbx\n";
                        str += "    cmp rax, rbx\n";
                        str += "    sete al\n";
                        str += "    movzx rax, al\n";
                        str += "    push rax\n";
                    }
                    Intrinsic::NotEqual => {
                        str += "    pop rax\n";
                        str += "    pop rbx\n";
                        str += "    cmp rax, rbx\n";
                        str += "    setne al\n";
                        str += "    movzx rax, al\n";
                        str += "    push rax\n";
                    }

                    Intrinsic::Drop => {
                        str += "    pop rax\n";
                    }
                    Intrinsic::Dup => {
                        str += "    pop rax\n";
                        str += "    push rax\n";
                        str += "    push rax\n";
                    }
                    Intrinsic::Over => {
                        str += "    pop rbx\n";
                        str += "    pop rax\n";
                        str += "    push rax\n";
                        str += "    push rbx\n";
                        str += "    push rax\n";
                    }
                    Intrinsic::Swap => {
                        str += "    pop rbx\n";
                        str += "    pop rax\n";
                        str += "    push rbx\n";
                        str += "    push rax\n";
                    }
                    Intrinsic::Rot => {
                        str += "    pop rax\n";
                        str += "    pop rbx\n";
                        str += "    pop rcx\n";
                        str += "    push rax\n";
                        str += "    push rcx\n";
                        str += "    push rbx\n";
                    }

                    Intrinsic::Syscall0 => str += &syscall(0),
                    Intrinsic::Syscall1 => str += &syscall(1),
                    Intrinsic::Syscall2 => str += &syscall(2),
                    Intrinsic::Syscall3 => str += &syscall(3),
                    Intrinsic::Syscall4 => str += &syscall(4),
                    Intrinsic::Syscall5 => str += &syscall(5),
                    Intrinsic::Syscall6 => str += &syscall(6),

                    Intrinsic::Shl => {
                        str += "    pop rcx\n";
                        str += "    pop rbx\n";
                        str += "    shl rbx, cl\n";
                        str += "    push rbx\n";
                    }
                    Intrinsic::Shr => {
                        str += "    pop rcx\n";
                        str += "    pop rbx\n";
                        str += "    shr rbx, cl\n";
                        str += "    push rbx\n";
                    }
                    Intrinsic::Or => {
                        str += "    pop rax\n";
                        str += "    pop rbx\n";
                        str += "    or rbx, rax\n";
                        str += "    push rbx\n";
                    }
                    Intrinsic::And => {
                        str += "    pop rax\n";
                        str += "    pop rbx\n";
                        str += "    and rbx, rax\n";
                        str += "    push rbx\n";
                    }
                    Intrinsic::Xor => {
                        str += "    pop rax\n";
                        str += "    pop rbx\n";
                        str += "    xor rbx, rax\n";
                        str += "    push rbx\n";
                    }
                    Intrinsic::Not => {
                        str += "    pop rax\n";
                        str += "    not rax\n";
                        str += "    push rax\n";
                    }

                    Intrinsic::Load8 => {
                        str += "    ;; @\n";
                        str += "    pop rax\n";
                        str += "    xor rbx, rbx\n";
                        str += "    mov bl, [rax]\n";
                        str += "    push rbx\n";
                    }
                    Intrinsic::Store8 => {
                        str += "    ;; !\n";
                        str += "    pop rax\n";
                        str += "    pop rbx\n";
                        str += "    mov [rax], bl\n";
                    }
                    Intrinsic::Load16 => {
                        str += "    ;; @16\n";
                        str += "    pop rax\n";
                        str += "    xor rbx, rbx\n";
                        str += "    mov bx, [rax]\n";
                        str += "    push rbx\n";
                    }
                    Intrinsic::Store16 => {
                        str += "    ;; !16\n";
                        str += "    pop rax\n";
                        str += "    pop rbx\n";
                        str += "    mov [rax], bx\n";
                    }
                    Intrinsic::Load32 => {
                        str += "    ;; @32\n";
                        str += "    pop rax\n";
                        str += "    xor rbx, rbx\n";
                        str += "    mov ebx, [rax]\n";
                        str += "    push rbx\n";
                    }
                    Intrinsic::Store32 => {
                        str += "    ;; !32\n";
                        str += "    pop rax\n";
                        str += "    pop rbx\n";
                        str += "    mov [rax], ebx\n";
                    }
                    Intrinsic::Load64 => {
                        str += "    ;; @64\n";
                        str += "    pop rax\n";
                        str += "    xor rbx, rbx\n";
                        str += "    mov rbx, [rax]\n";
                        str += "    push rbx\n";
                    }
                    Intrinsic::Store64 => {
                        str += "    ;; !64\n";
                        str += "    pop rax\n";
                        str += "    pop rbx\n";
                        str += "    mov [rax], rbx\n";
                    }

                    Intrinsic::CastBool
                    | Intrinsic::CastInt
                    | Intrinsic::CastPtr
                    | Intrinsic::CastPtrToPlus
                    | Intrinsic::CastPtrToMinus
                    | Intrinsic::StackInfo => {} // _ => compile_error!("RAAAAGHHHH U DIDNT FINISH IMPLEMENTING YET >:C"),
                }
            }
            Operation::PushConst(PushConst { constant, .. }) => {
                str += "    ;; -- constant --\n";
                str += &format!("    mov rax, {}\n", constant.value);
                str += "    push rax\n";
            }
            Operation::PushInt(PushInt { value, .. }) => {
                str += "    ;; -- int --\n";
                str += &format!("    mov rax, {}\n", value);
                str += "    push rax\n";
            }
            Operation::PushMem(PushMem { id, .. }) => {
                str += "    ;; -- mem --\n";
                str += &format!("    push mem_{}\n", id);
            }
            Operation::PushStr(PushStr {
                is_cstr,
                len,
                value,
                ..
            }) => {
                str += "    ;; -- str --\n";
                if !is_cstr {
                    str += &format!("    mov rax, {}\n", len);
                    str += "    push rax\n";
                }
                if len > &0 {
                    str += &format!("    push str_{}\n", value);
                } else {
                    str += "    push 0\n";
                }
            }
            Operation::Keyword(OpKeyword {
                keyword,
                reference,
                loc,
            }) => match keyword {
                Keyword::If | Keyword::IfStar => {
                    if let Some(reference) = reference {
                        str += "    ;; -- if --\n";
                        str += "    pop rax\n";
                        str += "    cmp rax, 0\n";
                        str += &format!("    je addr_{reference}__\n");
                    } else {
                        err(
                            loc,
                            "No reference for this if-block defined. Probably a linker issue",
                        );
                    }
                }
                Keyword::Else => {
                    if let Some(reference) = reference {
                        str += "    ;; -- else --\n";
                        str += &format!("addr_{}:\n", reversed.get(&ip).unwrap());
                        str += &format!("    jmp addr_{reference}\n");
                        str += &format!("addr_{}__:\n", reversed.get(&ip).unwrap());
                    } else {
                        err(
                            loc,
                            "No reference for this if-block defined. Probably a linker issue",
                        );
                    }
                }
                Keyword::Do => {
                    if let Some(reference) = reference {
                        str += "    ;; -- do --\n";
                        str += "    pop rax\n";
                        str += "    cmp rax, 0\n";
                        str += &format!("    je addr_{reference}\n");
                    } else {
                        err(loc, "No reference for this do");
                    }
                }
                Keyword::While => {
                    str += "    ;; -- while --\n";
                    str += &format!("addr_{}:\n", reversed.get(&ip).unwrap());
                }
                Keyword::End => {
                    str += "    ;; -- end --\n";
                    if let Some(reference) = reference {
                        str += &format!("    jmp addr_{reference}\n");
                    }
                    str += &format!("addr_{}:\n", reversed.get(&ip).unwrap());
                }
            },
            Operation::None(..) => {}
            Operation::Proc(Proc {
                id,
                skip_to,
                loc,
                local_mem_size,
                name,
                ..
            }) => {
                let is_export = program
                    .contracts
                    .get(*id)
                    .unwrap()
                    .attributes
                    .has_attribute("__export__");
                if let Some(skip_to) = skip_to {
                    str += &format!("    ;; -- fn_skip({name}) --\n");
                    str += &format!("    jmp addr_{skip_to}\n");
                    str += &format!("    ;; -- fn({name}) --\n");
                    str += &format!("addr_{id}:\n");
                    if is_export {
                        str += &format!("fn_{}:\n", name);
                    }
                    str += &format!("    sub rsp, {}\n", local_mem_size);
                    str += "    mov [ret_stack_rsp], rsp\n";
                    str += "    mov rsp, rax\n";
                } else {
                    err(
                        loc,
                        "Expected skip_to to be something, but found nothing. Linker error?",
                    );
                }
            }
            Operation::CallProc(CallProc { id, .. }) => {
                str += "    ;; -- call fn --\n";
                str += "    mov rax, rsp\n";
                str += "    mov rsp, [ret_stack_rsp]\n";
                str += &format!("    call addr_{}\n", *id);
                str += "    mov [ret_stack_rsp], rsp\n";
                str += "    mov rsp, rax\n";
            }
            Operation::Ret(Ret {
                dealloc_len,
                is_end,
                ..
            }) => {
                str += "    ;; end/ret\n";
                str += "    mov rax, rsp\n";
                str += "    mov rsp, [ret_stack_rsp]\n";
                str += &format!("    add rsp, {}\n", dealloc_len);
                str += "    ret\n";
                if *is_end {
                    str += &format!("    addr_{}:\n", reversed.get(&ip).unwrap());
                }
            }
            Operation::PushLocalMem(PushLocalMem { off, .. }) => {
                str += "    ;; -- push_local_mem --\n";
                str += "    mov rax, [ret_stack_rsp]\n";
                str += &format!("    add rax, {off}\n");
                str += "    push rax\n";
            }
            Operation::Assembly(PushAssembly { asm, .. }) => {
                str += asm;
                str.push('\n');
            }
            Operation::Typefence(..) => {}
        }
        ip += 1;
    }

    str += "    mov rax, 60\n";
    str += "    mov rdi, 0\n";
    str += "    syscall\n";
    str += "\nsegment .data\n";
    if program.main_fn.is_none() {
        str += "    __str_no_main: db 'No main function found...',0xa\n"; // 0xa: a new-line character
        str += "    __str_no_main_len: EQU $ - __str_no_main\n";
    }

    for (i, s) in program.strings.iter().enumerate() {
        str += &format!(
            "    str_{i}: db {}",
            s.as_bytes()
                .iter()
                .map(|f| f.to_string())
                .reduce(|acc, s| format!("{acc}, {s}"))
                .unwrap_or_default()
        );
        str += "\n"
    }
    str += "\nsegment .bss\n";
    str += "    ret_stack_rsp: resq 1\n";
    str += "    ret_stack: resb 4096\n";
    str += "    ret_stack_end:\n";
    for (id, sz) in program.mems {
        str += &format!("    mem_{}: resb {}\n", id, sz);
    }

    str
}
