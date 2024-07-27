use std::fs::{remove_file, File};
use std::io::{Read, Write};
use std::os::unix::fs::FileExt;
use std::path::PathBuf;
use std::process::Command;

use crate::error::{err, err_generic, Log};
use crate::parser::{
    CallProc, Intrinsic, Keyword, OpIntrinsic, OpKeyword, Operation, Proc, Program, PushAssembly,
    PushConst, PushFnPtr, PushInt, PushLocalMem, PushMem, PushStr, Ret,
};
use crate::typecheck::Type;
use crate::utils::iota;
use crate::{CallingConvention, Config};

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
    str += "    syscall\n";
    str += "    xor rbx, rbx\n"; // xor rbx, rbx
    str += "    cmp rax, rbx\n"; // cmp rax, rbx
    let id = iota();
    str += &format!("    jge after_syscall_{id}\n"); // jge .after_syscall_{id}
    str += "    mov rbx, rax\n"; // mov rax, rbx
    str += "    not rbx\n"; // not rbx
    str += "    add rbx, 1\n"; // add rbx, 1
    str += "    mov rax, -1\n"; // mov rax, -1
    str += &format!("after_syscall_{id}:\n"); // .after_syscall_{id}:
    str += "    mov [errno], rbx\n"; // mov [errno], rbx
    str += "    push rax\n";

    str
}

pub const ARGS_REGS: &[&str] = &["rdi", "rsi", "rdx", "rcx", "r8", "r9"];

pub fn compile(mut program: Program, config: &Config, path: &PathBuf) -> std::io::Result<()> {
    let mut str: String = String::with_capacity(1000);

    for contract in &program.contracts {
        if contract
            .1
            .attributes
            .has_attribute("__provided_externally__")
        {
            let name = contract
                .1
                .attributes
                .get_value("__provided_externally__")
                .unwrap_or(&contract.1.name);
            str.push_str("extern ");
            str.push_str(name);
            str.push('\n');
        }
    }

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
        let main_fn_contract = program.contracts.get(&id).unwrap_or_else(|| {
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

        if !config.no_run_fns && program.run_functions.len() > 0 {
            str += "    ;; -- call run functions --\n";
            str += "    mov rax, rsp\n";
            str += "    mov rsp, [ret_stack_rsp]\n";

            for id in &program.run_functions {
                str += &format!("    call addr_{}\n", *id);
            }

            str += "    ;; -- call main --\n";
        } else {
            str += "    ;; -- call main --\n";
            str += "    mov rax, rsp\n";
            str += "    mov rsp, [ret_stack_rsp]\n";
        }
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
        if !config.no_run_fns && program.run_functions.len() > 0 {
            str += "    ;; -- call run functions --\n";
            str += "    mov rax, rsp\n";
            str += "    mov rsp, [ret_stack_rsp]\n";

            for id in &program.run_functions {
                str += &format!("    call addr_{}\n", *id);
            }

            str += "    mov [ret_stack_rsp], rsp\n";
            str += "    mov rsp, rax\n";
        }

        // print the no main fn to stdout
        str += "    ;; -- print no main fn to stdout --\n";
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
                    Intrinsic::Plus | Intrinsic::StructPtrPlus => {
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

                    Intrinsic::Load | Intrinsic::Store => err(
                        &loc,
                        "@ and ! intrinsics aren't supported for no typechecking",
                    ),

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

                    Intrinsic::CallFnPtr => {
                        str += "    ;; call\n";
                        str += "    pop rbx\n";
                        str += "    mov rax, rsp\n";
                        str += "    mov rsp, [ret_stack_rsp]\n";
                        str += "    call rbx\n";
                        str += "    mov [ret_stack_rsp], rsp\n";
                        str += "    mov rsp, rax\n";
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
            Operation::PushInt(PushInt { value, .. })
            | Operation::PushPtr(PushInt { value, .. }) => {
                str += "    ;; -- int --\n";
                str += &format!("    mov rax, {}\n", value);
                str += "    push rax\n";
            }
            Operation::PushMem(PushMem { id, off, .. }) => {
                str += "    ;; -- mem --\n";
                str += &format!("    push mem_{}+{}\n", id, off);
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
                        if program.ops[*program.refs.get(&(*reference as usize)).unwrap()]
                            .as_keyword()
                            .unwrap()
                            .keyword
                            != Keyword::End
                        {
                            str += &format!("    je addr_{reference}__\n");
                        } else {
                            str += &format!("    je addr_{reference}\n");
                        }
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
                    .get(id)
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
            Operation::CallProc(CallProc {
                id,
                externally_provided,
                loc
            }) => {
                str += "    ;; -- call fn --\n";
                if let Some(name) = externally_provided {
                    let contract = program.contracts.get(id).unwrap();

                    let convention = match contract
                        .attributes
                        .get_value("__calling_convention__")
                        .map(|str| str.as_str())
                    {
                        None => err_generic("You have to specify a calling convention"),
                        Some("C") => CallingConvention::CStyle,
                        Some("raw") => CallingConvention::Raw,
                        Some(convention) => err(loc, format!(
                            "Calling Convention {convention} is not supported by this target"
                        )),
                    };

                    for in_type in &contract.in_types {
                        if !in_type.works_for_calling_convention(convention) {
                            err(loc, format!("The {convention} does not support type {in_type:?}"));
                        }
                    }
                    for out_type in &contract.out_types {
                        if !out_type.works_for_calling_convention(convention) {
                            err(loc, format!("The {convention} does not support type {out_type:?}"));
                        }
                    }

                    if convention == CallingConvention::CStyle {
                        if contract.out_types.len() > 1 {
                            err(loc, format!("The {convention} does not support functions returning more than 1 argument"));
                        }
                        for i in 0..contract.in_types.len().min(ARGS_REGS.len()) {
                            str += &format!("    pop {}\n", ARGS_REGS[i]);
                        }
                        str += &format!("    call {}\n", name);
                        str += "    push rax\n";
                    } else if convention == CallingConvention::Raw {
                        str += &format!("    call {}\n", name);
                    }
                } else {
                    str += "    mov rax, rsp\n";
                    str += "    mov rsp, [ret_stack_rsp]\n";
                    str += &format!("    call addr_{}\n", *id);
                    str += "    mov [ret_stack_rsp], rsp\n";
                    str += "    mov rsp, rax\n";
                }
            }

            Operation::PushFnPtr(PushFnPtr { contract_id, .. }) => {
                let contract = program.contracts.get(contract_id).unwrap();
                let name = if contract.attributes.has_attribute("__provided_externally__") {
                    contract
                        .attributes
                        .get_value("__provided_externally__")
                        .unwrap_or(&contract.name)
                        .to_string()
                } else {
                    format!("addr_{}", *contract_id)
                };

                str += "    ;; -- push fn ptr --\n";
                str += &format!("    push {name}\n");
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
            Operation::Typefence(..) | Operation::Cast(..) | Operation::None(..) => {}
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
        str += ", 0x0\n"
    }
    str += "\nsegment .bss\n";
    str += "    ret_stack_rsp: resq 1\n";
    str += "    ret_stack: resb 4096\n";
    str += "    ret_stack_end:\n";
    str += "    errno: resq 1\n";
    for (id, sz) in program.mems {
        str += &format!("    mem_{}: resb {}\n", id, sz);
    }

    let mut f = File::create("_.asm")?;
    f.write_all_at(str.as_bytes(), 0)?;

    for file in &config.asm_files_to_include {
        let mut file = File::open(file)?;
        loop {
            let mut arr = [0_u8; 4096];
            let read = file.read(&mut arr)?;
            if read < 1 {
                break;
            }
            f.write(&arr[0..read])?;
        }
    }

    f.flush()?;
    drop(f);

    let out_name = path
        .file_stem()
        .expect("No file name found")
        .to_str()
        .expect("The file to compile is not valid");

    unsafe {
        let assembler = Command::new("nasm")
            .args(["-g", "-felf64", "-o", "_.o", "_.asm"])
            .log()
            .output()
            .expect("Failed to execute process");

        println!("{}", String::from_utf8_unchecked(assembler.stdout));

        if !assembler.status.success() {
            err_generic(format!(
                "Failed to run the assembler!\n\n{}",
                String::from_utf8_unchecked(assembler.stderr)
            ));
        }

        let mut linker_args = vec![
            "-o",
            out_name,
            "-dynamic-linker",
            "/lib64/ld-linux-x86-64.so.2",
        ];

        for path in &config.library_link_paths {
            linker_args.push("-L");
            linker_args.push(
                path.to_str()
                    .expect("A path in the link-path argument did not have a valid folder path"),
            );
        }
        for path in &config.library_links {
            linker_args.push("-l");
            linker_args.push(path);
        }

        if config.create_shared {
            linker_args.push("--shared");
        }

        linker_args.push("_.o");
        let linker = Command::new("ld")
            .args(linker_args)
            .log()
            .output()
            .expect("Failed to execute process");

        println!("{}", String::from_utf8_unchecked(linker.stdout));

        if !linker.status.success() {
            err_generic(format!(
                "Failed to run the linker!\n\n{}",
                String::from_utf8_unchecked(linker.stderr)
            ));
        }
    }

    if !config.keep_files {
        // remove_file("_.asm").expect("Failed to remove _.asm file (compilation artifact)");
        remove_file("_.o").expect("Failed to remove _.o file (compilation artifact)");
    }

    if config.run_after_compilation {
        let child = Command::new(format!("./{}", out_name))
            .log()
            .spawn()
            .expect("Failed to launch child")
            .wait()?;

        println!("Program exited with {}", child);
    }

    Ok(())
}
