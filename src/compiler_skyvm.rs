use std::{fs::File, io::Write, os::unix::fs::FileExt, process::Command};

use crate::{
    error::{err, err_generic, Log},
    parser::{
        CallProc, Intrinsic, Keyword, OpIntrinsic, OpKeyword, Operation, Proc, Program,
        PushAssembly, PushConst, PushInt, PushLocalMem, PushMem, PushStr, Ret,
    },
    typecheck::Type,
    utils::{get_stdpath, iota}, Config,
};

const START: &str = r#"
mov %rax .ret_stack_end
store32 .ret_stack_rsp %rax
"#;

/* const SYSCALL_REGS: &[&str] = &["rdi", "rsi", "rdi", "r10", "r8", "r9"];

#[inline]
fn syscall(mut num: u8) -> String {
    if num > 6 {
        num = 6;
    }

    let mut str = "pop rax\n".to_string();

    for i in 0..num {
        str += &format!("pop {}\n", SYSCALL_REGS[i as usize]);
    }
    str += "syscall\n";
    str += "xor rbx, rbx\n"; // xor rbx, rbx
    str += "cmp rax, rbx\n"; // cmp rax, rbx
    let id = iota();
    str += &format!("jge after_syscall_{id}\n"); // jge .after_syscall_{id}
    str += "mov rbx, rax\n"; // mov rax, rbx
    str += "not rbx\n"; // not rbx
    str += "add rbx, 1\n"; // add rbx, 1
    str += "mov rax, -1\n"; // mov rax, -1
    str += &format!("after_syscall_{id}:\n"); // .after_syscall_{id}:
    str += "mov [errno], rbx\n"; // mov [errno], rbx
    str += "push rax\n";

    str
} */

pub fn compile(mut program: Program, config: &Config) -> std::io::Result<()> {
    let mut str: String = String::with_capacity(1000);
    str.push_str(START);

    macro_rules! comparison {
        ($jmp: expr, $str: expr) => {{
            $str += "pop %rbx\n";
            $str += "pop %rax\n";
            $str += "mov %rcx 1\n";
            $str += "cmp %rax %rbx\n";
            let id = iota();
            $str += &format!("{} .after_eq_{}\n", $jmp, id);
            $str += "dec %rcx\n";
            $str += &format!("#label after_eq_{}\n", id);
            $str += "push %rcx\n";
        }};
    }

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
            str += ";; -- call run functions --\n";
            str += "mov %rax %rsp\n";
            str += "load32 .ret_stack_rsp %rsp\n";
            
            for id in &program.run_functions {
                str += &format!("call .addr_{}\n", *id);
            }

            str += ";; -- call main --\n";
        } else {
            str += ";; -- call main --\n";
            str += "mov %rax %rsp\n";
            str += "load32 .ret_stack_rsp %rsp\n";
        }
        str += &format!("call .addr_{}\n", id);
        str += "store32 .ret_stack_rsp %rsp\n";
        str += "mov %rsp %rax\n";
        str += ";; -- exit --\n";

        str += "hlt\n";
        if main_fn_contract.out_types.len() > 0 {
            err(&main_fn_contract.loc, "The main function cannot have a return type (Expected signature: fn main in ... end)")
        }
    } else {
        if !config.no_run_fns && program.run_functions.len() > 0 {
            str += ";; -- call run functions --\n";
            str += "mov %rax %rsp\n";
            str += "load32 .ret_stack_rsp %rsp\n";

            for id in &program.run_functions {
                str += &format!("call .addr_{}", *id);
            }

            str += "store32 .ret_stack_rsp %rsp\n";
            str += "mov %rsp %rax\n";
        }

        str += "hlt\n";
    }

    let mut ip: usize = 0;
    program.reverse_refs();
    let reversed = program.reversed_refs;

    while ip < program.ops.len() {
        let op = &program.ops[ip];
        match op {
            Operation::Intrinsic(OpIntrinsic { op, loc }) => {
                str += &format!(";; -- {} --\n", op.to_str());
                match op {
                    Intrinsic::Print => err(loc, "print is not supported"),
                    Intrinsic::Here => {
                        let _str = format!("{}", loc);
                        let len = _str.len();
                        let i = program.strings.len();
                        program.strings.push(_str);
                        str += &format!("push {}\n", len);
                        str += &format!("push .str_{i}\n");
                    }
                    Intrinsic::Plus | Intrinsic::StructPtrPlus => {
                        str += "pop %rax\n";
                        str += "pop %rbx\n";
                        str += "add %rax %rbx\n";
                        str += "push %rax\n";
                    }
                    Intrinsic::Minus => {
                        str += "pop %rax\n";
                        str += "pop %rbx\n";
                        str += "sub %rbx %rax\n";
                        str += "push %rbx\n";
                    }
                    Intrinsic::Mul => {
                        str += "pop %rax\n";
                        str += "pop %rbx\n";
                        str += "mul %rax %rbx\n";
                        str += "push %rax\n";
                    }
                    Intrinsic::DivMod => {
                        str += "pop %rbx\n";
                        str += "pop %rax\n";
                        str += "div %rax %rbx\n";
                        str += "push %rax\n";
                        str += "push %rbx\n";
                    }

                    Intrinsic::LessThan => comparison!("jl", str),
                    Intrinsic::LessThanEqual => comparison!("jle", str),
                    Intrinsic::GreaterThan => comparison!("jg", str),
                    Intrinsic::GreaterThanEqual => comparison!("jge", str),
                    Intrinsic::Equal => comparison!("je", str),
                    Intrinsic::NotEqual => comparison!("jne", str),

                    Intrinsic::Drop => {
                        str += "pop %rax\n";
                    }
                    Intrinsic::Dup => {
                        str += "pop %rax\n";
                        str += "push %rax\n";
                        str += "push %rax\n";
                    }
                    Intrinsic::Over => {
                        str += "pop %rbx\n";
                        str += "pop %rax\n";
                        str += "push %rax\n";
                        str += "push %rbx\n";
                        str += "push %rax\n";
                    }
                    Intrinsic::Swap => {
                        str += "pop %rbx\n";
                        str += "pop %rax\n";
                        str += "push %rbx\n";
                        str += "push %rax\n";
                    }
                    Intrinsic::Rot => {
                        str += "pop %rax\n";
                        str += "pop %rbx\n";
                        str += "pop %rcx\n";
                        str += "push %rax\n";
                        str += "push %rcx\n";
                        str += "push %rbx\n";
                    }

                    Intrinsic::Syscall0 => err(loc, "No syscall support yet :/"),
                    Intrinsic::Syscall1 => err(loc, "No syscall support yet :/"),
                    Intrinsic::Syscall2 => err(loc, "No syscall support yet :/"),
                    Intrinsic::Syscall3 => err(loc, "No syscall support yet :/"),
                    Intrinsic::Syscall4 => err(loc, "No syscall support yet :/"),
                    Intrinsic::Syscall5 => err(loc, "No syscall support yet :/"),
                    Intrinsic::Syscall6 => err(loc, "No syscall support yet :/"),

                    Intrinsic::Shl => {
                        str += "pop %rcx\n";
                        str += "pop %rbx\n";
                        str += "shl %rbx %rax\n";
                        str += "push %rbx\n";
                    }
                    Intrinsic::Shr => {
                        str += "pop %rax\n";
                        str += "pop %rbx\n";
                        str += "shr %rbx %rax\n";
                        str += "push %rbx\n";
                    }
                    Intrinsic::Or => {
                        str += "pop %rax\n";
                        str += "pop %rbx\n";
                        str += "or %rbx %rax\n";
                        str += "push %rbx\n";
                    }
                    Intrinsic::And => {
                        str += "pop %rax\n";
                        str += "pop %rbx\n";
                        str += "and %rbx %rax\n";
                        str += "push %rbx\n";
                    }
                    Intrinsic::Xor => {
                        str += "pop %rax\n";
                        str += "pop %rbx\n";
                        str += "xor %rbx %rax\n";
                        str += "push %rbx\n";
                    }
                    Intrinsic::Not => {
                        str += "pop %rax\n";
                        str += "not %rax\n";
                        str += "push %rax\n";
                    }

                    Intrinsic::Load | Intrinsic::Store => unreachable!(),
                    
                    Intrinsic::Load8 => {
                        str += ";; @8\n";
                        str += "pop %rax\n";
                        str += "load8 %rax %rbx\n";
                        str += "push %rbx\n";
                    }
                    Intrinsic::Store8 => {
                        str += ";; !8\n";
                        str += "pop %rax\n";
                        str += "pop %rbx\n";
                        str += "store8 %rax %rbx\n";
                    }

                    Intrinsic::Load16 => {
                        str += ";; @16\n";
                        str += "pop %rax\n";
                        str += "load16 %rax %rbx\n";
                        str += "push %rbx\n";
                    }
                    Intrinsic::Store16 => {
                        str += ";; !16\n";
                        str += "pop %rax\n";
                        str += "pop %rbx\n";
                        str += "store16 %rax %rbx\n";
                    }

                    Intrinsic::Load32 => {
                        str += ";; @32\n";
                        str += "pop %rax\n";
                        str += "load32 %rax %rbx\n";
                        str += "push %rbx\n";
                    }
                    Intrinsic::Store32 => {
                        str += ";; !32\n";
                        str += "pop %rax\n";
                        str += "pop %rbx\n";
                        str += "store32 %rax %rbx\n";
                    }
                    Intrinsic::Load64 => err(&loc, "@64 and !64 are not supported"),
                    Intrinsic::Store64 => err(&loc, "@64 and !64 are not supported"),

                    Intrinsic::CastBool
                    | Intrinsic::CastInt
                    | Intrinsic::CastPtr
                    | Intrinsic::CastPtrToPlus
                    | Intrinsic::CastPtrToMinus
                    | Intrinsic::StackInfo => {} // _ => compile_error!("RAAAAGHHHH U DIDNT FINISH IMPLEMENTING YET >:C"),
                }
            }
            Operation::PushConst(PushConst { constant, .. }) => {
                str += ";; -- constant --\n";
                str += &format!("push {}\n", constant.value);
            }
            Operation::PushInt(PushInt { value, .. }) | Operation::PushPtr(PushInt { value, .. }) => {
                str += ";; -- int --\n";
                str += &format!("push {}\n", value);
            }
            Operation::PushMem(PushMem { id, off, .. }) => {
                str += ";; -- mem --\n";
                if *off > 0 {
                    str += &format!("mov %rax .mem_{}\n", *id);
                    str += &format!("add %rax {}\n", *off);
                    str += "push %rax\n";
                } else {
                    str += &format!("push .mem_{}\n", id);
                }
            }
            Operation::PushStr(PushStr {
                is_cstr,
                len,
                value,
                ..
            }) => {
                str += ";; -- str --\n";
                if !is_cstr {
                    str += &format!("push {}\n", len);
                }
                if len > &0 {
                    str += &format!("push .str_{}\n", value);
                } else {
                    str += "push 0\n";
                }
            }
            Operation::Keyword(OpKeyword {
                keyword,
                reference,
                loc,
            }) => match keyword {
                Keyword::If | Keyword::IfStar => {
                    if let Some(reference) = reference {
                        str += ";; -- if --\n";
                        str += "sub %rbx %rbx\n";
                        str += "pop %rax\n";
                        str += "cmp %rax %rbx\n";
                        if program.ops[*program.refs.get(&(*reference as usize)).unwrap()]
                            .as_keyword()
                            .unwrap()
                            .keyword
                            != Keyword::End
                        {
                            str += &format!("je .addr_{reference}__\n");
                        } else {
                            str += &format!("je .addr_{reference}\n");
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
                        str += ";; -- else --\n";
                        str += &format!("#label addr_{}\n", reversed.get(&ip).unwrap());
                        str += &format!("jmp .addr_{reference}\n");
                        str += &format!("#label addr_{}__\n", reversed.get(&ip).unwrap());
                    } else {
                        err(
                            loc,
                            "No reference for this if-block defined. Probably a linker issue",
                        );
                    }
                }
                Keyword::Do => {
                    if let Some(reference) = reference {
                        str += ";; -- do --\n";
                        str += "pop %rax\n";
                        str += "sub %rbx %rbx\n";
                        str += "cmp %rax %rbx\n";
                        str += &format!("je .addr_{reference}\n");
                    } else {
                        err(loc, "No reference for this do");
                    }
                }
                Keyword::While => {
                    str += ";; -- while --\n";
                    str += &format!("#label addr_{}\n", reversed.get(&ip).unwrap());
                }
                Keyword::End => {
                    str += ";; -- end --\n";
                    if let Some(reference) = reference {
                        str += &format!("jmp .addr_{reference}\n");
                    }
                    str += &format!("#label addr_{}\n", reversed.get(&ip).unwrap());
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
                    str += &format!(";; -- fn_skip({name}) --\n");
                    str += &format!("jmp .addr_{skip_to}\n");
                    str += &format!(";; -- fn({name}) --\n");
                    str += &format!("#label addr_{id}\n");
                    if is_export {
                        str += &format!("#label fn_{}\n", name);
                    }
                    str += &format!("sub %rsp {}\n", local_mem_size);
                    str += "store32 .ret_stack_rsp %rsp\n";
                    str += "mov %rsp %rax\n";
                } else {
                    err(
                        loc,
                        "Expected skip_to to be something, but found nothing. Linker error?",
                    );
                }
            }
            Operation::CallProc(CallProc { id, .. }) => {
                str += ";; -- call fn --\n";
                str += "mov %rax %rsp\n";
                str += "load32 .ret_stack_rsp %rsp\n";
                str += &format!("call .addr_{}\n", *id);
                str += "store32 .ret_stack_rsp %rsp\n";
                str += "mov %rsp %rax\n";
            }
            Operation::Ret(Ret {
                dealloc_len,
                is_end,
                ..
            }) => {
                str += ";; end/ret\n";
                str += "mov %rax %rsp\n";
                str += "load32 .ret_stack_rsp %rsp\n";
                str += &format!("add %rsp {}\n", dealloc_len);
                str += "ret\n";
                if *is_end {
                    str += &format!("#label addr_{}\n", reversed.get(&ip).unwrap());
                }
            }
            Operation::PushLocalMem(PushLocalMem { off, .. }) => {
                str += ";; -- push_local_mem --\n";
                str += "load32 .ret_stack_rsp %rax\n";
                str += &format!("add %rax {off}\n");
                str += "push %rax\n";
            }
            Operation::Assembly(PushAssembly { asm, .. }) => {
                str += asm;
                str.push('\n');
            }
            Operation::Typefence(..) | Operation::None(..)  | Operation::Cast(..) => {}
        }
        ip += 1;
    }

    str += "hlt\n";
    if program.main_fn.is_none() {
        str += "#value __str_no_main b 0x4e 0x6f 0x20 0x6d 0x61 0x69 0x6e 0x20 0x66 0x75 0x6e 0x63 0x74 0x69 0x6f 0x6e 0x20 0x66 0x6f 0x75 0x6e 0x64 0x2e 0x2e 0x2e 0xa 0x0\n";
        // 0xa: a new-line character; "No main function found..."
    }

    for (i, s) in program.strings.iter().enumerate() {
        str += &format!(
            "#value str_{i} b {}",
            s.as_bytes()
                .iter()
                .map(|f| f.to_string())
                .reduce(|acc, s| format!("{acc} {s}"))
                .unwrap_or_default()
        );
        str += " 0x0\n"
    }
    str += "\n";
    str += "#memory ret_stack_rsp 4\n";
    str += "#memory ret_stack 4096\n";
    str += "#memory ret_stack_end 0\n";
    // str += "#memory errno: 4\n";
    for (id, sz) in program.mems {
        str += &format!("#memory mem_{} {}\n", id, sz);
    }

    let mut f = File::create("default.asm")?;
    f.write_all_at(str.as_bytes(), 0)?;
    f.flush()?;
    drop(f);

    let mut execpath = get_stdpath();
    execpath.push("skyvm");
    execpath.push("assembler");
    let cmd = Command::new(&execpath)
        .args(["default.asm"])
        .log()
        .output()
        .expect("Failed to execute process");

    unsafe {
        println!("{}", String::from_utf8_unchecked(cmd.stdout));
        println!("{}", String::from_utf8_unchecked(cmd.stderr));
    }

    Ok(())
}
