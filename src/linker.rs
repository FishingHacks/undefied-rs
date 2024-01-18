use crate::{
    error::err,
    parser::{Keyword, OpKeyword, Operation, Program},
    utils::iota,
};

fn add_ref(ip: usize, prog: &mut Program) -> usize {
    let i = iota();
    prog.refs.insert(i, ip);
    i
}

pub fn link(program: &mut Program) {
    let mut stack: Vec<usize> = Vec::new();
    let mut current_fn: Option<usize> = None;

    for ip in 0..program.ops.len() {
        // let prg = program;
        let op = program.ops[ip].clone();

        if let Some(p) = op.as_proc() {
            if current_fn.is_some() {
                err(&p.loc, "Function inside of function isnt allowed");
            }
            current_fn = Some(p.id);
            program.refs.insert(p.id, ip);
            stack.push(p.id);
        } else if let Some(r) = op.as_ret() {
            if stack.is_empty() || current_fn.is_none() {
                err(&r.loc, "Expected return to be in a function");
            }
            let proc_id =
                current_fn.unwrap_or_else(|| err(&r.loc, "Expected return to be in a function"));
            let proc_ip = program
                .refs
                .get(&proc_id)
                .unwrap_or_else(|| err(&r.loc, "Expected proc_id to be a valid id"));
            if *proc_ip >= program.ops.len() {
                err(
                    &r.loc,
                    "Expected proc_ip to be a valid ip pointing to a proc",
                )
            }
            let proc = program.ops[*proc_ip].as_proc().unwrap_or_else(|| {
                err(
                    &r.loc,
                    "Expected proc_ip to be a valid ip pointing to a proc",
                )
            });
            program.ops[ip].as_ret_mut().unwrap().dealloc_len = proc.local_mem_size;
            if r.is_end {
                let block_ip = *program
                    .refs
                    .get(&stack.pop().unwrap())
                    .expect("Failed to get reference");
                let op1 = &mut program.ops[block_ip];
                if let Some(op1) = op1.as_proc_mut() {
                    op1.skip_to = Some({
                        let i = iota();
                        program.refs.insert(i, ip);
                        i as u64
                    });
                } else {
                    err(
                        op1.get_location(),
                        format!("Expected proc but found {:?}", op1),
                    );
                }
                current_fn = None;
            }
        } else if let Some(k) = op.as_keyword() {
            match k.keyword {
                Keyword::If => {
                    stack.push(add_ref(ip, program));
                }
                Keyword::Else => {
                    if stack.is_empty() {
                        err(&k.loc, "Could not find the starting keyword for the block");
                    }
                    let id = add_ref(ip, program);
                    let kw_ip = program
                        .refs
                        .get(&stack.pop().unwrap())
                        .expect("Failed to get kw_ip");
                    let op = &mut program.ops[*kw_ip];
                    let kw = if let Operation::Keyword(kw) = op { kw } else {
                        err(op.get_location(), "Expected keyword but found something else");
                    };
                    
                    if kw.keyword != Keyword::If && kw.keyword != Keyword::IfStar {
                        err(
                            &k.loc,
                            format!(
                                "Expected if or if* ahead of else, but found {}\nFound here: {}",
                                kw.keyword, kw.loc
                            ),
                        )
                    }
                    kw.reference = Some(id as u64);

                    let kw = program.ops[*kw_ip].as_keyword().unwrap();

                    if kw.keyword == Keyword::IfStar {
                        let else_ip = program
                            .refs
                            .get(&stack.pop().unwrap())
                            .expect("Could not find else_ip");
                        let else_op = &program.ops[*else_ip];
                        if let Operation::Keyword(OpKeyword {
                            keyword: Keyword::Else,
                            ..
                        }) = else_op
                        {
                            program.ops[*else_ip].as_keyword_mut().unwrap().reference = Some(id as u64);
                        } else if let Operation::Keyword(other_kw) = else_op {
                            err(
                                &kw.loc,
                                format!(
                                    "if* needs to be prepended by else, found {} (At {})",
                                    other_kw.keyword, other_kw.loc
                                ),
                            );
                        } else {
                            err(&kw.loc, format!("if* needs to be prepended by else, found something else (At {})", else_op.get_location()));
                        }
                    }
                    stack.push(id);
                }
                Keyword::While => {
                    stack.push(add_ref(ip, program));
                }
                Keyword::Do => {
                    if stack.is_empty() {
                        err(&k.loc, "Could not find the starting keyword for the block");
                    }
                    program.ops[ip]
                        .as_keyword_mut()
                        .expect("Expected keyword")
                        .reference = Some(stack.pop().unwrap() as u64);
                    stack.push(add_ref(ip, program));
                }
                Keyword::End => {
                    if stack.is_empty() {
                        err(&k.loc, "Could not find the starting keyword for the block");
                    }
                    let block_ip = *program
                        .refs
                        .get(&stack.pop().unwrap())
                        .expect("Failed to get reference");
                    let op1 = &mut program.ops[block_ip];
                    if let Some(op1) = op1.as_keyword() {
                        match op1.keyword {
                            Keyword::End | Keyword::IfStar => err(
                                &k.loc,
                                format!(
                                    "Expected if, else or do, but found {} (At {})",
                                    op1.keyword, op1.loc
                                ),
                            ),
                            Keyword::While => {
                                err(&k.loc, "Expected if, else or do, but found while")
                            }
                            Keyword::If | Keyword::Else => {
                                let me = {
                                    let i = iota();
                                    program.refs.insert(i, ip);
                                    i as u64
                                };
                                program.ops[block_ip]
                                    .as_keyword_mut()
                                    .expect("checked")
                                    .reference = Some(me);
                            }
                            Keyword::Do => {
                                if let Some(while_reference) = op1.reference {
                                    program.ops[ip].as_keyword_mut().unwrap().reference =
                                        Some(while_reference);
                                } else {
                                    err(&op1.loc, "Expected do to have a reference to while")
                                }
                                let me = {
                                    let i = iota();
                                    program.refs.insert(i, ip);
                                    i as u64
                                };
                                program.ops[block_ip]
                                    .as_keyword_mut()
                                    .expect("checked")
                                    .reference = Some(me);
                            }
                        }
                    } else {
                        err(
                            &k.loc,
                            format!("End can only end keywords and procs (found {:?})", op1),
                        );
                    }
                }
                Keyword::IfStar => {
                    if stack.is_empty() {
                        err(&k.loc, "if* has to go after else");
                    }
                    let else_id = stack.pop().unwrap();
                    let else_ip = program
                        .refs
                        .get(&else_id)
                        .expect("Could not obtain else ip");
                    let else_op = &program.ops[*else_ip];
                    if let Operation::Keyword(OpKeyword {
                        keyword: Keyword::Else,
                        ..
                    }) = else_op
                    {
                        stack.push(else_id);
                        stack.push(add_ref(ip, program));
                    } else {
                        err(&k.loc, "if* has to go after else");
                    }
                }
            }
        }
    }

    if !stack.is_empty() {
        for el in stack {
            let reference = *program.refs.get(&el).unwrap();
            println!("{:?}", program.ops[reference]);
        }
        panic!("expected end but found nothing");
    }
}
