use std::{fs::File, io::Write, os::unix::fs::FileExt, path::Path};

use crate::{
    parser::{
        CallProc, Keyword, OpIntrinsic, OpKeyword, Operation, Proc, Program, PushAssembly,
        PushConst, PushInt, PushLocalMem, PushMem, PushStr, Typefence,
    },
    typecheck::type_vec_to_str,
};

pub fn cf_to_str(program: &Program) -> String {
    let mut str = "digraph Program {\n".to_string();

    str += "    Node_start [label=\"Start\"];\n";
    str += "    Node_start -> Node_0;\n";

    for (ip, op) in program.ops.clone().into_iter().enumerate() {
        match op {
            Operation::PushConst(PushConst { constant, .. }) => {
                str += &format!(
                    "    Node_{ip} [label=\"Constant (value: {}, type: {})\"];\n",
                    constant.value, constant.typ
                );
                str += &format!("    Node_{ip} -> Node_{};\n", ip + 1);
            }
            Operation::Typefence(Typefence { expected_types, .. }) => {
                str += &format!(
                    "    Node_{ip} [label=\"typefence {}\"];\n",
                    type_vec_to_str(&expected_types)
                );
                str += &format!("    Node_{ip} -> Node_{};\n", ip + 1);
            }
            Operation::None(..) => {}
            Operation::PushLocalMem(PushLocalMem { off, .. }) => {
                str += &format!("    Node_{ip} [label=\"push_local_mem({off})\"];\n");
                str += &format!("    Node_{ip} -> Node_{};\n", ip + 1);
            }
            Operation::Ret(..) => {
                str += &format!("    Node_{ip} [label=\"return\"];\n");
            }
            Operation::CallProc(CallProc { id, .. }) => {
                let name = &program.ops[*program.refs.get(&id).unwrap()]
                    .as_proc()
                    .unwrap()
                    .name;
                str += &format!("    Node_{ip} [label=\"call {name}\"];\n");
                str += &format!("    Node_{ip} -> Node_{};\n", ip + 1);
            }
            Operation::Proc(Proc { name, skip_to, .. }) => {
                str += &format!("    Node_{ip} [label=\"fn {name}\"];\n");
                str += &format!("    Node_{ip} -> Node_{};\n", ip + 1);
                if let Some(skip_to) = skip_to {
                    str += &format!("    Node_{ip} -> Node_{};\n", skip_to);
                }
            }
            Operation::PushInt(PushInt { value, .. }) => {
                str += &format!("    Node_{ip} [label=\"{value}\"];\n");
                str += &format!("    Node_{ip} -> Node_{};\n", ip + 1);
            }
            Operation::Assembly(PushAssembly { .. }) => {
                str += &format!("    Node_{ip} [label=\"assembly\"];\n");
                str += &format!("    Node_{ip} -> Node_{};\n", ip + 1);
            }
            Operation::PushMem(PushMem { id, .. }) => {
                str += &format!("    Node_{ip} [label=\"Memory #{id}\"];\n");
                str += &format!("    Node_{ip} -> Node_{};\n", ip + 1);
            }
            Operation::PushStr(PushStr { value, .. }) => {
                str += &format!(
                    "    Node_{ip} [label=\"{}\"];\n",
                    escape_str(&program.strings[value as usize])
                );
                str += &format!("    Node_{ip} -> Node_{};\n", ip + 1);
            }
            Operation::Intrinsic(OpIntrinsic { op, .. }) => {
                str += &format!("    Node_{ip} [label=\"{}\"];\n", op.to_str());
                str += &format!("    Node_{ip} -> Node_{};\n", ip + 1);
            }
            Operation::Keyword(OpKeyword {
                keyword, reference, ..
            }) => match keyword {
                Keyword::If | Keyword::Do => {
                    str += &format!("    Node_{ip} [label=\"{}\"];\n", keyword.to_str());
                    str += &format!("    Node_{ip} -> Node_{} [label=true];\n", ip + 1);
                    if let Some(reference) = reference.and_then(|f| program.refs.get(&(f as usize)))
                    {
                        str += &format!(
                            "    Node_{ip} -> Node_{} [label=false style=dashed];\n",
                            *reference
                        );
                    } else {
                        panic!("If/do needs a reference");
                    }
                }
                Keyword::While => {
                    str += &format!("    Node_{ip} [label=\"{}\"];\n", keyword.to_str());
                    str += &format!("    Node_{ip} -> Node_{};\n", ip + 1);
                }
                Keyword::IfStar => todo!(),
                Keyword::Else => {
                    str += &format!("    Node_{ip} [label=\"{}\"];\n", keyword.to_str());
                    if let Some(reference) = reference.and_then(|f| program.refs.get(&(f as usize)))
                    {
                        str += &format!("    Node_{ip} -> Node_{};\n", *reference);
                    } else {
                        panic!("Else needs a reference");
                    }
                }
                Keyword::End => {
                    str += &format!("    Node_{ip} [label=\"{}\"];\n", keyword.to_str());
                    if let Some(reference) = reference.and_then(|f| program.refs.get(&(f as usize)))
                    {
                        str += &format!("    Node_{ip} -> Node_{};\n", *reference);
                    }
                    str += &format!("    Node_{ip} -> Node_{};\n", ip + 1);
                }
            },
        }
    }

    str += &format!("    Node_{} [label=\"End\"];\n", program.ops.len());

    str += "}\n";
    str
}

fn escape_str(value: &str) -> String {
    value
        .replace('\\', "\\\\")
        .replace('\n', "\\n")
        .replace('\t', "\\t")
        .replace('\"', "\\\"")
        .replace('\r', "\\r")
}

pub fn dump_to_file(name: &str, program: &Program) -> std::io::Result<()> {
    let str = cf_to_str(program);

    let mut f = File::create(Path::new(name))?;
    f.write_all_at(str.as_bytes(), 0)?;
    f.flush()?;
    drop(f);

    Ok(())
}
