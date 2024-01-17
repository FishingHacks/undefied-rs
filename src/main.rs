use std::{
    fs::{remove_file, File},
    io::Write,
    os::unix::fs::FileExt,
    process::{Command, Stdio},
};

use crate::error::Log;

mod optimization_dce;
mod compiler;
// mod control_flow_dump;
mod error;
mod linker;
mod parser;
mod tokenizer;
mod typecheck;
mod utils;

const CODE: &str = r#"
include "a"
"#;

pub enum CompilationTargets {
    Linux
}

pub struct Config {
    pub target: CompilationTargets,
    pub no_inline: bool,
}

fn main() -> std::io::Result<()> {
    let config: Config = Config { target: CompilationTargets::Linux, no_inline: false };

    let tokens = tokenizer::parse(CODE.to_string(), &"<built-in>".to_string());
    let mut program = parser::parse_tokens(tokens, &config);
    linker::link(&mut program);
    typecheck::typecheck(&program);
    optimization_dce::optimize(&mut program);

    // control_flow_dump::dump_to_file("cf.dot", &program)?;
    let str = compiler::compile(program);
    let mut f = File::create("_.asm")?;
    f.write_all_at(str.as_bytes(), 0)?;
    f.flush()?;
    drop(f);

    unsafe {
        println!(
            "{}",
            String::from_utf8_unchecked(
                Command::new("nasm")
                    .args(["-felf64", "-o", "_.o", "_.asm"])
                    .log()
                    .output()
                    .expect("Failed to execute process")
                    .stdout
            )
        );

        println!(
            "{}",
            String::from_utf8_unchecked(
                Command::new("ld")
                    .args(["-o", "index", "_.o"])
                    .log()
                    .output()
                    .expect("Failed to execute process")
                    .stdout
            )
        );
    }

    // remove_file("_.asm").expect("Failed to remove _.asm file (compilation artifact)");
    remove_file("_.o").expect("Failed to remove _.o file (compilation artifact)");

    let child = Command::new("./index")
        .log()
        .stdin(Stdio::piped())
        .stderr(Stdio::piped())
        .stdout(Stdio::piped())
        .spawn()
        .expect("Failed to launch child")
        .wait_with_output()
        .expect("couldnt get output");

    unsafe {
        println!("{}", String::from_utf8_unchecked(child.stdout));
        println!("{}", String::from_utf8_unchecked(child.stderr));
    }

    println!("Program exited with {}", child.status);

    Ok(())
}
