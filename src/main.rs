/**
 * ROADMAP:
 * - impl predef macros
 * - impl predef consts
 * - finish parameters
 * - call convention (!!!)
 * - cmdline:
 *     - <option>
 *         -h, --help: print help
 *         -v, --version: get the current version
 *     
 *     - undefied com/compile filename.undefied [options]
 *         -r, --run: run the program after compilation
 *         -u, --unsafe: disable typechecking
 *         -O0: disable optimizations
 *         -O1: Apply Level 1 Optimizations
 *         --keep: generate .asm and .o files
 *         -t, --target <target>: Set the compilation target
 *         -i<file>, --include <file>: include a .undefied or .asm file
 *         -l<name>, --link <name>: link to a library
 *         -L<path>, --link-path <path>: specify a path the linker should look in
 *         --shared: Generate a shared library file (.so file)
 *         -D<constant>=<value>, --define <constant> <value>: Define a constant set to a value
 *         -M<name> <value>, --macro <name> <value>: Set a macro to a value (str)
 *         -cM<name> <value>, --cmacro <name> <value>: Set a macro to a value (cstr)
 *         -S, --no-sideeffects, --no-run-fns: Do not run functions marked with .param __run_function__
 *
 *     - undefied check/typecheck [options] filename.undefied
 *         -t, --target <target>: Set the compilation target
 *         -D<constant>=<value>, --define <constant> <value>: Define a constant set to a value
 *         -M<name> <value>, --macro <name> <value>: Set a macro to a value (str)
 *         -cM<name> <value>, --cmacro <name> <value>: Set a macro to a value (cstr)
 *
 *     - undefied config config-file
 *         config file type: JSON or TOML
 *         should include all options passed to compile
 */
use std::{fmt::{Debug, Display}, hash::Hash, ops::Index};

use parser::{AttributeList, Program};

mod compiler_linux;
mod compiler_skyvm;
mod optimization_dce;
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

#[derive(Clone, Copy)]
pub enum CompilationTarget {
    Linux,
    Skyvm,
}

impl Display for CompilationTarget {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Linux => f.write_str("Linux"),
            Self::Skyvm => f.write_str("SkyVM"),
        }
    }
}

impl CompilationTarget {
    pub fn asm_will_work(&self, attrs: &AttributeList) -> bool {
        let empty_str = String::default();
        let supports_value = attrs.get_value("__supports__").unwrap_or(&empty_str);
        if attrs.has_attribute("__supports_all__")
            || supports_value == "all"
            || attrs.has_attribute("empty_asm")
        {
            return true;
        }

        match self {
            Self::Linux => {
                supports_value == "linux"
                    || supports_value == "x86_64"
                    || supports_value == "x86_64-linux"
                    || attrs.has_attribute("__supports_linux__")
                    || attrs.has_attribute("__supports_x86_64__")
                    || attrs.has_attribute("__supports_x86_64-linux__")
            }
            Self::Skyvm => supports_value == "skyvm" || attrs.has_attribute("__supports_skyvm__"),
        }
    }
}

static mut STRINGS: Option<Vec<String>> = None;

fn init_strings() {
    unsafe {
        if let None = STRINGS {
            STRINGS = Some(vec!["".to_string()])
        }
    }
}

macro_rules! get_strings {
    () => {{
        init_strings();
        if let Some(v) = &STRINGS {
            v
        } else {
            unreachable!()
        }
    }};
}

#[derive(Clone, Copy, PartialEq, Eq, Default)]
#[repr(transparent)]
pub struct GlobalString(usize);

impl Hash for GlobalString {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.hash(state)
    }
}

impl GlobalString {
    pub fn get(&self) -> &'static String {
        get_str(self.0)
    }

    pub fn new(str: &String) -> Self {
        Self(find_string(str).unwrap_or(insert_str(str.clone())))
    }

    pub fn from_static(str: &str) -> Self {
        Self(find_string(str).unwrap_or(insert_str(str.to_string())))
    }

    pub fn from_str(str: String) -> Self {
        Self(find_string(&str).unwrap_or(insert_str(str)))
    }
}

impl Debug for GlobalString {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.get())
    }
}

impl Display for GlobalString {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.get())
    }
}

fn find_string(search_str: &str) -> Option<usize> {
    for (idx, str) in unsafe { get_strings!().iter().enumerate() } {
        if str == search_str {
            return Some(idx);
        }
    }
    None
}

fn insert_str(str: String) -> usize {
    unsafe {
        if let Some(v) = &mut STRINGS {
            let idx = v.len();
            v.push(str);
            idx
        } else {
            unreachable!()
        }
    }
}

fn get_str(idx: usize) -> &'static String {
    unsafe { get_strings!().index(idx) }
}

#[derive(Clone, Copy)]
pub struct Config {
    pub target: CompilationTarget,
    pub no_inline: bool,
    pub no_run_fns: bool,
}

fn main() {
    let config: Config = Config {
        target: CompilationTarget::Linux,
        no_inline: false,
        no_run_fns: false,
    };

    let tokens = tokenizer::parse(CODE.to_string(), &"<built-in>".to_string());
    let mut program = parser::parse_tokens(tokens, &config);
    linker::link(&mut program);
    typecheck::typecheck(&mut program);
    optimization_dce::optimize(&mut program);

    // control_flow_dump::dump_to_file("cf.dot", &program)?;
    match compile(config.target, program, &config) {
        Err(e) => println!("Execution failed: {e}"),
        Ok(..) => {}
    }
}

fn compile(target: CompilationTarget, program: Program, config: &Config) -> std::io::Result<()> {
    match target {
        CompilationTarget::Linux => compiler_linux::compile(program, config),
        CompilationTarget::Skyvm => compiler_skyvm::compile(program, config),
    }
}
