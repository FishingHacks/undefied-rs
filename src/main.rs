/**
 * ROADMAP:
 * - impl predef consts (time)
 * - call convention (!!!)
 * - cmdline:
 *     - <option>
 *         -h, --help: print help
 *         -v, --version: get the current version
 *     
 *     - undefied com/compile filename.undefied [options]
 *         -r, --run: run the program after compilation
 *         -u, --unsafe: disable typechecking
 *         -o, --optimizations [0 | 1]: disable optimizations
 *         --keep: generate .asm and .o files
 *         -t, --target <target>: Set the compilation target
 *         -i <file>, --include <file>: include a .undefied or .asm file
 *         -l <name>, --link <name>: link to a library
 *         -L <path>, --link-path <path>: specify a path the linker should look in
 *         --shared: Generate a shared library file (.so file)
 *         -d <constant> <value>, --define <constant> <value>: Define a constant set to a value
 *         -m <name> <value>, --macro <name> <value>: Set a macro to a value (str)
 *         -M <name> <value>, --cmacro <name> <value>: Set a macro to a value (cstr)
 *         -S, --no-sideeffects, --no-run-fns: Do not run functions marked with .param __run_function__
 *         -F, --global-functions, --no-inline: disable function inlining
 *
 *     - undefied check/typecheck [options] filename.undefied
 *         -t, --target <target>: Set the compilation target
 *         -D<constant>=<value>, --define <constant> <value>: Define a constant set to a value
 *         -m <name> <value>, --macro <name> <value>: Set a macro to a value (str)
 *         -M <name> <value>, --cmacro <name> <value>: Set a macro to a value (cstr)
 *         -F, --global-functions, --no-inline: disable function inlining
 *
 *     - undefied config config-file
 *         config file type: JSON or TOML
 *         should include all options passed to compile
 */
use std::{
    collections::HashMap,
    ffi::OsStr,
    fmt::{Arguments, Debug, Display},
    fs::read_to_string,
    hash::Hash,
    ops::Index,
    path::PathBuf,
    time::{SystemTime, UNIX_EPOCH},
};

use clap::{
    builder::{PathBufValueParser, TypedValueParser, ValueParser},
    crate_authors, crate_name, value_parser, Arg, ArgAction, Command, Parser, ValueHint,
};
use error::err_generic;
use parser::AttributeList;

use crate::{error::info_generic, tokenizer::try_parse_num};

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

pub struct Version {
    major: u8,
    minor: u8,
}

impl Version {
    pub const fn new(major: u8, minor: u8) -> Self {
        Self { major, minor }
    }
}

impl Display for Version {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{}.{}", self.major, self.minor))
    }
}

pub const VERSION: Version = Version::new(1, 0);

#[derive(Debug, Clone, Copy)]
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

    pub fn from_str(str: &str) -> Option<Self> {
        match str {
            "linux" => Some(Self::Linux),
            "skyvm" => Some(Self::Skyvm),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Optimizations {
    dead_code_elimination: bool,
}

pub const OPTIMIZATION_LEVELS: [Optimizations; 2] = [
    Optimizations {
        dead_code_elimination: false,
    },
    Optimizations {
        dead_code_elimination: true,
    },
];

#[derive(Debug, Clone, PartialEq)]
pub enum PredefValue {
    Constant(u64),
    ConstantBool(bool),
    String(String),
    CString(String),
}

#[derive(Clone, Debug)]
pub struct Config {
    pub target: CompilationTarget,
    pub no_inline: bool,
    pub no_run_fns: bool,
    pub run_after_compilation: bool,
    pub no_typechecking: bool,
    pub optimizations: Optimizations,
    pub keep_files: bool,
    pub asm_files_to_include: Vec<PathBuf>,
    pub undefied_files_to_include: Vec<PathBuf>,
    pub library_links: Vec<String>,
    pub library_link_paths: Vec<PathBuf>,
    pub create_shared: bool,
    pub predefined: HashMap<String, PredefValue>,
}

impl Config {
    fn default(target: CompilationTarget) -> Self {
        Self {
            target,
            asm_files_to_include: vec![],
            library_link_paths: vec![],
            library_links: vec![],
            undefied_files_to_include: vec![],
            predefined: HashMap::default(),
            optimizations: OPTIMIZATION_LEVELS[0],
            no_inline: false,
            create_shared: false,
            keep_files: false,
            no_run_fns: false,
            no_typechecking: false,
            run_after_compilation: false,
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

fn main() {
    let compile_cmd = Command::new("compile")
        .visible_alias("com")
        .about("Compile your program using provided options")
        .arg(
            Arg::new("target")
                .short('t')
                .long("target")
                .help("The target to compile to")
                .value_parser(["linux", "skyvm"])
                .required(true),
        )
        .arg(
            Arg::new("run")
                .short('r')
                .long("run")
                .help("Run the program after compilation")
                .action(ArgAction::SetTrue),
        )
        .arg(
            Arg::new("keep")
                .long("keep")
                .help("generate _.asm and _.o files")
                .action(ArgAction::SetTrue),
        )
        .arg(
            Arg::new("unsafe")
                .long("unsafe")
                .short('u')
                .action(ArgAction::SetTrue)
                .help("Disable typechecking"),
        )
        .arg(
            Arg::new("optimizations")
                .short('o')
                .long("optimizations")
                .value_parser(["0", "1"])
                .help("Set the optimizations levels")
                .default_value("1")
                .hide_default_value(true),
        )
        .arg(
            Arg::new("shared")
                .long("shared")
                .action(ArgAction::SetTrue)
                .help("Compile to a shared library"),
        )
        .arg(
            Arg::new("include")
                .short('i')
                .long("include")
                .action(ArgAction::Append)
                .help("Include a .undefied or .asm file")
                .value_hint(ValueHint::FilePath)
                .value_parser(PathBufValueParser::new())
                .value_name("file"),
        )
        .arg(
            Arg::new("link")
                .long("link")
                .short('l')
                .help("Link to a library")
                .value_name("library")
                .action(ArgAction::Append),
        )
        .arg(
            Arg::new("link-path")
                .long("link-path")
                .short('L')
                .help("Specify a path the linker should look into")
                .action(ArgAction::Append)
                .value_hint(ValueHint::DirPath)
                .value_parser(PathBufValueParser::new())
                .value_name("path"),
        )
        .arg(
            Arg::new("no_run_functions")
                .action(ArgAction::SetTrue)
                .short('S')
                .long("no-sideeffects")
                .alias("no-run-fns")
                .help("Do not run functions marked with .param __run_function__"),
        )
        .arg(
            Arg::new("no_inline")
                .long("global-functions")
                .alias("no-inline")
                .short('F')
                .action(ArgAction::SetTrue)
                .help("Disable function inlining"),
        )
        .arg(
            Arg::new("constant")
                .long("define")
                .short('d')
                .help("Define a constant globally")
                .value_names(["constant", "value"])
                .num_args(2)
                .action(ArgAction::Append),
        )
        .arg(
            Arg::new("macro")
                .long("macro")
                .short('m')
                .help("Define a string globally")
                .value_names(["name", "value"])
                .num_args(2)
                .action(ArgAction::Append),
        )
        .arg(
            Arg::new("cmacro")
                .long("cmacro")
                .short('M')
                .help("Define a cstring globally")
                .value_names(["name", "value"])
                .num_args(2)
                .action(ArgAction::Append),
        )
        .arg(
            Arg::new("file")
                .value_hint(ValueHint::FilePath)
                .value_parser(PathBufValueParser::new())
                .help("The file to compile")
                .required(true),
        );

    let typecheck_cmd = Command::new("typecheck")
        .visible_alias("check")
        .about("Compile your program using provided options")
        .arg(
            Arg::new("target")
                .short('t')
                .long("target")
                .help("The target to compile to")
                .value_parser(["linux", "skyvm"])
                .required(true),
        )
        .arg(
            Arg::new("no_inline")
                .long("global-functions")
                .alias("no-inline")
                .short('F')
                .action(ArgAction::SetTrue)
                .help("Disable function inlining"),
        )
        .arg(
            Arg::new("include")
                .short('i')
                .long("include")
                .action(ArgAction::Append)
                .help("Include a .undefied file")
                .value_hint(ValueHint::FilePath)
                .value_parser(PathBufValueParser::new())
                .value_name("file"),
        )
        .arg(
            Arg::new("constant")
                .long("define")
                .short('d')
                .help("Define a constant globally")
                .value_names(["constant", "value"])
                .num_args(2)
                .action(ArgAction::Append),
        )
        .arg(
            Arg::new("macro")
                .long("macro")
                .short('m')
                .help("Define a string globally")
                .value_names(["name", "value"])
                .num_args(2)
                .action(ArgAction::Append),
        )
        .arg(
            Arg::new("cmacro")
                .long("cmacro")
                .short('M')
                .help("Define a cstring globally")
                .value_names(["name", "value"])
                .num_args(2)
                .action(ArgAction::Append),
        )
        .arg(
            Arg::new("file")
                .value_hint(ValueHint::FilePath)
                .value_parser(PathBufValueParser::new())
                .help("The file to compile")
                .required(true),
        );

    let config_cmd = Command::new("config")
        .about("Compile a project using a undefied config-file. Supported Types: JSON or TOML")
        .arg(
            Arg::new("file")
                .value_hint(ValueHint::FilePath)
                .value_parser(PathBufValueParser::new())
                .help("The config filepath")
                .value_name("file")
                .required(true),
        );

    let mut command = Command::new(crate_name!())
        .version(VERSION.to_string())
        .propagate_version(true)
        .bin_name(crate_name!())
        .disable_version_flag(true)
        .arg(
            Arg::new("version")
                .long("version")
                .short('v')
                .action(ArgAction::Version)
                .help("Print version"),
        )
        .author(crate_authors!())
        .subcommand(compile_cmd)
        .subcommand(typecheck_cmd)
        .subcommand(config_cmd);
    let matches = command.clone().get_matches();

    if let Some(matches) = matches.subcommand_matches("compile") {
        let mut config = Config::default(
            CompilationTarget::from_str(matches.get_one::<String>("target").unwrap())
                .expect("Failed to parse the compilation target"),
        );

        let path = matches
            .get_one::<PathBuf>("file")
            .expect("No file argument specified")
            .clone();

        config.run_after_compilation = matches.get_flag("run");
        config.keep_files = matches.get_flag("keep");
        config.no_typechecking = matches.get_flag("unsafe");
        config.create_shared = matches.get_flag("shared");
        config.no_inline = matches.get_flag("no_inline");
        config.no_run_fns = matches.get_flag("no_run_functions");

        let optimization_level = matches.get_one::<String>("optimizations").unwrap();
        config.optimizations = match optimization_level.as_str() {
            "0" => OPTIMIZATION_LEVELS[0],
            "1" => OPTIMIZATION_LEVELS[1],
            _ => err_generic(format!(
                "Could not parse optimization level {}",
                optimization_level
            )),
        };

        for path in matches.get_many::<PathBuf>("include").unwrap_or_default() {
            match path.extension().and_then(|f| f.to_str()) {
                Some("undefied") => config.undefied_files_to_include.push(path.clone()),
                Some("asm") => config.asm_files_to_include.push(path.clone()),
                _ => {
                    err_generic(format!("Could not parse file {}", path.display()));
                }
            }
        }

        for link in matches.get_many::<String>("link").unwrap_or_default() {
            config.library_links.push(link.clone());
        }

        for link_path in matches.get_many::<PathBuf>("link-path").unwrap_or_default() {
            config.library_link_paths.push(link_path.clone());
        }

        let mut last_name: Option<String> = None;
        for value in matches.get_many::<String>("constant").unwrap_or_default() {
            if let Some(str) = last_name.take() {
                let num = try_parse_num(value);
                if let Some(num) = num {
                    config.predefined.insert(str, PredefValue::Constant(num));
                } else {
                    err_generic(format!("Could not parse {} as a number", value));
                }
            } else {
                last_name = Some(value.clone());
            }
        }
        assert!(last_name.is_none());

        for value in matches.get_many::<String>("macro").unwrap_or_default() {
            if let Some(str) = last_name.take() {
                config
                    .predefined
                    .insert(str, PredefValue::String(value.clone()));
            } else {
                last_name = Some(value.clone());
            }
        }
        assert!(last_name.is_none());

        for value in matches.get_many::<String>("cmacro").unwrap_or_default() {
            if let Some(str) = last_name.take() {
                config
                    .predefined
                    .insert(str, PredefValue::CString(value.clone()));
            } else {
                last_name = Some(value.clone());
            }
        }
        assert!(last_name.is_none());

        compile(path, config);
    } else if let Some(matches) = matches.subcommand_matches("typecheck") {
        let mut config = Config::default(
            CompilationTarget::from_str(matches.get_one::<String>("target").unwrap())
                .expect("Failed to parse the compilation target"),
        );

        let path = matches
            .get_one::<PathBuf>("file")
            .expect("No file argument specified")
            .clone();

        config.no_inline = matches.get_flag("no_inline");

        for path in matches.get_many::<PathBuf>("include").unwrap_or_default() {
            match path.extension().and_then(|f| f.to_str()) {
                Some("undefied") => config.undefied_files_to_include.push(path.clone()),
                _ => {
                    err_generic(format!("Could not parse file {}", path.display()));
                }
            }
        }

        let mut last_name: Option<String> = None;
        for value in matches.get_many::<String>("constant").unwrap_or_default() {
            if let Some(str) = last_name.take() {
                let num = try_parse_num(value);
                if let Some(num) = num {
                    config.predefined.insert(str, PredefValue::Constant(num));
                } else {
                    err_generic(format!("Could not parse {} as a number", value));
                }
            } else {
                last_name = Some(value.clone());
            }
        }
        assert!(last_name.is_none());

        for value in matches.get_many::<String>("macro").unwrap_or_default() {
            if let Some(str) = last_name.take() {
                config
                    .predefined
                    .insert(str, PredefValue::String(value.clone()));
            } else {
                last_name = Some(value.clone());
            }
        }
        assert!(last_name.is_none());

        for value in matches.get_many::<String>("cmacro").unwrap_or_default() {
            if let Some(str) = last_name.take() {
                config
                    .predefined
                    .insert(str, PredefValue::CString(value.clone()));
            } else {
                last_name = Some(value.clone());
            }
        }
        assert!(last_name.is_none());

        let file_name = path
            .to_str()
            .expect("The file to typecheck is not valid")
            .to_string();

        insert_predefs(&file_name, &mut config);

        let mut tokens = tokenizer::parse(
            deal_with_error(
                format_args!("failed to read file {}: ", path.display()),
                read_to_string(&path),
            ),
            &file_name,
        );

        for file in &config.undefied_files_to_include {
            tokens.extend(tokenizer::parse(
                deal_with_error(
                    format_args!("failed to read file {}: ", &file.display()),
                    read_to_string(&file),
                ),
                &file
                    .to_str()
                    .expect("a file provided to the include argument was not a valid file")
                    .to_string(),
            ))
        }

        let mut program = parser::parse_tokens(tokens, &config);
        linker::link(&mut program);

        typecheck::typecheck(&mut program);

        info_generic("Finished typechecking with no errors");
        return;
    } else if let Some(matches) = matches.subcommand_matches("config") {
        todo!()
    } else {
        let _ = command.print_help();
        return;
    }

    // let mut config: Config = Config::default(CompilationTarget::Linux);
    // config.optimizations.dead_code_elimination = true;

    // let file = "a.undefied".to_string();

    // compile(file, config);
}

fn insert_predefs(file_name: &String, config: &mut Config) {
    config.predefined.insert(
        "__UNDEFIED_VERSION__".to_string(),
        PredefValue::String(VERSION.to_string()),
    );
    config.predefined.insert(
        "__UNDEFIED_VERSION_CSTR__".to_string(),
        PredefValue::CString(VERSION.to_string()),
    );
    config.predefined.insert(
        "__BASE_FILE_STR__".to_string(),
        PredefValue::String(file_name.clone()),
    );
    config.predefined.insert(
        "__BASE_FILE_CSTR__".to_string(),
        PredefValue::CString(file_name.clone()),
    );
    config.predefined.insert(
        "__TARGET_STR__".to_string(),
        PredefValue::String(config.target.to_string()),
    );
    config.predefined.insert(
        "__TARGET_CSTR__".to_string(),
        PredefValue::CString(config.target.to_string()),
    );

    let time = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("backwards time");

    config
        .predefined
        .insert("__DATE_DAY__".to_string(), PredefValue::Constant(0));
    config
        .predefined
        .insert("__DATE_WEEK_DAY__".to_string(), PredefValue::Constant(0));
    config
        .predefined
        .insert("__DATE_MONTH__".to_string(), PredefValue::Constant(0));
    config
        .predefined
        .insert("__DATE_YEAR__".to_string(), PredefValue::Constant(0));
    config
        .predefined
        .insert("__TIME_HOURS__".to_string(), PredefValue::Constant(0));
    config
        .predefined
        .insert("__TIME_MINUTES__".to_string(), PredefValue::Constant(0));
    config
        .predefined
        .insert("__TIME_SECONDS__".to_string(), PredefValue::Constant(0));
    config.predefined.insert(
        "__TIME_MILLISECONDS__".to_string(),
        PredefValue::Constant(time.as_millis() as u64),
    );
    config.predefined.insert(
        "__UNDEFIED_MAJOR__".to_string(),
        PredefValue::Constant(VERSION.major as u64),
    );
    config.predefined.insert(
        "__UNDEFIED_MINOR__".to_string(),
        PredefValue::Constant(VERSION.minor as u64),
    );
    config.predefined.insert(
        "__TYPECHECK__".to_string(),
        PredefValue::ConstantBool(!config.no_typechecking),
    );
    config.predefined.insert(
        "__TARGET__".to_string(),
        PredefValue::Constant(config.target as u64),
    );
    config.predefined.insert(
        "__TARGET_LINUX__".to_string(),
        PredefValue::Constant(CompilationTarget::Linux as u64),
    );
    config.predefined.insert(
        "__TARGET_SKYVM__".to_string(),
        PredefValue::Constant(CompilationTarget::Skyvm as u64),
    );

    config.predefined.insert(
        "__UNIXTIME__".to_string(),
        PredefValue::Constant(time.as_millis() as u64),
    );
    config
        .predefined
        .insert("__DATE_UTC_DAY__".to_string(), PredefValue::Constant(0));
    config.predefined.insert(
        "__DATE_UTC_WEEK_DAY__".to_string(),
        PredefValue::Constant(0),
    );
    config
        .predefined
        .insert("__DATE_UTC_MONTH__".to_string(), PredefValue::Constant(0));
    config
        .predefined
        .insert("__DATE_UTC_YEAR__".to_string(), PredefValue::Constant(0));
    config
        .predefined
        .insert("__TIME_UTC_HOURS__".to_string(), PredefValue::Constant(0));
    config
        .predefined
        .insert("__TIME_UTC_MINUTES__".to_string(), PredefValue::Constant(0));
    config
        .predefined
        .insert("__TIME_UTC_SECONDS__".to_string(), PredefValue::Constant(0));
    config.predefined.insert(
        "__TIME_UTC_MILLISECONDS__".to_string(),
        PredefValue::Constant(0),
    );
}

fn compile(file: PathBuf, mut config: Config) {
    let file_name = file
        .to_str()
        .expect("The file to compile is not valid")
        .to_string();

    insert_predefs(&file_name, &mut config);

    let mut tokens = tokenizer::parse(
        deal_with_error(
            format_args!("failed to read file {}: ", file.display()),
            read_to_string(&file),
        ),
        &file_name,
    );

    for file in &config.undefied_files_to_include {
        tokens.extend(tokenizer::parse(
            deal_with_error(
                format_args!("failed to read file {}: ", &file.display()),
                read_to_string(&file),
            ),
            &file
                .to_str()
                .expect("a file provided to the include argument was not a valid file")
                .to_string(),
        ))
    }

    let mut program = parser::parse_tokens(tokens, &config);
    linker::link(&mut program);

    if !config.no_typechecking {
        typecheck::typecheck(&mut program);
    }

    if config.optimizations.dead_code_elimination {
        optimization_dce::optimize(&mut program);
    }

    if let Err(e) = match config.target {
        CompilationTarget::Linux => compiler_linux::compile(program, &config, &file),
        CompilationTarget::Skyvm => compiler_skyvm::compile(program, &config, &file),
    } {
        err_generic(format!("Compilation failed: {}", e));
    }
}

pub fn deal_with_error<T, E: Display>(str: Arguments<'_>, val: Result<T, E>) -> T {
    match val {
        Ok(v) => v,
        Err(e) => err_generic(format!("{}{}", str, e)),
    }
}
