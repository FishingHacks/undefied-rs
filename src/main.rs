use clap::{
    builder::PathBufValueParser, crate_authors, crate_name, Arg, ArgAction, Command, ValueHint,
};
use error::err_generic;
use parser::AttributeList;
/**
 * ROADMAP:
 * - impl predef consts (time)
 * - call convention (!!!)
 */
use std::{
    collections::HashMap,
    fmt::{Arguments, Debug, Display},
    fs::read_to_string,
    path::PathBuf,
    time::{SystemTime, UNIX_EPOCH},
};
use toml::Table;

use crate::{error::info_generic, tokenizer::try_parse_num};
use global_strings::GlobalString;

mod compiler_linux;
mod optimization_dce;
// mod control_flow_dump;
mod global_strings;
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
}

impl Display for CompilationTarget {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Linux => f.write_str("Linux"),
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
        }
    }

    pub fn from_str(str: &str) -> Option<Self> {
        match str {
            "linux" => Some(Self::Linux),
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
        let path = matches.get_one::<PathBuf>("file").unwrap();

        let is_json = match path.extension().and_then(|f| f.to_str()) {
            Some("json") => true,
            Some("toml") => false,
            Some(ext) => err_generic(format!("file type {} is not supported", ext)),
            _ => err_generic("A file with that extension is not supported"),
        };

        let contents = deal_with_error(
            format_args!("Could not read file {}: ", path.display()),
            read_to_string(path),
        );

        if is_json {
            let table: serde_json::Map<String, serde_json::Value> = deal_with_error(
                format_args!("Could not parse json value: "),
                serde_json::from_str(&contents),
            );

            let mut config = Config::default(
                match table
                    .get("target")
                    .and_then(|f| f.as_str())
                    .and_then(|f| CompilationTarget::from_str(f))
                {
                    Some(v) => v,
                    None => err_generic("Could not find the target field"),
                },
            );

            let file = match table.get("file").and_then(|f| f.as_str()) {
                Some(v) => PathBuf::from(v),
                None => err_generic("Could not find the target file"),
            };

            macro_rules! get_bool {
                ($table: expr, $value: expr) => {
                    $table
                        .get($value)
                        .and_then(|f| f.as_bool())
                        .unwrap_or(false)
                };
            }

            config.run_after_compilation = get_bool!(table, "run");
            config.no_inline =
                get_bool!(table, "no-inline") || get_bool!(table, "global-functions");
            config.create_shared = get_bool!(table, "shared");
            config.no_run_fns =
                get_bool!(table, "no-sideeffects") || get_bool!(table, "no-run-functions");
            config.keep_files = get_bool!(table, "keep");
            config.no_typechecking = get_bool!(table, "unsafe");

            if let Some(value) = table.get("optimizations") {
                if let serde_json::Value::Number(v) = value {
                    if let Some(v) = v.as_u64() {
                        if v as usize >= OPTIMIZATION_LEVELS.len() {
                            err_generic(format!(
                                "Optimizations have to be 0..{}",
                                OPTIMIZATION_LEVELS.len()
                            ));
                        }
                        config.optimizations = OPTIMIZATION_LEVELS[v as usize];
                    }
                } else if let serde_json::Value::Object(v) = value {
                    config.optimizations.dead_code_elimination =
                        get_bool!(v, "dead_code_elimination");
                }
            }

            if let Some(value) = table.get("include") {
                if let serde_json::Value::Array(include_paths) = value {
                    for path in include_paths
                        .iter()
                        .map(|path| path.as_str().map(|str| PathBuf::from(str)))
                    {
                        let path = match path {
                            Some(v) => v,
                            _ => continue,
                        };
                        match path.extension().and_then(|str| str.to_str()) {
                            Some("asm") => config.asm_files_to_include.push(path),
                            Some("undefied") => config.undefied_files_to_include.push(path),
                            _ => err_generic("Please only include .undefied or .asm files"),
                        }
                    }
                }
            }

            if let Some(value) = table.get("libraries").and_then(|v| v.as_array()) {
                config.library_links = value
                    .iter()
                    .map(|f| f.as_str().unwrap_or(""))
                    .filter(|f| f.len() > 0)
                    .map(|f| f.to_string())
                    .collect();
            }

            if let Some(value) = table.get("library_paths").and_then(|v| v.as_array()) {
                config.library_link_paths = value
                    .iter()
                    .map(|f| f.as_str().unwrap_or(""))
                    .filter(|f| f.len() > 0)
                    .map(|f| PathBuf::from(f))
                    .collect();
            }

            if let Some(constants) = table.get("constants").and_then(|f| f.as_object()) {
                for (name, value) in constants {
                    if let serde_json::Value::Number(v) = value {
                        if let Some(v) = v.as_u64() {
                            config
                                .predefined
                                .insert(name.clone(), PredefValue::Constant(v));
                        }
                    }
                }
            }

            if let Some(constants) = table.get("macros").and_then(|f| f.as_object()) {
                for (name, value) in constants {
                    if let serde_json::Value::String(v) = value {
                        config
                            .predefined
                            .insert(name.clone(), PredefValue::String(v.clone()));
                    }
                }
            }

            if let Some(constants) = table.get("cmacros").and_then(|f| f.as_object()) {
                for (name, value) in constants {
                    if let serde_json::Value::String(v) = value {
                        config
                            .predefined
                            .insert(name.clone(), PredefValue::CString(v.clone()));
                    }
                }
            }

            compile(file, config);
        } else {
            macro_rules! get_bool {
                ($table: expr, $value: expr) => {
                    $table
                        .get($value)
                        .and_then(|f| f.as_bool())
                        .unwrap_or(false)
                };
            }

            let table = deal_with_error(
                format_args!("Could not parse toml: "),
                contents.parse::<Table>(),
            );
            let mut config = if let Some(str) = table["target"].as_str() {
                let target = CompilationTarget::from_str(str);
                Config::default(match target {
                    Some(v) => v,
                    None => err_generic(format!("Failed to parse target {str}")),
                })
            } else {
                err_generic("the type of the target field is not a string");
            };
            let file = match table["file"].as_str() {
                Some(v) => PathBuf::from(v),
                None => err_generic("The type of the file field is not a string"),
            };

            config.run_after_compilation = get_bool!(table, "run");
            config.no_inline =
                get_bool!(table, "no-inline") || get_bool!(table, "global-functions");
            config.create_shared = get_bool!(table, "shared");
            config.no_run_fns =
                get_bool!(table, "no-sideeffects") || get_bool!(table, "no-run-functions");
            config.keep_files = get_bool!(table, "keep");
            config.no_typechecking = get_bool!(table, "unsafe");

            if let Some(value) = table.get("optimizations") {
                if let toml::Value::Integer(v) = value {
                    if *v < 0 || *v as usize >= OPTIMIZATION_LEVELS.len() {
                        err_generic(format!(
                            "Optimizations have to be 0..{}",
                            OPTIMIZATION_LEVELS.len()
                        ));
                    }
                    config.optimizations = OPTIMIZATION_LEVELS[*v as usize];
                } else if let toml::Value::Table(v) = value {
                    config.optimizations.dead_code_elimination =
                        get_bool!(v, "dead_code_elimination");
                }
            }

            if let Some(value) = table.get("include") {
                if let toml::Value::Array(include_paths) = value {
                    for path in include_paths
                        .iter()
                        .map(|path| path.as_str().map(|str| PathBuf::from(str)))
                    {
                        let path = match path {
                            Some(v) => v,
                            _ => continue,
                        };
                        match path.extension().and_then(|str| str.to_str()) {
                            Some("asm") => config.asm_files_to_include.push(path),
                            Some("undefied") => config.undefied_files_to_include.push(path),
                            _ => err_generic("Please only include .undefied or .asm files"),
                        }
                    }
                }
            }

            if let Some(value) = table.get("libraries").and_then(|v| v.as_array()) {
                config.library_links = value
                    .iter()
                    .map(|f| f.as_str().unwrap_or(""))
                    .filter(|f| f.len() > 0)
                    .map(|f| f.to_string())
                    .collect();
            }

            if let Some(value) = table.get("library_paths").and_then(|v| v.as_array()) {
                config.library_link_paths = value
                    .iter()
                    .map(|f| f.as_str().unwrap_or(""))
                    .filter(|f| f.len() > 0)
                    .map(|f| PathBuf::from(f))
                    .collect();
            }

            if let Some(constants) = table.get("constants").and_then(|f| f.as_table()) {
                for (name, value) in constants {
                    if let toml::Value::Integer(v) = value {
                        config
                            .predefined
                            .insert(name.clone(), PredefValue::Constant(*v as u64));
                    }
                }
            }

            if let Some(constants) = table.get("macros").and_then(|f| f.as_table()) {
                for (name, value) in constants {
                    if let toml::Value::String(v) = value {
                        config
                            .predefined
                            .insert(name.clone(), PredefValue::String(v.clone()));
                    }
                }
            }

            if let Some(constants) = table.get("cmacros").and_then(|f| f.as_table()) {
                for (name, value) in constants {
                    if let toml::Value::String(v) = value {
                        config
                            .predefined
                            .insert(name.clone(), PredefValue::CString(v.clone()));
                    }
                }
            }

            compile(file, config);
        }
    } else {
        let _ = command.print_help();
        return;
    }
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CallingConvention {
    Raw,
    CStyle,
}

impl Display for CallingConvention {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Raw => f.write_str("Raw Calling Convention"),
            Self::CStyle => f.write_str("C-Style Calling Convention"),
        }
    }
}