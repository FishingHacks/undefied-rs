use std::process::{Command, exit};

use crate::{tokenizer::Loc, typecheck::Type};

macro_rules! __exit {
    () => {
        if std::env::var("RUST_BACKTRACE").is_ok() {
            panic!()
        } else {
            exit(1);
        }
    };
}

pub fn err<T: Into<String>>(loc: &Loc, str: T) -> ! {
    eprintln!("{} [ERR]: {}", loc, str.into());
    __exit!();
}

pub fn not_enough_types(loc: &Loc, types_expected: u8, types_actual: usize) -> ! {
    println!("{loc} [ERR]: Expected {types_expected} arguments, but found only {types_actual}\n");
    __exit!();
}

pub fn check_type_mismatch(loc: &Loc, type_expected: &Type, type_actual: &Type) {
    if !type_expected.equal_to(type_actual) {
        println!("{loc} [ERR]: Expected {type_expected}, but found {type_actual}\n");
        __exit!();
    }
}

pub fn check_type_mismatch_multiple(loc: &Loc, type_expected: &[Type], type_actual: &Type) {
    if type_expected.iter().find(|typ| typ.equal_to(type_actual)).is_none() {
        println!(
            "{loc} [ERR]: Expected {}, but found {type_actual}\n",
            type_expected
                .iter()
                .map(|t| format!("`{t}`"))
                .reduce(|acc, s| format!("{acc}, {s}"))
                .unwrap_or_default()
        );
        __exit!();
    }
}

pub fn warn<T: Into<String>>(loc: &Loc, str: T) {
    println!("{} [WARN]: {}", loc, str.into());
}

pub fn info<T: Into<String>>(loc: &Loc, str: T) {
    println!("{} [INFO]: {}", loc, str.into());
}

pub trait Log {
    fn log(&mut self) -> &mut Self;
    fn warn(&mut self) -> &mut Self;
    fn err(&mut self) -> !;
}

macro_rules! _impllog {
    ($typ: ty) => {
        impl Log for $typ {
            fn log(&mut self) -> &mut Self {
                println!("{self}");
                self
            }
            fn warn(&mut self) -> &mut Self {
                self.log();
                self
            }
            fn err(&mut self) -> &mut Self {
                self.log();
                __exit!();
            }
        }
    };
}

impl Log for Command {
    fn log(&mut self) -> &mut Self {
        run_cmd(self);
        self
    }
    fn warn(&mut self) -> &mut Self {
        self.log();
        self
    }
    fn err(&mut self) -> ! {
        self.log();
        __exit!();
    }
}

pub fn run_cmd(cmd: &Command) {
    println!(
        "[CMD]: Running {} {}",
        cmd.get_program().to_str().unwrap(),
        cmd.get_args()
            .map(|arg| arg.to_str().unwrap().to_string())
            .reduce(|a, b| format!("{a}, {b}"))
            .unwrap_or_default()
    );
}

pub fn err_generic<S: Into<String>>(str: S) -> ! {
    eprintln!("[ERR]: {}", str.into());
    __exit!();
}