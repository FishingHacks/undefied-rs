use std::{path::PathBuf, env::current_exe};

use crate::{parser::Constant, typecheck::Type};

static mut INT: usize = 0;

pub fn iota() -> usize {
    unsafe {
        let i = INT;
        INT += 1;
        i
    }
}

const CHARS: &[char] = &[
    '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f',
];

pub fn stringify_ptr(mut ptr: usize) -> String {
    let mut str = String::with_capacity(10);

    while ptr > 0 {
        str.push(CHARS[ptr & 0xf]);
        ptr >>= 4;
    }

    str.push_str("x0");

    str.chars().rev().collect::<String>()
}

pub fn stringify_const(constant: &Constant) -> String {
    match constant.typ {
        Type::Bool if constant.value == 0 => "false".to_string(),
        Type::Bool if constant.value != 0 => "true".to_string(),
        Type::Ptr => stringify_ptr(constant.value as usize),
        _ => constant.value.to_string(),
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Twos<T1, T2> {
    A(T1),
    B(T2),
}

impl<T1: Copy, T2: Copy> Copy for Twos<T1, T2> {}

impl<T1: ToString, T2: ToString> ToString for Twos<T1, T2> {
    fn to_string(&self) -> String {
        match self {
            Self::A(v) => v.to_string(),
            Self::B(v) => v.to_string(),
        }
    }
}

impl<T1, T2> Twos<T1, T2> {
    pub fn a(value: T1) -> Self {
        Self::A(value)
    }

    pub fn b(value: T2) -> Self {
        Self::B(value)
    }
}

pub fn get_stdpath() -> PathBuf {
    let mut path = PathBuf::from(current_exe().expect("Could not determine std library path"));
    path.pop();
    path.push("std");
    path
}
