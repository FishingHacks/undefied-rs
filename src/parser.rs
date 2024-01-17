use std::collections::HashMap;
use std::fmt::{Debug, Display};
use std::path::PathBuf;
use std::str::FromStr;
use std::vec;
use std::fs::read_to_string;

use crate::error::{info, warn};
use crate::tokenizer::{self, CharToken, IntegerToken, WordToken};
use crate::typecheck::{type_vec_to_str, Type};
use crate::utils::{iota, stringify_const, Twos};
use crate::Config;
use crate::{
    error::err,
    tokenizer::{Loc, StringToken, Token},
};

#[derive(Debug, Clone, Copy)]
pub enum Intrinsic {
    Print,
    Here,

    Plus,
    Minus,
    Mul,
    DivMod,

    LessThan,
    LessThanEqual,
    GreaterThan,
    GreaterThanEqual,
    Equal,
    NotEqual,

    Drop,
    Dup,
    Over,
    Swap,
    Rot,

    Syscall0,
    Syscall1,
    Syscall2,
    Syscall3,
    Syscall4,
    Syscall5,
    Syscall6,

    Shl,
    Shr,
    Or,
    And,
    Not,
    Xor,

    CastBool,
    CastInt,
    CastPtr,
    CastPtrToPlus,
    CastPtrToMinus,
    StackInfo,

    Load8,
    Load16,
    Load32,
    Load64,
    Store8,
    Store16,
    Store32,
    Store64,
}

impl FromStr for Intrinsic {
    fn from_str(str: &str) -> Result<Self, Self::Err> {
        match str {
            "print" => Ok(Self::Print),
            "here" => Ok(Self::Here),

            "+" => Ok(Self::Plus),
            "-" => Ok(Self::Minus),
            "*" => Ok(Self::Mul),
            "/%" => Ok(Self::DivMod),
            "divmod" => Ok(Self::DivMod),

            "<" => Ok(Self::LessThan),
            "<=" => Ok(Self::LessThanEqual),
            ">" => Ok(Self::GreaterThan),
            ">=" => Ok(Self::GreaterThanEqual),
            "=" => Ok(Self::Equal),
            "!=" => Ok(Self::NotEqual),

            "drop" => Ok(Self::Drop),
            "dup" => Ok(Self::Dup),
            "over" => Ok(Self::Over),
            "swap" => Ok(Self::Swap),
            "rot" => Ok(Self::Rot),

            "syscall0" => Ok(Self::Syscall0),
            "syscall1" => Ok(Self::Syscall1),
            "syscall2" => Ok(Self::Syscall2),
            "syscall3" => Ok(Self::Syscall3),
            "syscall4" => Ok(Self::Syscall4),
            "syscall5" => Ok(Self::Syscall5),
            "syscall6" => Ok(Self::Syscall6),

            "shl" => Ok(Self::Shl),
            "shr" => Ok(Self::Shr),
            "or" => Ok(Self::Or),
            "and" => Ok(Self::And),
            "not" => Ok(Self::Not),
            "xor" => Ok(Self::Xor),

            "cast(bool)" => Ok(Self::CastBool),
            "cast(int)" => Ok(Self::CastInt),
            "cast(ptr)" => Ok(Self::CastPtr),
            "cast+(ptr-to)" => Ok(Self::CastPtrToPlus),
            "cast-(ptr-to)" => Ok(Self::CastPtrToMinus),
            "???" => Ok(Self::StackInfo),

            "@8" => Ok(Self::Load8),
            "@16" => Ok(Self::Load16),
            "@32" => Ok(Self::Load32),
            "@64" => Ok(Self::Load64),
            "!8" => Ok(Self::Store8),
            "!16" => Ok(Self::Store16),
            "!32" => Ok(Self::Store32),
            "!64" => Ok(Self::Store64),

            _ => Err(()),
        }
    }

    type Err = ();
}

impl Intrinsic {
    pub fn to_str(&self) -> &str {
        match self {
            Self::Print => "print",
            Self::Here => "here",
            Self::Plus => "+",
            Self::Minus => "-",
            Self::Mul => "*",
            Self::DivMod => "/%",
            Self::LessThan => "<",
            Self::LessThanEqual => "<=",
            Self::GreaterThan => ">",
            Self::GreaterThanEqual => ">=",
            Self::Equal => "=",
            Self::NotEqual => "!=",
            Self::Drop => "drop",
            Self::Dup => "dup",
            Self::Over => "over",
            Self::Swap => "swap",
            Self::Rot => "rot",
            Self::Syscall0 => "syscall0",
            Self::Syscall1 => "syscall1",
            Self::Syscall2 => "syscall2",
            Self::Syscall3 => "syscall3",
            Self::Syscall4 => "syscall4",
            Self::Syscall5 => "syscall5",
            Self::Syscall6 => "syscall6",
            Self::Shl => "shl",
            Self::Shr => "shr",
            Self::Or => "or",
            Self::And => "and",
            Self::Not => "not",
            Self::Xor => "xor",
            Self::CastBool => "cast(bool)",
            Self::CastInt => "cast(int)",
            Self::CastPtr => "cast(ptr)",
            Self::CastPtrToPlus => "cast+(ptr-to)",
            Self::CastPtrToMinus => "cast-(ptr-to)",
            Self::StackInfo => "???",
            Self::Load8 => "@8",
            Self::Load16 => "@16",
            Self::Load32 => "@32",
            Self::Load64 => "@64",
            Self::Store8 => "!8",
            Self::Store16 => "!16",
            Self::Store32 => "!32",
            Self::Store64 => "!64",
        }
    }
}
#[derive(Debug, Clone)]
pub struct OpIntrinsic {
    pub loc: Loc,
    pub op: Intrinsic,
}
#[derive(Debug, Clone)]
pub struct PushAssembly {
    pub loc: Loc,
    pub asm: String,
    pub expected_types: Vec<Type>,
    pub return_types: Vec<Type>,
}
#[derive(Debug, Clone)]
pub struct Typefence {
    pub loc: Loc,
    pub expected_types: Vec<Type>,
}
#[derive(Debug, Clone)]
pub struct Ret {
    pub loc: Loc,
    pub is_end: bool,
    pub dealloc_len: usize,
}
#[derive(Debug, Clone)]
pub struct PushStr {
    pub loc: Loc,
    pub value: u64,
    pub len: u64,
    pub is_cstr: bool,
}
#[derive(Debug, Clone)]
pub struct PushInt {
    pub loc: Loc,
    pub value: u64,
}
#[derive(Debug, Clone)]
pub struct PushMem {
    pub loc: Loc,
    pub id: u64,
}
#[derive(Debug, Clone)]
pub struct PushLocalMem {
    pub loc: Loc,
    pub off: usize,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Keyword {
    If,
    IfStar,
    Else,
    While,
    Do,
    End,
}

impl Display for Keyword {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.to_str())
    }
}

impl FromStr for Keyword {
    fn from_str(str: &str) -> Result<Self, Self::Err> {
        match str {
            "if" => Ok(Self::If),
            "if*" => Ok(Self::IfStar),
            "else" => Ok(Self::Else),
            "while" => Ok(Self::While),
            "do" => Ok(Self::Do),
            "end" => Ok(Self::End),
            _ => Err(()),
        }
    }
    type Err = ();
}

impl Keyword {
    pub fn to_str(&self) -> &str {
        match self {
            Self::If => "if",
            Self::IfStar => "if*",
            Self::Else => "else",
            Self::While => "while",
            Self::Do => "do",
            Self::End => "end",
        }
    }
}

#[derive(Debug, Clone)]
pub struct OpKeyword {
    pub loc: Loc,
    pub reference: Option<u64>,
    pub keyword: Keyword,
}

#[derive(Debug, Clone)]
pub struct Proc {
    pub id: usize,
    pub skip_to: Option<u64>,
    pub loc: Loc,
    pub local_mem_size: usize,
    pub name: String,
    pub used: bool,
}

#[derive(Debug, Clone)]
pub struct CallProc {
    pub id: usize,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct PushConst {
    pub constant: Constant,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub enum Operation {
    Intrinsic(OpIntrinsic),
    Ret(Ret),
    PushStr(PushStr),
    PushInt(PushInt),
    Keyword(OpKeyword),
    PushMem(PushMem),
    Proc(Proc),
    CallProc(CallProc),
    PushLocalMem(PushLocalMem),
    PushConst(PushConst),
    Assembly(PushAssembly),
    Typefence(Typefence),
    None(Loc),
}

impl Display for Operation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            Self::Intrinsic(..) => "Intrinsic",
            Self::Ret(..) => "Ret",
            Self::PushStr(..) => "PushStr",
            Self::PushInt(..) => "PushInt",
            Self::Keyword(..) => "Keyword",
            Self::PushMem(..) => "PushMem",
            Self::Proc(..) => "Proc",
            Self::CallProc(..) => "CallProc",
            Self::PushLocalMem(..) => "PushLocalMem",
            Self::PushConst(..) => "PushConst",
            Self::Assembly(..) => "Assembly",
            Self::Typefence(..) => "Typefence",
            Self::None(..) => "None",
        })
    }
}

impl Operation {
    pub fn intrinsic(loc: Loc, intrinsic: Intrinsic) -> Self {
        Self::Intrinsic(OpIntrinsic { loc, op: intrinsic })
    }
    pub fn push_str(loc: Loc, value: u64, len: u64, is_cstr: bool) -> Self {
        Self::PushStr(PushStr {
            loc,
            value,
            len,
            is_cstr,
        })
    }
    pub fn ret(loc: Loc, is_end: bool) -> Self {
        Self::Ret(Ret {
            loc,
            is_end,
            dealloc_len: 0,
        })
    }
    pub fn push_int(loc: Loc, value: u64) -> Self {
        Self::PushInt(PushInt { loc, value })
    }
    pub fn push_mem(loc: Loc, id: u64) -> Self {
        Self::PushMem(PushMem { loc, id })
    }
    pub fn push_local_mem(loc: Loc, off: usize) -> Self {
        Self::PushLocalMem(PushLocalMem { loc, off })
    }
    pub fn call_proc(loc: Loc, id: usize) -> Self {
        Self::CallProc(CallProc { loc, id })
    }
    pub fn push_const(loc: Loc, constant: Constant) -> Self {
        Self::PushConst(PushConst { loc, constant })
    }
    pub fn push_asm(
        loc: Loc,
        asm: String,
        expected_types: Vec<Type>,
        return_types: Vec<Type>,
    ) -> Self {
        Self::Assembly(PushAssembly {
            loc,
            asm,
            expected_types,
            return_types,
        })
    }
    pub fn push_typefence(loc: Loc, expected_types: Vec<Type>) -> Self {
        Self::Typefence(Typefence {
            loc,
            expected_types,
        })
    }

    pub fn proc(loc: Loc, id: usize, name: String, used: bool) -> Self {
        Self::Proc(Proc {
            loc,
            id,
            skip_to: None,
            local_mem_size: 0,
            name,
            used,
        })
    }
    pub fn keyword(loc: Loc, keyword: Keyword) -> Self {
        Self::Keyword(OpKeyword {
            loc,
            reference: None,
            keyword,
        })
    }

    pub fn get_location(&self) -> &Loc {
        match self {
            Self::Intrinsic(OpIntrinsic { loc, .. })
            | Self::Keyword(OpKeyword { loc, .. })
            | Self::PushInt(PushInt { loc, .. })
            | Self::PushStr(PushStr { loc, .. })
            | Self::PushMem(PushMem { loc, .. })
            | Self::PushLocalMem(PushLocalMem { loc, .. })
            | Self::Ret(Ret { loc, .. })
            | Self::CallProc(CallProc { loc, .. })
            | Self::PushConst(PushConst { loc, .. })
            | Self::Proc(Proc { loc, .. })
            | Self::Assembly(PushAssembly { loc, .. })
            | Self::Typefence(Typefence { loc, .. })
            | Self::None(loc) => loc,
        }
    }

    pub fn as_keyword(&self) -> Option<&OpKeyword> {
        match self {
            Self::Keyword(k) => Some(k),
            _ => None,
        }
    }

    pub fn as_proc_mut(&mut self) -> Option<&mut Proc> {
        match self {
            Self::Proc(k) => Some(k),
            _ => None,
        }
    }

    pub fn as_proc(&self) -> Option<&Proc> {
        match self {
            Self::Proc(k) => Some(k),
            _ => None,
        }
    }

    pub fn as_ret(&self) -> Option<&Ret> {
        match self {
            Self::Ret(k) => Some(k),
            _ => None,
        }
    }

    pub fn as_ret_mut(&mut self) -> Option<&mut Ret> {
        match self {
            Self::Ret(k) => Some(k),
            _ => None,
        }
    }

    pub fn as_keyword_mut(&mut self) -> Option<&mut OpKeyword> {
        match self {
            Self::Keyword(k) => Some(k),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, Default)]
pub struct ProcedureContract {
    pub in_types: Vec<Type>,
    pub out_types: Vec<Type>,
    pub id: usize,
    pub loc: Loc,
    pub attributes: AttributeList,
}

#[derive(Clone)]
pub struct Program {
    pub main_fn: Option<usize>,
    pub ops: Vec<Operation>,
    pub strings: Vec<String>,
    loc_eof: Loc,
    pub refs: HashMap<usize, usize>,
    pub mems: HashMap<u64, usize>,
    pub contracts: Vec<ProcedureContract>,
    pub reversed_refs: HashMap<usize, usize>,
}

impl Default for Program {
    fn default() -> Self {
        Self {
            loc_eof: Loc::new(&"<unknown>".to_string(), 0, 0),
            mems: HashMap::default(),
            ops: vec![],
            refs: HashMap::default(),
            reversed_refs: HashMap::default(),
            strings: vec![],
            main_fn: Option::default(),
            contracts: vec![],
        }
    }
}

impl Program {
    pub fn get_last_loc(&self) -> &Loc {
        &self.loc_eof
    }

    #[allow(dead_code)]
    pub fn adjust_refs(&mut self, ip_start: usize, len: usize) {
        for reference in self.refs.values_mut() {
            if *reference >= ip_start {
                *reference += len;
            }
        }
    }

    pub fn reverse_refs(&mut self) {
        self.reversed_refs.clear();
        if self.refs.len() > self.reversed_refs.capacity() {
            self.reversed_refs.reserve(self.refs.len() - self.reversed_refs.capacity());
        }

        for (k, v) in self.refs.iter() {
            self.reversed_refs.insert(*v, *k);
        }
    }
}

#[macro_export]
macro_rules! refc {
    ($tokens: expr) => {
        RefCell::new($tokens)
    };
}

#[derive(Debug, Clone)]
pub struct Constant {
    pub value: u64,
    pub loc: Loc,
    pub typ: Type,
}

impl Constant {
    pub fn new(value: u64, loc: Loc, typ: Type) -> Self {
        Self { value, loc, typ }
    }
}

#[derive(Clone, Default)]
pub struct AttributeList {
    inner: HashMap<String, Option<String>>,
}

impl AttributeList {
    pub fn clear(&mut self) {
        self.inner.clear();
    }

    pub fn add_attribute(&mut self, name: String) {
        self.inner.insert(name, None);
    }

    pub fn add_attribute_value(&mut self, name: String, value: String) {
        self.inner.insert(name, Some(value));
    }

    pub fn has_attribute(&self, name: &str) -> bool {
        self.inner.contains_key(name)
    }

    pub fn has_attribute_value(&self, name: &str, value: &str) -> bool {
        if let Some(v) = self.inner.get(name) {
            match v {
                None => false,
                Some(str) => str == value,
            }
        } else {
            return false;
        }
    }

    pub fn get_value(&self, name: &str) -> Option<&String> {
        if let Some(v) = self.inner.get(name) {
            v.as_ref()
        } else {
            None
        }
    }
}

impl Debug for AttributeList {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut str = String::with_capacity(self.inner.capacity() + 4);
        str.push('[');
        str.push('\n');
        str.push_str(
            &self
                .inner
                .iter()
                .map(|(k, v)| {
                    if let Some(v) = v {
                        format!("Attribute<{k}: \"{v}\">")
                    } else {
                        format!("Attribute<{k}>")
                    }
                })
                .reduce(|a, b| format!("{a},\n{b}"))
                .unwrap_or_default(),
        );
        str.push('\n');
        str.push(']');

        f.write_str(&str)
    }
}

pub const KEYWORDS_NESTING: &[Keyword] = &[Keyword::If, Keyword::While];
pub const PREPROCESSOR_NAMES: &[&str] = &[
    ".param",
    ".defparam",
    ".ifn",
    ".if",
    ".else",
    ".end",
    ".defparam",
    ".param",
    "undef",
    ".str",
    ".cstr",
    ".ifndef",
    ".ifdef",
    ".ifc",
    ".nifc",
    ".is",
    ".isn",
    ".pragma",
];
pub const PREPROCESSOR_CONDITIONAL_BRANCH: &[&str] = &[".end", ".else"];
pub const SPECIAL_KEYWORD_NAMES: &[&str] = &[
    "include",
    "fn",
    "inline",
    "const",
    "memory",
    "assembly",
    "typefence",
];

struct DotIsStruct {
    is_macro: bool,
    is_fn: bool,
    is_const: bool,
    is_memory: bool,
    is_local_memory: bool,
    is_intrinsic: bool,
    is_keyword: bool,
}

impl DotIsStruct {
    fn is(&self, str: &str, loc: &Loc) -> bool {
        match str {
            "macro" => self.is_macro,
            "fn" => self.is_fn,
            "const" => self.is_const,
            "memory" => self.is_memory,
            "local_memory" => self.is_local_memory,
            "intrinsic" => self.is_intrinsic,
            "keyword" => self.is_keyword,
            _ => err(loc, format!("No type {str} found!")),
        }
    }
}

pub fn parse_tokens(mut tokens: Vec<Token>, config: &Config) -> Program {
    let mut program = Program::default();

    let mut mems: HashMap<String, u64> = HashMap::new();
    let mut mems_local: HashMap<String, usize> = HashMap::new();
    let mut procs: HashMap<String, usize> = HashMap::new();
    let mut inline_procs: HashMap<String, Vec<Token>> = HashMap::new();
    let mut consts: HashMap<String, Constant> = HashMap::new();
    let mut offset: u64 = 0;
    let mut pragma_multiple: Vec<String> = vec![];
    let mut included_files: Vec<String> = vec![];

    let mut ip: usize = 0;
    let mut nesting: u64 = 0;
    let mut current_fn: Option<usize> = None;
    let mut current_attrs = AttributeList::default();
    let mut inline_next = false;
    let mut tokens_to_insert: Vec<Token> = vec![];
    let mut preprocessor_results: Vec<bool> = vec![];

    macro_rules! expect_str_tok {
        ($loc: expr) => {{
            ip += 1;
            if let Some(k) = tokens.get(ip).and_then(|tok| tok.as_str_token()) {
                k
            } else {
                err($loc, "Expected a string token");
            }
        }};
    }

    macro_rules! expect_tok {
        ($loc: expr) => {{
            ip += 1;
            if let Some(k) = tokens.get(ip) {
                k
            } else {
                err($loc, "Expected a token");
            }
        }};
    }

    macro_rules! expect_word_tok {
        ($loc: expr) => {{
            ip += 1;
            if let Some(k) = tokens.get(ip).and_then(|tok| tok.as_word_token()) {
                k
            } else {
                err($loc, "Expected a word token");
            }
        }};
    }

    macro_rules! is_name_defined {
        ($v: expr) => {{
            mems.contains_key($v)
                || (mems_local.contains_key($v) && current_fn.is_some())
                || procs.contains_key($v)
                || consts.contains_key($v)
                || inline_procs.contains_key($v)
                || $v == "proc"
                || $v == "include"
                || $v == "memory"
                || PREPROCESSOR_NAMES.contains(&$v.as_str())
                || Keyword::from_str($v).is_ok()
                || Intrinsic::from_str($v).is_ok()
                || SPECIAL_KEYWORD_NAMES.contains(&$v.as_str())
        }};
    }

    macro_rules! ensure_in_fn {
        ($loc: expr) => {
            if current_fn.is_none() {
                err($loc, "Code is not allowed outside of functions");
            }
        };
    }

    if !tokens.is_empty() {
        let mut loc = tokens[tokens.len() - 1].get_location().clone();
        loc.line += 1;
        program.loc_eof = loc;
    }

    while ip < tokens.len() {
        let tok = &tokens[ip];

        if tok.as_word_token().is_none() {
            current_attrs.clear();
            inline_next = false;
        }
        if !preprocessor_results.is_empty()
            && !preprocessor_results[preprocessor_results.len() - 1]
            && match tok {
                Token::WordToken(WordToken { value, .. }) => {
                    !PREPROCESSOR_CONDITIONAL_BRANCH.contains(&value.as_str())
                }
                _ => true,
            }
        {
            ip += 1;
            continue;
        }
        match tok {
            Token::StringToken(t) => {
                ensure_in_fn!(&t.loc);
                let i = program
                    .strings
                    .iter()
                    .position(|f| f.eq(&t.value))
                    .unwrap_or_else(|| {
                        let idx = program.strings.len();
                        program.strings.push(t.value.clone());
                        idx
                    }) as u64;
                program.ops.push(Operation::push_str(
                    t.loc.clone(),
                    i,
                    t.value.len() as u64,
                    t.is_cstr,
                ));
            }
            Token::CharToken(c) => {
                ensure_in_fn!(&c.loc);
                program
                    .ops
                    .push(Operation::push_int(c.loc.clone(), c.char as u64));
            }
            Token::IntegerToken(i) => {
                ensure_in_fn!(&i.loc);
                program
                    .ops
                    .push(Operation::push_int(i.loc.clone(), i.value));
            }
            Token::WordToken(w) => {
                if w.value != "fn"
                    && w.value != ".param"
                    && w.value != ".defparam"
                    && w.value != "assembly"
                {
                    current_attrs.clear();
                }
                if w.value != "fn" && inline_next {
                    err(&w.loc, "inline has to be follow by a function definition");
                }
                if let Ok(op) = Intrinsic::from_str(&w.value) {
                    program.ops.push(Operation::intrinsic(w.loc.clone(), op));
                } else if let Ok(keyword) = Keyword::from_str(&w.value) {
                    ensure_in_fn!(&w.loc);
                    if keyword == Keyword::End {
                        if nesting > 0 {
                            nesting -= 1;
                            program
                                .ops
                                .push(Operation::keyword(w.loc.clone(), Keyword::End));
                        } else {
                            current_fn = None;
                            mems_local.clear();
                            program.ops.push(Operation::ret(w.loc.clone(), true));
                        }
                    } else {
                        if KEYWORDS_NESTING.contains(&keyword) {
                            nesting += 1;
                        }
                        program.ops.push(Operation::keyword(w.loc.clone(), keyword));
                    }
                } else if w.value == "include" {
                    let str_tok: &StringToken = expect_str_tok!(&w.loc);
                    let mut path_str = str_tok.value.clone();
                    path_str += ".undefied";
                    let mut path = PathBuf::new();
                    path.push(&w.loc.file);
                    path.pop();
                    path.push(path_str);
                    let path = path
                        .to_str()
                        .expect("Could not form path string")
                        .to_string();
                    if path == w.loc.file {
                        err(&w.loc, "Cannot include file from itself");
                    } else if !included_files.contains(&path) || pragma_multiple.contains(&path) {
                        info(&w.loc, format!("Including {:?}", &path));
                        included_files.push(path.clone());
                        match read_to_string(&path) {
                            Ok(str) => {
                                let new_tokens = tokenizer::parse(str, &path);
                                for t in new_tokens.into_iter().rev() {
                                    tokens.insert(ip + 1, t);
                                }
                            }
                            Err(err_val) => {
                                err(
                                    &w.loc,
                                    format!("Failed to include {}:\n{}", str_tok.value, err_val),
                                );
                            }
                        }
                    }
                } else if w.value == "const" {
                    let name = (expect_word_tok!(&w.loc)).value.clone();
                    if is_name_defined!(&name) {
                        err(&w.loc, format!("Name {} is already defined!", name));
                    }
                    let mut stack: Vec<u64> = vec![];
                    let mut typ: Type = Type::Int;
                    ip += 1;
                    let mut should_end = false;
                    let mut is_offset: bool = false;
                    let mut is_offset_reset: bool = false;
                    while ip < tokens.len() {
                        let op = &tokens[ip];
                        match op {
                            Token::CharToken(..)
                            | Token::CommentToken(..)
                            | Token::StringToken(..) => err(
                                op.get_location(),
                                "This token is not supported in constants",
                            ),
                            Token::IntegerToken(IntegerToken { value, .. }) => stack.push(*value),
                            Token::WordToken(WordToken { value, loc }) => match value.as_str() {
                                "+" => {
                                    if stack.len() < 2 {
                                        err(loc, "Expected 2 values on the stack")
                                    }
                                    let a = stack.pop().unwrap();
                                    let b = stack.pop().unwrap();
                                    stack.push(b + a);
                                }
                                "*" => {
                                    if stack.len() < 2 {
                                        err(loc, "Expected 2 values on the stack")
                                    }
                                    let a = stack.pop().unwrap();
                                    let b = stack.pop().unwrap();
                                    stack.push(b * a);
                                }
                                "/" => {
                                    if stack.len() < 2 {
                                        err(loc, "Expected 2 values on the stack")
                                    }
                                    let a = stack.pop().unwrap();
                                    let b = stack.pop().unwrap();
                                    stack.push(b / a);
                                }
                                "-" => {
                                    if stack.len() < 2 {
                                        err(loc, "Expected 2 values on the stack")
                                    }
                                    let a = stack.pop().unwrap();
                                    let b = stack.pop().unwrap();
                                    stack.push(b - a);
                                }
                                "cast(ptr)" => {
                                    if Type::Ptr.equal_to(&typ) {
                                        info(loc, "Type is already Pointer");
                                    }
                                    typ = Type::Ptr;
                                }
                                "cast(bool)" => {
                                    if Type::Bool.equal_to(&typ) {
                                        info(loc, "Type is already Bool");
                                    }
                                    typ = Type::Bool;
                                }
                                "cast(int)" => {
                                    if Type::Int.equal_to(&typ) {
                                        info(loc, "Type is already Int");
                                    }
                                    typ = Type::Int;
                                }
                                "offset" => is_offset = true,
                                "reset" => is_offset_reset = true,
                                "end" => should_end = true,
                                _ => {
                                    if let Some(Constant { value, .. }) = consts.get(value) {
                                        stack.push(*value);
                                    } else {
                                        err(
                                            loc,
                                            format!("word {value} is not supported in constants"),
                                        )
                                    }
                                }
                            },
                        }
                        if should_end {
                            break;
                        }
                        ip += 1;
                    }
                    if ip >= tokens.len()
                        || tokens[ip]
                            .as_word_token()
                            .and_then(|f| if f.value == "end" { Some(true) } else { None })
                            .is_none()
                    {
                        if ip >= tokens.len() {
                            err(&program.get_last_loc(), "Expected `end` but found nothing");
                        } else {
                            err(
                                tokens[ip].get_location(),
                                "Expected `end` but found something else",
                            );
                        }
                    }
                    if stack.len() != 1 && !is_offset_reset {
                        err(
                            tokens[ip].get_location(),
                            format!(
                                "Expected exactly 1 element on the stack, but found {} elements",
                                stack.len()
                            ),
                        );
                    }
                    let val = stack.pop().unwrap_or(0);
                    let mut constant = if is_offset || is_offset_reset {
                        offset
                    } else {
                        val
                    };
                    if is_offset {
                        offset += val;
                    }
                    if is_offset_reset {
                        offset = 0;
                    }
                    if typ.equal_to(&Type::Bool) {
                        constant = if constant > 0 { 1 } else { 0 };
                    }
                    consts.insert(name, Constant::new(constant, w.loc.clone(), typ));
                } else if w.value == "memory" {
                    let name = (expect_word_tok!(&w.loc)).value.clone();
                    if is_name_defined!(&name) && current_fn.is_none() {
                        err(&w.loc, format!("Name {} is already defined!", name));
                    }
                    let mut stack: Vec<u64> = vec![];
                    ip += 1;
                    let mut should_end = false;
                    while ip < tokens.len() {
                        let op = &tokens[ip];
                        match op {
                            Token::CharToken(..)
                            | Token::CommentToken(..)
                            | Token::StringToken(..) => {
                                err(op.get_location(), "This token is not supported in memories")
                            }
                            Token::IntegerToken(IntegerToken { value, .. }) => stack.push(*value),
                            Token::WordToken(WordToken { value, loc }) => match value.as_str() {
                                "+" => {
                                    if stack.len() < 2 {
                                        err(loc, "Expected 2 values on the stack")
                                    }
                                    let a = stack.pop().unwrap();
                                    let b = stack.pop().unwrap();
                                    stack.push(b + a);
                                }
                                "*" => {
                                    if stack.len() < 2 {
                                        err(loc, "Expected 2 values on the stack")
                                    }
                                    let a = stack.pop().unwrap();
                                    let b = stack.pop().unwrap();
                                    stack.push(b * a);
                                }
                                "/" => {
                                    if stack.len() < 2 {
                                        err(loc, "Expected 2 values on the stack")
                                    }
                                    let a = stack.pop().unwrap();
                                    let b = stack.pop().unwrap();
                                    stack.push(b / a);
                                }
                                "-" => {
                                    if stack.len() < 2 {
                                        err(loc, "Expected 2 values on the stack")
                                    }
                                    let a = stack.pop().unwrap();
                                    let b = stack.pop().unwrap();
                                    stack.push(b - a);
                                }
                                "end" => should_end = true,

                                _ => {
                                    if let Some(Constant { value, .. }) = consts.get(value) {
                                        stack.push(*value);
                                    } else {
                                        err(
                                            loc,
                                            format!("word {value} is not supported in memories"),
                                        )
                                    }
                                }
                            },
                        }
                        if should_end {
                            break;
                        }
                        ip += 1;
                    }
                    if ip >= tokens.len()
                        || tokens[ip]
                            .as_word_token()
                            .and_then(|f| if f.value == "end" { Some(true) } else { None })
                            .is_none()
                    {
                        if ip >= tokens.len() {
                            err(&program.get_last_loc(), "Expected `end` but found nothing");
                        } else {
                            err(
                                tokens[ip].get_location(),
                                "Expected `end` but found something else",
                            );
                        }
                    }
                    if stack.len() != 1 {
                        err(
                            tokens[ip].get_location(),
                            format!(
                                "Expected exactly 1 element on the stack, but found {} elements",
                                stack.len()
                            ),
                        );
                    }
                    let len = stack.pop().unwrap() as usize;

                    if let Some(ip) = current_fn {
                        let op = program.ops[ip]
                            .as_proc_mut()
                            .expect("Expected current_fn to point to a proc");
                        if is_name_defined!(&name) {
                            warn(
                                &w.loc,
                                "Shadowing a global variable; This might be intentional",
                            );
                        }
                        mems_local.insert(name, op.local_mem_size);
                        op.local_mem_size += len;
                    } else {
                        let id = iota() as u64;
                        program.mems.insert(id, len);
                        mems.insert(name, id);
                    }
                } else if w.value == "fn" && (!inline_next || config.no_inline) {
                    inline_next = false;
                    if current_fn.is_some() {
                        err(&w.loc, "function definition in function definition")
                    }
                    mems_local.clear();
                    let name_tok = expect_word_tok!(&w.loc);
                    if is_name_defined!(&name_tok.value) {
                        err(
                            &name_tok.loc,
                            format!("Name {} is already defined!", &name_tok.value),
                        );
                    }

                    let name = &name_tok.value;
                    if program.main_fn.is_some() && name == "main" {
                        err(
                            &name_tok.loc,
                            "there's already a main function present in your codebase",
                        );
                    }

                    let mut contract = ProcedureContract::default();
                    contract.loc = w.loc.clone();
                    let id = iota();
                    contract.id = id;

                    let mut out_types = false;
                    let mut type_tokens: Vec<WordToken> = vec![];

                    ip += 1;
                    while ip < tokens.len() {
                        let tok = &tokens[ip];

                        if let Token::WordToken(tok) = tok {
                            if tok.value == "--" && !out_types {
                                contract.in_types = Type::from_tokens(&type_tokens);
                                type_tokens.clear();
                                out_types = true;
                            } else if tok.value == "in" {
                                if out_types {
                                    contract.out_types = Type::from_tokens(&type_tokens);
                                } else {
                                    contract.in_types = Type::from_tokens(&type_tokens);
                                }
                                type_tokens.clear();
                                break;
                            } else {
                                type_tokens.push(tok.clone());
                            }
                        } else if out_types {
                            err(
                                tok.get_location(),
                                "Expected a type or `in`, but found something else",
                            );
                        } else {
                            err(
                                tok.get_location(),
                                "Expected a type, `--` or `in`, but found something else",
                            );
                        }

                        ip += 1;
                    }

                    if ip >= tokens.len() {
                        err(program.get_last_loc(), "open ended function declaration");
                    }

                    contract.attributes = current_attrs;
                    current_attrs = AttributeList::default();

                    if name == "main" && !contract.attributes.has_attribute("__nomain__") {
                        // validate main function contract
                        if contract.in_types.len() > 0 {
                            err(&contract.loc, "Expected 0 inputs for the main function");
                        }
                        if contract.out_types.len() > 1 {
                            err(
                                &contract.loc,
                                "Expected the return type of the main function to be int or nothing",
                            );
                        }
                        if contract
                            .out_types
                            .get(0)
                            .and_then(|t| Some(!Type::Int.equal_to(t)))
                            .unwrap_or(false)
                        {
                            err(
                                &contract.loc,
                                "Expected the return type of the main function to be int or nothing",
                            );
                        }
                        program.main_fn = Some(id);
                    }

                    current_fn = Some(program.ops.len());
                    program.refs.insert(id, program.ops.len());
                    program.ops.push(Operation::proc(
                        w.loc.clone(),
                        id,
                        name.clone(),
                        (name == "main" && !contract.attributes.has_attribute("__nomain__"))
                            || contract.attributes.has_attribute("__export__"),
                    ));
                    procs.insert(name.clone(), id);
                    program.contracts.insert(id, contract);
                } else if w.value == "fn" && inline_next && !config.no_inline {
                    inline_next = false;
                    let name_tok = expect_word_tok!(&w.loc);
                    if is_name_defined!(&name_tok.value) {
                        err(
                            &name_tok.loc,
                            format!("Name {} is already defined!", &name_tok.value),
                        );
                    }

                    if current_fn.is_some() {
                        err(
                            &w.loc,
                            "function definition inside of a function is not allowed",
                        );
                    }

                    let mut def_out_types = false;
                    let mut type_tokens: Vec<WordToken> = vec![];
                    let mut in_types: Vec<Type> = vec![];
                    let mut out_types: Vec<Type> = vec![];

                    ip += 1;
                    while ip < tokens.len() {
                        let tok = &tokens[ip];

                        if let Token::WordToken(tok) = tok {
                            if tok.value == "--" && !def_out_types {
                                in_types = Type::from_tokens(&type_tokens);
                                type_tokens.clear();
                                def_out_types = true;
                            } else if tok.value == "in" {
                                if def_out_types {
                                    out_types = Type::from_tokens(&type_tokens);
                                } else {
                                    in_types = Type::from_tokens(&type_tokens);
                                }
                                type_tokens.clear();
                                break;
                            } else {
                                type_tokens.push(tok.clone());
                            }
                        } else if def_out_types {
                            err(
                                tok.get_location(),
                                "Expected a type or `in`, but found something else",
                            );
                        } else {
                            err(
                                tok.get_location(),
                                "Expected a type, `--` or `in`, but found something else",
                            );
                        }

                        ip += 1;
                    }

                    if ip >= tokens.len() {
                        err(program.get_last_loc(), "open ended function declaration");
                    }

                    let mut proc_tokens: Vec<Token> = Vec::with_capacity(30);

                    if in_types.len() > 0 {
                        proc_tokens.push(Token::word_token(w.loc.clone(), "typefence".to_string()));
                        for str in type_vec_to_str(&in_types).split(' ') {
                            proc_tokens.push(Token::word_token(w.loc.clone(), str.to_string()));
                        }
                        proc_tokens.push(Token::word_token(w.loc.clone(), "end".to_string()));
                    }

                    let mut nesting = 0;

                    ip += 1;

                    while ip < tokens.len() {
                        let tok = &tokens[ip];

                        if let Token::WordToken(WordToken { value, .. }) = tok {
                            if let Ok(kw) = Keyword::from_str(value) {
                                if kw == Keyword::End && nesting > 0 {
                                    nesting -= 1;
                                } else if kw == Keyword::End {
                                    break;
                                } else if KEYWORDS_NESTING.contains(&kw) {
                                    nesting += 1;
                                }
                            } else if value == "assembly" {
                                nesting += 1;
                            }
                        }

                        proc_tokens.push(tok.clone());

                        ip += 1;
                    }

                    if ip >= tokens.len() {
                        err(program.get_last_loc(), "open ended block");
                    }

                    if out_types.len() > 0 {
                        proc_tokens.push(Token::word_token(w.loc.clone(), "typefence".to_string()));
                        for str in type_vec_to_str(&out_types).split(' ') {
                            proc_tokens.push(Token::word_token(w.loc.clone(), str.to_string()));
                        }
                        proc_tokens.push(Token::word_token(w.loc.clone(), "end".to_string()));
                    }

                    inline_procs.insert(name_tok.value.clone(), proc_tokens);
                } else if w.value == "assembly" {
                    match config.target {
                        crate::CompilationTargets::Linux => {
                            if !current_attrs.has_attribute("__supports_linux__") {
                                err(&w.loc, "Assembly does not support target linux; please use the preprocessor to remove exclude it!");
                            }
                        }
                    }

                    current_attrs.clear();

                    let mut expected_types: Vec<Type> = vec![];
                    let mut return_types: Vec<Type> = vec![];
                    let mut assembly: String = String::new();

                    let mut type_tokens: Vec<WordToken> = vec![];
                    let mut returning_bool = false;
                    ip += 1;

                    while ip <= tokens.len() {
                        let token = &tokens[ip];

                        if let Token::WordToken(token) = token {
                            if token.value == "--" && !returning_bool {
                                expected_types = Type::from_tokens(&type_tokens);
                                type_tokens.clear();
                                returning_bool = true
                            } else if token.value == "in" {
                                return_types = Type::from_tokens(&type_tokens);
                                type_tokens.clear();
                                break;
                            } else {
                                type_tokens.push(token.clone());
                            }
                        } else {
                            err(
                                token.get_location(),
                                "Expected a word, but found something else",
                            );
                        }

                        ip += 1;
                    }

                    if ip >= tokens.len() {
                        err(&program.get_last_loc(), "Expected `in`, but found nothing");
                    }

                    ip += 1;

                    while ip <= tokens.len() {
                        let token = &tokens[ip];

                        if let Token::StringToken(token) = token {
                            if token.value.len() > 0 {
                                if assembly.len() > 0 {
                                    assembly.push('\n');
                                }
                                assembly.push_str(&token.value);
                            }
                        } else if let Token::WordToken(token) = token {
                            if token.value == "end" {
                                break;
                            }
                            err(
                                &token.loc,
                                "Expected a string or `end`, but found something else",
                            );
                        } else {
                            err(
                                token.get_location(),
                                "Expected a string or `end`, but found something else",
                            );
                        }

                        ip += 1;
                    }

                    if ip >= tokens.len() {
                        err(&program.get_last_loc(), "Expected `end`, but found nothing");
                    }

                    program.ops.push(Operation::push_asm(
                        w.loc.clone(),
                        assembly,
                        expected_types,
                        return_types,
                    ));
                } else if w.value == "typefence" {
                    let mut type_tokens: Vec<WordToken> = vec![];
                    ip += 1;

                    while ip <= tokens.len() {
                        let token = &tokens[ip];

                        if let Token::WordToken(token) = token {
                            if token.value == "end" {
                                break;
                            } else {
                                type_tokens.push(token.clone());
                            }
                        } else {
                            err(
                                token.get_location(),
                                "Expected a word, but found something else",
                            );
                        }

                        ip += 1;
                    }

                    if ip >= tokens.len() {
                        err(&program.get_last_loc(), "Expected `end`, but found nothing");
                    }

                    program.ops.push(Operation::push_typefence(
                        w.loc.clone(),
                        Type::from_tokens(&type_tokens),
                    ));
                } else if w.value == ".param" {
                    current_attrs.add_attribute(expect_word_tok!(&w.loc).value.clone());
                } else if w.value == ".defparam" {
                    let name = expect_word_tok!(&w.loc);
                    ip += 1;
                    if ip >= tokens.len() {
                        err(&name.loc, "Expected a value, but found nothing");
                    }

                    current_attrs.add_attribute_value(
                        name.value.clone(),
                        parse_value(&tokens[ip], &consts, &inline_procs).to_string(),
                    );
                } else if w.value == "undef" {
                    let name = expect_word_tok!(&w.loc);
                    procs.remove(&name.value);
                    consts.remove(&name.value);
                    mems.remove(&name.value);
                    inline_procs.remove(&name.value);
                } else if w.value == "inline" {
                    inline_next = true;
                } else if w.value == ".log"
                    || w.value == ".warn"
                    || w.value == ".error"
                    || w.value == ".info"
                {
                    ip += 1;
                    if ip >= tokens.len() {
                        err(
                            program.get_last_loc(),
                            "Expected a constant, string, number or cstring, but found nothing",
                        );
                    }
                    let value = parse_value(&tokens[ip], &consts, &inline_procs).to_string();

                    if w.value == ".log" || w.value == ".info" {
                        info(&w.loc, value);
                    } else if w.value == ".warn" {
                        warn(&w.loc, value);
                    } else {
                        err(&w.loc, value);
                    }
                } else if w.value == ".str" || w.value == ".cstr" {
                    ensure_in_fn!(&w.loc);
                    ip += 1;
                    if ip >= tokens.len() {
                        err(
                            program.get_last_loc(),
                            "Expected a constant or number, but found nothing",
                        );
                    }
                    let value = match &tokens[ip] {
                        Token::IntegerToken(IntegerToken { value, .. }) => value.to_string(),
                        Token::WordToken(WordToken { loc, value }) => {
                            if let Some(constant) = consts.get(value) {
                                stringify_const(constant)
                            } else {
                                err(loc, format!("No constant with name {}", value));
                            }
                        }
                        _ => err(tokens[ip].get_location(), "Expected a constant or number"),
                    };
                    let i = program
                        .strings
                        .iter()
                        .position(|f| f.eq(&value))
                        .unwrap_or_else(|| {
                            let idx = program.strings.len();
                            program.strings.push(value.clone());
                            idx
                        }) as u64;
                    program.ops.push(Operation::push_str(
                        w.loc.clone(),
                        i,
                        value.len() as u64,
                        w.value == ".cstr",
                    ));
                } else if w.value == ".if" || w.value == ".ifn" {
                    let word = expect_word_tok!(&w.loc);
                    if let Some(value) = consts.get(&word.value) {
                        let mut boolean = match value.value {
                            1 => true,
                            _ => false,
                        };
                        if w.value == ".ifn" {
                            boolean = !boolean;
                        }
                        preprocessor_results.push(boolean);
                    } else {
                        err(&w.loc, format!("Could not find constant {}", word.value));
                    }
                } else if w.value == ".else" {
                    if let Some(value) = preprocessor_results.pop() {
                        preprocessor_results.push(!value);
                    } else {
                        err(
                            &w.loc,
                            ".else has to follow some conditional preprocessor statement",
                        )
                    }
                } else if w.value == ".end" {
                    if let None = preprocessor_results.pop() {
                        err(
                            &w.loc,
                            ".end has to follow some conditional preprocessor statement or .else",
                        )
                    }
                } else if w.value == ".ifc" || w.value == ".nifc" {
                    let left_value = parse_value(expect_tok!(&w.loc), &consts, &inline_procs);
                    let right_value = parse_value(expect_tok!(&w.loc), &consts, &inline_procs);
                    let expr = expect_word_tok!(&w.loc);

                    let joined: PreprocessorTypesPair = match left_value {
                        Twos::A(left_value) => match right_value {
                            Twos::A(right_value) => Twos::A((left_value, right_value)),
                            Twos::B(..) => err(&w.loc, "left and right expressions must have the same type (left: string, right: int)"),
                        },
                        Twos::B(left_value) => match right_value {
                            Twos::A(..) => err(&w.loc, "left and right expressions must have the same type (left: int, right: string)"),
                            Twos::B(right_value) => Twos::B((left_value, right_value)),
                        },
                    };

                    let mut boolean = run_comparison(joined, &expr.value, &w.loc);
                    if w.value == ".nifc" {
                        boolean = !boolean;
                    }
                    preprocessor_results.push(boolean);
                } else if w.value == ".ifdef" || w.value == ".ifndef" {
                    let WordToken { value, .. } = expect_word_tok!(&w.loc);
                    let mut boolean = mems.contains_key(value)
                        || procs.contains_key(value)
                        || inline_procs.contains_key(value)
                        || consts.contains_key(value);

                    if w.value == ".ifndef" {
                        boolean = !boolean;
                    }
                    preprocessor_results.push(boolean);
                } else if w.value == ".is" || w.value == ".isn" {
                    let WordToken { value: name, .. } = expect_word_tok!(&w.loc);
                    let WordToken {
                        value: identifier,
                        loc,
                    } = expect_word_tok!(&w.loc);
                    let is_defined_values = DotIsStruct {
                        is_const: consts.contains_key(name),
                        is_fn: procs.contains_key(name),
                        is_local_memory: mems_local.contains_key(name),
                        is_macro: inline_procs.contains_key(name),
                        is_memory: mems.contains_key(name),
                        is_intrinsic: Intrinsic::from_str(name).is_ok(),
                        is_keyword: Keyword::from_str(name).is_ok()
                            || SPECIAL_KEYWORD_NAMES.contains(&name.as_str()),
                    };
                    let identifiers = identifier.split('|').collect::<Vec<&str>>();
                    let mut boolean = false;
                    for ident in identifiers {
                        if is_defined_values.is(ident, loc) {
                            boolean = true;
                            break;
                        }
                    }
                    if w.value == ".isn" {
                        boolean = !boolean;
                    }
                    preprocessor_results.push(boolean);
                } else if w.value == ".pragma" {
                    let WordToken { value, loc } = expect_word_tok!(&w.loc);
                    if value == "multiple" {
                        pragma_multiple.push(w.loc.file.clone());
                    } else if value == "once" {
                        let mut indices: Vec<usize> = vec![];
                        for (i, val) in pragma_multiple.iter().enumerate() {
                            if *val == w.loc.file {
                                indices.push(i);
                            }
                        }
                        for idx in indices {
                            pragma_multiple.remove(idx);
                        }
                    } else {
                        err(loc, format!("Expected once or multiple, but found {value}"));
                    }
                } else if let Some(offset) = mems_local.get(&w.value) {
                    ensure_in_fn!(&w.loc);
                    program
                        .ops
                        .push(Operation::push_local_mem(w.loc.clone(), *offset));
                } else if let Some(id) = mems.get(&w.value) {
                    ensure_in_fn!(&w.loc);
                    program.ops.push(Operation::push_mem(w.loc.clone(), *id));
                } else if let Some(id) = procs.get(&w.value) {
                    if program
                        .contracts
                        .get(*id)
                        .unwrap()
                        .attributes
                        .has_attribute("deprecated")
                    {
                        warn(&w.loc, format!("{} is marked as deprecated", w.value));
                    }
                    ensure_in_fn!(&w.loc);
                    program.ops.push(Operation::call_proc(w.loc.clone(), *id));
                } else if let Some(constant) = consts.get(&w.value) {
                    ensure_in_fn!(&w.loc);
                    program
                        .ops
                        .push(Operation::push_const(w.loc.clone(), constant.clone()));
                } else if let Some(proc_tokens) = inline_procs.get(&w.value) {
                    ensure_in_fn!(&w.loc);
                    for token in proc_tokens {
                        let mut token = token.clone();
                        token.get_location_mut().file = w.loc.file.clone();
                        token.get_location_mut().char = w.loc.char;
                        token.get_location_mut().line = w.loc.line;
                        tokens_to_insert.push(token);
                    }
                } else if w.value == "return" {
                    ensure_in_fn!(&w.loc);
                    program.ops.push(Operation::ret(w.loc.clone(), false));
                } else {
                    // let (l, r) = tokens.split_at(ip);
                    // let mut r_iter = r.into_iter();
                    // println!(
                    //     "{}\n---->> {}\n{}",
                    //     l.iter()
                    //         .map(|f| format!("{f}"))
                    //         .reduce(|a, b| format!("{a} {b}"))
                    //         .unwrap_or_default(),
                    //     r_iter.next().unwrap(),
                    //     r_iter
                    //         .map(|f| format!("{f}"))
                    //         .reduce(|a, b| format!("{a} {b}"))
                    //         .unwrap_or_default()
                    // );
                    err(&w.loc, format!("Could not parse {}", w.value));
                }
            }
            _ => {}
        }
        ip += 1;

        while let Some(tok) = tokens_to_insert.pop() {
            tokens.insert(ip, tok);
        }
        tokens_to_insert.clear();
    }

    program
}

type PreprocessorTypes = Twos<String, u64>;
type PreprocessorTypesPair = Twos<(String, String), (u64, u64)>;

fn parse_value(
    token: &Token,
    constants: &HashMap<String, Constant>,
    macros: &HashMap<String, Vec<Token>>,
) -> PreprocessorTypes {
    match token {
        Token::StringToken(StringToken { value, .. }) => Twos::a(value.clone()),
        Token::IntegerToken(IntegerToken { value, .. }) => Twos::b(*value),
        Token::CharToken(CharToken { char, .. }) => Twos::a(char.to_string()),
        Token::WordToken(WordToken { value, loc }) => {
            if let Some(constant) = constants.get(value) {
                Twos::b(constant.value)
            } else if let Some(toks) = macros.get(value) {
                if toks.len() != 1 {
                    err(loc, format!("Couldn't find constant {}", value))
                } else if let Token::StringToken(StringToken { value, .. }) = &toks[0] {
                    Twos::a(value.clone())
                } else {
                    err(&loc, format!("Couldn't find constant {}", value))
                }
            } else {
                err(loc, format!("Couldn't find constant {}", value));
            }
        }
        _ => err(
            token.get_location(),
            "Only a constant, number or string is supported",
        ),
    }
}

fn run_comparison(values: PreprocessorTypesPair, str: &str, loc: &Loc) -> bool {
    macro_rules! err_ord_str {
        () => {
            err(loc, "Strings only allow = and !=")
        };
    }
    macro_rules! compare_ord {
        ($comparison: expr) => {
            match values {
                Twos::A(_) => err_ord_str!(),
                Twos::B((left, right)) => $comparison(&left, &right),
            }
        };
    }

    match str {
        "<" => compare_ord!(PartialOrd::lt),
        ">" => compare_ord!(PartialOrd::gt),
        "<=" => compare_ord!(PartialOrd::le),
        ">=" => compare_ord!(PartialOrd::ge),
        "!=" => match values {
            Twos::A((left, right)) => left != right,
            Twos::B((left, right)) => left != right,
        },
        "=" => match values {
            Twos::A((left, right)) => left == right,
            Twos::B((left, right)) => left == right,
        },
        _ => err(loc, format!("Unknown comparison {str}")),
    }
}
