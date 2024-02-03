use std::collections::HashMap;
use std::fmt::{Debug, Display, Write};
use std::fs::read_to_string;
use std::path::PathBuf;
use std::str::FromStr;
use std::vec;

use crate::error::{info, warn};
use crate::tokenizer::{self, try_parse_int, CharToken, IntegerToken, WordToken};
use crate::typecheck::{type_vec_to_str, Type};
use crate::utils::{get_stdpath, iota, stringify_const, Twos};
use crate::{
    error::err,
    tokenizer::{Loc, StringToken, Token},
};
use crate::{Config, GlobalString};

#[derive(Debug, Clone, Copy)]
pub enum Intrinsic {
    Print,
    Here,

    StructPtrPlus,

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

    Load,
    Load8,
    Load16,
    Load32,
    Load64,
    Store,
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

            "@" => Ok(Self::Load),
            "@8" => Ok(Self::Load8),
            "@16" => Ok(Self::Load16),
            "@32" => Ok(Self::Load32),
            "@64" => Ok(Self::Load64),
            "!" => Ok(Self::Store),
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
            Self::Load => "@",
            Self::Load8 => "@8",
            Self::Load16 => "@16",
            Self::Load32 => "@32",
            Self::Load64 => "@64",
            Self::Store => "!",
            Self::Store8 => "!8",
            Self::Store16 => "!16",
            Self::Store32 => "!32",
            Self::Store64 => "!64",
            Self::StructPtrPlus => "StructPtrPlus<+>",
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
    pub typ: Type,
    pub off: usize,
}
#[derive(Debug, Clone)]
pub struct PushLocalMem {
    pub loc: Loc,
    pub off: usize,
    pub typ: Type,
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
    pub externally_provided: Option<GlobalString>,
}

#[derive(Debug, Clone)]
pub struct PushConst {
    pub constant: Constant,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct Cast {
    pub typ: Type,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub enum Operation {
    Intrinsic(OpIntrinsic),
    Ret(Ret),
    PushStr(PushStr),
    PushInt(PushInt),
    PushPtr(PushInt),
    Keyword(OpKeyword),
    PushMem(PushMem),
    Proc(Proc),
    CallProc(CallProc),
    PushLocalMem(PushLocalMem),
    PushConst(PushConst),
    Assembly(PushAssembly),
    Typefence(Typefence),
    Cast(Cast),
    None(Loc),
}

impl Display for Operation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            Self::Intrinsic(..) => "Intrinsic",
            Self::Ret(..) => "Ret",
            Self::PushStr(..) => "PushStr",
            Self::PushInt(..) => "PushInt",
            Self::PushPtr(..) => "PushInt",
            Self::Keyword(..) => "Keyword",
            Self::PushMem(..) => "PushMem",
            Self::Proc(..) => "Proc",
            Self::CallProc(..) => "CallProc",
            Self::PushLocalMem(..) => "PushLocalMem",
            Self::PushConst(..) => "PushConst",
            Self::Assembly(..) => "Assembly",
            Self::Typefence(..) => "Typefence",
            Self::None(..) => "None",
            Self::Cast(..) => "Cast",
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
    pub fn cast(loc: Loc, typ: Type) -> Self {
        Self::Cast(Cast { loc, typ })
    }
    pub fn push_int(loc: Loc, value: u64) -> Self {
        Self::PushInt(PushInt { loc, value })
    }
    pub fn push_mem(loc: Loc, id: u64, typ: Type, off: usize) -> Self {
        Self::PushMem(PushMem { loc, id, typ, off })
    }
    pub fn push_local_mem(loc: Loc, off: usize, typ: Type) -> Self {
        Self::PushLocalMem(PushLocalMem { loc, off, typ })
    }
    pub fn push_ptr(loc: Loc, value: u64) -> Self {
        Self::PushPtr(PushInt { loc, value })
    }
    pub fn call_proc(loc: Loc, id: usize, externally_provided: Option<GlobalString>) -> Self {
        Self::CallProc(CallProc {
            loc,
            id,
            externally_provided,
        })
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
            | Self::PushPtr(PushInt { loc, .. })
            | Self::PushStr(PushStr { loc, .. })
            | Self::PushMem(PushMem { loc, .. })
            | Self::PushLocalMem(PushLocalMem { loc, .. })
            | Self::Ret(Ret { loc, .. })
            | Self::CallProc(CallProc { loc, .. })
            | Self::PushConst(PushConst { loc, .. })
            | Self::Proc(Proc { loc, .. })
            | Self::Assembly(PushAssembly { loc, .. })
            | Self::Typefence(Typefence { loc, .. })
            | Self::Cast(Cast { loc, .. })
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
    pub contracts: HashMap<usize, ProcedureContract>,
    pub reversed_refs: HashMap<usize, usize>,
    structs: Vec<Struct>,
    pub run_functions: Vec<usize>,
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
            structs: vec![],
            main_fn: Option::default(),
            contracts: HashMap::default(),
            run_functions: vec![],
        }
    }
}

impl Program {
    pub fn get_last_loc(&self) -> &Loc {
        &self.loc_eof
    }

    pub fn get_struct_size(&self, id: usize) -> Option<usize> {
        self.structs.get(id).map(|f| f.get_size())
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
            self.reversed_refs
                .reserve(self.refs.len() - self.reversed_refs.capacity());
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
    ".has_param",
    ".hasn_param",
    ".check_param",
    ".checkn_param",
    ".get_param",
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
    "struct",
];

struct DotIsStruct {
    is_macro: bool,
    is_fn: bool,
    is_const: bool,
    is_memory: bool,
    is_local_memory: bool,
    is_intrinsic: bool,
    is_keyword: bool,
    is_struct: bool,
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
            "struct" => self.is_struct,
            _ => err(loc, format!("No type {str} found!")),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Struct {
    types: HashMap<String, Type>,
    pub name: String,
    __cached_size: Option<usize>,
    added_order: Vec<String>,
}

impl PartialEq for Struct {
    fn eq(&self, other: &Self) -> bool {
        if self.name != other.name || self.types.len() != other.types.len() {
            return false;
        }

        let mut i: usize = 0;
        for (name, typ) in self.types.iter() {
            i += 1;
            if match other.types.get(name) {
                None => true,
                Some(other_typ) => typ.equal_to_strict(other_typ),
            } {
                return false;
            }
        }
        if i != self.types.len() - 1 {
            return false;
        }

        true
    }
}

impl Struct {
    fn new(name: String) -> Self {
        Self {
            types: HashMap::default(),
            name,
            __cached_size: Option::default(),
            added_order: vec![],
        }
    }

    fn get_type(&self, str: &str) -> Option<Type> {
        self.types.get(str).cloned()
    }

    fn get_offset(&self, str: &str) -> Option<usize> {
        let mut offset: usize = 0;
        for name in &self.added_order {
            if name == str {
                return Some(offset);
            }
            offset += self.types.get(name).unwrap().get_size();
        }
        None
    }

    fn get_size(&self) -> usize {
        if let Some(sz) = self.__cached_size {
            return sz;
        }
        self.types
            .values()
            .map(|typ| typ.get_size())
            .reduce(|a, b| a + b)
            .unwrap_or_default()
    }
}

impl Display for Struct {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("struct ")?;
        f.write_str(&self.name)?;
        f.write_str("\n")?;

        for (name, typ) in &self.types {
            f.write_str("    ")?;
            f.write_str(name)?;
            f.write_char(' ')?;
            Display::fmt(typ, f)?;
            f.write_str(" in\n")?;
        }

        f.write_str("end")
    }
}

#[derive(Default)]
pub struct Structs(HashMap<String, usize>);

impl Structs {
    pub fn insert(&mut self, new_struct: Struct, prog: &mut Program) {
        self.0.insert(new_struct.name.clone(), prog.structs.len());
        prog.structs.push(new_struct);
    }

    pub fn is_struct(&self, name: &str) -> bool {
        self.0.contains_key(name)
    }

    pub fn clear(&mut self, name: &str) {
        self.0.remove(name);
    }

    pub fn get_struct_id(&self, name: &str) -> Option<usize> {
        self.0.get(name).copied()
    }

    pub fn is_word_defined(&self, word: &str, program: &Program) -> bool {
        if word.starts_with('@') || word.starts_with('!') {
            self.get(&word[1..], program, true).is_some()
        } else if (word.starts_with("sizeof(") || word.starts_with("offset("))
            && word.ends_with(")")
        {
            self.get(&word[7..word.len() - 1], program, false).is_some()
        } else {
            self.get(&word, program, true).is_some()
        }
    }

    fn get_ops_from_word(&self, word: &str, loc: &Loc, prog: &Program) -> Option<Vec<Operation>> {
        if word.starts_with('@') || word.starts_with('!') {
            let (offset, _, typ) = self.get(&word[1..], prog, true)?;
            // let store_load_intrinsic =
            //     match Type::Sized(size).get_op_load_store(word.starts_with('@')) {
            //         Some(v) => v,
            //         None => err(loc, format!("Size {size} does not support storing/loading")),
            //     };

            Some(vec![
                Operation::push_ptr(*loc, offset as u64),
                Operation::intrinsic(*loc, Intrinsic::StructPtrPlus),
                Operation::cast(*loc, Type::PtrTo(Box::new(typ))),
                Operation::intrinsic(
                    *loc,
                    if word.starts_with('@') {
                        Intrinsic::Load
                    } else {
                        Intrinsic::Store
                    },
                ),
            ])
        } else if word.starts_with("sizeof(") && word.ends_with(")") {
            let (_, size, ..) = self.get(&word[7..word.len() - 1], prog, false)?;
            Some(vec![Operation::push_int(*loc, size as u64)])
        } else if word.starts_with("offset(") && word.ends_with(")") {
            let (offset, ..) = self.get(&word[7..word.len() - 1], prog, false)?;
            Some(vec![Operation::push_int(*loc, offset as u64)])
        } else {
            let (offset, _, typ) = self.get(&word, prog, false)?;

            Some(vec![
                Operation::push_ptr(*loc, offset as u64),
                Operation::intrinsic(*loc, Intrinsic::StructPtrPlus),
                Operation::cast(*loc, Type::PtrTo(Box::new(typ))),
            ])
        }
    }

    fn get(&self, name: &str, program: &Program, needs_type: bool) -> Option<(usize, usize, Type)> {
        let mut offset = 0usize;

        let mut paths = name.split('.');
        let first = match paths.next() {
            Some(v) => v,
            None => return None,
        };
        let paths: Vec<&str> = paths.collect();

        let mut cur_struct = self
            .get_struct_id(first)
            .and_then(|f| program.structs.get(f).cloned());
        let mut cur_size = match &cur_struct {
            Some(v) => v.get_size(),
            _ => 0,
        };
        let mut cur_array: Option<(usize, Type)> = None;
        if cur_struct.is_none() {
            return None;
        }
        let mut cur_typ = Type::Struct(
            GlobalString::from_static(first),
            self.get_struct_id(first).unwrap(),
            cur_struct.as_ref().unwrap().get_size(),
        );

        for path in paths {
            let (newstruct_id, off, new_size, new_typ) = if let Some(structt) = cur_struct {
                if let Some((member, off)) = structt.get_type(path).zip(structt.get_offset(path)) {
                    match &member {
                        Type::Struct(_, id, ..) => (Some(*id), off, 0, member),
                        Type::Array(typ, len) => {
                            cur_array = Some((*len, *typ.clone()));
                            (None, off, 0, member)
                        }
                        _ => (None, off, member.get_size(), member),
                    }
                } else {
                    return None;
                }
            } else if let Some((len, typ)) = cur_array {
                cur_array = None;

                let index = match try_parse_int(path) {
                    Some(v) => v as usize,
                    None => return None,
                };

                if index >= len {
                    return None;
                }

                let off = typ.get_size() * index;

                match typ {
                    Type::Struct(_, id, ..) => (Some(id), off, 0, typ),
                    Type::Array(typ, len) => {
                        cur_array = Some((len, *typ.clone()));
                        (None, off, 0, *typ)
                    }
                    _ => (None, off, typ.get_size(), typ),
                }
            } else {
                return None;
            };

            cur_typ = new_typ;
            offset += off;
            if let Some(newstruct_id) = newstruct_id {
                cur_struct = match program.structs.get(newstruct_id).cloned() {
                    Some(v) => {
                        cur_size = v.get_size();
                        Some(v)
                    }
                    None => return None,
                };
            } else {
                cur_struct = None;
                cur_size = new_size;
            }
        }

        if (cur_struct.is_some() || cur_array.is_some()) && needs_type {
            return None;
        }

        Some((offset, cur_size, cur_typ))
    }
}

pub struct Memories(HashMap<String, (u64, Type)>, HashMap<String, (usize, Type)>);

impl Memories {
    pub fn new() -> Self {
        Self(HashMap::new(), HashMap::new())
    }

    pub fn insert_local_mem(&mut self, str: String, offset: usize, typ: Type) {
        self.1.insert(str, (offset, typ));
    }

    pub fn insert_mem(&mut self, str: String, id: u64, typ: Type) {
        self.0.insert(str, (id, typ));
    }

    pub fn clear_local_memories(&mut self) {
        self.1.clear();
    }

    pub fn remove(&mut self, str: &String) {
        self.0.remove(str);
        self.1.remove(str);
    }

    pub fn is_local_defined(&mut self, str: &str) -> bool {
        self.1.contains_key(str)
    }

    pub fn is_global_defined(&mut self, str: &str) -> bool {
        self.0.contains_key(str)
    }

    pub fn is_defined(
        &self,
        name: &String,
        include_local_vars: bool,
        prog: &Program,
        structs: &Structs,
    ) -> bool {
        if name.starts_with('!') || name.starts_with('@') {
            self.get(&name[1..], true, include_local_vars, prog, structs)
                .is_some()
        } else if (name.starts_with("offset(") || name.starts_with("sizeof("))
            && name.ends_with(')')
        {
            self.get(
                &name[7..name.len() - 1],
                false,
                include_local_vars,
                prog,
                structs,
            )
            .is_some()
        } else {
            self.get(name, false, include_local_vars, prog, structs)
                .is_some()
        }
    }

    pub fn get_ops_from_word(
        &self,
        name: &String,
        prog: &Program,
        structs: &Structs,
        loc: &Loc,
    ) -> Option<Vec<Operation>> {
        if name.starts_with('!') || name.starts_with('@') {
            let (is_local, offset, id, typ) = self.get(&name[1..], true, true, prog, structs)?;

            let intrinsic = if name.starts_with('!') {
                Intrinsic::Store
            } else {
                Intrinsic::Load
            };
            if is_local {
                Some(vec![
                    Operation::push_local_mem(*loc, offset + id as usize, typ),
                    Operation::intrinsic(*loc, intrinsic),
                ])
            } else {
                Some(vec![
                    Operation::push_mem(*loc, id, typ, offset),
                    Operation::intrinsic(*loc, intrinsic),
                ])
            }
        } else if name.starts_with("offset(") && name.ends_with(')') {
            let (_, offset, ..) = self.get(&name[7..name.len() - 1], false, true, prog, structs)?;
            Some(vec![Operation::push_int(*loc, offset as u64)])
        } else if name.starts_with("sizeof(") && name.ends_with(')') {
            let (_, _, _, typ) = self.get(&name[7..name.len() - 1], false, true, prog, structs)?;
            Some(vec![Operation::push_int(*loc, typ.get_size() as u64)])
        } else {
            let (is_local, offset, id, typ) = self.get(&name, false, true, prog, structs)?;

            if is_local {
                Some(vec![Operation::push_local_mem(
                    *loc,
                    id as usize + offset,
                    typ,
                )])
            } else {
                Some(vec![Operation::push_mem(*loc, id, typ, offset)])
            }
        }
    }

    fn get(
        &self,
        str: &str,
        needs_value: bool,
        include_local_vars: bool,
        prog: &Program,
        structs: &Structs,
    ) -> Option<(bool, usize, u64, Type)> {
        // is_local, offset, id, typ
        let mut paths = str.split('.');

        let name = paths.next()?;

        let (is_local, id, typ) = {
            if let Some((id, typ)) = self.0.get(name) {
                (false, *id, typ.clone())
            } else if !include_local_vars {
                return None;
            } else if let Some((id, typ)) = self.1.get(name) {
                (true, *id as u64, typ.clone())
            } else {
                return None;
            }
        };

        let mut offset: usize = 0;
        let mut typ = typ;

        loop {
            match typ {
                Type::Array(ref array_typ, sz) => {
                    if let Some(v) = paths.next() {
                        let value = try_parse_int(v)? as usize;
                        if value >= sz {
                            return None;
                        }
                        offset += array_typ.get_size() * value;
                        typ = *array_typ.clone();
                    } else {
                        break;
                    }
                }
                Type::Struct(name, ..) => {
                    let mut path: String = String::with_capacity(100);
                    path.push_str(name.get());

                    while let Some(v) = paths.next() {
                        path.push('.');
                        path.push_str(v);
                    }
                    let (struct_offset, _, typ) = structs.get(&path, prog, needs_value)?;
                    return Some((is_local, offset + struct_offset, id, typ));
                }
                _ if paths.next().is_some() => return None,
                _ => break,
            }
        }

        if needs_value {
            match typ {
                Type::Array(..) | Type::Struct(..) => return None,
                _ => {}
            }
        }

        Some((is_local, offset, id, typ))
    }
}

pub fn parse_tokens(mut tokens: Vec<Token>, config: &Config) -> Program {
    let mut program = Program::default();

    let mut mems: Memories = Memories::new();
    let mut procs: HashMap<String, usize> = HashMap::new();
    let mut inline_procs: HashMap<String, Vec<Token>> = HashMap::new();
    let mut consts: HashMap<String, Constant> = HashMap::new();
    let mut offset: u64 = 0;
    let mut pragma_multiple: Vec<GlobalString> = vec![];
    let mut included_files: Vec<GlobalString> = vec![];
    let mut structs = Structs::default();
    let mut type_aliases: HashMap<String, (Type, bool)> = HashMap::default();

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

    macro_rules! expect_num_tok {
        ($loc: expr) => {{
            ip += 1;
            if let Some(k) = tokens.get(ip).and_then(|tok| tok.as_int_token()) {
                k
            } else {
                err($loc, "Expected a number token");
            }
        }};
    }

    macro_rules! is_name_defined {
        ($v: expr) => {{
            mems.is_defined($v, current_fn.is_some(), &program, &structs)
                || procs.contains_key($v)
                || consts.contains_key($v)
                || type_aliases.contains_key($v)
                || inline_procs.contains_key($v)
                || $v == "proc"
                || $v == "include"
                || $v == "memory"
                || PREPROCESSOR_NAMES.contains(&$v.as_str())
                || Keyword::from_str($v).is_ok()
                || Intrinsic::from_str($v).is_ok()
                || SPECIAL_KEYWORD_NAMES.contains(&$v.as_str())
                || structs.is_word_defined(&$v.as_str(), &program)
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
                    t.loc,
                    i,
                    t.value.len() as u64,
                    t.is_cstr,
                ));
            }
            Token::CharToken(c) => {
                ensure_in_fn!(&c.loc);
                program.ops.push(Operation::push_int(c.loc, c.char as u64));
            }
            Token::IntegerToken(i) => {
                ensure_in_fn!(&i.loc);
                program.ops.push(Operation::push_int(i.loc, i.value));
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
                    program.ops.push(Operation::intrinsic(w.loc, op));
                } else if let Ok(keyword) = Keyword::from_str(&w.value) {
                    ensure_in_fn!(&w.loc);
                    if keyword == Keyword::End {
                        if nesting > 0 {
                            nesting -= 1;
                            program.ops.push(Operation::keyword(w.loc, Keyword::End));
                        } else {
                            current_fn = None;
                            mems.clear_local_memories();
                            program.ops.push(Operation::ret(w.loc, true));
                        }
                    } else {
                        if KEYWORDS_NESTING.contains(&keyword) {
                            nesting += 1;
                        }
                        program.ops.push(Operation::keyword(w.loc, keyword));
                    }
                } else if w.value == "include" {
                    if current_fn.is_some() {
                        warn(&w.loc, "include in function body");
                    }
                    let str_tok: &StringToken = expect_str_tok!(&w.loc);
                    let mut path_str = str_tok.value.clone();
                    path_str += ".undefied";
                    let path = if path_str.starts_with("@") {
                        let mut path = get_stdpath();
                        path.push(&path_str[1..]);
                        let path = path
                            .to_str()
                            .expect("Could not form path string")
                            .to_string();
                        path
                    } else {
                        let mut path = PathBuf::from(&w.loc.file.get());
                        path.pop();
                        path.push(path_str);
                        let path = path
                            .to_str()
                            .expect("Could not form path string")
                            .to_string();
                        path
                    };
                    let path_gstr = GlobalString::new(&path);
                    if path.eq(w.loc.file.get()) {
                        err(&w.loc, "Cannot include file from itself");
                    } else if !included_files.contains(&path_gstr)
                        || pragma_multiple.contains(&path_gstr)
                    {
                        info(&w.loc, format!("Including {:?}", &path));
                        included_files.push(path_gstr);
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
                    if current_fn.is_some() {
                        err(&w.loc, "Cannot define a constant inside of a function");
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
                    consts.insert(name, Constant::new(constant, w.loc, typ));
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
                                    if value.starts_with("sizeof(") && value.ends_with(")") {
                                        if let Some((_, size, ..)) =
                                            structs.get(&value[7..value.len() - 1], &program, false)
                                        {
                                            stack.push(size as u64);
                                            if should_end {
                                                break;
                                            }
                                            ip += 1;
                                            continue;
                                        }
                                    }
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
                        if is_name_defined!(&name) {
                            warn(
                                &w.loc,
                                "Shadowing a global variable; This might be intentional",
                            );
                        }
                        let op = program.ops[ip]
                            .as_proc_mut()
                            .expect("Expected current_fn to point to a proc");
                        mems.insert_local_mem(name, op.local_mem_size, Type::Any);
                        op.local_mem_size += len;
                    } else {
                        let id = iota() as u64;
                        program.mems.insert(id, len);
                        mems.insert_mem(name, id, Type::Any);
                    }
                } else if w.value == "var" {
                    let name = (expect_word_tok!(&w.loc)).value.clone();
                    if is_name_defined!(&name) && current_fn.is_none() {
                        err(&w.loc, format!("Name {} is already defined!", name));
                    }
                    let mut stack: Vec<WordToken> = vec![];
                    ip += 1;

                    while ip < tokens.len() {
                        let token = &tokens[ip];

                        match token {
                            Token::WordToken(v) => {
                                if v.value == "end" {
                                    break;
                                }
                                stack.push(v.clone());
                            }
                            _ => err(
                                token.get_location(),
                                "Expected a type or end, but found something else",
                            ),
                        }

                        ip += 1;
                    }

                    if ip >= tokens.len() {
                        err(
                            program.get_last_loc(),
                            "Expected a type or end, but found nothing",
                        );
                    }

                    let mut types = Type::from_tokens(&stack, &structs, &program, &type_aliases);
                    let typ = match types.pop() {
                        Some(v) => v,
                        None => err(&tokens[ip].get_location(), "Expected exactly one type"),
                    };

                    let len = typ.get_size();

                    if let Some(ip) = current_fn {
                        if is_name_defined!(&name) {
                            warn(
                                &w.loc,
                                "Shadowing a global variable; This might be intentional",
                            );
                        }
                        let op = program.ops[ip]
                            .as_proc_mut()
                            .expect("Expected current_fn to point to a proc");
                        mems.insert_local_mem(name, op.local_mem_size, typ);
                        op.local_mem_size += len;
                    } else {
                        let id = iota() as u64;
                        program.mems.insert(id, len);
                        mems.insert_mem(name, id, typ);
                    }
                } else if w.value == "fn" && (!inline_next || config.no_inline) {
                    inline_next = false;
                    if current_fn.is_some() {
                        err(&w.loc, "function definition in function definition")
                    }
                    mems.clear_local_memories();
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
                    contract.loc = w.loc;
                    let id = iota();
                    contract.id = id;

                    let mut out_types = false;
                    let mut type_tokens: Vec<WordToken> = vec![];

                    ip += 1;
                    while ip < tokens.len() {
                        let tok = &tokens[ip];

                        if let Token::WordToken(tok) = tok {
                            if tok.value == "--" && !out_types {
                                contract.in_types = Type::from_tokens(
                                    &type_tokens,
                                    &structs,
                                    &program,
                                    &type_aliases,
                                );
                                type_tokens.clear();
                                out_types = true;
                            } else if tok.value == "in" {
                                if out_types {
                                    contract.out_types = Type::from_tokens(
                                        &type_tokens,
                                        &structs,
                                        &program,
                                        &type_aliases,
                                    );
                                } else {
                                    contract.in_types = Type::from_tokens(
                                        &type_tokens,
                                        &structs,
                                        &program,
                                        &type_aliases,
                                    );
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

                    if Type::has_non_stack_sized_types(&contract.in_types)
                        || Type::has_non_stack_sized_types(&contract.out_types)
                    {
                        err(
                            &tokens[ip].get_location_mut(),
                            "fn does not support non-stack sized types (>8 bytes)",
                        )
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
                        w.loc,
                        id,
                        name.clone(),
                        (name == "main" && !contract.attributes.has_attribute("__nomain__"))
                            || contract.attributes.has_attribute("__export__") || contract.attributes.has_attribute("__run_function__"),
                    ));
                    if !contract.attributes.has_attribute("__fn_anonymous__") && !contract.attributes.has_attribute("__fn_anon__") {
                        procs.insert(name.clone(), id);
                    }
                    if contract.attributes.has_attribute("__run_function__") {
                        if contract.in_types.len() > 0 || contract.out_types.len() > 0 {
                            err(&w.loc, "__run_function__ functions are not allowed to take or return any values");
                        }
                        program.run_functions.push(id);
                    }
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
                                in_types = Type::from_tokens(
                                    &type_tokens,
                                    &structs,
                                    &program,
                                    &type_aliases,
                                );
                                type_tokens.clear();
                                def_out_types = true;
                            } else if tok.value == "in" {
                                if def_out_types {
                                    out_types = Type::from_tokens(
                                        &type_tokens,
                                        &structs,
                                        &program,
                                        &type_aliases,
                                    );
                                } else {
                                    in_types = Type::from_tokens(
                                        &type_tokens,
                                        &structs,
                                        &program,
                                        &type_aliases,
                                    );
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

                    if Type::has_generics(&in_types) || Type::has_generics(&out_types) {
                        err(
                            &tokens[ip].get_location_mut(),
                            "inline fn does not support generics",
                        )
                    } else if Type::has_non_stack_sized_types(&in_types)
                        || Type::has_non_stack_sized_types(&out_types)
                    {
                        err(
                            &tokens[ip].get_location_mut(),
                            "inline fn does not support non-stack sized types (>8 bytes)",
                        )
                    }

                    let mut proc_tokens: Vec<Token> = Vec::with_capacity(30);

                    if in_types.len() > 0 {
                        proc_tokens.push(Token::word_token(w.loc, "typefence".to_string()));
                        for str in type_vec_to_str(&in_types).split(' ') {
                            proc_tokens.push(Token::word_token(w.loc, str.to_string()));
                        }
                        proc_tokens.push(Token::word_token(w.loc, "end".to_string()));
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
                            } else if value == "assembly"
                                || value == "memory"
                                || value == "typefence"
                            {
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
                        proc_tokens.push(Token::word_token(w.loc, "typefence".to_string()));
                        for str in type_vec_to_str(&out_types).split(' ') {
                            proc_tokens.push(Token::word_token(w.loc, str.to_string()));
                        }
                        proc_tokens.push(Token::word_token(w.loc, "end".to_string()));
                    }

                    inline_procs.insert(name_tok.value.clone(), proc_tokens);
                } else if w.value == "assembly" {
                    if !config.target.asm_will_work(&current_attrs) {
                        err(&w.loc, format!("Assembly does not support target {}; please use the preprocessor to exclude it", config.target));
                    }
                    let empty_asm = current_attrs.has_attribute("empty_asm");

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
                                expected_types = Type::from_tokens(
                                    &type_tokens,
                                    &structs,
                                    &program,
                                    &type_aliases,
                                );
                                type_tokens.clear();
                                returning_bool = true
                            } else if token.value == "in" {
                                if returning_bool {
                                    return_types = Type::from_tokens(
                                        &type_tokens,
                                        &structs,
                                        &program,
                                        &type_aliases,
                                    );
                                } else {
                                    expected_types = Type::from_tokens(
                                        &type_tokens,
                                        &structs,
                                        &program,
                                        &type_aliases,
                                    );
                                }
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

                    if Type::has_non_stack_sized_types(&expected_types)
                        || Type::has_non_stack_sized_types(&return_types)
                    {
                        err(
                            tokens[ip].get_location(),
                            "assembly does not support types non-stack sized types (>8 bytes)",
                        );
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

                    if assembly.len() < 1 && !empty_asm {
                        warn(&w.loc, "Assembly body is empty (consider using .param empty_asm. This will also remove the need for a target attribute)");
                    } else if assembly.len() > 0 && empty_asm {
                        err(&w.loc, "Assembly body should have been empty, but it isnt");
                    }

                    program.ops.push(Operation::push_asm(
                        w.loc,
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

                    let type_tokens =
                        Type::from_tokens(&type_tokens, &structs, &program, &type_aliases);

                    if Type::has_generics(&type_tokens) {
                        err(
                            tokens[ip].get_location(),
                            "typefence does not support generics",
                        );
                    } else if Type::has_non_stack_sized_types(&type_tokens) {
                        err(
                            tokens[ip].get_location(),
                            "typefence does not support types non-stack sized types (>8 bytes)",
                        );
                    }

                    program
                        .ops
                        .push(Operation::push_typefence(w.loc, type_tokens));
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
                    if name.value.contains("..") {
                        err(&w.loc, "cannot undefine a struct or type function");
                    }
                    procs.remove(&name.value);
                    consts.remove(&name.value);
                    mems.remove(&name.value);
                    inline_procs.remove(&name.value);
                    structs.clear(&name.value);
                    type_aliases.remove(&name.value);
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
                        w.loc,
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
                    let mut boolean = mems.is_local_defined(value)
                        || mems.is_global_defined(value)
                        || procs.contains_key(value)
                        || inline_procs.contains_key(value)
                        || consts.contains_key(value)
                        || structs.is_struct(value);

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
                        is_local_memory: mems.is_local_defined(name),
                        is_macro: inline_procs.contains_key(name),
                        is_memory: mems.is_global_defined(name),
                        is_intrinsic: Intrinsic::from_str(name).is_ok(),
                        is_keyword: Keyword::from_str(name).is_ok()
                            || SPECIAL_KEYWORD_NAMES.contains(&name.as_str()),
                        is_struct: structs.is_struct(name),
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
                        pragma_multiple.push(w.loc.file);
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
                } else if w.value == ".has_param" || w.value == ".hasn_param" {
                    let function_name = expect_word_tok!(&w.loc);
                    let parameter = expect_word_tok!(&function_name.loc);
                    if let Some(func) = procs.get(&function_name.value) {
                        let mut boolean = match program.contracts.get(func) {
                            Some(v) => v.attributes.has_attribute(parameter.value.as_str()),
                            None => err(
                                &function_name.loc,
                                format!(
                                    "Could not find any function with the name {}",
                                    function_name.value
                                ),
                            ),
                        };
                        if w.value == ".hasn_param" {
                            boolean = !boolean;
                        }
                        preprocessor_results.push(boolean);
                    } else {
                        err(
                            &function_name.loc,
                            format!(
                                "Could not find any function with the name {}",
                                function_name.value
                            ),
                        )
                    }
                } else if w.value == ".check_param" || w.value == ".checkn_param" {
                    let function_name = expect_word_tok!(&w.loc);
                    let parameter = expect_word_tok!(&function_name.loc);
                    ip += 1;
                    if ip >= tokens.len() {
                        err(&parameter.loc, "Expected a value but found nothing");
                    }
                    let value = parse_value(&tokens[ip], &consts, &inline_procs).to_string();

                    if let Some(func) = procs.get(&function_name.value) {
                        let mut boolean = match program.contracts.get(func) {
                            Some(v) => v
                                .attributes
                                .has_attribute_value(parameter.value.as_str(), value.as_str()),
                            None => err(
                                &function_name.loc,
                                format!(
                                    "Could not find any function with the name {}",
                                    function_name.value
                                ),
                            ),
                        };
                        if w.value == ".checkn_param" {
                            boolean = !boolean;
                        }
                        preprocessor_results.push(boolean);
                    } else {
                        err(
                            &function_name.loc,
                            format!(
                                "Could not find any function with the name {}",
                                function_name.value
                            ),
                        )
                    }
                } else if w.value == ".get_param" {
                    let function_name = expect_word_tok!(&w.loc);
                    let parameter = expect_word_tok!(&function_name.loc);
                    if let Some(func) = procs.get(&function_name.value) {
                        let value = match program.contracts.get(func) {
                            Some(v) => v.attributes.get_value(parameter.value.as_str()),
                            None => err(
                                &function_name.loc,
                                format!(
                                    "Could not find any function with the name {}",
                                    function_name.value
                                ),
                            ),
                        };
                        let value = match value {
                            Some(v) => v,
                            None => err(
                                &parameter.loc,
                                format!(
                                    "Function {} does not have parameter {}",
                                    function_name.value, parameter.value
                                ),
                            ),
                        };
                        tokens_to_insert.push(Token::str_token(w.loc, value.clone(), false));
                    } else {
                        err(
                            &function_name.loc,
                            format!(
                                "Could not find any function with the name {}",
                                function_name.value
                            ),
                        )
                    }
                } else if w.value == "struct" {
                    if current_fn.is_some() {
                        err(&w.loc, "Structs cannot be defined inside a function");
                    }
                    let name = expect_word_tok!(&w.loc);
                    if is_name_defined!(&name.value) {
                        err(&name.loc, format!("{} is already defined", name.value));
                    }

                    let mut new_struct: Struct = Struct::new(name.value.clone());

                    loop {
                        let name = expect_word_tok!(&program.get_last_loc());
                        if name.value == "end" {
                            break;
                        }

                        let mut type_tokens: Vec<WordToken> = vec![];
                        ip += 1;

                        loop {
                            if ip >= tokens.len() {
                                err(
                                    &program.get_last_loc(),
                                    "Expected a type or in, but found nothing",
                                );
                            }
                            let token = &tokens[ip];
                            if let Token::WordToken(tok) = token {
                                if tok.value == "in" {
                                    break;
                                }
                                type_tokens.push(tok.clone());
                            } else {
                                err(
                                    token.get_location(),
                                    "Expected `in` or a type, but found something else",
                                );
                            }

                            ip += 1;
                        }

                        let mut types =
                            Type::from_tokens(&type_tokens, &structs, &program, &type_aliases);
                        let typ = if let Some(typ) = types.pop() {
                            typ
                        } else {
                            err(
                                tokens[ip].get_location(),
                                "Expected a type but found multiple",
                            );
                        };
                        if typ.is_generic() {
                            err(
                                tokens[ip].get_location(),
                                "Structs dont support generic types!",
                            );
                        }
                        new_struct.types.insert(name.value.clone(), typ);
                        new_struct.added_order.push(name.value.clone());
                    }

                    new_struct.__cached_size = Some(new_struct.get_size());
                    structs.insert(new_struct, &mut program);
                } else if w.value == "new_type" {
                    let name = expect_word_tok!(&w.loc);

                    if is_name_defined!(&name.value) {
                        err(&name.loc, "Name already defined");
                    }

                    let size = expect_num_tok!(&name.loc);
                    type_aliases
                        .insert(name.value.clone(), (Type::Sized(size.value as usize), true));
                } else if w.value == "hard_type" || w.value == "type" {
                    let name = expect_word_tok!(&w.loc);

                    if is_name_defined!(&name.value) {
                        err(&name.loc, "Name already defined");
                    }

                    let mut type_tokens: Vec<WordToken> = vec![];

                    ip += 1;
                    while ip < tokens.len() {
                        let tok = &tokens[ip];

                        match tok {
                            Token::WordToken(v) => {
                                if v.value == "end" {
                                    break;
                                }
                                type_tokens.push(v.clone());
                            }
                            _ => err(
                                tok.get_location(),
                                "Expected a type or end but found something else",
                            ),
                        }

                        ip += 1;
                    }

                    if ip >= tokens.len() {
                        err(
                            &program.get_last_loc(),
                            "Expected a type or end, but found nothing",
                        );
                    }

                    let mut types =
                        Type::from_tokens(&type_tokens, &structs, &program, &type_aliases);

                    let typ = if let Some(typ) = types.pop() {
                        typ
                    } else {
                        err(
                            tokens[ip].get_location(),
                            "Expected only one type but found multiple",
                        );
                    };

                    type_aliases.insert(name.value.clone(), (typ, w.value == "hard_type"));
                } else if let Some(mut ops) = structs.get_ops_from_word(&w.value, &w.loc, &program)
                {
                    ensure_in_fn!(&w.loc);
                    program.ops.append(&mut ops);
                } else if let Some(mut ops) =
                    mems.get_ops_from_word(&w.value, &program, &structs, &w.loc)
                {
                    ensure_in_fn!(&w.loc);
                    program.ops.append(&mut ops);
                } else if let Some(id) = procs.get(&w.value) {
                    let contract = program.contracts.get(id).unwrap();
                    if contract.attributes.has_attribute("deprecated") {
                        warn(&w.loc, format!("{} is marked as deprecated", w.value));
                    }
                    ensure_in_fn!(&w.loc);
                    if contract.attributes.has_attribute("__provided_externally__") {
                        program.ops.push(Operation::call_proc(
                            w.loc,
                            *id,
                            Some(contract
                                .attributes
                                .get_value("__provided_externally__")
                                .map(|f| GlobalString::new(f))
                                .unwrap_or(GlobalString::new(&w.value))),
                        ));
                    } else {
                        program.ops.push(Operation::call_proc(w.loc, *id, None));
                    }
                } else if let Some(constant) = consts.get(&w.value) {
                    ensure_in_fn!(&w.loc);
                    program
                        .ops
                        .push(Operation::push_const(w.loc, constant.clone()));
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
                    program.ops.push(Operation::ret(w.loc, false));
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

        let update_lastloc = tokens_to_insert.len() > 0;
        while let Some(tok) = tokens_to_insert.pop() {
            tokens.insert(ip, tok);
        }
        if update_lastloc {
            program.loc_eof.file = tokens[tokens.len() - 1].get_location().file.clone();
        }
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
