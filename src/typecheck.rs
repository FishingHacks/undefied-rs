use std::{
    collections::HashMap,
    fmt::{Debug, Display},
    process::exit,
};

use crate::{
    error::{check_type_mismatch, check_type_mismatch_multiple, err, info, not_enough_types},
    parser::{
        CallProc, Cast, Constant, Intrinsic, Keyword, OpIntrinsic, OpKeyword, Operation, Proc,
        ProcedureContract, Program, PushAssembly, PushConst, PushFnPtr, PushInt, PushLocalMem,
        PushMem, PushStr, Ret, Structs, Typefence,
    },
    tokenizer::{try_parse_num, Loc, WordToken},
    GlobalString,
};

#[derive(Clone)]
pub enum Type {
    Int,
    Bool,
    Ptr,
    Any,
    T1,
    T2,
    T3,
    T4,
    T5,
    T6,
    T7,
    T8,
    T9,
    PtrTo(Box<Type>),
    Array(Box<Type>, usize),
    Sized(usize),
    Named(GlobalString, Box<Type>),
    Struct(GlobalString, usize, usize),
    FnPtr(Vec<Type>, Vec<Type>),
}

impl PartialEq for Type {
    fn eq(&self, other: &Self) -> bool {
        self.equal_to_strict(other)
    }
}

impl Type {
    pub fn get_size(&self) -> usize {
        match self {
            Self::Bool => 1,
            Self::Sized(v) => *v,
            Self::Struct(_, _, sz) => *sz,
            Self::Array(typ, elements) => typ.get_size() * elements,
            Self::Named(_, typ) => typ.get_size(),
            _ => 8,
        }
    }

    pub fn equal_to(&self, equal_to: &Self) -> bool {
        matches!(self, Type::Any)
            || matches!(equal_to, Type::Any)
            || self.__check_eq(equal_to, false)
    }

    pub fn equal_to_strict(&self, equal_to: &Self) -> bool {
        matches!(self, Type::Any)
            || matches!(equal_to, Type::Any)
            || self.__check_eq(equal_to, true)
    }

    fn __unsafe_to_u8(&self) -> u8 {
        match self {
            Self::Int => 1,
            Self::Bool => 2,
            Self::Ptr => 3,
            Self::Any => 4,
            Self::T1 => 5,
            Self::T2 => 6,
            Self::T3 => 7,
            Self::T4 => 8,
            Self::T5 => 9,
            Self::T6 => 10,
            Self::T7 => 11,
            Self::T8 => 12,
            Self::T9 => 13,
            _ => panic!(),
        }
    }

    fn to_u8(&self) -> u8 {
        match self {
            Self::Int => 1,
            Self::Bool => 2,
            Self::Ptr => 3,
            Self::Any => 4,
            Self::T1 => 5,
            Self::T2 => 6,
            Self::T3 => 7,
            Self::T4 => 8,
            Self::T5 => 9,
            Self::T6 => 10,
            Self::T7 => 11,
            Self::T8 => 12,
            Self::T9 => 13,
            Self::PtrTo(..) => 14,
            Self::Sized(..) => 15,
            Self::Struct(..) => 16,
            Self::Array(..) => 17,
            Self::Named(..) => 18,
            Self::FnPtr(..) => 19,
        }
    }

    fn __check_eq(&self, other: &Self, strict: bool) -> bool {
        match self {
            Self::PtrTo(self_inner) => match &other {
                &Self::PtrTo(other_inner) => {
                    if strict {
                        Type::equal_to_strict(&self_inner, &other_inner)
                    } else {
                        Type::equal_to(&self_inner, &other_inner)
                    }
                }
                Self::Ptr => strict,
                _ => false,
            },
            Self::Sized(v) => *v == other.get_size(),
            Self::Struct(_, self_id, ..) => match other {
                Self::Struct(_, other_id, ..) => self_id == other_id,
                _ => false,
            },
            Self::Array(typ, elements) => match other {
                Self::Array(typ_other, elements_other) => {
                    typ.equal_to(&typ_other) && *elements == *elements_other
                }
                _ => false,
            },
            Self::Named(name_self, ..) => match other {
                Self::Named(name_other, ..) => name_self.get().eq(name_other.get()),
                _ => false,
            },
            Self::FnPtr(self_in, self_out) => match other {
                Self::FnPtr(other_in, other_out) => other_in == self_in && other_out == self_out,
                _ => false,
            },
            _ => match other {
                Self::PtrTo(..)
                | Self::Struct(..)
                | Self::Array(..)
                | Self::Named(..)
                | Self::FnPtr(..) => false,
                Self::Sized(sz) => self.get_size() == *sz,
                _ => self.__unsafe_to_u8() == other.__unsafe_to_u8(),
            },
        }
    }

    fn non_ptrto_from_str(string: &String) -> Option<Self> {
        match string.as_str() {
            "bool" => Some(Self::Bool),
            "int" => Some(Self::Int),
            "ptr" => Some(Self::Ptr),
            "any" => Some(Self::Any),
            "T1" => Some(Self::T1),
            "T2" => Some(Self::T2),
            "T3" => Some(Self::T3),
            "T4" => Some(Self::T4),
            "T5" => Some(Self::T5),
            "T6" => Some(Self::T6),
            "T7" => Some(Self::T7),
            "T8" => Some(Self::T8),
            "T9" => Some(Self::T9),
            _ => None,
        }
    }

    pub fn is_generic(&self) -> bool {
        matches!(
            self,
            Self::T1
                | Self::T2
                | Self::T3
                | Self::T4
                | Self::T5
                | Self::T6
                | Self::T7
                | Self::T8
                | Self::T9
        )
    }

    pub fn is_non_stack_size(&self) -> bool {
        self.get_size() > 8 || matches!(self, Self::Struct(..) | Self::Array(..))
    }

    pub fn has_generics(types: &Vec<Self>) -> bool {
        for typ in types {
            if typ.is_generic() {
                return true;
            }
        }
        false
    }

    pub fn has_non_stack_sized_types(types: &Vec<Self>) -> bool {
        for typ in types {
            if typ.is_non_stack_size() {
                return true;
            }
        }
        false
    }

    fn get_inner<'a>(&'a self) -> &'a Type {
        match self {
            Self::Named(_, typ) => typ.get_inner(),
            other => other,
        }
    }

    pub fn from_tokens(
        tokens: &Vec<WordToken>,
        structs: &Structs,
        program: &Program,
        type_aliases: &HashMap<String, (Type, bool)>,
    ) -> Vec<Self> {
        let mut types: Vec<Self> = vec![];

        let mut is_recording_fn = false;
        let mut is_collecting_out = false;

        let mut in_types: Vec<WordToken> = vec![];
        let mut out_types: Vec<WordToken> = vec![];

        for WordToken { loc, value } in tokens {
            if is_recording_fn {
                if value == "--" && !is_collecting_out {
                    is_collecting_out = true;
                } else if value == "--" && is_collecting_out {
                    err(loc, "Return types are already being specified");
                } else if value == "end" {
                    is_recording_fn = false;
                    is_collecting_out = false;

                    types.push(Type::FnPtr(
                        Type::from_tokens(&in_types, structs, program, type_aliases),
                        Type::from_tokens(&out_types, structs, program, type_aliases),
                    ));

                    in_types.clear();
                    out_types.clear();
                } else if is_collecting_out {
                    out_types.push(WordToken {
                        loc: *loc,
                        value: value.clone(),
                    })
                } else {
                    in_types.push(WordToken {
                        loc: *loc,
                        value: value.clone(),
                    })
                }

                continue;
            }

            if let Some(typ) = Self::non_ptrto_from_str(value) {
                types.push(typ);
            } else if value == "ptr-to" {
                if let Some(typ) = types.pop() {
                    types.push(Self::PtrTo(Box::new(typ)));
                } else {
                    err(loc, "ptr-to has to follow a type it points to");
                }
            } else if let Some(struct_id) = structs.get_struct_id(&value) {
                types.push(Type::Struct(
                    GlobalString::new(value),
                    struct_id,
                    program.get_struct_size(struct_id).unwrap(),
                ));
            } else if let Some((typ, is_hard_type)) = type_aliases.get(value) {
                if !is_hard_type {
                    let typ = typ.get_inner();
                    types.push(typ.clone());
                } else {
                    types.push(Self::Named(GlobalString::new(value), Box::new(typ.clone())));
                }
            } else if let Some(len) = Type::get_array(&value, loc) {
                if let Some(typ) = types.pop() {
                    types.push(Self::Array(Box::new(typ), len));
                } else {
                    err(loc, "array(...) has to follow a type it points to");
                }
            } else if value == "fn" {
                is_recording_fn = true;
            } else {
                err(loc, format!("Unknown type: {}", value));
            }
        }

        if is_recording_fn {
            err(
                &tokens[tokens.len() - 1].loc,
                "Expected a type, `--` or end, but found nothing",
            );
        }

        return types;
    }

    fn get_array(str: &str, loc: &Loc) -> Option<usize> {
        if !str.starts_with("array(") || !str.ends_with(')') {
            return None;
        }
        if str.starts_with("array(-") {
            err(loc, "Arrays cannot have negative amount of elements");
        }
        if let Some(num) = try_parse_num(&str[6..str.len() - 1]) {
            if num < 1 {
                err(loc, "Arrays cannot have 0 amount of elements");
            }
            Some(num as usize)
        } else {
            None
        }
    }

    pub fn to_str(&self) -> String {
        match self {
            Self::Bool => "bool".to_string(),
            Self::Int => "int".to_string(),
            Self::Ptr => "ptr".to_string(),
            Self::Any => "any".to_string(),
            Self::T1 => "T1".to_string(),
            Self::T2 => "T2".to_string(),
            Self::T3 => "T3".to_string(),
            Self::T4 => "T4".to_string(),
            Self::T5 => "T5".to_string(),
            Self::T6 => "T6".to_string(),
            Self::T7 => "T7".to_string(),
            Self::T8 => "T8".to_string(),
            Self::T9 => "T9".to_string(),
            Self::Sized(v) => format!("Sized Type<{v}>"),
            Self::Struct(v, ..) => v.get().clone(),
            Self::PtrTo(other) => format!("{} ptr-to", other.to_str()),
            Self::Array(other_type, elements) => {
                format!("{} array({})", other_type.to_str(), *elements)
            }
            Self::Named(gstr, ..) => gstr.get().clone(),
            Self::FnPtr(in_types, out_types) if out_types.len() > 0 => format!(
                "fn {} -- {} end",
                type_vec_to_str(in_types),
                type_vec_to_str(out_types)
            ),
            Self::FnPtr(in_types, ..) => format!("fn {} end", type_vec_to_str(in_types)),
        }
    }

    pub fn get_op_load_store(&self, load: bool) -> Option<Intrinsic> {
        if load {
            match self.get_size() {
                1 => Some(Intrinsic::Load8),
                2 => Some(Intrinsic::Load16),
                4 => Some(Intrinsic::Load32),
                8 => Some(Intrinsic::Load64),
                _ => None,
            }
        } else {
            match self.get_size() {
                1 => Some(Intrinsic::Store8),
                2 => Some(Intrinsic::Store16),
                4 => Some(Intrinsic::Store32),
                8 => Some(Intrinsic::Store64),
                _ => None,
            }
        }
    }
}

pub fn type_vec_to_str(vec: &Vec<Type>) -> String {
    if vec.len() < 1 {
        return "<void>".to_string();
    }
    vec.iter()
        .map(|f| f.to_str().to_string())
        .reduce(|a, b| format!("{a} {b}"))
        .unwrap_or_default()
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.to_str())
    }
}
impl Debug for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.to_str())
    }
}

#[derive(Clone, Debug)]
pub struct TypecheckType {
    pub typ: Type,
    pub loc: Loc,
}

impl Display for TypecheckType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{}: {}", self.loc, self.typ))
    }
}

impl TypecheckType {
    fn new(typ: Type, loc: Loc) -> Self {
        Self { typ, loc }
    }
}

struct StackSnapshot {
    stack: Vec<TypecheckType>,
    keyword: Keyword,
    loc: Loc,
}

fn is_stack_equal(a: &Vec<TypecheckType>, b: &Vec<TypecheckType>) -> bool {
    if a.len() != b.len() {
        return false;
    }

    for i in 0..a.len() {
        if !a[i].typ.equal_to(&b[i].typ) {
            return false;
        }
    }

    true
}

struct ProcBody {
    id: usize,
    loc: Loc,
}

struct TypecheckResult {
    func_bodies: Vec<ProcBody>,
    stack: Vec<TypecheckType>,
}

fn check_typefence(
    stack: &mut Vec<TypecheckType>,
    expected: &Vec<Type>,
    loc: &Loc,
    pop_values: bool,
) {
    if stack.len() < expected.len() {
        err(
            loc,
            format!(
                "Stack doesnt have the right elements; Expected: {}, found: {}",
                type_vec_to_str(expected),
                stringify_stack(stack)
            ),
        );
    }
    let print_stack = stack.clone();
    let mut i: usize = 1;
    for v in expected.iter().rev() {
        let stack_v = if pop_values {
            stack.pop().unwrap()
        } else {
            stack[stack.len() - i].clone()
        };
        if !stack_v.typ.equal_to(v) {
            err(
                loc,
                format!(
                    "Stack doesnt have the right elements, Expected: {}, found:\n{}",
                    type_vec_to_str(expected),
                    stringify_stack(&print_stack)
                ),
            );
        }
        i += 1;
    }
}

fn check_proctypefence(
    stack: &mut Vec<TypecheckType>,
    expected_types: &Vec<Type>,
    return_types: &Vec<Type>,
    loc: &Loc,
) {
    let mut types: [Option<Type>; 9] = [None, None, None, None, None, None, None, None, None];
    if stack.len() < expected_types.len() {
        err(
            loc,
            format!(
                "Stack doesn't have the right elements, expected: {}, found:\n{}",
                type_vec_to_str(expected_types),
                stringify_stack(stack)
            ),
        );
    }
    let print_stack = stack
        .split_at(stack.len() - expected_types.len())
        .1
        .to_vec();

    for t in expected_types.iter().rev() {
        let stack_type = stack.pop().unwrap().typ;

        if t.is_generic() {
            let idx = (t.to_u8() - Type::T1.to_u8()) as usize;
            if let Some(typ) = &types[idx] {
                if !stack_type.equal_to_strict(typ) {
                    err(
                        loc,
                        format!(
                            "Stack doesn't have the right elements, expected: {}, found:\n{}",
                            type_vec_to_str(expected_types),
                            stringify_stack(&print_stack)
                        ),
                    );
                }
            } else {
                types[idx] = Some(stack_type);
            }
        } else if !stack_type.equal_to(t) {
            err(
                loc,
                format!(
                    "Stack doesn't have the right elements, expected: {}, found:\n{}",
                    type_vec_to_str(expected_types),
                    stringify_stack(&print_stack)
                ),
            );
        }
    }

    for typ in return_types.iter() {
        if typ.is_generic() {
            if let Some(typ) = &types[(typ.to_u8() - Type::T1.to_u8()) as usize] {
                stack.push(TypecheckType::new(typ.clone(), loc.clone()));
            } else {
                err(
                    loc,
                    format!("Could not infer returntype for {}", typ.to_str()),
                )
            }
        } else {
            stack.push(TypecheckType::new(typ.clone(), loc.clone()));
        }
    }
}

fn typecheck_(
    program: &mut Program,
    stack: &Vec<TypecheckType>,
    expected_return_val: Option<&Vec<Type>>,
    current_contract: Option<&ProcedureContract>,
    start: usize,
) -> TypecheckResult {
    let mut stack: Vec<TypecheckType> = stack.clone();
    let mut stack_snapshots: Vec<StackSnapshot> = Vec::new();
    let mut fn_bodies: Vec<ProcBody> = vec![];

    let mut ip: usize = start;

    while ip < program.ops.len() {
        let op = &program.ops[ip];
        match op {
            Operation::None(..) => {}
            Operation::Cast(Cast { loc, typ }) => {
                if stack.is_empty() {
                    not_enough_types(loc, 1, stack.len());
                }

                stack.pop();
                stack.push(TypecheckType::new(typ.clone(), loc.clone()));
            }
            Operation::CallProc(CallProc { id, loc, .. }) => {
                let contract = if let Some(v) = program.contracts.get(id) {
                    v
                } else {
                    err(&loc, "Could not find contract for the function call");
                };
                check_proctypefence(&mut stack, &contract.in_types, &contract.out_types, loc);
            }
            Operation::Proc(Proc { id, loc, .. }) => {
                while ip < program.ops.len() {
                    if let Operation::Ret(Ret { is_end: true, .. }) = program.ops[ip] {
                        break;
                    }
                    ip += 1;
                }
                if ip >= program.ops.len() {
                    err(program.get_last_loc(), "Could not find end of procedure");
                }
                fn_bodies.push(ProcBody {
                    id: *id,
                    loc: loc.clone(),
                });
            }
            Operation::Ret(Ret { is_end, loc, .. }) => {
                if current_contract.is_none() {
                    err(loc, "Reached return but no contract (typecheck)");
                }
                if let Some(expected_return_val) = expected_return_val {
                    check_typefence(&mut stack, expected_return_val, loc, false);
                    if *is_end {
                        return TypecheckResult {
                            func_bodies: fn_bodies,
                            stack,
                        };
                    } else {
                        stack = stack_snapshots[stack_snapshots.len() - 1].stack.clone();
                    }
                } else {
                    err(loc, "Reached return but no contract (typecheck)");
                }
            }
            Operation::PushConst(PushConst {
                constant: Constant { typ, .. },
                loc,
            }) => stack.push(TypecheckType::new(typ.clone(), loc.clone())),
            Operation::Assembly(PushAssembly {
                expected_types,
                return_types,
                loc,
                ..
            }) => {
                check_typefence(&mut stack, expected_types, loc, true);
                for typ in return_types {
                    stack.push(TypecheckType::new(typ.clone(), loc.clone()));
                }
            }
            Operation::Typefence(Typefence {
                expected_types,
                loc,
            }) => check_typefence(&mut stack, expected_types, loc, false),
            Operation::PushInt(PushInt { loc, .. }) => {
                stack.push(TypecheckType::new(Type::Int, loc.clone()))
            }
            Operation::PushPtr(PushInt { loc, .. }) => {
                stack.push(TypecheckType::new(Type::Ptr, loc.clone()))
            }
            Operation::PushMem(PushMem { loc, typ, .. })
            | Operation::PushLocalMem(PushLocalMem { loc, typ, .. }) => {
                if matches!(typ, Type::Any) {
                    stack.push(TypecheckType::new(Type::Ptr, loc.clone()))
                } else {
                    stack.push(TypecheckType::new(
                        Type::PtrTo(Box::new(typ.clone())),
                        loc.clone(),
                    ))
                }
            }
            Operation::PushStr(PushStr { loc, is_cstr, .. }) => {
                if !is_cstr {
                    stack.push(TypecheckType::new(Type::Int, loc.clone()));
                }
                stack.push(TypecheckType::new(Type::Ptr, loc.clone()));
            }
            Operation::Intrinsic(OpIntrinsic { loc, op }) => match op {
                Intrinsic::Print => {
                    if stack.is_empty() {
                        not_enough_types(loc, 1, stack.len())
                    }
                    stack.pop();
                }
                Intrinsic::Here => {
                    stack.push(TypecheckType::new(Type::Int, loc.clone()));
                    stack.push(TypecheckType::new(Type::Ptr, loc.clone()));
                }
                Intrinsic::CastPtrToPlus => {
                    if let Some(TypecheckType { loc, typ }) = stack.pop() {
                        stack.push(TypecheckType::new(Type::PtrTo(Box::new(typ)), loc));
                    } else {
                        not_enough_types(loc, 1, stack.len());
                    }
                }
                Intrinsic::CastPtrToMinus => {
                    if let Some(TypecheckType { loc: typ_loc, typ }) = stack.pop() {
                        match typ {
                            Type::PtrTo(typ) => {
                                stack.push(TypecheckType::new(typ.as_ref().clone(), typ_loc));
                            }
                            _ => err(
                                loc,
                                format!("Expected a ptr-to, but found {}", typ.to_str()),
                            ),
                        }
                    } else {
                        not_enough_types(loc, 1, stack.len());
                    }
                }
                Intrinsic::Plus
                | Intrinsic::Minus
                | Intrinsic::Mul
                | Intrinsic::Shl
                | Intrinsic::Shr => {
                    if stack.len() < 2 {
                        not_enough_types(loc, 2, stack.len());
                    }
                    let a = stack.pop().unwrap();
                    let b = stack.pop().unwrap();
                    check_type_mismatch(loc, &Type::Int, &a.typ);
                    check_type_mismatch(loc, &Type::Int, &b.typ);
                    stack.push(TypecheckType::new(Type::Int, loc.clone()));
                }
                Intrinsic::StructPtrPlus => {
                    if stack.len() < 2 {
                        not_enough_types(loc, 2, stack.len());
                    }
                    let a = stack.pop().unwrap();
                    let b = stack.pop().unwrap();
                    check_type_mismatch(loc, &Type::Ptr, &a.typ);
                    check_type_mismatch(loc, &Type::Ptr, &b.typ);
                    stack.push(TypecheckType::new(Type::Ptr, loc.clone()));
                }
                Intrinsic::DivMod => {
                    if stack.len() < 2 {
                        not_enough_types(loc, 2, stack.len());
                    }
                    let a = stack.pop().unwrap();
                    let b = stack.pop().unwrap();
                    check_type_mismatch(loc, &Type::Int, &a.typ);
                    check_type_mismatch(loc, &Type::Int, &b.typ);
                    stack.push(TypecheckType::new(Type::Int, loc.clone()));
                    stack.push(TypecheckType::new(Type::Int, loc.clone()));
                }
                Intrinsic::LessThan
                | Intrinsic::LessThanEqual
                | Intrinsic::GreaterThan
                | Intrinsic::GreaterThanEqual
                | Intrinsic::Equal
                | Intrinsic::NotEqual => {
                    if stack.len() < 2 {
                        not_enough_types(loc, 2, stack.len());
                    }
                    let a = stack.pop().unwrap();
                    let b = stack.pop().unwrap();
                    if !a.typ.equal_to(&b.typ) {
                        err(loc, "Expected the type of lhs to be the type of rhs");
                    }
                    stack.push(TypecheckType::new(Type::Bool, loc.clone()));
                }
                Intrinsic::Drop => {
                    if stack.is_empty() {
                        not_enough_types(loc, 1, stack.len());
                    }
                    stack.pop();
                }
                Intrinsic::Dup => {
                    if stack.is_empty() {
                        not_enough_types(loc, 1, stack.len());
                    }
                    let typ = stack.pop().unwrap();
                    stack.push(typ.clone());
                    stack.push(typ);
                }
                Intrinsic::Over => {
                    if stack.len() < 2 {
                        not_enough_types(loc, 2, stack.len());
                    }
                    let a = stack.pop().unwrap();
                    let b = stack.pop().unwrap();
                    stack.push(b.clone());
                    stack.push(a);
                    stack.push(b);
                }
                Intrinsic::Swap => {
                    if stack.len() < 2 {
                        not_enough_types(loc, 2, stack.len());
                    }
                    let a = stack.pop().unwrap();
                    let b = stack.pop().unwrap();
                    stack.push(a);
                    stack.push(b);
                }
                Intrinsic::Rot => {
                    if stack.len() < 3 {
                        not_enough_types(loc, 3, stack.len());
                    }
                    let a = stack.pop().unwrap();
                    let b = stack.pop().unwrap();
                    let c = stack.pop().unwrap();
                    stack.push(a);
                    stack.push(c);
                    stack.push(b);
                }
                Intrinsic::Syscall0 => {
                    if stack.is_empty() {
                        not_enough_types(loc, 1, stack.len());
                    }
                    check_type_mismatch(loc, &Type::Int, &stack.pop().unwrap().typ);
                    stack.push(TypecheckType::new(Type::Int, loc.clone()));
                }
                Intrinsic::Syscall1 => {
                    if stack.len() < 2 {
                        not_enough_types(loc, 2, stack.len());
                    }
                    check_type_mismatch(loc, &Type::Int, &stack.pop().unwrap().typ);
                    for _ in 1..2 {
                        stack.pop();
                    }
                    stack.push(TypecheckType::new(Type::Int, loc.clone()));
                }
                Intrinsic::Syscall2 => {
                    if stack.len() < 3 {
                        not_enough_types(loc, 3, stack.len());
                    }
                    check_type_mismatch(loc, &Type::Int, &stack.pop().unwrap().typ);
                    for _ in 1..3 {
                        stack.pop();
                    }
                    stack.push(TypecheckType::new(Type::Int, loc.clone()));
                }
                Intrinsic::Syscall3 => {
                    if stack.len() < 4 {
                        not_enough_types(loc, 4, stack.len());
                    }
                    check_type_mismatch(loc, &Type::Int, &stack.pop().unwrap().typ);
                    for _ in 1..4 {
                        stack.pop();
                    }
                    stack.push(TypecheckType::new(Type::Int, loc.clone()));
                }
                Intrinsic::Syscall4 => {
                    if stack.len() < 5 {
                        not_enough_types(loc, 5, stack.len());
                    }
                    check_type_mismatch(loc, &Type::Int, &stack.pop().unwrap().typ);
                    for _ in 1..5 {
                        stack.pop();
                    }
                    stack.push(TypecheckType::new(Type::Int, loc.clone()));
                }
                Intrinsic::Syscall5 => {
                    if stack.len() < 6 {
                        not_enough_types(loc, 6, stack.len());
                    }
                    check_type_mismatch(loc, &Type::Int, &stack.pop().unwrap().typ);
                    for _ in 1..6 {
                        stack.pop();
                    }
                    stack.push(TypecheckType::new(Type::Int, loc.clone()));
                }
                Intrinsic::Syscall6 => {
                    if stack.len() < 7 {
                        not_enough_types(loc, 7, stack.len());
                    }
                    check_type_mismatch(loc, &Type::Int, &stack.pop().unwrap().typ);
                    for _ in 1..7 {
                        stack.pop();
                    }
                    stack.push(TypecheckType::new(Type::Int, loc.clone()));
                }
                Intrinsic::Or | Intrinsic::And | Intrinsic::Xor => {
                    if stack.is_empty() {
                        not_enough_types(loc, 1, stack.len());
                    }
                    let a = stack.pop().unwrap().typ;
                    let b = stack.pop().unwrap().typ;
                    check_type_mismatch_multiple(loc, &[Type::Int, Type::Bool], &a);
                    check_type_mismatch_multiple(loc, &[Type::Int, Type::Bool], &b);
                    check_type_mismatch(loc, &a, &b);
                    stack.push(TypecheckType::new(a, loc.clone()));
                }
                Intrinsic::Not => {
                    if stack.is_empty() {
                        not_enough_types(loc, 1, stack.len());
                    }
                    let a = stack.pop().unwrap().typ;
                    check_type_mismatch_multiple(loc, &[Type::Int, Type::Bool], &a);
                    stack.push(TypecheckType::new(a.clone(), loc.clone()));
                }
                Intrinsic::CastBool => {
                    if stack.is_empty() {
                        not_enough_types(loc, 1, stack.len());
                    }
                    stack.pop();
                    stack.push(TypecheckType::new(Type::Bool, loc.clone()));
                }
                Intrinsic::CastInt => {
                    if stack.is_empty() {
                        not_enough_types(loc, 1, stack.len());
                    }
                    stack.pop();
                    stack.push(TypecheckType::new(Type::Int, loc.clone()));
                }
                Intrinsic::CastPtr => {
                    if stack.is_empty() {
                        not_enough_types(loc, 1, stack.len());
                    }
                    stack.pop();
                    stack.push(TypecheckType::new(Type::Ptr, loc.clone()));
                }
                Intrinsic::StackInfo => {
                    println!("Stackinfo at {loc}:");
                    print!("{}", stringify_stack(&stack));
                    exit(0);
                }
                Intrinsic::Load8 | Intrinsic::Load16 | Intrinsic::Load32 | Intrinsic::Load64 => {
                    if stack.is_empty() {
                        not_enough_types(loc, 1, stack.len());
                    }
                    check_type_mismatch(loc, &Type::Ptr, &stack.pop().unwrap().typ);
                    stack.push(TypecheckType::new(Type::Int, loc.clone()));
                }
                Intrinsic::Store8
                | Intrinsic::Store16
                | Intrinsic::Store32
                | Intrinsic::Store64 => {
                    if stack.len() < 2 {
                        not_enough_types(loc, 2, stack.len());
                    }
                    check_type_mismatch(loc, &Type::Ptr, &stack.pop().unwrap().typ);
                    check_type_mismatch(loc, &Type::Int, &stack.pop().unwrap().typ);
                }
                Intrinsic::Load => {
                    if stack.is_empty() {
                        not_enough_types(loc, 1, stack.len());
                    }

                    let typ = stack.pop().unwrap();
                    let unwrapped = match typ.typ {
                        Type::PtrTo(t) => *t,
                        _ => err(
                            &typ.loc,
                            "@ expects a ptr to a type, not a unspecific pointer",
                        ),
                    };
                    let intrinsic = match unwrapped.get_op_load_store(true) {
                        Some(v) => v,
                        None => err(
                            loc,
                            format!("Could not get a load intrinsic for {}", unwrapped),
                        ),
                    };
                    stack.push(TypecheckType::new(unwrapped, loc.clone()));

                    program.ops[ip] = Operation::intrinsic(loc.clone(), intrinsic);
                }
                Intrinsic::Store => {
                    if stack.len() < 2 {
                        not_enough_types(loc, 1, stack.len());
                    }

                    let ptr_typ = stack.pop().unwrap();
                    let unwrapped = match ptr_typ.typ {
                        Type::PtrTo(t) => *t,
                        _ => err(loc, "! expects a ptr to a type, not a unspecific pointer"),
                    };
                    let intrinsic = match unwrapped.get_op_load_store(false) {
                        Some(v) => v,
                        None => err(
                            loc,
                            format!("Could not get a store intrinsic for {}", unwrapped),
                        ),
                    };

                    let storing_typ = stack.pop().unwrap();

                    if !unwrapped.equal_to_strict(&storing_typ.typ) {
                        err(loc, "! expects the type of the value and the type of the pointer to be of the same type");
                    }

                    program.ops[ip] = Operation::intrinsic(loc.clone(), intrinsic);
                }
                Intrinsic::CallFnPtr => {
                    if stack.is_empty() {
                        not_enough_types(loc, 1, stack.len());
                    }
                    let top = stack.pop().unwrap();
                    let (in_types, out_types) = match top.typ {
                        Type::FnPtr(in_types, out_types) => (in_types, out_types),
                        typ => err(
                            loc,
                            format!("Expected a function pointer type, but found {}", typ),
                        ),
                    };

                    check_proctypefence(&mut stack, &in_types, &out_types, loc);
                }
            },
            Operation::Keyword(OpKeyword { keyword, loc, .. }) => match keyword {
                Keyword::If => {
                    if stack.is_empty() {
                        not_enough_types(loc, 1, 0);
                    }
                    check_type_mismatch(loc, &Type::Bool, &stack.pop().unwrap().typ);
                    stack_snapshots.push(StackSnapshot {
                        stack: stack.clone(),
                        keyword: Keyword::If,
                        loc: loc.clone(),
                    });
                }
                Keyword::Else => {
                    let snapshot = match stack_snapshots.pop() {
                        Some(v) => v,
                        None => err(loc, "Expected to find a stack snapshot, but didnt find one"),
                    };
                    if snapshot.keyword != Keyword::If {
                        err(loc, "Snapshot not initiated by if; unclosed block");
                    }
                    stack_snapshots.push(StackSnapshot {
                        stack: stack.clone(),
                        keyword: Keyword::If,
                        loc: loc.clone(),
                    });
                    stack = snapshot.stack;
                }
                Keyword::While => {
                    stack_snapshots.push(StackSnapshot {
                        stack: stack.clone(),
                        keyword: Keyword::While,
                        loc: loc.clone(),
                    });
                }
                Keyword::Do => {
                    if stack_snapshots.is_empty() {
                        err(
                            loc,
                            "Expected to have a snapshot from while, but didnt find anything",
                        );
                    }
                    if stack.is_empty() {
                        not_enough_types(loc, 1, 0);
                    }
                    check_type_mismatch(loc, &Type::Bool, &stack.pop().unwrap().typ);
                    let top_snapshot = &stack_snapshots[stack_snapshots.len() - 1];
                    if !is_stack_equal(&top_snapshot.stack, &stack) {
                        err(loc, format!(
                            "After do, the stack has to have the same state as before the while (Before: {}, After: {})",
                            stringify_stack(&top_snapshot.stack), stringify_stack(&stack)))
                    }
                }
                Keyword::IfStar => {
                    if stack.is_empty() {
                        err(loc, "Expected a bool, but found nothing");
                    }
                    check_type_mismatch(loc, &Type::Bool, &stack.pop().unwrap().typ);
                    stack_snapshots.push(StackSnapshot {
                        keyword: Keyword::If,
                        stack: stack.clone(),
                        loc: loc.clone(),
                    });
                }
                Keyword::End => {
                    let snapshot = match stack_snapshots.pop() {
                        Some(v) => v,
                        None => err(loc, "Expected to find a stack snapshot, but didnt find one"),
                    };
                    match snapshot.keyword {
                        Keyword::If => {
                            if !is_stack_equal(&snapshot.stack, &stack) {
                                err(&snapshot.loc,
                                    format!("Stack after the if-block does not match the stack before the if-block (Before: {}, After: {})\n\nDid you mean to add an else to the if-block?",
                                    stringify_stack(&snapshot.stack), stringify_stack(&stack)));
                            }
                        }
                        Keyword::While => {
                            if !is_stack_equal(&snapshot.stack, &stack) {
                                err(&snapshot.loc,
                                    format!("Stack after the while-block does not match the stack before the if-block (Before: {}, After: {})",
                                    stringify_stack(&snapshot.stack), stringify_stack(&stack)));
                            }
                        }
                        Keyword::End | Keyword::Do | Keyword::Else | Keyword::IfStar => {
                            panic!("Unreachable")
                        }
                    }
                }
            },
            Operation::PushFnPtr(PushFnPtr { contract_id, loc }) => {
                let contract = match program.contracts.get(&contract_id) {
                    Some(v) => v,
                    None => err(loc, "Could not get the contract for the function"),
                };

                stack.push(TypecheckType::new(
                    Type::FnPtr(contract.in_types.clone(), contract.out_types.clone()),
                    *loc,
                ))
            }
        }

        ip += 1;
    }

    TypecheckResult {
        stack,
        func_bodies: fn_bodies,
    }
}

pub fn typecheck(program: &mut Program) {
    let TypecheckResult { func_bodies, stack } = typecheck_(program, &vec![], None, None, 0);
    if stack.len() > 0 {
        err(
            program.get_last_loc(),
            format!("Found data on the stack: {}", stringify_stack(&stack)),
        );
    }
    for proc in func_bodies {
        let contract = program
            .contracts
            .get(&proc.id)
            .unwrap_or_else(|| {
                err(
                    &proc.loc,
                    "Could not acquire contract of function. (typecheck)",
                )
            })
            .clone();
        if contract.attributes.has_attribute("__typecheck_ignore__") {
            info(&proc.loc, "UNSAFE: This function is not being typechecked");
            continue;
        } else if contract.attributes.has_attribute("__provided_externally__") {
            continue;
        }
        let stack = contract
            .in_types
            .iter()
            // .rev()
            .map(|typ| TypecheckType::new(typ.clone(), contract.loc.clone()))
            .collect::<Vec<TypecheckType>>();
        let TypecheckResult {
            func_bodies: fn_bodies,
            stack,
        } = typecheck_(
            program,
            &stack,
            Some(&contract.out_types),
            Some(&contract),
            *program
                .refs
                .get(&proc.id)
                .unwrap_or_else(|| err(&proc.loc, "couldnt get reference for proc id"))
                + 1,
        );

        if fn_bodies.len() > 0 {
            println!(
                "definitions at:\n{}",
                fn_bodies
                    .iter()
                    .map(|b| format!("{}", b.loc))
                    .reduce(|a, b| format!("{a}\n{b}"))
                    .unwrap_or_default()
            );
            err(&contract.loc, "Function definition in this function");
        }
        if stack.len() != contract.out_types.len()
            || stack
                .iter()
                .zip(&contract.out_types)
                .map(|(a, b)| !b.equal_to(&a.typ))
                .any(|f| f)
        {
            err(
                &contract.loc,
                format!(
                    "Expected: {}, found:\n{}",
                    type_vec_to_str(&contract.out_types),
                    stringify_stack(&stack)
                ),
            )
        }
    }
}

fn stringify_stack(stack: &Vec<TypecheckType>) -> String {
    if stack.is_empty() {
        return "<empty>".to_string();
    }
    stack
        .iter()
        .map(|f| format!("{} at {}", f.typ, f.loc))
        .reduce(|a, b| format!("{a}\n{b}"))
        .unwrap_or_default()
}
