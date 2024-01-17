use std::{
    fmt::{Debug, Display},
    process::exit,
};

use crate::{
    error::{check_type_mismatch, check_type_mismatch_multiple, err, info, not_enough_types},
    parser::{
        CallProc, Constant, Intrinsic, Keyword, OpIntrinsic, OpKeyword, Operation, Proc,
        ProcedureContract, Program, PushAssembly, PushConst, PushInt, PushLocalMem, PushMem,
        PushStr, Ret, Typefence,
    },
    tokenizer::{Loc, WordToken},
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
}

impl Type {
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
            _ => match other {
                Self::PtrTo(..) => false,
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

    pub fn from_tokens(tokens: &Vec<WordToken>) -> Vec<Self> {
        let mut types: Vec<Self> = vec![];

        for WordToken { loc, value } in tokens {
            if let Some(typ) = Self::non_ptrto_from_str(value) {
                types.push(typ);
            } else if value == "ptr-to" {
                if let Some(typ) = types.pop() {
                    types.push(Self::PtrTo(Box::new(typ)));
                } else {
                    err(loc, "ptr-to has to follow a type it points to");
                }
            } else {
                err(loc, format!("Unknown type: {}", value));
            }
        }

        return types;
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
            Self::PtrTo(other) => format!("{} ptr-to", other.to_str()),
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

#[derive(Clone)]
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
        if a[i].typ.equal_to(&b[i].typ) {
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
    let print_stack = stack.split_at(stack.len() - expected_types.len()).1.to_vec();


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
                err(loc, format!("Could not infer returntype for {}", typ.to_str()))
            }
        } else {
            stack.push(TypecheckType::new(typ.clone(), loc.clone()));
        }
    }
}

fn typecheck_(
    program: &Program,
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
            Operation::CallProc(CallProc { id, loc }) => {
                let contract = if let Some(v) = program.contracts.get(*id) {
                    v
                } else {
                    err(&loc, "Could not find contract for the function call");
                };
                check_proctypefence(&mut stack, &contract.in_types, &contract.out_types, loc);
            }
            Operation::Proc(Proc { id, loc, .. }) => {
                while ip < program.ops.len() {
                    if let Operation::Ret(..) = program.ops[ip] {
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
            Operation::PushMem(PushMem { loc, .. })
            | Operation::PushLocalMem(PushLocalMem { loc, .. }) => {
                stack.push(TypecheckType::new(Type::Ptr, loc.clone()))
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
                    check_type_mismatch(loc, &Type::Int, &a.typ);
                    check_type_mismatch(loc, &Type::Int, &b.typ);
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
                    stack.push(typ.clone());
                }
                Intrinsic::Over => {
                    if stack.len() < 2 {
                        not_enough_types(loc, 2, stack.len());
                    }
                    let a = stack.pop().unwrap();
                    let b = stack.pop().unwrap();
                    stack.push(b.clone());
                    stack.push(a.clone());
                    stack.push(b.clone());
                }
                Intrinsic::Swap => {
                    if stack.len() < 2 {
                        not_enough_types(loc, 2, stack.len());
                    }
                    let a = stack.pop().unwrap().typ;
                    let b = stack.pop().unwrap().typ;
                    stack.push(TypecheckType::new(a, loc.clone()));
                    stack.push(TypecheckType::new(b, loc.clone()));
                }
                Intrinsic::Rot => {
                    if stack.len() < 3 {
                        not_enough_types(loc, 3, stack.len());
                    }
                    let a = stack.pop().unwrap().typ;
                    let b = stack.pop().unwrap().typ;
                    let c = stack.pop().unwrap().typ;
                    stack.push(TypecheckType::new(a, loc.clone()));
                    stack.push(TypecheckType::new(c, loc.clone()));
                    stack.push(TypecheckType::new(b, loc.clone()));
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
        }

        ip += 1;
    }

    TypecheckResult {
        stack,
        func_bodies: fn_bodies,
    }
}

pub fn typecheck(program: &Program) {
    let TypecheckResult { func_bodies, stack } = typecheck_(program, &vec![], None, None, 0);
    if stack.len() > 0 {
        err(
            program.get_last_loc(),
            format!("Found data on the stack: {}", stringify_stack(&stack)),
        );
    }
    for proc in func_bodies {
        let contract = program.contracts.get(proc.id).unwrap_or_else(|| {
            err(
                &proc.loc,
                "Could not acquire contract of function. (typecheck)",
            )
        });
        if contract.attributes.has_attribute("__typecheck_ignore__") {
            info(&proc.loc, "UNSAFE: This function is not being typechecked");
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
