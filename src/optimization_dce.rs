use crate::parser::{CallProc, Operation, Proc, Program, PushFnPtr, Ret};

pub fn optimize(program: &mut Program) {
    let mut for_visit: Vec<usize> = program
        .contracts
        .iter()
        .map(|(_, contract)| contract.id)
        .filter(|id| match &program.ops[*program.refs.get(id).unwrap_or(&0)] {
            Operation::Proc(Proc { used, .. }) => *used,
            _ => false,
        })
        .collect();

    let mut visited: Vec<usize> = vec![];

    while for_visit.len() > 0 {
        let current_proc = for_visit.pop().unwrap();
        visited.push(current_proc);

        let mut ip = *program
            .refs
            .get(&current_proc)
            .expect("Failed to obtain ip for current proc");

        program.ops[ip].as_proc_mut().unwrap().used = true;

        ip += 1;

        while ip < program.ops.len() {
            match &program.ops[ip] {
                Operation::Ret(Ret { is_end, .. }) => {
                    if *is_end {
                        break;
                    }
                }
                Operation::CallProc(CallProc { id, .. }) => {
                    if !visited.contains(id) {
                        for_visit.push(*id);
                    }
                }
                Operation::PushFnPtr(PushFnPtr { contract_id, .. }) => {
                    if !visited.contains(contract_id) {
                        for_visit.push(*contract_id);
                    }
                }
                _ => {}
            }
            ip += 1;
        }
    }

    for (_, contract) in &program.contracts {
        let mut ip = *program.refs.get(&contract.id).unwrap();
        if !program.ops[ip].as_proc().unwrap().used {
            while ip < program.ops.len() {
                let operation = &program.ops[ip];
                if let Operation::Ret(Ret {
                    is_end: true, loc, ..
                }) = operation
                {
                    program.ops[ip] = Operation::None(loc.clone());
                    break;
                }
                program.ops[ip] = Operation::None(operation.get_location().clone());

                ip += 1;
            }
        }
    }
}
