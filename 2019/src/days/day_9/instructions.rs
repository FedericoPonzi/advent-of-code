use crate::days::day_9::{InstrOutput, Instruction, Program};
use std::io;

#[derive(Debug)]
pub(crate) struct Sum;
impl Instruction for Sum {
    fn get_params_num(&self) -> usize {
        2
    }
    fn execute(&self, params: Vec<i64>, program: &mut Program) -> Option<InstrOutput> {
        Some(InstrOutput::StoreOp(params[0] + params[1]))
    }
}

#[derive(Debug)]
pub struct Mult;
impl Instruction for Mult {
    fn get_params_num(&self) -> usize {
        2
    }
    fn execute(&self, params: Vec<i64>, program: &mut Program) -> Option<InstrOutput> {
        Some(InstrOutput::StoreOp(params[0] * params[1]))
    }
}

#[derive(Debug)]
pub struct InputCmd;
impl Instruction for InputCmd {
    fn get_params_num(&self) -> usize {
        1
    }

    fn execute(&self, params: Vec<i64>, program: &mut Program) -> Option<InstrOutput> {
        let get_i64_as_input = || {
            let mut buf = String::new();
            io::stdin()
                .read_line(&mut buf)
                .expect("error: unable to read userinput");
            buf.trim().parse().expect("Not an i64.")
        };
        //println!("Required input...");
        let input: i64 = program
            .external_input
            .next()
            .unwrap_or_else(get_i64_as_input);
        Some(InstrOutput::StoreInput(input, params[0] as usize))
    }
}
#[derive(Debug)]
pub struct OutputCmd;
impl Instruction for OutputCmd {
    fn get_params_num(&self) -> usize {
        1
    }

    fn execute(&self, params: Vec<i64>, program: &mut Program) -> Option<InstrOutput> {
        println!("> {}", params[0]);
        Some(InstrOutput::Output(params[0]))
    }
}
/// if the first parameter is non-zero,
/// it sets the instruction pointer to the value from the second parameter.
/// Otherwise, it does nothing.
#[derive(Debug)]
pub struct Btrue;
impl Instruction for Btrue {
    fn get_params_num(&self) -> usize {
        2
    }

    fn execute(&self, params: Vec<i64>, program: &mut Program) -> Option<InstrOutput> {
        if params[0] != 0 {
            Some(InstrOutput::Jump(params[1]))
        } else {
            None
        }
    }
}
/// if the first parameter is zero,
/// it sets the instruction pointer to the value from the second parameter.
/// Otherwise, it does nothing.
#[derive(Debug)]
pub struct Bfalse;
impl Instruction for Bfalse {
    fn get_params_num(&self) -> usize {
        2
    }

    fn execute(&self, params: Vec<i64>, program: &mut Program) -> Option<InstrOutput> {
        if params[0] == 0 {
            Some(InstrOutput::Jump(params[1]))
        } else {
            None
        }
    }
}
#[derive(Debug)]
pub struct Less;
impl Instruction for Less {
    fn get_params_num(&self) -> usize {
        2
    }

    fn execute(&self, params: Vec<i64>, program: &mut Program) -> Option<InstrOutput> {
        Some(InstrOutput::StoreOp(if params[0] < params[1] {
            1
        } else {
            0
        }))
    }
}
#[derive(Debug)]
pub struct Eq;
impl Instruction for Eq {
    fn get_params_num(&self) -> usize {
        2
    }

    fn execute(&self, params: Vec<i64>, program: &mut Program) -> Option<InstrOutput> {
        Some(InstrOutput::StoreOp(if params[0] == params[1] {
            1
        } else {
            0
        }))
    }
}

/// adjusts the relative base by the value of its only parameter.
/// The relative base increases (or decreases, if the value is negative) by the value of the parameter.
#[derive(Debug)]
pub struct Adjrbase;
impl Instruction for Adjrbase {
    fn get_params_num(&self) -> usize {
        1
    }

    fn execute(&self, params: Vec<i64>, program: &mut Program) -> Option<InstrOutput> {
        println!("Params adj:{:?}", params);
        Some(InstrOutput::AdjustRelativeBase(params[0]))
    }
}
#[derive(Debug)]
pub struct Hcf;
impl Instruction for Hcf {
    fn get_params_num(&self) -> usize {
        0
    }

    fn execute(&self, params: Vec<i64>, program: &mut Program) -> Option<InstrOutput> {
        Some(InstrOutput::Exit)
    }
}
