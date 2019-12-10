use crate::failure::ResultExt;
use itertools::Itertools;
use std::collections::BTreeMap;
use std::str::FromStr;
mod instructions;
use instructions::*;
use std::fmt::{Debug, Display};

trait Instruction: Debug {
    fn get_params_num(&self) -> usize;
    fn execute(&self, params: Vec<i64>, program: &mut Program) -> Option<InstrOutput>;
}
#[derive(Debug)]
enum InstrOutput {
    AdjustRelativeBase(i64),
    StoreOp(i64),
    Output(i64),
    Jump(i64),
    StoreInput(i64, usize),
    Exit,
}

/*
struct InstrOutput {
    // operation on the store.
    store_op: Option<StoreOp>,
    // output to stdout
    output: Option<i64>,
    // New position
    jump: Option<i64>,
}
*/

pub struct Program {
    code: String,
    pc: usize,
    program: Vec<i64>,
    relative_base: i64,
    external_input: Box<dyn Iterator<Item = i64>>,
}
impl FromStr for Program {
    type Err = failure::Error;

    fn from_str(input: &str) -> Result<Self, Self::Err> {
        let program_data = input
            .lines()
            .join("")
            .split(',')
            .map(|v| -> Result<i64, failure::Error> {
                v.parse::<i64>()
                    .context(format!("Digit:'{}'", v))
                    .map_err(failure::Error::from)
            })
            .collect::<Result<Vec<i64>, failure::Error>>()?;
        Ok(Program {
            code: input.to_owned(),
            pc: 0,
            program: program_data,
            relative_base: 0,
            external_input: Box::new(vec![].into_iter()),
        })
    }
}
#[derive(Debug)]
pub struct Output {
    final_program: Vec<i64>,
    outputs: Vec<i64>,
}
impl Output {
    pub fn new(final_program: Vec<i64>, outputs: Vec<i64>) -> Output {
        Output {
            final_program,
            outputs,
        }
    }
    pub fn output_str(&self) -> String {
        self.outputs.iter().map(|v| v.to_string()).join(",")
    }
    pub fn final_program_str(&self) -> String {
        self.final_program.iter().map(|v| v.to_string()).join(",")
    }
}
impl Program {
    pub fn new(code: &str) -> Result<Program, failure::Error> {
        code.parse()
    }
    pub fn get_params_from(
        &mut self,
        op: i64,
        pc: usize,
        program: &mut Vec<i64>,
        relative_base: i64,
        params: usize,
    ) -> Vec<i64> {
        let op = match params {
            2 => format!("{:04}", op),
            1 => format!("{:03}", op),
            0 => return vec![],
            _ => unreachable!(),
        };
        println!("OP: {}", op);
        let (without_opcode, opcode) = op.split_at(op.len() - 2);
        let mut get_val = |mode, pos, relative_base| {
            let val = program[pc + pos];
            match mode {
                '0' => {
                    if opcode == "03" {
                        println!("This is opcode input!!!");
                        return val;
                    } else {
                        println!("Opcode: {}", opcode);
                    }
                    let val = val as usize;
                    if val >= program.len() {
                        program.resize(program.len() + val + 1, 0);
                    }
                    println!("Mode 0! {}, {:?}", val, program[val]);
                    program[val]
                }
                '1' => val, // Immediate
                '2' => {
                    println!("Mode 2! {}, {}", relative_base, val);
                    let val = (relative_base + val as i64) as usize;
                    if val >= program.len() {
                        program.resize(val + 1, 0);
                    }
                    if opcode == "03" {
                        println!("This is opcode input!!!");
                        return val as i64;
                    }
                    println!("program[val] ={}, val = {}", program[val], val);

                    program[val]
                }
                _mode => {
                    //println!("Unr: {}", v);
                    unreachable!();
                }
            }
        };

        without_opcode
            .chars()
            .rev()
            .enumerate()
            .map(|(pos, mode)| get_val(mode, pos, relative_base))
            .collect()
    }
    fn setup_instructions(&self) -> BTreeMap<i64, Box<dyn Instruction>> {
        let sum: Box<dyn Instruction> = Box::new(Sum {});
        let mult: Box<dyn Instruction> = Box::new(Mult {});
        let input: Box<dyn Instruction> = Box::new(InputCmd {});
        let output: Box<dyn Instruction> = Box::new(OutputCmd {});
        let btrue: Box<dyn Instruction> = Box::new(Btrue {});
        let bfalse: Box<dyn Instruction> = Box::new(Bfalse {});
        let less: Box<dyn Instruction> = Box::new(Less {});
        let eq: Box<dyn Instruction> = Box::new(Eq {});
        let adjrbase: Box<dyn Instruction> = Box::new(Adjrbase {});
        let hcf: Box<dyn Instruction> = Box::new(Hcf {});
        btreemap! {
            1 => sum,
            2 => mult,
            3 => input,
            4 => output,
            5 => btrue,
            6 => bfalse,
            7 => less,
            8 => eq,
            9 => adjrbase,
            99 => hcf,
        }
    }
    pub fn run(&mut self, external_input_arr: Vec<i64>) -> crate::days::Result<Output> {
        self.external_input = Box::new(external_input_arr.into_iter());
        //let mut relative_base = 0i64;
        let mut outputs: Vec<i64> = Vec::new();
        let mut program = self.program.clone();
        let instructions = self.setup_instructions();

        while self.pc < program.len() {
            //println!("pc:{}", self.pc);
            let opcode = program[self.pc];
            println!("Opcode:{}, program len: {}", opcode, program.len());
            let next = instructions.get(&(opcode % 100)).unwrap();
            println!("next: {:?}", next);
            self.pc += 1;
            let params = self.get_params_from(
                opcode,
                self.pc,
                &mut program,
                self.relative_base,
                next.get_params_num() as usize,
            );
            println!("Params:{:?}", params);
            if let Some(ret) = next.execute(params, self) {
                //println!("Ret: {:?}", ret);
                match ret {
                    InstrOutput::Output(out) => {
                        outputs.push(out);
                        self.pc += next.get_params_num();
                    }
                    InstrOutput::Jump(jump) => {
                        self.pc = jump as usize;
                    }
                    InstrOutput::StoreInput(val, pos) => {
                        let final_index = pos;
                        if final_index > program.len() {
                            program.resize(program.len() + final_index, 0);
                        }
                        println!("Storing input :{:?}", final_index);
                        program[final_index] = val;
                        self.pc += next.get_params_num();
                    }
                    InstrOutput::StoreOp(val) => {
                        let final_index = program[self.pc + next.get_params_num()] as usize;
                        println!(
                            "final_index: {}, used index: {}",
                            final_index,
                            self.pc + next.get_params_num() + 1
                        );
                        if final_index > program.len() {
                            program.resize(program.len() + final_index, 0);
                        }
                        program[final_index] = val;
                        self.pc += 1 + next.get_params_num();
                    }
                    InstrOutput::AdjustRelativeBase(val) => {
                        println!(
                            "Received relative base, val: {}, old rb: {}",
                            val, self.relative_base
                        );
                        self.relative_base += val;
                        println!("New relative base: {}", self.relative_base);
                        self.pc += next.get_params_num();
                    }
                    InstrOutput::Exit => return Ok(Output::new(program, outputs)),
                }
            } else {
                // TODO: Convert to "NoOp"
                // For example, Btrue/Bfalse.
                // It should still consume the params:
                self.pc += next.get_params_num();
            }
        }
        Err(format_err!("Something went... wrong. Missing Exit opcode?"))
    }
}

pub fn solve(input: String) -> Result<Output, failure::Error> {
    let mut p: Program = input.parse()?;
    let out = p.run(vec![2])?;
    println!("Output: {:?}", out.outputs);
    println!("Last out: {:?}", out.outputs.last());
    Ok(out)
}

#[cfg(test)]
mod tests {
    use crate::days::day_9::{Output, Program};

    #[test]
    fn test_relative_id_single() -> Result<(), failure::Error> {
        assert_eq!(run_program("109, -1, 4, 1, 99", vec![])?.output_str(), "-1");
        Ok(())
    }
    #[test]
    fn test_relative_id() -> Result<(), failure::Error> {
        assert_eq!(
            run_program("109,10,203,0,99", vec![1])?.final_program_str(),
            "109,10,203,0,99,0,0,0,0,0,1"
        );

        assert_eq!(run_program("109, -1, 4, 1, 99", vec![])?.output_str(), "-1");
        assert_eq!(
            run_program("109, -1, 104, 1, 99", vec![])?.output_str(),
            "1"
        );

        assert_eq!(
            run_program("109,-1, 204, 1, 99", vec![])?.output_str(),
            "109"
        );
        assert_eq!(
            run_program("109,1,9,2,204,-6,99", vec![])?.output_str(),
            "204"
        );
        assert_eq!(
            run_program("109, 1, 109, 9, 204, -6, 99", vec![])?.output_str(),
            "204"
        );

        assert_eq!(
            run_program("109, 1, 209, -1, 204, -106, 99", vec![])?.output_str(),
            "204"
        );

        assert_eq!(
            run_program("109, 1, 3, 3, 204, 2, 99", vec![1])?.output_str(),
            "1"
        );
        assert_eq!(
            run_program("109, 1, 203, 2, 204, 2, 99", vec![1])?.output_str(),
            "1"
        );
        Ok(())
    }
    #[test]
    fn test_day9_basic() -> Result<(), failure::Error> {
        assert_eq!(
            run_program(
                "109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99".into(),
                vec![]
            )?
            .output_str(),
            "109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99"
        );
        assert_eq!(
            run_program("1102,34915192,34915192,7,4,7,99,0", vec![])?.output_str(),
            "1219070632396864"
        );
        assert_eq!(
            run_program("104,1125899906842624,99", vec![])?.output_str(),
            "1125899906842624"
        );
        Ok(())
    }

    fn run_program(p: &str, input: Vec<i64>) -> Result<Output, failure::Error> {
        let mut p: Program = p.replace(" ", "").parse()?;
        let r = p.run(input)?;
        Ok(r)
    }
    fn run_program_output(p: &str, input: Vec<i64>) -> Result<Vec<i64>, failure::Error> {
        Ok(run_program(p, input)?.outputs)
    }
    #[test]
    fn test_ops() -> Result<(), failure::Error> {
        // Using position mode, consider whether the input is equal to 8; output 1 (if it is) or 0 (if it is not).
        assert_eq!(
            run_program_output("3,9,8,9,10,9,4,9,99,-1,8".into(), vec![8])?[0],
            1
        );

        // Using position mode, consider whether the input is equal to 8; output 1 (if it is) or 0 (if it is not).
        assert_eq!(
            run_program_output("3,9,8,9,10,9,4,9,99,-1,8".into(), vec![9])?[0],
            0
        );

        // Using immediate mode, consider whether the input is equal to 8; output 1 (if it is) or 0 (if it is not).
        assert_eq!(
            run_program_output("3,3,1108,-1,8,3,4,3,99".into(), vec![8])?[0],
            1
        );
        // Using immediate mode, consider whether the input is equal to 8; output 1 (if it is) or 0 (if it is not).
        assert_eq!(
            run_program_output("3,3,1108,-1,8,3,4,3,99".into(), vec![9])?[0],
            0
        );

        Ok(())
    }

    #[test]
    fn test_less() -> Result<(), failure::Error> {
        // Using position mode, consider whether the input is less than 8; output 1 (if it is) or 0 (if it is not).
        assert_eq!(
            run_program_output("3,9,7,9,10,9,4,9,99,-1,8".into(), vec![7])?[0],
            1
        );
        // Using position mode, consider whether the input is less than 8; output 1 (if it is) or 0 (if it is not).
        assert_eq!(
            run_program_output("3,9,7,9,10,9,4,9,99,-1,8".into(), vec![9])?[0],
            0
        );

        // Using immediate mode, consider whether the input is less than 8; output 1 (if it is) or 0 (if it is not).
        assert_eq!(
            run_program_output("3,3,1107,-1,8,3,4,3,99".into(), vec![7])?[0],
            1
        );
        // Using immediate mode, consider whether the input is less than 8; output 1 (if it is) or 0 (if it is not).
        assert_eq!(
            run_program_output("3,3,1107,-1,8,3,4,3,99".into(), vec![9])?[0],
            0
        );
        Ok(())
    }
    #[test]
    pub fn test_complex_ex_day7() -> Result<(), failure::Error> {
        // The program will then output 999 if the input value is below 8, output 1000 if the input value is equal to 8,
        // or output 1001 if the input value is greater than 8.
        let program = "3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,
1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,
999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99
";
        assert_eq!(run_program_output(program, vec![7])?[0], 999);
        assert_eq!(run_program_output(program, vec![8])?[0], 1000);
        assert_eq!(run_program_output(program, vec![9])?[0], 1001);
        Ok(())
    }

    #[test]
    pub fn test_day5_part2() -> Result<(), failure::Error> {
        let p = "inputs/day5.txt";
        let input = std::fs::read_to_string(p).unwrap().trim().to_owned();
        assert_eq!(run_program_output(input.as_str(), vec![5])?[0], 14195011);
        Ok(())
    }

    #[test]
    pub fn test_jumps_day7() -> Result<(), failure::Error> {
        // take an input, then output 0 if the input was zero or 1 if the input was non-zero
        // Position:
        assert_eq!(
            run_program_output("3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9".into(), vec![0])?[0],
            0
        );
        assert_eq!(
            run_program_output("3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9".into(), vec![1])?[0],
            1
        );
        // Immediate:
        assert_eq!(
            run_program_output("3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9".into(), vec![0])?[0],
            0
        );
        assert_eq!(
            run_program_output("3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9".into(), vec![1])?[0],
            1
        );
        Ok(())
    }
}
