use regex::Regex;
use std::str::FromStr;

#[derive(Debug, Clone)]
enum Instruction {
    /// `nop` stands for No OPeration - it does nothing.
    NoOperation(i64),
    /// `acc` increases or decreases a single global value called
    /// the accumulator by the value given in the argument.
    Accumulator(i64),
    ///`jmp` jumps to a new instruction relative to itself.
    Jump(i64),
}
impl FromStr for Instruction {
    type Err = String;

    fn from_str(line: &str) -> Result<Self, Self::Err> {
        let regex = Regex::new(r"(?P<instr>\w+) (?P<arg>[+-]?[0-9]+)").unwrap();
        let captures = regex.captures(line).unwrap();
        let instr = captures.name("instr").unwrap().as_str();
        let arg: i64 = captures.name("arg").unwrap().as_str().parse().unwrap();
        match instr {
            "acc" => Ok(Instruction::Accumulator(arg)),
            "jmp" => Ok(Instruction::Jump(arg)),
            "nop" => Ok(Instruction::NoOperation(arg)),
            _ => Err(format!("Unknown instruction found = {}", instr)),
        }
    }
}

#[derive(Debug, Default)]
struct Context {
    program_counter: i64,
    accumulator: i64,
}

#[derive(Debug, Default)]
struct Executor {
    context: Context,
    program: Program,
    seen: Vec<i64>,
}
type Program = Vec<Instruction>;

impl Executor {
    fn new(program: Program) -> Self {
        Self {
            program,
            ..Default::default()
        }
    }
    fn step(&mut self) -> bool {
        let pc = self.context.program_counter;
        let instruction = self.program.get(pc as usize).expect(&format!(
            "Missing instruction at position: {}",
            self.context.program_counter
        ));
        // Loop detector:
        if self.seen.contains(&pc) {
            return false;
        }
        self.seen.push(pc);
        Self::execute(&mut self.context, instruction);
        true
    }

    fn run_until_end(&mut self) {
        while self.context.program_counter as usize != self.program.len() {
            self.step();
        }
    }
    fn run_until_loop(&mut self) {
        while self.step() && self.context.program_counter < self.program.len() as i64 {}
    }
    fn execute(context: &mut Context, instruction: &Instruction) {
        match instruction {
            Instruction::NoOperation(_) => {
                context.program_counter += 1;
            }
            Instruction::Accumulator(val) => {
                context.accumulator += *val;
                context.program_counter += 1;
            }
            Instruction::Jump(val) => context.program_counter += *val,
        }
    }
}
fn day_8_part_2(mut program: Program) -> i64 {
    let mut executor = Executor::new(program.clone());
    executor.run_until_loop();
    let seen = executor.seen;
    let exit_pc = program.len();
    let program_completed = |pc: i64| exit_pc as i64 == pc;
    let base_program = program.clone();
    for (index, instruction) in program.iter_mut().enumerate() {
        let fixed = if seen.contains(&(index as i64)) {
            match instruction {
                Instruction::Jump(val) if !seen.contains(&(index as i64 + 1)) => {
                    Some(Instruction::NoOperation(*val))
                }
                Instruction::NoOperation(val) if !seen.contains(&(*val + index as i64)) => {
                    Some(Instruction::NoOperation(*val))
                }
                _ => None,
            }
        } else {
            None
        };
        if let Some(instr) = fixed {
            let mut new_program = base_program.clone();
            new_program.remove(index);
            new_program.insert(index, instr);
            let mut executor = Executor::new(new_program.clone());
            executor.run_until_loop();
            if program_completed(executor.context.program_counter) {
                return executor.context.accumulator;
            }
        }
    }
    unreachable!()
}

fn day_8(input: Vec<Instruction>) -> i64 {
    let mut executor = Executor::new(input);
    executor.run_until_loop();
    executor.context.accumulator
}

#[cfg(test)]
mod tests {
    use crate::day_8::{day_8, day_8_part_2};
    use crate::shared::load_input_parsed;

    #[test]
    fn test_day_8() {
        let input_sample = load_input_parsed("8-test");
        assert_eq!(day_8(input_sample.clone()), 5);
        let input = load_input_parsed("8");
        assert_eq!(day_8(input.clone()), 1137);
        assert_eq!(day_8_part_2(input_sample), 8);
        assert_eq!(day_8_part_2(input), 1125);
    }
}
