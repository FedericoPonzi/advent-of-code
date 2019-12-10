use crate::failure::ResultExt;
use itertools::Itertools;
use std::collections::BTreeMap;
use std::io;

enum OpCode {
    Sum,
    Mult,
    Input,
    Output,
    HCF,
    /// if the first parameter is non-zero,
    /// it sets the instruction pointer to the value from the second parameter.
    /// Otherwise, it does nothing.
    BTRUE,
    /// if the first parameter is zero,
    /// it sets the instruction pointer to the value from the second parameter.
    /// Otherwise, it does nothing.
    BFALSE,
    LESS,
    EQ,
}
impl From<i64> for OpCode {
    fn from(v: i64) -> Self {
        match v {
            1 => OpCode::Sum,
            2 => OpCode::Mult,
            3 => OpCode::Input,
            4 => OpCode::Output,
            5 => OpCode::BTRUE,
            6 => OpCode::BFALSE,
            7 => OpCode::LESS,
            8 => OpCode::EQ,
            99 => OpCode::HCF,
            v => {
                println!("Unreachable! OpCode:  {}", v);
                unreachable!()
            }
        }
    }
}

pub fn get_params_from(op: i64, i: usize, program: &mut Vec<i64>, params: usize) -> Vec<i64> {
    let op = match params {
        2 => format!("{:04}", op),
        1 => format!("{:03}", op),
        _ => unreachable!(),
    };
    let (without_opcode, _opcode) = op.split_at(op.len() - 2);
    let get_val = |mode, val| match mode {
        '0' => program[val as usize],
        '1' => val,
        _mode => {
            //println!("Unr: {}", v);
            unreachable!();
        }
    };

    without_opcode
        .chars()
        .rev()
        .enumerate()
        .map(|(pos, mode)| get_val(mode, program[i + pos]))
        .collect()
}

/// external_input_arr = input to pass with the OpCode::Input.
/// Returns Output from OpCode::Output
pub fn run_program(input: String, external_input_arr: Vec<i64>) -> Result<i64, failure::Error> {
    let mut ret = 0;
    let mut external_input = external_input_arr.clone().into_iter();
    let mut program: Vec<i64> = input
        .lines()
        .join("")
        .split(',')
        .map(|v| -> Result<i64, failure::Error> {
            v.parse::<i64>()
                .context(format!("Digit:'{}'", v))
                .map_err(failure::Error::from)
        })
        .collect::<Result<Vec<i64>, failure::Error>>()?;
    let mut pc = 0;
    while pc < program.len() {
        //println!("v:{:?}", program);
        let opcode_v = program[pc];
        //println!("i:{}, v:{}", pc, opcode_v);
        let opcode: OpCode = OpCode::from(opcode_v % 100);
        pc += 1;
        let steps = match opcode {
            OpCode::Sum => {
                let params = get_params_from(opcode_v, pc, &mut program, 2);
                let final_index = program[pc + 2];
                program[final_index as usize] = params[0] + params[1];
                3
            }
            OpCode::Mult => {
                let params = get_params_from(opcode_v, pc, &mut program, 2);
                let final_index = program[pc + 2];
                program[final_index as usize] = params[0] * params[1];
                3
            }
            OpCode::Input => {
                let get_i64_as_input = || {
                    let mut buf = String::new();
                    io::stdin()
                        .read_line(&mut buf)
                        .expect("error: unable to read userinput");
                    buf.trim().parse().expect("Not an i64.")
                };
                //println!("Required input...");
                let input: i64 = external_input.next().unwrap_or_else(get_i64_as_input);
                //println!("input: {}, iter: {:?}", input, external_input_arr);
                let pos = program[pc];
                program[pos as usize] = input;
                1
            }
            OpCode::Output => {
                let params = get_params_from(opcode_v, pc, &mut program, 1);
                ret = params[0];
                println!(" > {}", params[0]);
                1
            }
            OpCode::BTRUE => {
                let params = get_params_from(opcode_v, pc, &mut program, 2);
                let steps = if params[0] != 0 {
                    pc = params[1] as usize;
                    0
                } else {
                    2
                };
                steps
            }
            OpCode::BFALSE => {
                let params = get_params_from(opcode_v, pc, &mut program, 2);
                let steps = if params[0] == 0 {
                    pc = params[1] as usize;
                    0
                } else {
                    2
                };
                steps
            }

            OpCode::LESS => {
                let params = get_params_from(opcode_v, pc, &mut program, 2);
                let final_index = program[pc + 2] as usize;
                println!("params: {:?}", params);
                program[final_index] = if params[0] < params[1] { 1 } else { 0 };
                3
            }
            OpCode::EQ => {
                let params = get_params_from(opcode_v, pc, &mut program, 2);
                let final_index = program[pc + 2] as usize;
                program[final_index] = if params[0] == params[1] { 1 } else { 0 };
                3
            }
            OpCode::HCF => return Ok(ret),
        };
        pc += steps;
    }
    Err(format_err!("Something Strange happened."))
}

fn next_lex_perm(set: &mut [i64]) -> bool {
    if set.len() == 0 {
        return false;
    }
    let mut i = set.len() - 1;
    while i > 0 && set[i - 1] >= set[i] {
        i -= 1;
    }
    if i == 0 {
        return false;
    }
    let mut j = set.len() - 1;
    while set[j] <= set[i - 1] {
        j -= 1;
    }
    set.swap(i - 1, j);
    set[i..].reverse();
    true
}

/// Day 7
pub fn search_highest_signal(input: String) -> Result<i64, failure::Error> {
    let mut phaser_input = vec![4, 3, 2, 1, 0];
    let mut max = 0;

    let mut memoization: BTreeMap<Vec<i64>, i64> = BTreeMap::new();
    let mut phaser_input = [0, 1, 2, 3, 4];
    while next_lex_perm(&mut phaser_input) {
        let mut last = 0i64;
        for el in phaser_input.into_iter() {
            let external_input = vec![*el as i64, last];
            last = if memoization.contains_key(&external_input) {
                *memoization.get(&external_input).unwrap()
            } else {
                let res = run_program(input.clone(), external_input.clone())?;
                memoization.insert(external_input, res);
                res
            };
        }
        if last > max {
            max = last
        }
    }
    Ok(max)
}

pub fn solve(input: String) -> Result<i64, failure::Error> {
    let max = search_highest_signal(input)?;
    Ok(max as i64)
}

#[cfg(test)]
mod test {
    use crate::days::day_7::{run_program, solve};

    #[test]
    fn basic_test_day_7() -> Result<(), failure::Error> {
        assert_eq!(
            solve("3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0".into()).unwrap(),
            43210
        );
        assert_eq!(
            solve(
                "3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0".into()
            )
            .unwrap(),
            54312
        );
        assert_eq!(
            solve(
                "3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,
1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0"
                    .into()
            )?,
            65210
        );
        Ok(())
    }

    #[test]
    #[ignore]
    fn test_ops() -> Result<(), failure::Error> {
        // Using position mode, consider whether the input is equal to 8; output 1 (if it is) or 0 (if it is not).
        assert_eq!(run_program("3,9,8,9,10,9,4,9,99,-1,8".into(), vec![8])?, 1);
        // Using position mode, consider whether the input is equal to 8; output 1 (if it is) or 0 (if it is not).
        assert_eq!(run_program("3,9,8,9,10,9,4,9,99,-1,8".into(), vec![9])?, 0);

        // Using immediate mode, consider whether the input is equal to 8; output 1 (if it is) or 0 (if it is not).
        assert_eq!(run_program("3,3,1108,-1,8,3,4,3,99".into(), vec![8])?, 1);
        // Using immediate mode, consider whether the input is equal to 8; output 1 (if it is) or 0 (if it is not).
        assert_eq!(run_program("3,3,1108,-1,8,3,4,3,99".into(), vec![9])?, 0);

        Ok(())
    }
    #[test]
    #[ignore]
    fn test_less() -> Result<(), failure::Error> {
        // Using position mode, consider whether the input is less than 8; output 1 (if it is) or 0 (if it is not).
        assert_eq!(run_program("3,9,7,9,10,9,4,9,99,-1,8".into(), vec![7])?, 1);
        // Using position mode, consider whether the input is less than 8; output 1 (if it is) or 0 (if it is not).
        assert_eq!(run_program("3,9,7,9,10,9,4,9,99,-1,8".into(), vec![9])?, 0);

        // Using immediate mode, consider whether the input is less than 8; output 1 (if it is) or 0 (if it is not).
        assert_eq!(run_program("3,3,1107,-1,8,3,4,3,99".into(), vec![7])?, 1);
        // Using immediate mode, consider whether the input is less than 8; output 1 (if it is) or 0 (if it is not).
        assert_eq!(run_program("3,3,1107,-1,8,3,4,3,99".into(), vec![9])?, 0);
        Ok(())
    }
    #[test]
    #[ignore]
    pub fn test_jumps() -> Result<(), failure::Error> {
        // take an input, then output 0 if the input was zero or 1 if the input was non-zero
        // Position:
        assert_eq!(
            run_program("3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9".into(), vec![0])?,
            0
        );
        assert_eq!(
            run_program("3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9".into(), vec![1])?,
            1
        );
        // Immediate:
        assert_eq!(
            run_program("3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9".into(), vec![0])?,
            0
        );
        assert_eq!(
            run_program("3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9".into(), vec![1])?,
            1
        );
        Ok(())
    }

    #[test]
    #[ignore]
    pub fn test_complex_ex() -> Result<(), failure::Error> {
        // The program will then output 999 if the input value is below 8, output 1000 if the input value is equal to 8,
        // or output 1001 if the input value is greater than 8.
        let program = "3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,
1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,
999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99
"
        .to_owned();
        assert_eq!(run_program(program.clone(), vec![7])?, 999);
        assert_eq!(run_program(program.clone(), vec![8])?, 1000);
        assert_eq!(run_program(program, vec![9])?, 1001);
        Ok(())
    }

    #[test]
    #[ignore]
    pub fn test_day5_part2() -> Result<(), failure::Error> {
        let p = "inputs/day5.txt";
        let input = std::fs::read_to_string(p).unwrap().trim().to_owned();
        assert_eq!(run_program(input, vec![5])?, 14195011);
        Ok(())
    }
}
