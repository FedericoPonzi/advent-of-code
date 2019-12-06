use crate::failure::ResultExt;
use itertools::Itertools;
use std::io;

enum OpCode {
    Sum,
    Mult,
    Input,
    Output,
    HCF,
}
impl From<i64> for OpCode {
    fn from(v: i64) -> Self {
        match v {
            1 => OpCode::Sum,
            2 => OpCode::Mult,
            3 => OpCode::Input,
            4 => OpCode::Output,
            99 => OpCode::HCF,
            v => {
                println!("Unrrrr: {}", v);
                unreachable!()
            }
        }
    }
}

pub fn get_params_from(op: i64, i: usize, program: &mut Vec<i64>, params: usize) -> Vec<i64> {
    let mut op = match params {
        2 => format!("{:04}", op),
        1 => format!("{:03}", op),
        _ => unreachable!(),
    };
    let (without_opcode, _opcode) = op.split_at(op.len() - 2);
    let get_val = |mode, val| match mode {
        '0' => program[val as usize],
        '1' => val,
        mode => {
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

pub fn solve(input: String) -> Result<String, failure::Error> {
    println!("{:05}", 1);
    println!("{:05}", 102);

    let mut program: Vec<i64> = input
        .split(',')
        .map(|v| -> Result<i64, failure::Error> {
            v.parse::<i64>()
                .context(format!("Digit:'{}'", v))
                .map_err(failure::Error::from)
        })
        .collect::<Result<Vec<i64>, failure::Error>>()?;
    let mut i = 0;
    while i < program.len() {
        println!("v:{:?}", program);
        let opcode_v = program[i];
        println!("i:{}, v:{}", i, opcode_v);
        let opcode: OpCode = OpCode::from(opcode_v % 100);
        i += 1;
        let steps = match opcode {
            OpCode::Sum => {
                let params = get_params_from(opcode_v, i, &mut program, 2);
                println!("{:?}", params);
                let final_index = program[i + 2];
                println!(
                    "Opcode: {}, put {} + {} = {} in position {}",
                    opcode_v,
                    params[0],
                    params[1],
                    params[0] + params[1],
                    final_index
                );
                program[final_index as usize] = params[0] + params[1];
                3
            }
            OpCode::Mult => {
                let params = get_params_from(opcode_v, i, &mut program, 2);
                let final_index = program[i + 2];
                println!(
                    "Opcode: {}, put {} * {} = {} in position {}",
                    opcode_v,
                    params[0],
                    params[1],
                    params[0] * params[1],
                    final_index
                );
                program[final_index as usize] = params[0] * params[1];
                3
            }
            OpCode::Input => {
                // get input
                let mut buf = String::new();
                io::stdin()
                    .read_line(&mut buf)
                    .expect("error: unable to read user input");
                let input: i64 = buf.trim().parse().expect("Not an i64.");
                let pos = program[i];
                program[pos as usize] = input;
                1
            }
            OpCode::Output => {
                let p = get_params_from(opcode_v, i, &mut program, 1);
                println!("P: {:?}", p);
                println!(" > {}", p[0]);
                1
            }
            OpCode::HCF => {
                return Ok(program
                    .iter()
                    .map(ToString::to_string)
                    .fold("".to_string(), |v, p| {
                        if v != "".to_string() {
                            format!("{}", v)
                        } else {
                            p
                        }
                    }))
            }
        };
        i += steps;
    }
    Err(format_err!("Something Strange happened."))
}

#[cfg(test)]
mod test {
    use crate::days::day_5::solve;

    #[test]
    fn basic_test() {
        assert_eq!(
            solve("3,0,4,0,99".parse().unwrap()).unwrap(),
            "1,0,4,0,99".to_owned()
        );
        assert_eq!(
            solve("1002,4,3,4,33".parse().unwrap()).unwrap(),
            "1002,4,3,4,99".to_owned()
        );
    }
}
