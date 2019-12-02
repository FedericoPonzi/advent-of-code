enum OpCode {
    Add = 1,
    Multiply = 2,
    HaltAndCatchFire = 99,
}

use ToString;

pub fn apply<F>(index: usize, program: &mut Vec<i64>, operation: F)
where
    F: Fn(i64, i64) -> i64,
{
    let a = program[index + 1] as usize;
    let b = program[index + 2] as usize;
    let res_pos = program[index + 3] as usize;
    program[res_pos] = operation(program[a], program[b]);
}

pub fn solve(input: &str) -> Result<String, failure::Error> {
    let mut program: Vec<i64> = input
        .split(',')
        .map(|v| -> Result<i64, failure::Error> { v.parse::<i64>().map_err(failure::Error::from) })
        .collect::<Result<Vec<i64>, failure::Error>>()?;
    let mut i = 0;
    let sum = |a, b| -> i64 { a + b };
    let mult = |a, b| -> i64 { a * b };
    let get_vals = while i < program.len() {
        let v = program[i];
        let s = match v {
            1 => apply(i, &mut program, sum),
            2 => apply(i, &mut program, mult),
            99 => {
                return Ok(program
                    .iter()
                    .map(ToString::to_string)
                    .fold("".to_string(), |v, p| {
                        if v != "".to_string() {
                            format!("{},{}", v, p)
                        } else {
                            p
                        }
                    }))
            }
            _ => return Err(format_err!("Error, unexpected int")),
        };
        i += 4;
    };
    Err(format_err!("Something Strange happened."))
}

#[cfg(test)]
mod test {
    use crate::days::day_2::solve;

    #[test]
    pub fn should_solve() {
        let program = "1,9,10,3,2,3,11,0,99,30,40,50";

        assert_eq!(solve("1,0,0,0,99").unwrap(), "2,0,0,0,99".to_owned());
        assert_eq!(solve("2,3,0,3,99").unwrap(), "2,3,0,6,99".to_owned());
        assert_eq!(solve("2,4,4,5,99,0").unwrap(), "2,4,4,5,99,9801".to_owned());
        assert_eq!(
            solve("1,1,1,4,99,5,6,0,99").unwrap(),
            "30,1,1,4,2,5,6,0,99".to_owned()
        );
    }
}
