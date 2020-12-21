use crate::day_18::Symbol::Number;
use std::str::FromStr;

#[derive(Debug, Copy, Clone, PartialEq)]
enum Symbol {
    Number(u64),
    OpSum,
    OpMult,
    LeftPar,
    RightPar,
}
impl From<char> for Symbol {
    fn from(s: char) -> Self {
        match s {
            '+' => Self::OpSum,
            '*' => Self::OpMult,
            '(' => Self::LeftPar,
            ')' => Self::RightPar,
            val => Self::Number(
                val.to_string()
                    .parse()
                    .expect(&format!("Unexpect char: {}", val)),
            ),
        }
    }
}
fn consume_operator<'a>(input: &'a Vec<Symbol>, index: &mut usize) -> &'a Symbol {
    let ret = match input.get(*index).unwrap() {
        op @ Symbol::OpMult | op @ Symbol::OpSum => op,
        _ => panic!("Unexpected"),
    };
    *index += 1;
    ret
}
fn eval(input: &Vec<Symbol>, index: &mut usize) -> u64 {
    let ret = match input.get(*index).unwrap() {
        Symbol::LeftPar => {
            *index += 1;
            resolve(input, index)
        }
        Symbol::Number(val) => *val,
        _ => panic!("Unexpeted, index: {}"),
    };
    *index += 1;
    ret
}
fn operation(left: u64, right: u64, op: &Symbol) -> u64 {
    match op {
        Symbol::OpSum => left + right,
        Symbol::OpMult => left * right,
        _ => panic!("Invalid symbol"),
    }
}

fn resolve(input: &Vec<Symbol>, index: &mut usize) -> u64 {
    let mut to_ret = eval(input, index);
    while *index < input.len() && *input.get(*index).unwrap() != Symbol::RightPar {
        let operator = consume_operator(input, index);
        let right = eval(input, index);
        to_ret = operation(to_ret, right, operator);
    }
    to_ret
}

fn day_18(input: Vec<Vec<Symbol>>) -> u64 {
    input.into_iter().map(|expr| resolve(&expr, &mut 0)).sum()
}

fn parse(input: &str) -> Vec<Vec<Symbol>> {
    input
        .lines()
        .map(|line| {
            line.chars()
                .into_iter()
                .filter(|ch| *ch != ' ')
                .map(|ch| ch.into())
                .collect()
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use crate::day_18::{day_18, parse};
    use crate::shared::{load_input_parsed, read_all};

    #[test]
    fn test_day_18() {
        assert_eq!(day_18(parse("2 * 3")), 6);
        assert_eq!(day_18(parse("2 + 3")), 5);
        assert_eq!(day_18(parse("2 * 3 + 5")), 11);
        assert_eq!(day_18(parse("2 * 3 + (4 * 5)")), 26);
        assert_eq!(
            day_18(parse("5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))")),
            12240
        );
        assert_eq!(
            day_18(parse("((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2")),
            13632
        );
        let input = read_all("18");
        assert_eq!(day_18(parse(input.as_str())), 30753705453324);
        //assert_eq!(day_18_part_2(input_sample), 6);
        //assert_eq!(day_18_part_2(input), 3435);
    }
}
