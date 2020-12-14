use regex::Regex;
use std::collections::HashMap;
use std::fmt;
use std::fmt::{Debug, Formatter};
use std::str::FromStr;

#[derive(PartialEq)]
enum Instruction {
    Mask(u64, u64),
    Write(u64, u64),
}
impl Debug for Instruction {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Instruction::Mask(un, ze) => write!(f, "Mask({:b}, {:b})", un, ze),
            Instruction::Write(pos, val) => write!(f, "Write({}, {:b})", pos, val),
        }
    }
}
impl Instruction {
    fn from_mask_str(val: &str) -> Instruction {
        //and mask will make sure all 0 are 0.
        let mut zeros = u64::max_value();
        let mut unos = 0u64;
        for (index, ch) in val.chars().rev().enumerate() {
            if ch == '0' {
                zeros &= !(1 << (index));
            }
            if ch == '1' {
                unos |= 1 << (index);
            }
        }
        Instruction::Mask(unos, zeros)
    }
}
impl FromStr for Instruction {
    type Err = ();

    fn from_str(line: &str) -> Result<Self, Self::Err> {
        if line.starts_with("mask") {
            let val = line.split_at("mask = ".len()).1;
            Ok(Instruction::from_mask_str(val))
        } else {
            let regex = Regex::new(r"mem\[(?P<pos>[0-9]+)] = (?P<val>[0-9]+)").unwrap();
            let captures = regex.captures(line).unwrap();
            let pos = captures.name("pos").unwrap().as_str().parse().unwrap();
            let val = captures.name("val").unwrap().as_str().parse().unwrap();
            Ok(Instruction::Write(pos, val))
        }
    }
}

fn day_14(input: Vec<Instruction>) -> u64 {
    let mut memory = HashMap::new();
    let mut curr_zero = u64::max_value();
    let mut curr_unos = 0;
    let max_mem_mask = 2u64.pow(36) - 1;

    for instr in input {
        match instr {
            Instruction::Mask(unos, zeros) => {
                curr_unos = unos;
                curr_zero = zeros;
            }
            Instruction::Write(pos, val) => {
                let val = val & curr_zero;
                let val = val | curr_unos;
                let val = val & max_mem_mask;
                memory.insert(pos, val);
            }
        }
    }
    memory.values().sum()
}

#[cfg(test)]
mod tests {
    use crate::day_14::{day_14, Instruction};
    use crate::shared::load_input_parsed;

    #[test]
    fn test_day_14() {
        Instruction::from_mask_str("0X1X");
        let input_sample = load_input_parsed("14-test");
        assert_eq!(day_14(input_sample), 165);
        let input = load_input_parsed("14");
        assert_eq!(day_14(input), 9628746976360);
        //assert_eq!(day_14_part_2(input_sample), 6);
        //assert_eq!(day_14_part_2(input), 3435);
    }
}
