use std::str::FromStr;

#[derive(Debug, Clone, PartialEq)]
enum Directions {
    North(isize),
    South(isize),
    East(isize),
    West(isize),
    RotateRight(isize),
    RotateLeft(isize),
    Forward(isize),
}
impl FromStr for Directions {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let splitted = s.split_at(1);
        let val = splitted.1.parse::<isize>().unwrap();
        let ret = match splitted.0 {
            "N" => Directions::North(val),
            "E" => Directions::East(val),
            "S" => Directions::South(-val),
            "W" => Directions::West(-val),
            "F" => Directions::Forward(val),
            "R" => Directions::RotateRight(val),
            "L" => Directions::RotateLeft(val),
            _ => unimplemented!(),
        };
        Ok(ret)
    }
}
fn convert_facing(val: isize) -> isize {
    match val {
        -90 => 270,
        -270 => 90,
        all => all.abs(),
    }
}
impl Directions {
    fn from_forward(mut facing: isize, steps: isize) -> Directions {
        match facing.abs() % 360 {
            0 => Directions::East(steps),
            90 => Directions::South(-steps),
            180 => Directions::West(-steps),
            270 => Directions::North(steps),
            _ => unreachable!(),
        }
    }
}
fn new_positon(
    direction: Directions,
    facing: isize,
    vertical: isize,
    horizontal: isize,
) -> (isize, isize, isize) {
    match direction {
        Directions::North(val) | Directions::South(val) => (facing, vertical + val, horizontal),
        Directions::East(val) | Directions::West(val) => (facing, vertical, horizontal + val),
        Directions::RotateLeft(val) => (convert_facing(facing - val), vertical, horizontal),
        Directions::RotateRight(val) => (convert_facing(facing + val), vertical, horizontal),
        Directions::Forward(val) => new_positon(
            Directions::from_forward(facing, val),
            facing,
            vertical,
            horizontal,
        ),
    }
}
fn day_12(input: Vec<Directions>) -> usize {
    let mut current = (0isize, 0isize, 0isize);
    for direction in input {
        current = new_positon(direction.clone(), current.0, current.1, current.2);
    }
    (current.1.abs() + current.2.abs()) as usize
}

#[cfg(test)]
mod tests {
    use crate::day_12::{day_12, Directions};
    use crate::shared::load_input_parsed;

    #[test]
    fn test_day_12() {
        let input_sample = load_input_parsed("12-test");
        assert_eq!(day_12(input_sample), 25);
        let input = load_input_parsed("12");
        assert_eq!(day_12(input), 1589);
        //assert_eq!(day_12_part_2(input_sample), 6);
        //assert_eq!(day_12_part_2(input), 3435);
    }
}
