use regex::{Matches, Regex};
use std::ops::Range;
use std::str::FromStr;

#[derive(Debug, Clone)]
struct Validation(Range<u64>, Range<u64>);
impl Validation {
    fn contains(&self, val: &u64) -> bool {
        self.0.start <= *val && *val <= self.0.end || self.1.start <= *val && *val <= self.1.end
    }
}
impl FromStr for Validation {
    type Err = ();

    fn from_str(line: &str) -> Result<Self, Self::Err> {
        let regex = Regex::new("(?:(?P<lrange>[0-9]+)-(?P<rrange>[0-9]+))").unwrap();
        let mut captures: Vec<Range<u64>> = regex
            .captures_iter(line)
            .map(|res| Range {
                start: res.name("lrange").unwrap().as_str().parse().unwrap(),
                end: res.name("rrange").unwrap().as_str().parse().unwrap(),
            })
            .collect();
        Ok(Validation(captures.remove(0), captures.remove(0)))
    }
}
fn day_16(input: String) -> u64 {
    let mut sections: Vec<&str> = input.split("\n\n").collect();
    let validations: Vec<Validation> = sections
        .get(0)
        .unwrap()
        .lines()
        .map(|line| line.parse().unwrap())
        .collect();
    let tickets = sections.remove(2);
    tickets
        .lines()
        .skip(1) // the "nearby tickets:" string
        .map(|line| line.split(","))
        .flatten()
        .map(|val| val.parse().unwrap())
        .filter(|val| {
            !validations
                .iter()
                .any(|validation| validation.contains(val))
        })
        .sum()
}

#[cfg(test)]
mod tests {
    use crate::day_16::day_16;
    use crate::shared::read_all;

    #[test]
    fn test_day_16() {
        let input_sample = read_all("16-test");
        assert_eq!(day_16(input_sample), 71);
        let input = read_all("16");
        assert_eq!(day_16(input), 20975);
        //assert_eq!(day_16_part_2(input_sample), 6);
        //assert_eq!(day_16_part_2(input), 3435);
    }
}
