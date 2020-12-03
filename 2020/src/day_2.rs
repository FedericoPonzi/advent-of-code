use regex::Regex;
use std::str::FromStr;

#[derive(Debug)]
struct Line {
    lower: usize,
    upper: usize,
    letter: char,
    password: String,
}
impl FromStr for Line {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let re = Regex::new(
            r"(?P<lower>[0-9]+)-(?P<upper>[0-9]+) (?P<letter>[a-z]): (?P<password>[a-z]+)",
        )
        .unwrap();

        let captures = re
            .captures(s)
            .expect(&format!("None value on line: '{}'", s));
        Ok(Self {
            lower: captures.name("lower").unwrap().as_str().parse().unwrap(),
            upper: captures.name("upper").unwrap().as_str().parse().unwrap(),
            letter: captures.name("letter").unwrap().as_str().parse().unwrap(),
            password: captures.name("password").unwrap().as_str().into(),
        })
    }
}
fn is_valid(line: &Line) -> bool {
    let found = line
        .password
        .chars()
        .into_iter()
        .filter(|letter| *letter == line.letter)
        .count();
    found >= line.lower && found <= line.upper
}
fn day_2(input: Vec<Line>) -> usize {
    input.into_iter().filter(is_valid).count()
}
#[cfg(test)]
mod tests {
    use crate::day_2::day_2;
    use crate::shared::load_input_parsed;

    #[test]
    fn test_day_2() {
        let example = vec!["1-3 a: abcde", "1-3 b: cdefg", "2-9 c: ccccccccc"];
        assert_eq!(
            day_2(example.into_iter().map(|x| x.parse().unwrap()).collect()),
            2
        );
        let input = load_input_parsed("2");
        assert_eq!(day_2(input), 560);
    }
}
