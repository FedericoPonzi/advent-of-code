use regex::Regex;
use std::collections::HashSet;
enum QuizPart {
    One,
    Two,
}

fn day_6(input: String) -> usize {
    solver(QuizPart::One, input)
}
fn day_6_part_2(input: String) -> usize {
    solver(QuizPart::Two, input)
}
fn solver(part: QuizPart, input: String) -> usize {
    Regex::new(r"\n\n")
        .unwrap()
        .split(input.as_str())
        .map(|group| group.split("\n").collect::<Vec<_>>())
        .map(|passengers| {
            let mut as_set = passengers
                .get(0)
                .unwrap()
                .chars()
                .collect::<HashSet<char>>();
            for s in passengers {
                let single = s.chars().collect::<HashSet<_>>();
                as_set = match part {
                    QuizPart::One => as_set.union(&single).cloned().collect(),
                    QuizPart::Two => as_set.intersection(&single).cloned().collect(),
                };
            }
            as_set.len()
        })
        .sum()
}

#[cfg(test)]
mod tests {
    use crate::day_6::{day_6, day_6_part_2};
    use crate::shared::read_all;

    #[test]
    fn test_day_6() {
        let input_sample = read_all("6-test");
        let input = read_all("6");
        assert_eq!(day_6(input_sample.clone()), 11);
        assert_eq!(day_6(input.clone()), 6714);
        assert_eq!(day_6_part_2(input_sample), 6);
        assert_eq!(day_6_part_2(input), 3435);
    }
}
