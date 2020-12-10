fn day_10(mut input: Vec<u64>) -> u64 {
    input.sort();
    input.insert(0, 0);
    let differences: Vec<u64> = input
        .clone()
        .into_iter()
        .zip(input.into_iter().skip(1))
        .map(|(first, second)| second - first)
        .collect();
    let ones = differences
        .iter()
        .cloned()
        .filter(|diff| *diff == 1)
        .count();
    let threes = differences.into_iter().filter(|diff| *diff == 3).count();
    (ones * (threes + 1)) as u64
}

#[cfg(test)]
mod tests {
    use crate::day_10::day_10;
    use crate::shared::load_input_parsed;

    #[test]
    fn test_day_10() {
        let input_sample = load_input_parsed("10-test");
        let input_iper_sample = r#"16
10
15
5
1
11
7
19
6
12
4"#;
        assert_eq!(
            day_10(
                input_iper_sample
                    .split_whitespace()
                    .into_iter()
                    .map(|row| row.parse::<u64>().unwrap())
                    .collect()
            ),
            35
        );
        assert_eq!(day_10(input_sample), 22 * 10);
        let input = load_input_parsed("10");
        assert_eq!(day_10(input), 2592);
        //assert_eq!(day_10_part_2(input_sample), 6);
        //assert_eq!(day_10_part_2(input), 3435);
    }
}
