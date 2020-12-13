fn day_13(input: Vec<String>) -> u64 {
    let timestamp = input.get(0).unwrap().parse::<u64>().unwrap();
    let res = input
        .get(1)
        .unwrap()
        .split(",")
        .filter(|val| *val != "x")
        .map(|val| val.parse::<u64>().unwrap())
        .map(|val| (val - timestamp % val, val))
        .min()
        .unwrap();
    res.0 * res.1
}

#[cfg(test)]
mod tests {
    use crate::day_13::day_13;
    use crate::shared::load_input_parsed;

    #[test]
    fn test_day_13() {
        let input_sample = load_input_parsed("13-test");
        assert_eq!(day_13(input_sample), 295);
        let input = load_input_parsed("13");
        assert_eq!(day_13(input), 2995);
        //assert_eq!(day_13_part_2(input_sample), 6);
        //assert_eq!(day_13_part_2(input), 3435);
    }
}
