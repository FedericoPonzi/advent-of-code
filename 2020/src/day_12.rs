fn day_12(input: Vec<String>) -> usize {

    unimplemented!();
}

#[cfg(test)]
mod tests {
    use crate::day_12::day_12;
    use crate::shared::load_input_parsed;

    #[test]
    fn test_day_12() {
        let input_sample = load_input_parsed("12-test");
        assert_eq!(day_12(input_sample), 4);
        let input = load_input_parsed("12");
        assert_eq!(day_12(input), 128);
        //assert_eq!(day_12_part_2(input_sample), 6);
        //assert_eq!(day_12_part_2(input), 3435);
    }
}

