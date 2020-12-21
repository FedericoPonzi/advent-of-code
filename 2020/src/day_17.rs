fn day_17(input: Vec<String>) -> usize {

    unimplemented!();
}

#[cfg(test)]
mod tests {
    use crate::day_17::day_17;
    use crate::shared::load_input_parsed;

    #[test]
    fn test_day_17() {
        let input_sample = load_input_parsed("17-test");
        assert_eq!(day_17(input_sample), 4);
        let input = load_input_parsed("17");
        assert_eq!(day_17(input), 128);
        //assert_eq!(day_17_part_2(input_sample), 6);
        //assert_eq!(day_17_part_2(input), 3435);
    }
}

