fn day_8(input: Vec<String>) -> usize {

}

#[cfg(test)]
mod tests {
    use crate::day_8::day_8;
    use crate::shared::load_input_parsed;

    #[test]
    fn test_day_8() {
        let input_sample = load_input_parsed("8-test");
        assert_eq!(day_8(input_sample), 4);
        let input = load_input_parsed("8");
        assert_eq!(day_8(input), 128);
        //assert_eq!(day_8_part_2(input_sample), 6);
        //assert_eq!(day_8_part_2(input), 3435);
    }
}

