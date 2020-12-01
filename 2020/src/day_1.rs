use std::collections::HashSet;

fn day_1(input: Vec<i64>) -> i64 {
    let set: HashSet<i64> = input.iter().map(|v| 2020 - v).collect();
    for el in input {
        if set.contains(&el) {
            return el * (2020 - el);
        }
    }
    unreachable!()
}

#[cfg(test)]
mod tests {
    use crate::day_1::day_1;
    use crate::shared::load_input_parsed;

    #[test]
    fn day_1_sample() {
        let first_example = vec![1721, 979, 366, 299, 675, 1456];
        assert_eq!(day_1(first_example), 514579);
        let input = load_input_parsed("1");
        assert_eq!(day_1(input), 1010884);
    }
}
