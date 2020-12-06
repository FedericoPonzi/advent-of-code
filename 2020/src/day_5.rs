fn convert(input: String) -> u64 {
    let as_bin = input
        .replace("B", "1")
        .replace("F", "0")
        .replace("R", "1")
        .replace("L", "0");
    u64::from_str_radix(as_bin.as_str(), 2).unwrap()
}

fn day_5_part_2(input: Vec<String>) -> u64 {
    let seats: Vec<u64> = input.into_iter().map(convert).collect();
    let missing: Vec<u64> = (2..1023)
        .into_iter()
        .filter(|seat| !seats.contains(seat))
        .collect();
    for missing_seat in missing {
        let before = missing_seat.checked_sub(1).unwrap();
        let after = missing_seat + 1;
        if seats.contains(&before) && seats.contains(&after) {
            return missing_seat as u64;
        }
    }
    unreachable!()
}

fn day_5(input: Vec<String>) -> u64 {
    input.into_iter().map(convert).max().unwrap()
}

#[cfg(test)]
mod tests {
    use crate::day_5::{day_5, day_5_part_2};
    use crate::shared::load_input_parsed;

    #[test]
    fn test_day_5() {
        let input = load_input_parsed("5-test");
        assert_eq!(day_5(input), 820);
        let input = load_input_parsed("5");
        assert_eq!(day_5(input.clone()), 951);
        assert_eq!(day_5_part_2(input), 653);
    }
}
