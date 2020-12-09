use std::collections::HashSet;
use std::iter::FromIterator;

fn day_9_part_2(input: Vec<u64>, preamble: usize) -> u64 {
    let invalid = day_9(input.clone(), preamble.clone());
    let mut index = 0;
    let mut windows = Vec::new();
    while index < input.len() {
        while windows.iter().sum::<u64>() < invalid {
            let val = input[index];
            windows.push(val);
            index += 1;
        }
        if windows.iter().sum::<u64>() == invalid {
            return windows.iter().min().unwrap() + windows.iter().max().unwrap();
        } else {
            windows.remove(0);
        }
    }
    unreachable!();
}
fn day_9(input: Vec<u64>, preamble: usize) -> u64 {
    input
        .clone()
        .into_iter()
        .skip(preamble as usize)
        .enumerate()
        .filter(|(index, el)| {
            let preamble_input: HashSet<u64> =
                HashSet::from_iter(input.iter().cloned().skip(*index).take(preamble));
            for sub in preamble_input.clone() {
                if preamble_input.contains(&(i64::abs(*el as i64 - sub as i64) as u64)) {
                    return false;
                }
            }
            true
        })
        .map(|(index, el)| el)
        .last()
        .unwrap()
}
#[cfg(test)]
mod tests {
    use crate::day_9::{day_9, day_9_part_2};
    use crate::shared::load_input_parsed;

    #[test]
    fn test_day_9() {
        let input_sample = load_input_parsed("9-test");
        assert_eq!(day_9(input_sample.clone(), 5), 127);
        let input = load_input_parsed("9");
        assert_eq!(day_9(input.clone(), 25), 257342611);
        assert_eq!(day_9_part_2(input_sample, 5), 62);
        assert_eq!(day_9_part_2(input, 25), 35602097);
    }
}
