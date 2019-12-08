use crate::days::Result;
use itertools::{min, Itertools};
use regex::Regex;

pub fn solve(input: String, layer_size: u64) -> Result<u64> {
    let layers: Vec<String> = input
        .trim()
        .chars()
        .chunks(layer_size as usize)
        .into_iter()
        .map(|chunk| chunk.collect::<String>())
        .collect::<Vec<_>>();
    let mut min_layer: (u64, u64, u64) = (std::u64::MAX, 0, 0);
    for l in layers {
        let val: (u64, u64, u64) = l.chars().fold((0, 0, 0), |acc, c| {
            if c == '0' {
                (acc.0 + 1, acc.1, acc.2)
            } else if c == '1' {
                (acc.0, acc.1 + 1, acc.2)
            } else if c == '2' {
                (acc.0, acc.1, acc.2 + 1)
            } else {
                acc
            }
        });

        if val.0 < min_layer.0 {
            min_layer = val;
        }
    }
    Ok(min_layer.1 * min_layer.2)
}

#[cfg(test)]
mod test {
    use crate::days::day_8::solve;
    use crate::days::Result;
    #[test]
    pub fn test_basic() -> Result<()> {
        assert_eq!(solve("123456789012".into(), 6)?, 1);
        Ok(())
    }
}
