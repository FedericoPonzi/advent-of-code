use std::num::ParseIntError;

pub fn compute_fuel(mass: u64) -> u64 {
    let v = mass as f64 / 3.0;
    ((v.floor() as i64) - 2) as u64
}
pub fn solve() -> Result<String, failure::Error> {
    let s = std::fs::read_to_string("/tmp/day1.txt")?;
    let res: Result<Vec<u64>, ParseIntError> =
        s.lines().into_iter().map(|l| l.parse::<u64>()).collect();
    let res: u64 = res?.into_iter().map(compute_fuel).sum();
    Ok(format!("required fuel: {}", res))
}

#[cfg(test)]
mod tests {
    use crate::days::day_1::compute_fuel;
    #[test]
    fn test_solve() {
        assert_eq!(2, compute_fuel(12));
        assert_eq!(2, compute_fuel(14));
        assert_eq!(654, compute_fuel(1969));
        assert_eq!(33583, compute_fuel(100756));
    }
}
