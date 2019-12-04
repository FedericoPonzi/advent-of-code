use itertools::Itertools;

pub fn solve(input: &str) -> Result<u64, failure::Error> {
    let ranges: Vec<&str> = input.split("-").collect();
    let start: u64 = ranges[0].parse().unwrap();
    let finish: u64 = ranges[1].parse().unwrap();
    let mut res: u64 = 0;
    println!("Start: {}, finish: {}", start, finish);
    for pass in start..=finish {
        if accepted(pass) {
            res += 1;
        }
    }
    Ok(res)
}
pub fn accepted(pass: u64) -> bool {
    let mut v: Vec<String> = pass
        .to_string()
        .split("")
        .map(|v| v.to_string())
        .collect::<Vec<String>>();
    let mut double_digits = false;
    for (i, c) in v.iter().skip(1).enumerate() {
        if *c == v[i] {
            double_digits = true;
            break;
        }
    }
    if !double_digits {
        //println!("No double digits! {}", pass);
        return false;
    }
    v.sort();
    let es: String = v.iter().join("");
    if es != pass.to_string() {
        //println!("Not sorted! es: {}, pass: {}", es, pass);
        return false;
    }

    true
}
#[cfg(test)]
mod test {
    use crate::days::day_4::accepted;
    #[test]
    pub fn should_check_acceptance_criterias() {
        assert!(accepted(11111));
        assert!(!accepted(223450));
        assert!(!accepted(123789));
    }
}
