use std::fmt::Debug;
use std::str::FromStr;

pub fn load_input(day: &str) -> String {
    let p = format!("inputs/day-{}.txt", day);
    std::fs::read_to_string(p).unwrap().trim().to_owned()
}

pub fn load_input_parsed<T>(day: &str) -> Vec<T>
where
    T: FromStr,
    <T as FromStr>::Err: Debug,
{
    load_input(day)
        .split("\n")
        .map(|line| line.parse::<T>().unwrap())
        .collect()
}
