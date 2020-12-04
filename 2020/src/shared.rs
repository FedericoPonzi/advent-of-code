use std::fmt::Debug;
use std::fs::File;
use std::io;
use std::io::{BufRead, Read};
use std::str::FromStr;
pub fn read_all(day: &str) -> String {
    let input_path = format!("inputs/day-{}.txt", day);
    let mut buffer = String::new();
    File::open(input_path)
        .unwrap()
        .read_to_string(&mut buffer)
        .unwrap();
    buffer
}

pub fn load_input_parsed<T>(day: &str) -> Vec<T>
where
    T: FromStr,
    <T as FromStr>::Err: Debug,
{
    let input_path = format!("inputs/day-{}.txt", day);
    io::BufReader::new(File::open(input_path).unwrap())
        .lines()
        .map(|line| line.unwrap().parse::<T>().unwrap())
        .collect()
}
