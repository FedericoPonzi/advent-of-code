#[macro_use]
extern crate failure;

#[allow(dead_code)]
mod days;
fn main() {
    println!("Hello, world!");
    let p = "inputs/day2.txt";

    //println!("Day 1: {}", days::day_1::solve().unwrap());
    println!(
        "Day 2: {}",
        days::day_2::solve(std::fs::read_to_string(p).unwrap().as_str()).unwrap()
    );
}
