#[macro_use]
extern crate failure;

#[allow(dead_code)]
mod days;
fn main() {
    println!("Hello, world!");
    let p = "inputs/day3.txt";
    let input = std::fs::read_to_string(p).unwrap();
    //println!("Day 1: {}", days::day_1::solve().unwrap());
    /*println!("Day 2: {}", days::day_2::solve(input).unwrap());*/
    println!("Day 3: {}", days::day_3::solve(input).unwrap());
}
