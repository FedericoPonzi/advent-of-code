#[macro_use]
extern crate failure;
#[macro_use]
extern crate maplit;

use days::day_10::solve;

#[allow(dead_code)]
mod days;
fn main() -> Result<(), failure::Error> {
    println!("Hello, world!");
    let load_input = |day: &str| -> String {
        let p = format!("inputs/day{}.txt", day);
        std::fs::read_to_string(p).unwrap().trim().to_owned()
    };
    //println!("Day 1: {}", days::day_1::solve()?);
    /*println!("Day 2: {}", days::day_2::solve(input)?);*/
    //println!("Day 3: {}", days::day_3::solve(input)?);
    //println!("Day 4: {}", days::day_4::solve("246515-739105")?);
    //println!("Day 5: {}", days::day_5::solve(load_input("5"))?);
    //println!("Day 6: {}", days::day_6::solve(load_input("6"))?);
    //println!("Day 7: {}", days::day_7::solve(load_input("7"))?);
    //println!("Day 8: {}", days::day_8::solve(load_input("8"), 25 * 6)?);
    //let day = "9";
    //println!("Day {}: {:?}", day, solve(load_input(day))?);
    let day = "10";
    println!("Day {}: {:?}", day, solve(load_input(day))?);
    Ok(())
}
