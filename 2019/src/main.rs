#[macro_use]
extern crate failure;

#[allow(dead_code)]
mod days;
fn main() -> Result<(), failure::Error> {
    println!("Hello, world!");
    let p = "inputs/day7.txt";
    let input = std::fs::read_to_string(p).unwrap().trim().to_owned();
    //println!("Day 1: {}", days::day_1::solve()?);
    /*println!("Day 2: {}", days::day_2::solve(input)?);*/
    //println!("Day 3: {}", days::day_3::solve(input)?);
    //println!("Day 4: {}", days::day_4::solve("246515-739105")?);
    //println!("Day 5: {}", days::day_5::solve(input)?);
    //println!("Day 6: {}", days::day_6::solve(input)?);
    println!("Day 7: {}", days::day_7::solve(input)?);

    Ok(())
}
