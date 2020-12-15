use std::collections::HashMap;
use std::vec::Vec;

fn day_15(mut input: Vec<u64>, iterations: usize) -> u64 {
    assert!(input.len() > 0);
    let mut seen: HashMap<u64, u64> = input
        .iter()
        .enumerate()
        .map(|(index, val)| (*val, index as u64 + 1))
        .collect();
    //println!("Seen: {:?}", seen);
    let mut last_spoken = input.last().unwrap().clone();
    for index in input.len()..iterations {
        let to_speak = seen
            .get(&last_spoken)
            .map(|val| {
                //println!("Last turn seen: {}, current index: {} + 1", val, index);
                if *val == index as u64 {
                    0
                } else {
                    (index as u64) - val
                }
            })
            .unwrap_or(0)
            .clone();

        //println!("To speak: {}", to_speak);
        seen.insert(last_spoken, index as u64);
        last_spoken = to_speak;

        //println!("Seen: {:?}", seen);
        //println!("Input: {:?}", input);
    }
    last_spoken
}

#[cfg(test)]
mod tests {
    use crate::day_15::day_15;
    use crate::shared::load_input_parsed;

    #[test]
    fn test_day_15() {
        assert_eq!(day_15(vec![0, 3, 6], 2020), 436);
        assert_eq!(day_15(vec![18, 8, 0, 5, 4, 1, 20], 2020), 253);
        //For part 2,
        assert_eq!(day_15(vec![18, 8, 0, 5, 4, 1, 20], 30000000), 13710);
    }
}
