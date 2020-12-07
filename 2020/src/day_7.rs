use regex::Regex;
use std::collections::HashMap;
use std::str::FromStr;

#[derive(Debug, PartialEq, Clone)]
struct Bag {
    name: String,
    inner_bags: HashMap<String, usize>,
}
impl FromStr for Bag {
    type Err = ();

    fn from_str(line: &str) -> Result<Self, Self::Err> {
        let regex_name = Regex::new(r"(\w+ \w+ bag)").unwrap();
        let regex_inner_bags = Regex::new(r"([0-9]+ \w+ \w+ bag[s]?)").unwrap();
        let bag_name = regex_name.captures(line).unwrap().get(1).unwrap().as_str();
        let inner_bags = regex_inner_bags
            .captures_iter(line)
            .map(|captures| captures.get(1).unwrap().as_str())
            .map(|bag| {
                let regex = Regex::new(r"(?P<amount>[0-9]+) (?P<bag_name>\w+ \w+ bag)").unwrap();
                let captures = regex.captures(bag).unwrap();
                let amount: usize = captures.name("amount").unwrap().as_str().parse().unwrap();
                let bag_name = captures.name("bag_name").unwrap().as_str().into();
                (bag_name, amount)
            })
            .collect::<HashMap<String, usize>>();

        Ok(Self {
            name: bag_name.into(),
            inner_bags,
        })
    }
}
fn resolve(
    input: Bag,
    full_wardrobe: &HashMap<String, Bag>,
    cache_wardrobe: &mut HashMap<String, bool>,
) -> bool {
    if input.name == "shiny gold bag" {
        return true;
    }
    if cache_wardrobe.contains_key(&input.name) {
        return *cache_wardrobe.get(&input.name).unwrap();
    }
    let ret = input.inner_bags.into_iter().any(|bag| {
        resolve(
            full_wardrobe.get(&bag.0).unwrap().clone(),
            full_wardrobe,
            cache_wardrobe,
        )
    });
    cache_wardrobe.insert(input.name.clone(), ret);
    ret
}

fn day_7(input: Vec<Bag>) -> usize {
    let full_wardrobe = input
        .iter()
        .cloned()
        .map(|bag| (bag.name.clone(), bag))
        .collect();
    let mut wardrobe = HashMap::new();
    input
        .into_iter()
        .map(|bag| resolve(bag.clone(), &full_wardrobe, &mut wardrobe))
        .filter(|contains_shiny| *contains_shiny)
        .count()
        - 1 // the shiny gold bag
}

#[cfg(test)]
mod tests {
    use crate::day_7::day_7;
    use crate::shared::load_input_parsed;

    #[test]
    fn test_day_7() {
        let input_sample = load_input_parsed("7-test");
        assert_eq!(day_7(input_sample), 4);
        let input = load_input_parsed("7");
        assert_eq!(day_7(input), 128);
        //assert_eq!(day_7_part_2(input_sample), 6);
        //assert_eq!(day_6_part_2(input), 3435);
    }
}
