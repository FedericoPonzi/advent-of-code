use regex::Regex;
use std::collections::HashMap;

//#[cached]
fn compute_rule(rules_map: &HashMap<usize, String>, rule: usize) -> String {
    let to_compute = rules_map.get(&rule).unwrap();
    let mut arms = Vec::new();
    let mut last = "".to_string();
    for ch in to_compute.split(" ") {
        if ch.chars().all(|ch| ch.is_numeric()) {
            last = format!(
                "{}(?:{})",
                last,
                compute_rule(rules_map, ch.parse::<u64>().unwrap() as usize)
            );
        } else if ch == "|" {
            arms.push(last);
            last = "".to_string();
        } else {
            last = format!("{}{}", last, ch);
        }
    }
    arms.push(last);
    arms.into_iter()
        .map(|arm| format!("(?:{})", arm))
        .collect::<Vec<String>>()
        .join("|")
}
/// Returns a regex.
fn compute_rules(rules: String) -> String {
    let mut rules_map: HashMap<usize, String> = HashMap::new();
    for line in rules.lines() {
        let parts: Vec<&str> = line.split(": ").into_iter().collect();
        let number: usize = parts.get(0).unwrap().parse().unwrap();
        rules_map.insert(number, parts.get(1).unwrap().to_string());
    }
    let rule = compute_rule(&rules_map, 0);
    //Should match the whole line
    format!("^({})$", rule).replace("\"", "")
}

fn day_19(input: String) -> usize {
    let sections: Vec<String> = input
        .split("\n\n")
        .into_iter()
        .map(|r| r.to_string())
        .collect();
    let rules = sections.get(0).unwrap();
    let inputs = sections.get(1).unwrap();
    let computed_rules = compute_rules(rules.to_string());
    let regex = Regex::new(computed_rules.as_str()).unwrap();
    inputs
        .lines()
        .filter(|line: &&str| regex.is_match(line))
        .count()
}

#[cfg(test)]
mod tests {
    use crate::day_19::{compute_rules, day_19};
    use crate::shared::{load_input_parsed, read_all};

    #[test]
    fn test_day_19() {
        let simpler = r#"0: 1 2
1: "a"
2: 1 3 | 3 1
3: "b""

aab
aba
aaa
bbb"#
            .to_string();
        assert_eq!(day_19(simpler), 2);
        let input_sample = read_all("19-test");
        assert_eq!(day_19(input_sample), 2);
        let input = read_all("19");
        assert_eq!(day_19(input), 210);
        //assert_eq!(day_19_part_2(input_sample), 6);
        //assert_eq!(day_19_part_2(input), 3435);
    }
}
