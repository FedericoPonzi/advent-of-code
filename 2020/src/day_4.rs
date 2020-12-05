use regex::Regex;

fn day_4(input: String) -> usize {
    // This was my initial regex
    // It's already filtering out some bad entries, leading to less matching wrt requested value.
    //let passport_regex = Regex::new(r"(?:(?P<byr>byr:[0-9]+)\s?|(?P<iyr>iyr:[0-9]+)\s?|(?P<eyr>eyr:[0-9]+)\s?|(?P<hgt>hgt:[0-9]+(?:cm|in)\s?)|(?P<ecl>ecl:[a-z]{3})\s?|(?P<hcl>hcl:#[a-z0-9]+)\s?|(?P<pid>pid:[0-9]+\s?)|(?P<cid>cid:[0-9]+)\s?)+\n").unwrap();
    let passport_regex = Regex::new(r"[a-z]{3}:[a-z#0-9]+\s?").unwrap();
    Regex::new("\n\n")
        .unwrap()
        .split(input.as_str())
        .map(|raw| {
            passport_regex
                .captures_iter(raw)
                .filter(|capture| !capture.get(0).unwrap().as_str().contains("cid"))
                .count()
        })
        .filter(|count| *count == 7)
        .count()
}

#[cfg(test)]
mod tests {
    use crate::day_4::day_4;
    use crate::shared::read_all;

    #[test]
    fn test_day_4() {
        let input = read_all("4-test");
        assert_eq!(day_4(input), 2);
        let input = read_all("4");
        assert_eq!(day_4(input), 170);
    }
}
