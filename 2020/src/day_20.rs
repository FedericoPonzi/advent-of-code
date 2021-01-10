use regex::Regex;
use std::collections::{HashMap, HashSet};
use std::str::FromStr;

#[derive(Debug, Ord, Eq, PartialOrd, PartialEq)]
enum Border {
    Top,
    TopFlipped,
    Right,
    RightFlipped,
    Left,
    LeftFlipped,
    Bottom,
    BottomFlipped,
}

#[derive(Debug, Ord, Eq, PartialOrd, PartialEq)]
struct Tile {
    id: u64,
    content: String,
}
impl Tile {
    fn new(id: u64, content: String) -> Self {
        Self { id, content }
    }

    #[rustfmt::skip]
    fn get_borders(&self) -> Vec<(String, u64, Border)> {
        let rows: Vec<&str> = self.content.lines().into_iter().collect();
        let first_row = rows[0].to_string();
        let last_row = rows.last().unwrap().to_string();
        let first_col: String = rows
            .iter()
            .map(|r| r.chars().nth(0).to_owned().unwrap().to_string())
            .collect();
        let last_col: String = rows
            .iter()
            .map(|r| r.chars().into_iter().last().to_owned().unwrap().to_string())
            .collect();
        vec![
            (first_row.clone(), self.id, Border::Top),
            (first_row.chars().rev().collect::<String>(), self.id, Border::TopFlipped),
            (last_col.clone(), self.id, Border::Right),
            (last_col.chars().rev().collect::<String>(), self.id, Border::RightFlipped),
            (first_col.clone(), self.id, Border::Left),
            (first_col.chars().rev().collect::<String>(), self.id, Border::LeftFlipped),
            (last_row.clone(), self.id, Border::Bottom),
            (last_row.chars().rev().collect::<String>(), self.id, Border::BottomFlipped),
        ]
    }
}
impl FromStr for Tile {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let first_row = s.lines().take(1).last().unwrap();
        let regex: Regex = Regex::new("[0-9]+").unwrap();
        let captures = regex.captures(first_row).unwrap();
        let tile_id = captures.get(0).unwrap().as_str();
        let content = s
            .lines()
            .skip(1)
            .map(|line| format!("{}\n", line))
            .collect();
        Ok(Tile {
            id: tile_id.parse().unwrap(),
            content,
        })
    }
}

fn day_20(input: String) -> u64 {
    let tiles: Vec<Tile> = input
        .split("\n\n")
        .into_iter()
        .map(Tile::from_str)
        .filter_map(Result::ok)
        .collect();
    let mut borders: Vec<(String, u64, Border)> = tiles
        .iter()
        .map(|tile| tile.get_borders())
        .flatten()
        .collect();
    borders.sort();

    let mut unmatching: HashMap<u64, u64> = HashMap::new();
    for (i, border) in borders.iter().enumerate() {
        if !(i > 0 && borders[i - 1].0 == border.0
            || i < borders.len() - 1 && borders[i + 1].0 == border.0)
        {
            let v = unmatching.entry(border.1).or_insert(0);
            *v += 1;
        }
    }
    unmatching
        .into_iter()
        .filter(|(k, v)| *v == 4)
        .map(|(k, v)| k)
        .product()
}

#[cfg(test)]
mod tests {
    use crate::day_20::day_20;
    use crate::shared::{load_input_parsed, read_all};

    #[test]
    fn test_day_20() {
        let input_sample = read_all("20-test");
        assert_eq!(day_20(input_sample), 20899048083289);
        let input = read_all("20");
        assert_eq!(day_20(input), 59187348943703);
        //assert_eq!(day_20_part_2(input_sample), 6);
        //assert_eq!(day_20_part_2(input), 3435);
    }
}
