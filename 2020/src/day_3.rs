use std::str::FromStr;
use std::vec::Vec;

struct Line(Vec<CellType>);

impl Line {
    fn get(&self, index: usize) -> Option<&CellType> {
        self.0.get(index % self.0.len())
    }
}

#[derive(Debug, PartialEq)]
enum CellType {
    Square,
    Tree,
}

impl From<char> for CellType {
    fn from(cell: char) -> Self {
        match cell {
            '.' => Self::Square,
            '#' => Self::Tree,
            _ => unreachable!(),
        }
    }
}

impl FromStr for Line {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(Line(s.chars().map(Into::into).collect()))
    }
}

fn day_3(input: Vec<Line>) -> usize {
    input
        .iter()
        .enumerate()
        .skip(1)
        .filter_map(|(position, line)| line.get(position * 3))
        .filter(|cell_type| **cell_type == CellType::Tree)
        .count()
}

#[cfg(test)]
mod tests {
    use crate::day_3::day_3;
    use crate::shared::load_input_parsed;

    #[test]
    fn test_day_3() {
        assert_eq!(day_3(load_input_parsed("3-test")), 7);
        assert_eq!(day_3(load_input_parsed("3")), 218);
    }
}
