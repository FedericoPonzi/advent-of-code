use crate::day_11::Cell::Floor;
use std::fmt;
use std::fmt::{Display, Formatter};

#[derive(Debug, Copy, Clone, PartialEq)]
enum Cell {
    Floor,
    EmptySeat,
    OccupiedSeat,
}
impl Display for Cell {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::Floor => ".",
                Self::EmptySeat => "L",
                Self::OccupiedSeat => "#",
            }
        )
    }
}
impl From<char> for Cell {
    fn from(s: char) -> Self {
        match s {
            '.' => Self::Floor,
            'L' => Self::EmptySeat,
            '#' => Self::OccupiedSeat,
            _ => panic!("Unreachable! {}", s),
        }
    }
}
fn evaluate_cell(x: isize, y: isize, table: &Vec<Vec<Cell>>) -> Cell {
    let default_empty = Cell::Floor;
    let missing_row = vec![];
    let get_cell = |x, y| {
        if x < 0 || y < 0 {
            return default_empty;
        }
        let x = x as usize;
        let y = y as usize;
        table
            .get(x)
            .unwrap_or(&missing_row)
            .get(y)
            .unwrap_or(&default_empty)
            .clone()
    };

    let top_left = get_cell(x - 1, y - 1);
    let top = get_cell(x - 1, y);
    let top_right = get_cell(x - 1, y + 1);
    let left = get_cell(x, y - 1);
    let center = get_cell(x, y);
    let right = get_cell(x, y + 1);
    let bottom_left = get_cell(x + 1, y - 1);
    let bottom = get_cell(x + 1, y);
    let bottom_right = get_cell(x + 1, y + 1);
    let adjacents_occupied: Vec<Cell> = vec![
        top_left,
        top,
        top_right,
        left,
        right,
        bottom_left,
        bottom,
        bottom_right,
    ];
    let count_adj_occupied = adjacents_occupied
        .into_iter()
        .filter(|el| *el == Cell::OccupiedSeat)
        .count();
    match center {
        Cell::OccupiedSeat if count_adj_occupied >= 4 => Cell::EmptySeat,
        Cell::EmptySeat if count_adj_occupied == 0 => Cell::OccupiedSeat,
        no_change => no_change,
    }
}
fn are_equal(before: &Vec<Vec<Cell>>, after: &Vec<Vec<Cell>>) -> bool {
    for (b, a) in before.iter().zip(after.iter()) {
        if b.iter()
            .zip(a.iter())
            .any(|(before_cell, after_cell)| before_cell != after_cell)
        {
            return false;
        }
    }
    return true;
}

fn day_11(input: String) -> usize {
    let mut table = vec![vec![]];
    for cell in input.chars() {
        if cell == '\n' {
            table.push(vec![]);
        } else {
            let as_cell: Cell = cell.into();
            table.last_mut().unwrap().push(as_cell);
        }
    }

    let mut new_table = vec![];
    loop {
        for x in 0..table.len() {
            new_table.push(vec![]);
            for y in 0..table.get(x).unwrap().len() {
                let row = new_table.last_mut().unwrap();
                row.push(evaluate_cell(x as isize, y as isize, &table));
            }
        }
        if are_equal(&table, &new_table) {
            break;
        }
        table = new_table;
        new_table = vec![];
    }

    table
        .into_iter()
        .flatten()
        .filter(|cell| *cell == Cell::OccupiedSeat)
        .count()
}

#[cfg(test)]
mod tests {
    use crate::day_11::day_11;
    use crate::shared::read_all;

    #[test]
    fn test_day_11() {
        let input_sample = read_all("11-test");
        assert_eq!(day_11(input_sample), 37);
        let input = read_all("11");
        assert_eq!(day_11(input), 2483);
        //assert_eq!(day_11_part_2(input_sample), 6);
        //assert_eq!(day_11_part_2(input), 3435);
    }
}
