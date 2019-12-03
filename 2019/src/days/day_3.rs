use std::borrow::Borrow;
use std::collections::BTreeSet;
use std::hash::Hash;
use std::iter::FromIterator;

#[derive(Copy, Clone, PartialEq, Eq, Hash, Ord, PartialOrd, Debug)]
pub struct Point {
    pub x: i64,
    pub y: i64,
}
impl Point {
    pub fn new(x: i64, y: i64) -> Self {
        Point { x, y }
    }
}
impl Point {
    fn distance(&self) -> i64 {
        self.x.abs() + self.y.abs()
    }
}

pub fn solve(input: String) -> Result<String, failure::Error> {
    let handle_row = |r: &str| {
        r.split(',')
            .fold(vec![], |mut coordinates: Vec<Point>, dir: &str| {
                let init = Point::new(0, 0);
                let last = coordinates.last().unwrap_or(&init).clone();
                let (dir, quantity) = dir.split_at(1);
                let quantity: i64 = quantity.parse().unwrap();
                match dir {
                    "U" => {
                        for y in last.y..=last.y + quantity {
                            coordinates.push(Point::new(last.x, y))
                        }
                    }
                    "R" => {
                        for x in last.x..=last.x + quantity {
                            coordinates.push(Point::new(x, last.y))
                        }
                    }
                    "D" => {
                        for y in (last.y - quantity..=last.y).rev() {
                            coordinates.push(Point::new(last.x, y));
                        }
                    }
                    "L" => {
                        for x in (last.x - quantity..=last.x).rev() {
                            coordinates.push(Point::new(x, last.y))
                        }
                    }
                    _ => unreachable!(),
                };
                coordinates
            })
    };
    let rows: Vec<Vec<Point>> = input.lines().map(handle_row).collect();
    let first: BTreeSet<Point> = BTreeSet::from_iter(rows[0].iter().cloned().skip(1));
    let second: BTreeSet<Point> = BTreeSet::from_iter(rows[1].iter().cloned().skip(1));

    let intersections: &Point = first
        .intersection(second.borrow())
        .min_by_key(|pt| pt.distance())
        .unwrap();
    Ok(intersections.distance().to_string())
}

#[cfg(test)]
mod test {
    use crate::days::day_3::solve;
    #[test]
    pub fn should_solve_easy() {
        assert_eq!(
            solve(
                r#"R8,U5,L5,D3
U7,R6,D4,L4"#
                    .into()
            )
            .unwrap(),
            "6".to_owned()
        );
    }
    #[test]
    pub fn should_solve() {
        assert_eq!(
            solve(
                r#"R75,D30,R83,U83,L12,D49,R71,U7,L72
U62,R66,U55,R34,D71,R55,D58,R83"#
                    .into()
            )
            .unwrap(),
            "159".to_owned()
        );
        assert_eq!(
            solve(
                r#"R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51
U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"#
                    .into()
            )
            .unwrap(),
            "135".to_owned()
        );
    }
}
