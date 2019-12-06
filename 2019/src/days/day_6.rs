use crate::failure::ResultExt;
use itertools::{fold, Itertools};
use std::collections::{HashMap, HashSet};
use std::io;

fn dfs(start: &str, tree: &HashMap<&str, Vec<&str>>, depth: u64) -> u64 {
    tree.get(start)
        .map(|children| {
            children
                .into_iter()
                .fold(depth, |acc, child| acc + dfs(child, tree, depth + 1))
        })
        .unwrap_or(depth)
}

pub fn solve(input: String) -> Result<u64, failure::Error> {
    let tree = input.trim().lines().fold(HashMap::new(), |mut acc, v| {
        let s: Vec<_> = v.split(')').collect();
        let (from, to) = (s[0], s[1]);
        let mut children: Vec<&str> = acc.get_mut(from).unwrap_or(&mut Vec::new()).to_vec();
        children.push(to);
        acc.insert(from, children);
        acc
    });
    Ok(dfs("COM", &tree, 0))
}

#[cfg(test)]
mod test {
    use crate::days::day_6::solve;

    #[test]
    fn basic_test() {
        let input = r#"COM)B11
B11)C11
C11)D11
D11)E11
E11)F11
B11)G11
G11)H11
D11)I11
E11)J11
J11)K11
K11)L11"#
            .to_owned();
        assert_eq!(42, solve(input).unwrap())
    }
}
