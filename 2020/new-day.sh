#!/bin/bash -e

day=${1:?Please provide a day number, like "$0 8" for day 8.}
touch inputs/day-${day}.txt inputs/day-${day}-test.txt
echo "mod day_${day};" >> src/lib.rs
cat > src/day_${day}.rs <<- EOF
fn day_${day}(input: Vec<String>) -> usize {

    unimplemented!();
}

#[cfg(test)]
mod tests {
    use crate::day_${day}::day_${day};
    use crate::shared::load_input_parsed;

    #[test]
    fn test_day_${day}() {
        let input_sample = load_input_parsed("${day}-test");
        assert_eq!(day_${day}(input_sample), 4);
        let input = load_input_parsed("${day}");
        assert_eq!(day_${day}(input), 128);
        //assert_eq!(day_${day}_part_2(input_sample), 6);
        //assert_eq!(day_${day}_part_2(input), 3435);
    }
}

EOF