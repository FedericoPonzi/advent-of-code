defmodule Day06Test do
  use ExUnit.Case
  doctest Aoc2022.Day06

  test "part 1-example" do
    assert Aoc2022.Day06.solve_wrapper("mjqjpqmgbljsphdztnvjfqwrcgsmlb",4) == 7
    assert Aoc2022.Day06.solve_wrapper("bvwbjplbgvbhsrlpgdmjqwftvncz",4) == 5
    assert Aoc2022.Day06.solve_wrapper("nppdvjthqldpwncqszvftbrmjlhg",4) == 6
    assert Aoc2022.Day06.solve_wrapper("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg",4) == 10
    assert Aoc2022.Day06.solve_wrapper("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw",4) == 11
  end

  test "part 1" do
    assert Aoc2022.Day06.solve("assets/day06.txt") == 1816
  end

  test "part 2-example" do

    assert Aoc2022.Day06.solve_wrapper("mjqjpqmgbljsphdztnvjfqwrcgsmlb",14) == 19
    assert Aoc2022.Day06.solve_wrapper("bvwbjplbgvbhsrlpgdmjqwftvncz",14) == 23
    assert Aoc2022.Day06.solve_wrapper("nppdvjthqldpwncqszvftbrmjlhg",14) == 23
    assert Aoc2022.Day06.solve_wrapper("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg",14) == 29
    assert Aoc2022.Day06.solve_wrapper("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw",14) == 26

  end

  test "part 2" do
    assert Aoc2022.Day06.solve2("assets/day06.txt") == 2625
  end
end
