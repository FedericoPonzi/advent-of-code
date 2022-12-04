defmodule Day4Test do
  use ExUnit.Case
  doctest Aoc2022.Day4

  test "part 1-example" do
    assert Aoc2022.Day4.solve("assets/day4_example.txt") == 2
  end

  test "part 1" do
    assert Aoc2022.Day4.solve("assets/day4.txt") == 515
  end

  test "part 2-example" do
    assert Aoc2022.Day4.solve2("assets/day4_example.txt") == 4
  end

  test "part 2" do
    assert Aoc2022.Day4.solve2("assets/day4.txt") == 883
  end

end
