defmodule Day2Test do
  use ExUnit.Case
  doctest Aoc2022.Day2

  test "part 1-example" do
    assert Aoc2022.Day2.solve("assets/day2_example.txt") == 15
  end

  test "part 1" do
    assert Aoc2022.Day2.solve("assets/day2.txt") == 15422
  end

  test "part 2-example" do
    assert Aoc2022.Day2.solve2("assets/day2_example.txt") == 12
  end

  test "part 2" do
    assert Aoc2022.Day2.solve2("assets/day2.txt") == 15442
  end

end
