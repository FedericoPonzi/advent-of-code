defmodule Day1Test do
  use ExUnit.Case
  doctest Aoc2022.Day1

  test "part 1-example" do
    assert Aoc2022.Day1.solve("assets/day1_example.txt") == 24000
  end

  test "part 1" do
    assert Aoc2022.Day1.solve("assets/day1.txt") == 68775
  end

  test "part 2-example" do
    assert Aoc2022.Day1.solve2("assets/day1_example.txt") == 45000
  end

  test "part 2" do
    assert Aoc2022.Day1.solve2("assets/day1.txt") == 202585
  end
end
