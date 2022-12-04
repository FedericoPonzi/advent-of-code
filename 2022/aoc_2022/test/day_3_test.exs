defmodule Day3Test do
  use ExUnit.Case
  doctest Aoc2022.Day3

  test "part 1-example" do
    assert Aoc2022.Day3.solve("assets/day3_example.txt") == 157
  end

  test "part 1" do
    assert Aoc2022.Day3.solve("assets/day3.txt") == 8240
  end


  test "part 2-example" do
    assert Aoc2022.Day3.solve2("assets/day3_example.txt") == 70
  end

  test "part 2" do
    assert Aoc2022.Day3.solve2("assets/day3.txt") == 2587
  end

end
