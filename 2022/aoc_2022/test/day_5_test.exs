defmodule Day4Test do
  use ExUnit.Case
  doctest Aoc2022.Day5

  test "part 1-example" do
    assert Aoc2022.Day5.solve("assets/day5_example.txt") == "CMZ"
  end

  test "part 1" do
    assert Aoc2022.Day5.solve("assets/day5.txt") == "TLFGBZHCN"
  end

  test "part 2-example" do
    assert Aoc2022.Day5.solve2("assets/day5_example.txt") == "MCD"
  end

  test "part 2" do
    assert Aoc2022.Day5.solve2("assets/day5.txt") == "QRQFHFWCL"
  end

end
