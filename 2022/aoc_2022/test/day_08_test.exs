defmodule Day08Test do
  use ExUnit.Case
  doctest Aoc2022.Day08

  test "part 1-example" do
    assert Aoc2022.Day08.solve("assets/day08_example.txt") == 21
  end

  @tag timeout: :infinity
  test "part 1" do
    # takes 611.9s sync
    #assert Aoc2022.Day08.solve("assets/day08.txt") == 1829
  end

  test "part 2-example" do
    assert Aoc2022.Day08.solve2("assets/day08_example.txt") == 8
  end

  test "part 2" do
    #assert Aoc2022.Day08.solve2("assets/day08.txt") == 1
  end
end
