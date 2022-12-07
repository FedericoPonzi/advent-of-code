defmodule Day07Test do
  use ExUnit.Case
  doctest Aoc2022.Day07

  test "part 1-example" do
    assert Aoc2022.Day07.solve("assets/day07_example.txt") == 95437
  end

  test "part 1" do
    #assert Aoc2022.Day07.solve("assets/day07.txt") == 1432936
  end

  test "part 2-example" do
    assert Aoc2022.Day07.solve2("assets/day07_example.txt") == 24933642
  end

  test "part 2" do
    assert Aoc2022.Day07.solve2("assets/day07.txt") == 272298
  end
end
