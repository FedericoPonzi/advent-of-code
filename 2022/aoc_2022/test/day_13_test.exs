defmodule Day13Test do
  use ExUnit.Case
  doctest Aoc2022.Day13

  test "part 1-compare" do
    assert Aoc2022.Day13.compare([1,1,3,1,1], [1,1,5,1,1])
    assert Aoc2022.Day13.compare([[1],[2,3,4]], [[1],4])
    assert !Aoc2022.Day13.compare([9], [[8,7,6]])
    assert Aoc2022.Day13.compare([[4,4],4,4], [[4,4],4,4,4])
    assert !Aoc2022.Day13.compare([7,7,7,7], [7,7,7])
    assert Aoc2022.Day13.compare([], [3])
    assert !Aoc2022.Day13.compare([[[]]], [[]])
    assert !Aoc2022.Day13.compare([1,[2,[3,[4,[5,6,7]]]],8,9], [1,[2,[3,[4,[5,6,0]]]],8,9])

  end
  test "part 1-example" do
    assert Aoc2022.Day13.solve("assets/day13_example.txt") == 13
  end

  test "part 1" do
    assert Aoc2022.Day13.solve("assets/day13.txt") == 6187
  end

end
