DAY=$(date +%d)
touch assets/day${DAY}.txt assets/day${DAY}_example.txt lib/day_${DAY}.ex test/day_${DAY}_test.exs
cat << EOF >> lib/day_${DAY}.ex
defmodule Aoc2022.Day${DAY} do
  def solve(filepath) do
    file = File.read!(filepath)|> String.trim() |> String.split(file, "\n\n")

  end

  def solve2(filepath) do
    file = File.read!(filepath)|> String.trim() |> String.split(file, "\n\n")
    
  end
end
EOF


cat << EOF >> test/day_${DAY}_test.exs
defmodule Day${DAY}Test do
  use ExUnit.Case
  doctest Aoc2022.Day${DAY}

  test "part 1-example" do
    assert Aoc2022.Day${DAY}.solve("assets/day${DAY}_example.txt") == 1
  end

  test "part 1" do
    assert Aoc2022.Day${DAY}.solve("assets/day${DAY}.txt") == 1
  end

  test "part 2-example" do
    #assert Aoc2022.Day${DAY}.solve2("assets/day${DAY}_example.txt") == 1
  end

  test "part 2" do
    #assert Aoc2022.Day${DAY}.solve2("assets/day${DAY}.txt") == 1
  end
end

EOF
