defmodule Aoc2022.Day2 do
  def solve(filepath) do
    scores = %{
      "A X" => 3 + 1,
      "A Y" => 6 + 2,
      "A Z" => 0 + 3,
      "B X" => 0 + 1,
      "B Y" => 3 + 2,
      "B Z" => 6 + 3,
      "C X" => 6 + 1,
      "C Y" => 0 + 2,
      "C Z" => 3 + 3}

    file = File.read!(filepath)
    file |> String.split("\n") |> Enum.map(fn x -> scores[x] end) |> Enum.sum()
  end

  def solve2(filepath) do
    file = File.read!(filepath)|>String.trim()
    # X means you need to lose, Y means you need to end the round in a draw, and Z means you need to win.
    # 1 for Rock, 2 for Paper, and 3 for Scissors
    # 0 if you lost, 3 if the round was a draw, and 6 if you won
    scores = %{
      "A X" => 0 + 3,
      "A Y" => 3 + 1,
      "A Z" => 6 + 2,

      "B X" => 0 + 1,
      "B Y" => 3 + 2,
      "B Z" => 6 + 3,

      "C X" => 0 + 2,
      "C Y" => 3 + 3,
      "C Z" => 6 + 1}

    file |> String.split("\n") |> Enum.map(fn x -> scores[x] end) |> Enum.sum()

  end
end
