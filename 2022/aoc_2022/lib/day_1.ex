defmodule Aoc2022.Day1 do
  def load_file(a, b) do
    a + b
  end

  def parse_text(a) do
    a + 2
  end

  def solve(filepath) do
    file = File.read!(filepath)
    elfs = String.split(file, "\n\n")
    hd(Enum.map(elfs, fn x -> String.split(x,"\n") |> Enum.map(&String.to_integer/1) |> Enum.sum() end) |> Enum.sort(:desc))
  end

  def solve2(filepath) do
    file = File.read!(filepath)
    elfs = String.split(file, "\n\n")
    Enum.map(elfs, fn x -> String.split(x,"\n") |> Enum.map(&String.to_integer/1) |> Enum.sum() end) |> Enum.sort(:desc) |> Enum.take(3) |> Enum.sum()
  end
end
