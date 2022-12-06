defmodule Aoc2022.Day06 do
  def solve(filepath) do
    file = File.read!(filepath) |> String.trim()
    solve_wrapper(file, 4)
  end
  def solve_wrapper(str, ch_len) do
    [{_s, index}] = str |> String.codepoints() |> Enum.chunk_every(ch_len, 1, :discard) |> Enum.map(&MapSet.new/1) |> Enum.with_index() |> Enum.filter(fn ({s, _id}) -> MapSet.size(s) == ch_len end) |> Enum.take(1)
    index + ch_len
  end

  def solve2(filepath) do
    file = File.read!(filepath) |> String.trim()
    solve_wrapper(file, 14)
  end
end
