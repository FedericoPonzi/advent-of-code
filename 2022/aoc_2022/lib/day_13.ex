defmodule Aoc2022.Day13 do

  def convert_to_list(left,right) do
    if is_number(left) && is_list(right) do
      [left]
    else
      left
    end
  end

  def compare(left, right) do
    if length(left) == 0 || length(right) == 0 do
      cond do
          length(left) == 0 && length(right) == 0 -> nil
          length(left) == 0-> true
          length(right) == 0 -> false
      end
    else
      [l_h | left] = left
      [r_h | right ] = right
      l_h = convert_to_list(l_h, r_h)
      r_h = convert_to_list(r_h, l_h)
      cond do
        is_number(l_h) && is_number(r_h) ->
          cond do
            l_h < r_h ->
              true
            l_h == r_h ->
              compare(left, right)
            l_h > r_h ->
              false
          end
        is_list(l_h) && is_list(r_h) ->
          val = compare(l_h, r_h)
          if val == nil do
            compare(left, right)
          else
            val
          end
        true ->
          #IO.inspect("WHATTTTT")
          false
      end
    end

  end

  def parse_line(line) do
    elem(Code.eval_string(line), 0)
  end

  def solve(filepath) do
    File.read!(filepath)|> String.trim()
    |> String.split("\n\n")
    |> Enum.with_index(1)
    |> Enum.map(fn {blocks, index} ->
      [left, right] = String.split(blocks, "\n")
      left = parse_line(left)
      right = parse_line(right)
      res = compare(left, right)
      if res do index else 0 end
    end) |> Enum.sum()
  end

  def solve2(filepath) do
    File.read!(filepath)|> String.trim() |> String.split("\n")

  end
end
