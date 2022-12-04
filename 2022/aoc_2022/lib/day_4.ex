defmodule Aoc2022.Day4 do
alias Aoc2022.Day4

  def to_int_pair(sections_pair) do
    res = sections_pair |> String.split("-")
    |> Enum.map(&String.to_integer/1)
    {hd(res), hd(tl(res))}
  end
  def needs_reevaluation(row, condition) do
    # 2-8,3-7

    [{first, first_end},{second, second_end}] = row |> String.split(",") |> Enum.map(fn x -> to_int_pair(x) end)
    condition.(first,first_end,second,second_end)

  end

  def cond1(first, first_end,second, second_end) do
    first_contains_second = first <= second && first_end >= second_end
    second_contains_first = second <= first && second_end >= first_end
    if first_contains_second || second_contains_first do
      1
    else
      0
    end
  end

  def solve(filepath) do
    file = File.read!(filepath)|>String.trim() |> String.split("\n")
    file |> Enum.map(fn x -> needs_reevaluation(x, &Day4.cond1/4) end) |> Enum.sum()
  end

  def cond2(first,first_end, second, second_end) do
    no_overlap = second >first_end || first > second_end
    if no_overlap do
      0
    else
      1
    end
  end

  def solve2(filepath) do
    file = File.read!(filepath)|>String.trim() |> String.split("\n")
    file |> Enum.map(fn x -> needs_reevaluation(x, &Day4.cond2/4) end) |> Enum.sum()
  end

end
