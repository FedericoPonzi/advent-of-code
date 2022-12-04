defmodule Aoc2022.Day3 do

  def single(str) do
    as_list = String.graphemes(str)
    str_len = round(length(as_list)/2)
    {first,second} = Enum.split(as_list, str_len)
    MapSet.intersection(Enum.into(first, MapSet.new), Enum.into(second, MapSet.new)) |> MapSet.to_list
  end

  def item_to_priority(grapheme) do
    val = hd(String.to_charlist(grapheme))
    a_val = hd(String.to_charlist("a"))
    biga_val = hd(String.to_charlist("A"))
    if val < a_val do
      val - biga_val + 1 + 26
    else
      val - a_val + 1
    end
  end

  def single2(str) do
    splitted = String.split(str, "\n") |> Enum.map(&String.graphemes/1)
    first = Enum.at(splitted,0)
    second = Enum.at(splitted, 1)
    third = Enum.at(splitted, 2)
    MapSet.intersection(MapSet.intersection(Enum.into(first, MapSet.new), Enum.into(second, MapSet.new)), Enum.into(third,MapSet.new)) |> MapSet.to_list
  end
  def solve(filepath) do
    File.read!(filepath)|> String.trim() |> String.split("\n") |>
    Enum.map(fn x-> single(x) |>Enum.map(&item_to_priority/1) end) |> List.flatten() |> Enum.sum()
  end
  def solve2(filepath) do
    file = File.read!(filepath) |> String.trim()
    Regex.scan(~r{.+\n.+\n.+\n?}, file) |> List.flatten()
    |> Enum.map(fn x -> single2(x) |> Enum.map(&item_to_priority/1) end) |> List.flatten() |> Enum.sum()
  end

end
