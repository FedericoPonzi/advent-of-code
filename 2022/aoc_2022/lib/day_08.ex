defmodule Aoc2022.Day08 do
  use Memoize

  defmemo map_at(map,y,x) do
    Enum.at(Enum.at(map, y), x)
  end

  defmemo highest(x, y, map,height, width, direction) do
    is_edge = x == 0 || x == width - 1 || y == 0 || y == height - 1
    el = map_at(map,y,x)
    if is_edge do
      el
    else
     case direction do
        :up ->
          max(highest(x, y-1, map, height, width, :up), el)
        :right ->
          max(highest(x+1, y, map, height, width, :right), el)
        :down ->
          max(highest(x, y+1, map, height, width, :down), el)
        :left ->
          max(highest(x-1, y, map, height, width, :left), el)
      end
    end
  end

  def is_visible(x, y, map, height, width) do
    el = map_at(map, y, x)
    is_edge = x == 0 || x == width - 1 || y == 0 || y == height - 1
    if is_edge do
      true
    else
      highest(x, y-1,map, height, width, :up) < el ||
      highest(x+1,y,map, height, width, :right) < el ||
      highest(x, y+1, map, height, width, :down) < el ||
      highest(x-1, y, map, height, width, :left) < el
    end
  end

  def is_visible_wrapper(index, map, height, width) do
    IO.puts(index)
    y = floor(index / height)
    x = floor(Integer.mod(index, width))
    ret = is_visible(x, y, map, height, width)
    ret

  end

  def solve(filepath) do
    file = File.read!(filepath)|> String.trim() |> String.split("\n") |> Enum.map(fn x -> String.graphemes(x) |> Enum.map(&String.to_integer/1) end )
    height = length(file)
    width = length(hd(file))
    IO.inspect(height*width)
    #  bottom row, the middle 5 is visible: [height*width - 8]
    # The right-middle 3 is visible [height*width - 12]
    0..height * width - 1
    #[height*width - 12]
    #[11]
    |> Enum.map(fn index -> is_visible_wrapper(index, file, height, width) end)  |> Enum.filter(fn x -> x end) |> Enum.count()

  end

  def solve2(filepath) do
    file = File.read!(filepath)|> String.trim() |> String.split("\n") |> Enum.map(fn x -> String.graphemes(x) |> Enum.map(&String.to_integer/1) end )

  end


end
