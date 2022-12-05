defmodule Aoc2022.Day5 do
  alias Aoc2022.Day5


  def move_container_fold1(movement, containers) do
    #{from, to, quantity} = movement
    {from, to, quantity} = movement
    List.foldl(0..(quantity-1) |> Enum.to_list, containers, fn(_iteration, acc) ->
      [popped | remaining] = acc[from]
      acc = %{acc | from => remaining}
      acc = %{acc | to => [popped | acc[to]]}
      acc
    end
    )
  end

  def move_containers(containers, movements, fold_fn) do
    List.foldl(movements, containers, fold_fn)
  end


  def build_solution(containers) do
    Enum.join(Enum.map(Map.values(containers), fn x -> hd(x) end))
  end

  def solve(filepath) do
    file = File.read!(filepath)
    {movements, containers} = parse(file)
    containers = move_containers(containers,movements, &move_container_fold1/2)
    build_solution(containers)
  end

  def parse(content)  do
      [containers_raw, movements_raw] = String.split(content, "\n\n")

      containers = parse_containers(containers_raw)

      movements = parse_movements(movements_raw)
      {movements, containers}
  end

  def replicate(n, x), do: for _ <- 1..n, do: x

  def parse_movements_line(line) do
    tokens = line |> String.split(" ")
    {String.to_integer(Enum.at(tokens, 3)), String.to_integer(Enum.at(tokens, 5)), String.to_integer(Enum.at(tokens, 1))}
  end

  def parse_movements(movements_raw) do
    String.split(movements_raw, "\n")
    |> Enum.map(&parse_movements_line/1)

  end
  def parse_containers(containers_raw) do
    containersl = String.split(containers_raw, "\n")
    last_row = List.last(containersl)
    containersl =  List.delete(containersl, last_row)

    last_row = last_row |> String.split("   ")|> Enum.map(&String.trim/1) |> Enum.map(&String.to_integer/1) |> Map.new(fn (x) -> {x, []} end)
    containersl = containersl
                  |> Enum.map(&split_string_every_4_chars_and_trim/1)
                  |> Enum.reverse() # reverse in order to have handy at the beginning the top element.
    List.foldl(containersl, last_row, &parse_containers_foldl/2)
  end

  def split_string_every_4_chars_and_trim(line) do
    # split every 4 characters
    String.split(line, ~r/.{4}/, include_captures: true, trim: true) |> Enum.map(&String.trim/1)
  end
  def parse_containers_foldl(cont_line, acc) do
    List.foldl(cont_line|> Enum.with_index(), acc, fn (col, acc) ->
      {element, index} = col
      index = index + 1 # they provided 1-based indexes :D
      element = element |> String.replace("[", "") |> String.replace("]", "")
      if String.length(element) > 0 do
        %{acc| index => [element | acc[index]]}
      else
        acc
      end
    end
    )

  end

  def solve2(filepath) do
    file = File.read!(filepath)
    {movements, containers} = parse(file)
    containers = move_containers(containers,movements, &move_container_fold2/2)
    build_solution(containers)
  end


  def move_container_fold2(movement, containers) do
    #{from, to, quantity} = movement
    {from, to, quantity} = movement
    {popped , remaining} = Enum.split(containers[from], quantity)
    containers = %{containers | from => remaining}
    containers = %{containers | to => popped ++ containers[to]}
    containers
  end

end
