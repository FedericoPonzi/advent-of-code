defmodule Aoc2022.Day07 do

  def parse_line(line) do
    cond do
      String.starts_with?(line, "$ cd ..") ->
        :cd_up
      String.starts_with?(line, "$ cd") ->
        {_command, directory} = String.split_at(line, String.length("$ cd "))
        {:cd, directory}
      String.starts_with?(line, "$ ls") ->
        :ls
      String.starts_with?(line, "dir ") ->
        {_dir, directory} = String.split_at(line, String.length("dir "))
        {:dir, directory}
      true ->
        [size, filename] = String.split(line, " ")
        {:file, size, filename}
    end
  end


  #{/ : {directories: {a : {directories: files}}, files: {(file, size) }}}
  def parse_ls(tokens, content) do
    if length(tokens) == 0 do
      {content, tokens}
    else
      [t | tokens_tail] = tokens
      case t do
        {:dir, directory} ->
          directories = Map.get(content, :dir_tree, %{})
          directories = Map.put(directories, directory, %{})
          content = Map.put(content, :dir_tree, directories)
          parse_ls(tokens_tail, content)
        {:file, size, name} ->
          files = Map.get(content, :files_tree, %{})
          files = Map.put(files, name, String.to_integer(size))
          content = Map.put(content, :files_tree, files)
          parse_ls(tokens_tail, content)
        _ -> {content, tokens}
      end
    end
  end
  def parse_tree(tokens, current_dir, current_tree) do
    if length(tokens) == 0 do
      {tokens, current_tree}
    else
      [t | tokens] = tokens
      case t do
        :ls  ->
          # parse the content of this directory, possibly consuming some tokens.
          {current_tree, tokens} = parse_ls(tokens, %{})
          parse_tree(tokens, current_dir, current_tree)
        :cd_up -> {tokens, current_tree}
        {:cd, directory} ->
          {tokens, dirs_tree} = parse_tree(tokens, directory, current_tree[:dir_tree][directory])
          current_tree_dir_tree = Map.put(current_tree[:dir_tree], directory, dirs_tree)
          current_tree = Map.put(current_tree, :dir_tree, current_tree_dir_tree)
          parse_tree(tokens, current_dir, current_tree)
      end
    end
  end

  def parse_input(content) do
    tokens = Enum.map(content, &parse_line/1)
    {_tokens, tree} = parse_tree(tokens, "/", %{})
    tree
  end

  def get_file_sizes(dir) do
    if dir != nil do
      Enum.sum(Map.values(dir))
    else
      0
    end
  end

  def compute_sizes(tree) do
    dir_sizes = if tree[:dir_tree] != nil do
        Enum.map(Map.keys(tree[:dir_tree]), fn x ->compute_sizes(tree[:dir_tree][x]) end) |> Enum.reduce({0,0}, fn (el, acc )->
        {elem(el, 0)+elem(acc, 0), elem(el,1)+elem(acc,1)}
      end)
    else
      {0, 0}
    end
    # return: my size + total_size
    file_sizes = get_file_sizes(tree[:files_tree])
    dir_size = elem(dir_sizes, 0) + file_sizes
    total_sizes = elem(dir_sizes, 1) + if dir_size <= 100000 do
      dir_size
    else
      0
    end
    {dir_size, total_sizes}
  end


  def solve(filepath) do
    file = File.read!(filepath)|> String.trim() |> String.split("\n")
    tree = parse_input(file)
    {_dir_size, total_sizes} = compute_sizes(tree)
    total_sizes
  end

  def solve2(filepath) do
    file = File.read!(filepath)|> String.trim() |> String.split("\n")
    tree = parse_input(file)
    {dir_size, _total_sizes} = compute_sizes(tree)
    required = 30000000 - (70000000 - dir_size)
    {_dir_size, to_delete} = compute_sizes2(tree, required, dir_size)
    to_delete
  end

  def is_best_option(size, required, current) do
    size >= required && size < current
  end
  def pick_smallest(el, acc, required) do
    if el >= required && el < acc do
      el
    else
      acc
    end
  end
  def compute_sizes2(tree, required, current) do
    dir_sizes = if tree[:dir_tree] != nil do
        Enum.map(Map.keys(tree[:dir_tree]), fn x ->compute_sizes2(tree[:dir_tree][x], required, current) end) |> Enum.reduce({0,current}, fn (el, acc )->
          {el_size, el_best} = el
          {acc_total_size, acc_best} = acc
        {el_size+acc_total_size, pick_smallest(el_best, acc_best, required)}
      end)
    else
      {0, current}
    end
    # return: my size + total_size
    file_sizes = get_file_sizes(tree[:files_tree])
    dir_size = elem(dir_sizes, 0) + file_sizes
    candidate = elem(dir_sizes, 1)
    current = if candidate >= required && candidate < current do
      candidate
    else
      current
    end
    current = if dir_size >= required && dir_size < current do
      dir_size
    else
      current
    end
    {dir_size, current}
  end
end
