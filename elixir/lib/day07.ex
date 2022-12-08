defmodule Aoc.Day07 do
  @moduledoc """
  Day 7: No Space Left On Device.
  """

  @input_file "../data/input07.txt"
  # @sample_file "../data/input07-sample.txt"

  # Main

  def main do
    tree =
      @input_file
      |> File.read!()
      |> String.split("\n", trim: true)
      |> build_tree()
      |> add_sizes()

    IO.inspect({
      part_one(tree),
      part_two(tree)
    })
  end

  def part_one(tree) do
    tree
    |> where_size_tree(fn size -> size <= 100_000 end)
    |> Enum.reduce(0, fn {_path, size}, sum -> sum + size end)
  end

  def part_two(tree) do
    space = df(tree)
    needed = 30_000_000 - space.available

    tree
    |> where_size_tree(fn size -> size >= needed end)
    |> Enum.sort_by(fn {_name, size} -> size end)
    |> List.first()
  end

  def build_tree(lines) do
    lines
    |> Enum.reduce({["/"], %{"/" => %{}}}, fn
      "$ ls", {paths, tree} ->
        {paths, tree}

      "$ cd ..", {[_path | paths], tree} ->
        {paths, tree}

      "$ cd /", {_paths, tree} ->
        {["/"], tree}

      "$ cd " <> path, {paths, tree} ->
        {[path | paths], tree}

      "dir " <> path, {paths, tree} ->
        {paths,
         update_in(tree, Enum.reverse([path | paths]), fn
           subtree when is_map(subtree) -> subtree
           _ -> %{files: %{}}
         end)}

      file, {paths, tree} ->
        [size, filename] =
          file
          |> String.split(" ")
          |> then(fn [s, f] -> [String.to_integer(s), f] end)

        {paths,
         update_in(tree, Enum.reverse([:files | paths]), fn
           files when is_map(files) -> Map.put(files, filename, size)
           _ -> %{filename => size}
         end)}
    end)
    |> elem(1)
  end

  def add_sizes(tree) do
    Enum.reduce(tree, tree, fn
      {:files, files}, tree ->
        Map.put(tree, :size_files, size_files(files))

      {key, _}, tree when is_atom(key) ->
        tree

      {_path, subtree}, tree when map_size(subtree) == 0 ->
        Map.put(tree, :size_files, 0)

      {path, subtree}, tree ->
        subtree = add_sizes(subtree)

        tree
        |> Map.put(path, subtree)
        |> put_in([path, :size_tree], size_tree(subtree))
    end)
  end

  def size_tree(%{size_tree: size_tree}) do
    size_tree
  end

  def size_tree(%{size_files: size_files} = tree) do
    size_files +
      (tree
       |> Map.drop([:files, :size_files])
       |> size_tree())
  end

  def size_tree(tree) do
    Enum.reduce(tree, 0, fn
      {:files, files}, total ->
        total + size_files(files)

      {key, _}, total when is_atom(key) ->
        total

      {_path, subtree}, total when map_size(subtree) == 0 ->
        total

      {_path, subtree}, total ->
        total + size_tree(subtree)
    end)
  end

  def size_files(files) do
    Enum.reduce(files, 0, fn {_, size}, total ->
      size + total
    end)
  end

  def where_size_tree(tree, pred_fn) do
    Enum.reduce(tree, [], fn
      {path, %{size_tree: size} = subtree}, result ->
        if pred_fn.(size) do
          [{path, size} | result]
        else
          result
        end ++ where_size_tree(subtree, pred_fn)

      _, result ->
        result
    end)
  end

  def df(tree, path \\ "/") do
    total = 70_000_000
    used = get_in(tree, [path, :size_tree])

    %{
      size: total,
      used: used,
      available: total - used
    }
  end
end
