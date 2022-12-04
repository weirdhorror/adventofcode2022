defmodule Day03 do
  @moduledoc """
  Rucksack Reorganization.
  """

  @input_file "../data/input03.txt"

  def sample_data do
    """
    vJrwpWtwJgWrhcsFMMfFFhFp
    jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
    PmmdzqPrVvPwwTWBwg
    wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
    ttgJtRGJQctTZtZT
    CrZsJsPPZsGzwwsLwLmpwMDw
    """
  end

  def read_input_data, do: File.read!(@input_file)
  def split_lines(data), do: String.split(data, "\n", trim: true)

  # Part One

  def part_one(options \\ []) do
    read_input_data()
    |> split_lines()
    |> split_pairs()
    |> map_shared_with_priority()
    |> format_result(options)
  end

  def split_pairs(lines) do
    Enum.map(lines, fn line ->
      line
      |> String.split_at(div(String.length(line), 2))
      |> Tuple.to_list()
    end)
  end

  # Part Two

  def part_two(options \\ []) do
    read_input_data()
    |> split_lines()
    |> chunk_groups()
    |> map_shared_with_priority()
    |> format_result(options)
  end

  def chunk_groups(lines) do
    Enum.chunk_every(lines, 3)
  end

  # Common

  def find_shared([first | rest]) do
    firsts = split_unique_letters(first)
    rests = Enum.map(rest, &split_unique_letters/1)

    firsts
    |> Enum.reduce(%{}, fn letter, shared ->
      if Enum.all?(rests, fn letters ->
           letter in letters
         end) do
        Map.put(shared, letter, true)
      else
        shared
      end
    end)
    |> Map.keys()
  end

  # Private

  defp split_unique_letters(string) do
    string
    |> String.split(~r//, trim: true)
    |> Enum.uniq()
  end

  defp map_shared_with_priority(pairs_or_groups) do
    Enum.map(pairs_or_groups, fn pair_or_group ->
      shared = find_shared(pair_or_group)

      priority =
        shared
        |> Enum.map(&priority_for/1)
        |> Enum.sum()

      %{
        shared: shared,
        priority: priority
      }
    end)
  end

  defp priority_for(n) when n in 97..122, do: n - 96
  defp priority_for(n) when n in 65..90, do: n - 38

  defp priority_for(<<letter::binary-size(1)>>)
       when is_binary(letter) do
    letter
    |> String.to_charlist()
    |> List.first()
    |> priority_for()
  end

  defp format_result(result, options) do
    sum = sum_shared_priorities(result)

    if options[:verbose] do
      {sum, result}
    else
      sum
    end
  end

  def sum_shared_priorities(shared) do
    Enum.reduce(shared, 0, fn
      %{priority: priority}, sum ->
        sum + priority

      shared_letters, sum ->
        priority =
          shared_letters
          |> Enum.map(&priority_for/1)
          |> Enum.sum()

        sum + priority
    end)
  end
end
