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
  def parse_lines(data), do: String.split(data, "\n", trim: true)

  # Part One

  def part_one do
    read_input_data()
    |> parse_lines()
    |> split_pairs()
    |> map_shared_in_pairs()
    |> sum_shared_priorities()
  end

  def part_one_verbose do
    read_input_data()
    |> parse_lines()
    |> split_pairs()
    |> map_shared_with_priority(&find_shared_in_pair/1)
    |> then(fn result ->
      {sum_shared_priorities(result), result}
    end)
  end

  def split_pairs(lines) do
    Enum.map(lines, fn line ->
      String.split_at(line, div(String.length(line), 2))
    end)
  end

  def map_shared_in_pairs(pairs) do
    Enum.map(pairs, &find_shared_in_pair/1)
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

  # Private

  defp split_letters(string), do: String.split(string, ~r//, trim: true)

  defp find_shared_in_pair({left, right}) do
    left
    |> String.myers_difference(right)
    |> Enum.filter(fn {key, _} -> key == :eq end)
    |> Keyword.values()
    |> Enum.join("")
    |> split_letters()
    |> Enum.uniq()
  end

  defp priority_for(<<letter::binary-size(1)>>)
       when is_binary(letter) do
    letter
    |> String.to_charlist()
    |> List.first()
    |> priority_for()
  end

  defp priority_for(n) when n in 97..122, do: n - 96
  defp priority_for(n) when n in 65..90, do: n - 38

  # Part Two

  def part_two do
    read_input_data()
    |> parse_lines()
    |> chunk_groups()
    |> map_shared_in_groups()
    |> sum_shared_priorities()
  end

  def part_two_verbose do
    read_input_data()
    |> parse_lines()
    |> chunk_groups()
    |> map_shared_with_priority(&find_shared_in_group/1)
    |> then(fn result ->
      {sum_shared_priorities(result), result}
    end)
  end

  def chunk_groups(lines) do
    Enum.chunk_every(lines, 3)
  end

  def map_shared_in_groups(groups) do
    Enum.map(groups, &find_shared_in_group/1)
  end

  # Private

  defp find_shared_in_group([one, two, three]) do
    ones = split_letters(one)
    twos = split_letters(two)
    threes = split_letters(three)

    ones
    |> Enum.reduce(%{}, fn letter, acc ->
      if letter in twos && letter in threes do
        Map.put(acc, letter, true)
      else
        acc
      end
    end)
    |> Map.keys()
  end

  defp map_shared_with_priority(list, find_shared_fn) do
    Enum.map(list, fn item ->
      shared = find_shared_fn.(item)

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
end
