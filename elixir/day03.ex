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
    |> map_shared_with_priority()
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
    Enum.map(pairs, &find_shared/1)
  end

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
    |> map_shared_with_priority()
    |> then(fn result ->
      {sum_shared_priorities(result), result}
    end)
  end

  def chunk_groups(lines) do
    Enum.chunk_every(lines, 3)
  end

  def map_shared_in_groups(groups) do
    Enum.map(groups, &find_shared/1)
  end

  # Private

  defp split_letters(string), do: String.split(string, ~r//, trim: true)

  defp priority_for(<<letter::binary-size(1)>>)
       when is_binary(letter) do
    letter
    |> String.to_charlist()
    |> List.first()
    |> priority_for()
  end

  defp priority_for(n) when n in 97..122, do: n - 96
  defp priority_for(n) when n in 65..90, do: n - 38

  defp find_shared([first | rest]) do
    firsts = split_letters(first)
    rests = Enum.map(rest, &split_letters/1)

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

  defp find_shared({a, b}), do: find_shared([a, b])

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
