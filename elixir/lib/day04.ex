defmodule Aoc.Day04 do
  @moduledoc """
  Day 4: Camp Cleanup.
  """

  @input_file "../data/input04.txt"
  @sample_file "../data/input04-sample.txt"

  def read_input_data, do: File.read!(@input_file)
  def read_sample_data, do: File.read!(@sample_file)
  def split_lines(data), do: String.split(data, "\n", trim: true)

  # Main

  def main do
    IO.inspect({
      part_one(),
      part_two()
    })
  end

  # Part One

  def part_one do
    read_input_data()
    |> split_lines()
    |> Enum.map(&split_map_set/1)
    |> Enum.map(&subsets?/1)
    |> Enum.filter(& &1)
    |> Enum.count()
  end

  def subsets?([left, right]) do
    MapSet.subset?(left, right) or
      MapSet.subset?(right, left)
  end

  # Part Two

  def part_two do
    read_input_data()
    |> split_lines()
    |> Enum.map(&split_map_set/1)
    |> Enum.map(&intersect?/1)
    |> Enum.filter(& &1)
    |> Enum.count()
  end

  def intersect?([left, right]) do
    not MapSet.disjoint?(left, right)
  end

  # Private

  defp split_map_set(line) do
    line
    |> String.split(",", trim: true)
    |> Enum.map(fn range ->
      [first, last] =
        range
        |> String.split("-", trim: true)
        |> Enum.map(&String.to_integer/1)

      first
      |> Range.new(last)
      |> MapSet.new()
    end)
  end
end
