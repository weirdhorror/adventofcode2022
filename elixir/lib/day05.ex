defmodule Aoc.Day05 do
  @moduledoc """
  Day 4: Camp Cleanup.
  """

  @input_file "../data/input05.txt"
  # @sample_file "../data/input05-sample.txt"

  # Main

  def main do
    IO.inspect({
      part_one().top_crates,
      part_two().top_crates
    })
  end

  def part_one, do: solve(9000)
  def part_two, do: solve(9001)

  def solve(crane_model) do
    @input_file
    |> File.read!()
    |> split_stack_moves()
    |> apply_moves(crane_model)
    |> put_top_crates()
  end

  @doc """
  Parse input date into a data structure with this shape:

      %{
        stacks: %{
          0 => ["C", "W", "R", "D", "J"],
          1 => ["Z", "D"],
          2 => ["F", "H", "C"],
          ...
        },
        moves: [
          %{move: 3, from: 8, to: 5},
          %{move: 7, from: 5, to: 1},
          %{move: 1, from: 0, to: 4},
          ...
        ]
      }

  """
  def split_stack_moves(input_data) do
    # list of stacks, list of moves
    [stacks, moves] =
      input_data
      |> String.split("\n\n", trim: true)
      |> Enum.map(&String.split(&1, "\n", trim: true))

    # pop stack_numbers from stacks list
    {stack_numbers, stacks} = List.pop_at(stacks, -1)

    # map stack number string indexes
    crate_indexes =
      stack_numbers
      |> String.split(~r//, trim: true)
      |> Enum.with_index()
      |> Enum.reduce([], fn {s, index}, acc ->
        if String.match?(s, ~r/\d+/) do
          [index | acc]
        else
          acc
        end
      end)
      |> Enum.reverse()

    # reshape stacks as list of crate strings at crate indexes
    stacks =
      crate_indexes
      |> Enum.with_index()
      |> Enum.into(%{}, fn {crate_index, index} ->
        {index,
         stacks
         |> Enum.reduce([], fn stack, acc ->
           crate = String.slice(stack, crate_index, 1)

           if String.match?(crate, ~r/\S+/) do
             [crate | acc]
           else
             acc
           end
         end)
         |> Enum.reverse()}
      end)

    # "move 1 from 2 to 1"
    re = ~r/move (?<move>\d+) from (?<from>\d+) to (?<to>\d+)/

    moves =
      Enum.map(moves, fn move ->
        %{"move" => move, "from" => from, "to" => to} = Regex.named_captures(re, move)

        %{
          from: String.to_integer(from) - 1,
          to: String.to_integer(to) - 1,
          move: String.to_integer(move)
        }
      end)

    %{
      stacks: stacks,
      moves: moves
    }
  end

  @doc """
  Apply moves to stacks and return given map with updated
  stacks.
  """
  def apply_moves(
        %{stacks: init_stacks, moves: moves} = data,
        crane_model
      ) do
    # 9000 model reverses crates, 9001 does not
    crane_model_fn =
      case crane_model do
        9000 -> fn crates -> Enum.reverse(crates) end
        9001 -> fn crates -> crates end
      end

    %{
      data
      | stacks:
          Enum.reduce(moves, init_stacks, fn move, stacks ->
            stacks
            |> Map.get_and_update(move.from, fn crates ->
              Enum.split(crates, move.move)
            end)
            |> then(fn {moved, result} ->
              update_in(result, [move.to], fn crates ->
                crane_model_fn.(moved) ++ crates
              end)
            end)
          end)
    }
  end

  @doc """
  Add `:top_crates` key to map for solution.
  """
  def put_top_crates(%{stacks: stacks} = data) do
    Map.put(
      data,
      :top_crates,
      stacks
      |> Enum.reduce([], fn
        {_, [crate | _]}, acc -> [crate | acc]
        _, acc -> acc
      end)
      |> Enum.reverse()
      |> Enum.join("")
    )
  end
end
