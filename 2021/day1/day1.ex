defmodule Day1 do
  def problem1() do
    read_file()
    |> count_increasing_depths()
  end

  def count_increasing_depths(depths) do
    depths
    |> Enum.chunk_every(2, 1, :discard)
    |> Enum.filter(fn [prev, next] -> next > prev end)
    |> length()
  end

  def problem2 do
    read_file()
    |> combine_depths()
    |> count_increasing_depths()
  end

  def combine_depths(depths) do
    depths
    |> Enum.chunk_every(3, 1, :discard)
    |> Enum.map(&Enum.sum/1)
  end

  defp read_file do
    "input.txt"
    |> File.read!()
    |> String.split("\n")
    |> Enum.reject(&(&1 == ""))
    |> Enum.map(&String.to_integer/1)
  end
end
