defmodule Day2 do
  def problem1() do
    read_file()
    |> Enum.reduce({0, 0}, fn
      {"forward", amount}, {horizontal, depth} ->
        {horizontal + amount, depth}

      {"up", amount}, {horizontal, depth} ->
        {horizontal, depth - amount}

      {"down", amount}, {horizontal, depth} ->
        {horizontal, depth + amount}
    end)
    |> then(fn {horizontal, depth} -> horizontal * depth end)
  end

  def problem2() do
    read_file()
    |> Enum.reduce({0, 0, 0}, fn
      {"forward", amount}, {horizontal, depth, aim} ->
        {horizontal + amount, depth + aim * amount, aim}

      {"up", amount}, {horizontal, depth, aim} ->
        {horizontal, depth, aim - amount}

      {"down", amount}, {horizontal, depth, aim} ->
        {horizontal, depth, aim + amount}
    end)
    |> then(fn {horizontal, depth, _aim} -> horizontal * depth end)
  end

  defp read_file() do
    "input.txt"
    |> File.read!()
    |> String.split("\n")
    |> Enum.reject(&(&1 == ""))
    |> Enum.map(&String.split(&1, " "))
    |> Enum.map(fn [command, amount] -> {command, String.to_integer(amount)} end)
  end
end
