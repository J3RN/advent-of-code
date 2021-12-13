defmodule Day13 do
  def problem1 do
    {dots, instructions} = parse_file("input")

    {dots, Enum.take(instructions, 1)}
    |> apply_folds()
    |> Enum.count()
  end

  def problem2 do
    parse_file("input")
    |> apply_folds()
    |> print()
  end

  defp apply_folds({paper, instructions}) do
    Enum.reduce(instructions, paper, fn
      {:x, scalar}, paper ->
        paper
        |> Enum.map(fn
          {x, y} when x > scalar -> {scalar - (x - scalar), y}
          {x, y} -> {x, y}
        end)
        |> MapSet.new()

      {:y, scalar}, paper ->
        paper
        |> Enum.map(fn
          {x, y} when y > scalar -> {x, scalar - (y - scalar)}
          {x, y} -> {x, y}
        end)
        |> MapSet.new()
    end)
  end

  defp print(paper) do
    max_x = paper |> Enum.map(&elem(&1, 0)) |> Enum.max()
    max_y = paper |> Enum.map(&elem(&1, 1)) |> Enum.max()

    for y <- 0..max_y do
      for x <- 0..max_x do
        if MapSet.member?(paper, {x, y}) do
          IO.write("#")
        else
          IO.write(" ")
        end
      end

      IO.write("\n")
    end

    IO.write("\n")

    paper
  end

  defp parse_file(file) do
    [dot_lines, instructions_lines] =
      file
      |> File.read!()
      |> String.split("\n\n")
      |> Enum.map(&String.split(&1, "\n"))

    dots =
      dot_lines
      |> Enum.map(&parse_dot_line/1)
      |> Enum.into(MapSet.new())

    instructions =
      instructions_lines
      |> Enum.reject(&(&1 == ""))
      |> Enum.map(&parse_instruction_line/1)

    {dots, instructions}
  end

  defp parse_dot_line(line) do
    [x, y] =
      line
      |> String.split(",")
      |> Enum.map(&String.to_integer/1)

    {x, y}
  end

  defp parse_instruction_line(line) do
    %{"axis" => axis, "scalar" => scalar} =
      Regex.named_captures(~r/fold along (?<axis>\w)=(?<scalar>\d+)/, line)

    {String.to_existing_atom(axis), String.to_integer(scalar)}
  end
end
