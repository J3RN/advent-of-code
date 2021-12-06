defmodule Day5 do
  def problem1 do
    coordinate_pairs = parse_file()

    ocean_floor_map = %{}

    coordinate_pairs
    |> Enum.reject(&diagonal?/1)
    |> Enum.reduce(ocean_floor_map, &apply_pair/2)
    |> Enum.count(fn {_coordinate, count} -> count > 1 end)
  end

  def problem2 do
    coordinate_pairs = parse_file()

    ocean_floor_map = %{}

    finished_ocean_map = Enum.reduce(coordinate_pairs, ocean_floor_map, &apply_pair/2)

    Enum.count(finished_ocean_map, fn {_coordinate, count} -> count > 1 end)
  end

  defp diagonal?({{x, _y1}, {x, _y2}}), do: false
  defp diagonal?({{_x1, y}, {_x2, y}}), do: false
  defp diagonal?(_), do: true

  defp apply_pair(coordinate_pair, ocean_floor_map) do
    case coordinate_pair do
      {{x, y1}, {x, y2}} ->
        Enum.reduce(y1..y2, ocean_floor_map, fn y, acc ->
          Map.update(acc, {x, y}, 1, &(&1 + 1))
        end)

      {{x1, y}, {x2, y}} ->
        Enum.reduce(x1..x2, ocean_floor_map, fn x, acc ->
          Map.update(acc, {x, y}, 1, &(&1 + 1))
        end)

      {{x1, y1}, {x2, y2}} ->
        Enum.reduce(Enum.zip(x1..x2, y1..y2), ocean_floor_map, fn coordinate, acc ->
          Map.update(acc, coordinate, 1, &(&1 + 1))
        end)
    end
  end

  defp parse_file do
    "input"
    |> File.read!()
    |> String.split("\n")
    |> Enum.reject(&(&1 == ""))
    |> Enum.map(fn row ->
      [pair_one, pair_two] = String.split(row, " -> ")
      {parse_pair(pair_one), parse_pair(pair_two)}
    end)
  end

  defp parse_pair(pair_string) do
    [x, y] = String.split(pair_string, ",")
    {String.to_integer(x), String.to_integer(y)}
  end
end
