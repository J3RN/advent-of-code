defmodule Day9 do
  @type value :: integer()
  @type coord :: {x :: integer, y :: integer}
  @type datum :: {coord, value}
  @type data :: [datum]

  @spec problem1() :: integer
  def problem1 do
    parse_file()
    |> find_low_points()
    |> calculate_risk()
  end

  @spec problem2() :: integer
  def problem2 do
    map = parse_file()

    map
    |> find_low_points()
    |> Enum.map(&find_basin_size([&1], map))
    |> Enum.sort(&(&1 >= &2))
    |> Enum.take(3)
    |> Enum.product()
  end

  @spec find_low_points(map) :: data
  defp find_low_points(map) do
    for datum <- map, low_point?(datum, map), do: datum
  end

  @spec low_point?(datum, map) :: integer
  defp low_point?({coord, value}, map) do
    coord
    |> neighbors(map)
    |> Enum.all?(&(elem(&1, 1) > value))
  end

  @spec calculate_risk(data) :: integer
  defp calculate_risk(low_points) do
    low_points
    |> Enum.map(&elem(&1, 1))
    |> Enum.map(&(&1 + 1))
    |> Enum.sum()
  end

  @spec find_basin_size(data, map, data) :: integer
  defp find_basin_size(next, map, visited \\ [])

  defp find_basin_size([], _map, visited), do: length(visited)

  defp find_basin_size(to_visit, map, visited) do
    next =
      Enum.map(to_visit, fn {coord, value} ->
        coord
        |> neighbors(map)
        |> Enum.filter(fn {_coord, value2} -> value2 > value && value2 != 9 end)
      end)
      |> List.flatten()
      |> Enum.uniq()

    visited = to_visit ++ visited

    next = Enum.reject(next, &(&1 in visited))

    find_basin_size(next, map, visited)
  end

  @spec neighbors(coord, map) :: data
  defp neighbors({x, y}, map) do
    [
      {x, y + 1},
      {x, y - 1},
      {x + 1, y},
      {x - 1, y}
    ]
    |> Enum.map(&{&1, Map.get(map, &1)})
    |> Enum.filter(&(not is_nil(elem(&1, 1))))
  end

  @spec parse_file() :: map
  defp parse_file do
    input =
      "input"
      |> File.read!()
      |> String.split("\n")
      |> Enum.reject(&(&1 == ""))
      |> Enum.map(&String.codepoints/1)

    for {row, y} <- Enum.with_index(input), {num, x} <- Enum.with_index(row), into: %{} do
      {{x, y}, String.to_integer(num)}
    end
  end
end
