defmodule Day11 do
  def problem1 do
    Enum.reduce(0..99, {0, parse_file()}, fn _, {flash_count, map} ->
      map = tick(map)
      flash_count = flash_count + Enum.count(map, fn {_k, v} -> v == 0 end)

      {flash_count, map}
    end)
  end

  def problem2 do
    sync_step(parse_file(), 0)
  end

  defp sync_step(map, step) do
    if Enum.all?(map, fn {_k, v} -> v == 0 end) do
      step
    else
      map
      |> tick()
      |> sync_step(step + 1)
    end
  end

  defp tick(map) do
    map
    |> increment()
    |> flash()
    |> print()
  end

  defp increment(map) do
    for x <- 0..9, y <- 0..9, reduce: map do
      map -> Map.update!(map, {x, y}, &(&1 + 1))
    end
  end

  defp flash(map) do
    ready_to_flash = ready_to_flash(map)

    if Enum.any?(ready_to_flash) do
      ready_to_flash
      |> Enum.reduce(map, fn coord, map ->
        map = Map.put(map, coord, 0)

        Enum.reduce(neighbors(coord), map, fn neighbor, map ->
          Map.update!(map, neighbor, &if(&1 == 0, do: 0, else: &1 + 1))
        end)
      end)
      |> flash()
    else
      map
    end
  end

  defp ready_to_flash(map) do
    map
    |> Enum.filter(fn {_k, v} -> v > 9 end)
    |> Enum.map(&elem(&1, 0))
  end

  defp neighbors({x, y}) do
    Enum.reject(
      [
        {x - 1, y - 1},
        {x, y - 1},
        {x + 1, y - 1},
        {x - 1, y},
        {x + 1, y},
        {x - 1, y + 1},
        {x, y + 1},
        {x + 1, y + 1}
      ],
      fn {x, y} -> x < 0 || x > 9 || y < 0 || y > 9 end
    )
  end

  defp print(map) do
    for y <- 0..9 do
      for x <- 0..9 do
        IO.write(Map.get(map, {x, y}))
      end

      IO.write("\n")
    end

    IO.write("\n")
    map
  end

  defp parse_file() do
    rows =
      "input"
      |> File.read!()
      |> String.split("\n")
      |> Enum.reject(&(&1 == ""))
      |> Enum.map(&to_charlist/1)

    for {row, y} <- Enum.with_index(rows), {cell, x} <- Enum.with_index(row), reduce: %{} do
      acc -> Map.put(acc, {x, y}, cell - 48)
    end
  end
end
