defmodule Day15 do
  defmodule Cell do
    # Since `nil` is greater than any integer by Erlang's term ordering, it essentially functions as "infinity"
    defstruct risk: nil, accumulated_risk: nil, least?: false
  end

  def problem1() do
    matrix = parse_file("input")

    max_x = matrix |> Map.keys() |> Enum.map(&elem(&1, 0)) |> Enum.max()
    max_y = matrix |> Map.keys() |> Enum.map(&elem(&1, 1)) |> Enum.max()

    start =
      matrix
      |> Map.get({0, 0})
      |> Map.merge(%{accumulated_risk: 0, least?: true})

    final_matrix =
      matrix
      |> Map.put({0, 0}, start)
      |> shortest_path([{{{0, 0}, start}, neighbors(matrix, {0, 0})}], {max_x, max_y})

    Map.get(final_matrix, {max_x, max_y}).accumulated_risk
  end

  defp shortest_path(matrix, candidates, finish) do
    case matrix do
      %{^finish => %Cell{least?: true}} ->
        matrix

      _ ->
        matrix =
          Enum.reduce(candidates, matrix, fn {{_pos, cell}, candidates}, matrix ->
            Enum.reduce(candidates, matrix, fn {npos, ncell}, matrix ->
              accumulated_risk = ncell.risk + cell.accumulated_risk

              if accumulated_risk < ncell.accumulated_risk do
                Map.update!(
                  matrix,
                  npos,
                  &Map.merge(&1, %{accumulated_risk: accumulated_risk})
                )
              else
                matrix
              end
            end)
          end)

        candidates =
          Enum.map(candidates, fn {ss, cs} ->
            {ss, Enum.map(cs, fn {cpos, _old_ccell} -> {cpos, Map.get(matrix, cpos)} end)}
          end)

        min =
          {min_pos, min_cell} =
          candidates
          |> Enum.flat_map(fn {_, cs} -> cs end)
          |> Enum.min_by(fn {_pos, cell} -> cell.accumulated_risk end)

        matrix = Map.put(matrix, min_pos, %Cell{min_cell | least?: true})

        candidates =
          [{min, neighbors(matrix, min_pos)} | candidates]
          |> Enum.map(fn {start, cs} -> {start, List.delete(cs, min)} end)
          |> Enum.reject(fn {_start, cs} -> cs == [] end)

        shortest_path(matrix, candidates, finish)
    end
  end

  defp print(matrix, max_x, max_y) do
    for y <- 0..max_y do
      for x <- 0..max_x do
        case Map.get(matrix, {x, y}) do
          %{least?: true, risk: risk} ->
            IO.write(IO.ANSI.bright() <> to_string(risk) <> IO.ANSI.reset())

          %{risk: risk} when not is_nil(risk) ->
            IO.write(risk)

          _ ->
            IO.write(".")
        end
      end

      IO.write("\n")
    end

    IO.write("\n")

    matrix
  end

  defp neighbors(matrix, {x, y}) do
    [
      {x - 1, y},
      {x + 1, y},
      {x, y - 1},
      {x, y + 1}
    ]
    |> Enum.map(&{&1, Map.get(matrix, &1)})
    |> Enum.reject(fn {_pos, cell} -> is_nil(cell) || cell.least? end)
  end

  def problem2 do
    matrix = parse_file("input")

    multipliers =
      for x <- 0..4, y <- 0..4 do
        {x, y}
      end

    max_x = matrix |> Map.keys() |> Enum.map(&elem(&1, 0)) |> Enum.max() |> Kernel.+(1)
    max_y = matrix |> Map.keys() |> Enum.map(&elem(&1, 1)) |> Enum.max() |> Kernel.+(1)

    matrix =
      Enum.reduce(matrix, matrix, fn {{x, y}, cell}, matrix ->
        Enum.reduce(multipliers, matrix, fn {multx, multy}, matrix ->
          Map.put(
            matrix,
            {x + multx * max_x, y + multy * max_y},
            Map.update!(cell, :risk, &(Integer.mod(&1 + multx + multy - 1, 9) + 1))
          )
        end)
      end)

    max_x = matrix |> Map.keys() |> Enum.map(&elem(&1, 0)) |> Enum.max()
    max_y = matrix |> Map.keys() |> Enum.map(&elem(&1, 1)) |> Enum.max()

    start =
      matrix
      |> Map.get({0, 0})
      |> Map.merge(%{accumulated_risk: 0, least?: true})

    final_matrix =
      matrix
      |> Map.put({0, 0}, start)
      |> shortest_path([{{{0, 0}, start}, neighbors(matrix, {0, 0})}], {max_x, max_y})

    Map.get(final_matrix, {max_x, max_y}).accumulated_risk
  end

  defp parse_file(filename) do
    matrix =
      filename
      |> File.read!()
      |> String.split("\n")
      |> Enum.map(&parse_line/1)

    for {row, y} <- Enum.with_index(matrix), {risk, x} <- Enum.with_index(row), reduce: %{} do
      map -> Map.put(map, {x, y}, %Cell{risk: risk})
    end
  end

  defp parse_line(line) do
    line |> String.to_charlist() |> Enum.map(&(&1 - 48))
  end
end
