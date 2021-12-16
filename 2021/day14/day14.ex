defmodule Day14 do
  def problem1 do
    {start, rules} = parse_file("input")

    freqs =
      start
      |> Enum.chunk_every(2, 1, :discard)
      |> Enum.frequencies()

    final_freqs =
      Enum.reduce(1..10, freqs, fn _, freqs ->
        apply_rules(freqs, rules)
      end)

    {min_count, max_count} =
      final_freqs
      |> Enum.reduce(%{}, fn {[a, _b], count}, counts ->
        Map.update(counts, a, count, &(&1 + count))
      end)
      |> Map.update(List.last(start), 1, &(&1 + 1))
      |> Map.values()
      |> Enum.min_max()

    max_count - min_count
  end

  def problem2 do
    {start, rules} = parse_file("input")

    freqs =
      start
      |> Enum.chunk_every(2, 1, :discard)
      |> Enum.frequencies()

    final_freqs =
      Enum.reduce(1..40, freqs, fn _, freqs ->
        apply_rules(freqs, rules)
      end)

    {min_count, max_count} =
      final_freqs
      |> Enum.reduce(%{}, fn {[a, _b], count}, counts ->
        Map.update(counts, a, count, &(&1 + count))
      end)
      |> Map.update(List.last(start), 1, &(&1 + 1))
      |> Map.values()
      |> Enum.min_max()

    max_count - min_count
  end

  defp apply_rules(freqs, rules) do
    Enum.reduce(freqs, %{}, fn {[a, b] = pair, count}, new_map ->
      result = Map.fetch!(rules, pair)

      new_map
      |> Map.update([a, result], count, &(&1 + count))
      |> Map.update([result, b], count, &(&1 + count))
    end)
  end

  defp parse_file(file) do
    [start, _ | rules] =
      file
      |> File.read!()
      |> String.split("\n")

    rules =
      rules
      |> Enum.reject(&(&1 == ""))
      |> Map.new(fn rule ->
        [pair, [result]] = rule |> String.split(" -> ") |> Enum.map(&String.to_charlist/1)

        {pair, result}
      end)

    {String.to_charlist(start), rules}
  end
end
