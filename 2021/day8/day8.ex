defmodule Day8 do
  def problem1 do
    parse_file()
    |> Enum.map(&Map.get(&1, "output_value"))
    |> Enum.concat()
    |> Enum.count(&(length(&1) in [2, 3, 4, 7]))
  end

  def problem2 do
    inputs = parse_file()

    inputs
    |> Enum.map(fn %{"signal_patterns" => patterns, "output_value" => output} ->
      mappings = deduce_mappings(patterns)

      output
      |> Enum.map(&decode(&1, mappings))
      |> Enum.map(&charlist_to_digit/1)
      |> Integer.undigits()
    end)
    |> Enum.sum()
  end

  defp deduce_mappings(signal_patterns) do
    one = find_one(signal_patterns)
    four = find_four(signal_patterns)
    seven = find_seven(signal_patterns)

    %{
      deduce_a(signal_patterns, one, four, seven) => ?a,
      deduce_b(signal_patterns, one, four, seven) => ?b,
      deduce_c(signal_patterns, one, four, seven) => ?c,
      deduce_d(signal_patterns, one, four, seven) => ?d,
      deduce_e(signal_patterns, one, four, seven) => ?e,
      deduce_f(signal_patterns, one, four, seven) => ?f,
      deduce_g(signal_patterns, one, four, seven) => ?g
    }
  end

  defp find_one(signal_patterns),
    do: Enum.find(signal_patterns, &(length(&1) == 2))

  defp find_four(signal_patterns),
    do: Enum.find(signal_patterns, &(length(&1) == 4))

  defp find_seven(signal_patterns),
    do: Enum.find(signal_patterns, &(length(&1) == 3))

  defp deduce_a(signal_patterns, one, four, seven) do
    Enum.find(?a..?g, fn a ->
      a not in one && a not in four && a in seven &&
        Enum.count(List.flatten(signal_patterns), &(&1 == a)) == 8
    end)
  end

  defp deduce_b(signal_patterns, one, four, seven) do
    Enum.find(?a..?g, fn b ->
      b not in one && b in four && b not in seven &&
        Enum.count(List.flatten(signal_patterns), &(&1 == b)) == 6
    end)
  end

  defp deduce_c(signal_patterns, one, four, seven) do
    Enum.find(?a..?g, fn c ->
      c in one && c in four && c in seven &&
        Enum.count(List.flatten(signal_patterns), &(&1 == c)) == 8
    end)
  end

  defp deduce_d(signal_patterns, one, four, seven) do
    Enum.find(?a..?g, fn d ->
      d not in one && d in four && d not in seven &&
        Enum.count(List.flatten(signal_patterns), &(&1 == d)) == 7
    end)
  end

  defp deduce_e(signal_patterns, one, four, seven) do
    Enum.find(?a..?g, fn e ->
      e not in one && e not in four && e not in seven &&
        Enum.count(List.flatten(signal_patterns), &(&1 == e)) == 4
    end)
  end

  defp deduce_f(signal_patterns, one, four, seven) do
    Enum.find(?a..?g, fn f ->
      f in one && f in four && f in seven &&
        Enum.count(List.flatten(signal_patterns), &(&1 == f)) == 9
    end)
  end

  defp deduce_g(signal_patterns, one, four, seven) do
    Enum.find(?a..?g, fn g ->
      g not in one && g not in four && g not in seven &&
        Enum.count(List.flatten(signal_patterns), &(&1 == g)) == 7
    end)
  end

  defp decode(clist, mapping) do
    Enum.map(clist, &Map.get(mapping, &1))
  end

  defp charlist_to_digit(clist) do
    case Enum.sort(clist) do
      'abcefg' -> 0
      'cf' -> 1
      'acdeg' -> 2
      'acdfg' -> 3
      'bcdf' -> 4
      'abdfg' -> 5
      'abdefg' -> 6
      'acf' -> 7
      'abcdefg' -> 8
      'abcdfg' -> 9
    end
  end

  defp parse_file do
    "input"
    |> File.read!()
    |> String.split("\n")
    |> Enum.reject(&(&1 == ""))
    |> Enum.map(&parse_line/1)
  end

  defp parse_line(line) do
    ~r/(?<signal_patterns>(?:\w+\s*)+) \| (?<output_value>.+)/
    |> Regex.named_captures(line)
    |> Map.map(fn {_key, val} -> val |> String.split() |> Enum.map(&String.to_charlist/1) end)
  end
end
