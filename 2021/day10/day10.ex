defmodule Day10 do
  def problem1 do
    parse_file()
    |> Enum.map(&score/1)
    |> Enum.sum()
  end

  defp score(line) do
    Enum.reduce_while(line, [], fn let, stack ->
      case let do
        ?) -> if(hd(stack) == ?(, do: {:cont, tl(stack)}, else: {:halt, 3})
        ?] -> if(hd(stack) == ?[, do: {:cont, tl(stack)}, else: {:halt, 57})
        ?} -> if(hd(stack) == ?{, do: {:cont, tl(stack)}, else: {:halt, 1197})
        ?> -> if(hd(stack) == ?<, do: {:cont, tl(stack)}, else: {:halt, 25137})
        other -> {:cont, [other | stack]}
      end
    end)
    |> case do
      val when is_integer(val) -> val
      _ -> 0
    end
  end

  def problem2 do
    parse_file()
    |> Enum.reject(&corrupted?/1)
    |> Enum.map(&completion_score/1)
    |> middle()
  end

  defp corrupted?(line) do
    score(line) != 0
  end

  defp completion_score(line) do
    line
    |> unmatched()
    |> match_score()
  end

  defp unmatched(line) do
    Enum.reduce(line, [], fn let, stack ->
      case let do
        close when close in [?), ?], ?}, ?>] -> tl(stack)
        open -> [open | stack]
      end
    end)
  end

  defp match_score(unmatched) do
    Enum.reduce(unmatched, 0, fn tok, score ->
      score * 5 + value(tok)
    end)
  end

  defp value(?(), do: 1
  defp value(?[), do: 2
  defp value(?{), do: 3
  defp value(?<), do: 4

  defp middle(list) do
    list
    |> Enum.sort()
    |> Enum.at(div(length(list), 2))
  end

  defp parse_file do
    "input"
    |> File.read!()
    |> String.split("\n")
    |> Enum.reject(&(&1 == ""))
    |> Enum.map(&String.to_charlist/1)
  end
end
