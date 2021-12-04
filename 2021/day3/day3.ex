defmodule Day3 do
  def problem1() do
    numbers = read_file()
    total = length(numbers)

    gamma =
      numbers
      # Transpose matrix
      |> Enum.zip()
      |> Enum.map(&Tuple.sum/1)
      |> Enum.map(&if &1 > total / 2, do: 1, else: 0)

    epsilon =
      Enum.map(gamma, fn
        1 -> 0
        0 -> 1
      end)

    Integer.undigits(gamma, 2) * Integer.undigits(epsilon, 2)
  end

  def problem2() do
    numbers = read_file()

    o2_num =
      numbers
      |> find_measurement(0, fn count, quorum -> if(count >= quorum, do: 1, else: 0) end)
      |> Integer.undigits(2)

    co2_num =
      numbers
      |> find_measurement(0, fn count, quorum -> if(count < quorum, do: 1, else: 0) end)
      |> Integer.undigits(2)

    o2_num * co2_num
  end

  defp find_measurement([num], _bitpos, _cmp), do: num

  defp find_measurement(nums, bitpos, cmp) do
    bit_criterion =
      nums
      |> Enum.map(&Enum.at(&1, bitpos))
      |> Enum.sum()
      |> cmp.(length(nums) / 2)

    nums
    |> Enum.filter(&(Enum.at(&1, bitpos) == bit_criterion))
    |> find_measurement(bitpos + 1, cmp)
  end

  defp read_file() do
    "input"
    |> File.read!()
    |> String.split("\n")
    |> Enum.reject(&(&1 == ""))
    |> Enum.map(fn row -> row |> String.to_charlist() |> Enum.map(&(&1 - 48)) end)
  end
end
