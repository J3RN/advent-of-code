contents = File.read!("input")

group_sums =
  contents
  |> String.trim_trailing("\n")
  |> String.split("\n\n")
  |> Enum.map(&String.split(&1, "\n"))
  |> Enum.map(fn group -> Enum.map(group, &String.to_integer/1) end)
  |> Enum.map(&Enum.sum/1)

max = Enum.max(group_sums)
IO.puts("Max: #{max}")

top3 =
  group_sums
  |> Enum.take(3)
  |> Enum.sum()

IO.puts("Sum of top 3: #{top3}")
