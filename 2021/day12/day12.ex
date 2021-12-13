defmodule Day12 do
  defmodule BigCave do
    defstruct name: nil, neighbors: []
  end

  defmodule LittleCave do
    defstruct name: nil, neighbors: []
  end

  def problem1() do
    parse_file()
    |> find_paths(&can_visit?/2)
    |> Enum.count()
  end

  def problem2() do
    parse_file()
    |> find_paths(&can_visit2?/2)
    |> Enum.count()
  end

  defp find_paths(caves, filter_fun) do
    start = Map.fetch!(caves, "start")
    find_paths(start, caves, [start], filter_fun)
  end

  defp find_paths(%LittleCave{name: "end"}, _caves, path, _filter_fun),
    do: [path |> Enum.reverse() |> Enum.map(& &1.name)]

  defp find_paths(cave, caves, path, filter_fun) do
    cave.neighbors
    |> Enum.map(&Map.fetch!(caves, &1))
    |> Enum.filter(&filter_fun.(&1, path))
    |> Enum.flat_map(&find_paths(&1, caves, [&1 | path], filter_fun))
  end

  defp can_visit?(cave, path) do
    case cave do
      %BigCave{} -> true
      %LittleCave{} = little_cave -> little_cave not in path
    end
  end

  defp can_visit2?(cave, path) do
    case cave do
      %BigCave{} ->
        true

      %LittleCave{name: "start"} ->
        false

      %LittleCave{} = little_cave ->
        little_cave not in path || !any_little_cave_visited_twice?(path)
    end
  end

  defp any_little_cave_visited_twice?(path) do
    path
    |> Enum.filter(&is_struct(&1, LittleCave))
    |> Enum.frequencies()
    |> Map.values()
    |> Enum.any?(&(&1 >= 2))
  end

  defp parse_file() do
    edges =
      "input"
      |> File.read!()
      |> String.split("\n")
      |> Enum.reject(&(&1 == ""))
      |> Enum.map(&String.split(&1, "-"))

    caves = edges |> List.flatten() |> Map.new(fn name -> {name, make_cave(name)} end)

    Enum.reduce(edges, caves, fn [from_name, to_name], caves ->
      from_cave =
        caves
        |> Map.fetch!(from_name)
        |> Map.update!(:neighbors, &[to_name | &1])

      to_cave =
        caves
        |> Map.fetch!(to_name)
        |> Map.update!(:neighbors, &[from_name | &1])

      caves
      |> Map.replace!(from_name, from_cave)
      |> Map.replace!(to_name, to_cave)
    end)
  end

  defp make_cave(name) do
    if name == String.upcase(name) do
      %BigCave{name: name}
    else
      %LittleCave{name: name}
    end
  end
end
