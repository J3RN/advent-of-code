import gleam/erlang/file
import gleam/list
import gleam/int
import gleam/io
import gleam/result
import gleam/string

pub fn problem1() -> Result(Nil, file.Reason) {
  read_input()
  |> result.map(fn(input) {
    input
    |> count_increasing_depths()
    |> int.to_string()
    |> io.println()
  })
}

pub fn problem2() -> Result(Nil, file.Reason) {
  read_input()
  |> result.map(fn(input) {
    input
    |> combine_depths()
    |> count_increasing_depths()
    |> int.to_string()
    |> io.println()
  })
}

fn combine_depths(depths) {
  depths
  |> list.window(3)
  |> list.map(int.sum)
}

fn count_increasing_depths(depths) {
  list.window_by_2(depths)
  |> list.filter(fn(pair) {
    let #(a, b) = pair
    a < b
  })
  |> list.length()
}

fn read_input() -> Result(List(Int), file.Reason) {
  try input = file.read("data/day1.txt")

  string.split(input, "\n")
  |> list.fold_right(
    [],
    fn(acc, e) {
      case int.parse(e) {
        Ok(i) -> [i, ..acc]
        Error(_) -> acc
      }
    },
  )
  |> Ok()
}
