defmodule Day4 do
  @type draw :: integer()

  @type cell :: integer() | {:chosen, integer()}
  @type row :: [cell]
  @type board :: [row]

  @spec problem1() :: integer
  def problem1 do
    parse_file()
    |> find_winning_board()
    |> calculate_score()
  end

  @spec problem2() :: integer
  def problem2 do
    parse_file()
    |> find_last_winning_board()
    |> calculate_score()
  end

  @spec find_winning_board({[draw], [board]}) :: {draw, board}
  defp find_winning_board({draws, boards}) do
    Enum.reduce_while(draws, boards, fn draw, boards ->
      boards = apply_draw(boards, draw)

      case winners(boards) do
        [] -> {:cont, boards}
        [winning_board | _others] -> {:halt, {draw, winning_board}}
      end
    end)
  end

  @spec find_last_winning_board({[draw], [board]}) :: {draw, board}
  defp find_last_winning_board({draws, [board]}) do
    make_winner(draws, board)
  end

  defp find_last_winning_board({[this_draw | next_draws], boards}) do
    boards = apply_draw(boards, this_draw)

    case winners(boards) do
      [] -> find_last_winning_board({next_draws, boards})
      winners -> find_last_winning_board({next_draws, boards -- winners})
    end
  end

  @spec make_winner([draw], board) :: {draw, board}
  defp make_winner(draws, board) do
    Enum.reduce_while(draws, board, fn draw, board ->
      [board] = apply_draw([board], draw)

      case winners([board]) do
        [] -> {:cont, board}
        [winning_board] -> {:halt, {draw, winning_board}}
        other -> raise "Other: #{inspect(other)}"
      end
    end)
  end

  @spec apply_draw([board], draw) :: [board]
  defp apply_draw(boards, draw) do
    update_in(boards, [Access.all(), Access.all(), Access.all()], fn
      ^draw -> {:chosen, draw}
      x -> x
    end)
  end

  @spec winners([board]) :: board | nil
  defp winners(boards) do
    horizontal_winners = Enum.filter(boards, &horizontal_winner?/1)
    vertical_winners = Enum.filter(boards, &vertical_winner?/1)

    Enum.uniq(horizontal_winners ++ vertical_winners)
  end

  @spec transpose_board(board) :: board
  defp transpose_board(board) do
    board
    |> Enum.zip()
    |> Enum.map(fn row -> Tuple.to_list(row) end)
  end

  @spec vertical_winner?(board) :: boolean()
  defp vertical_winner?(board) do
    board |> transpose_board() |> horizontal_winner?()
  end

  @spec horizontal_winner?(board) :: boolean()
  defp horizontal_winner?(board) do
    winning_row(board) != nil
  end

  @spec calculate_score({draw, board}) :: integer
  defp calculate_score({winning_draw, board}) do
    board
    |> unmarked()
    |> Enum.sum()
    |> Kernel.*(winning_draw)
  end

  @spec unmarked(board) :: [integer]
  defp unmarked(board) do
    for row <- board, cell <- row, !match?({:chosen, _}, cell), do: cell
  end

  @spec winning_row(board) :: row
  defp winning_row(board) do
    Enum.find(board, fn row ->
      Enum.all?(row, fn cell -> match?({:chosen, _}, cell) end)
    end)
  end

  @spec parse_file() :: {[draw], [board]}
  def parse_file do
    [draws_string | board_strings] =
      "input"
      |> File.read!()
      |> String.split("\n\n")

    draws = draws_string |> String.split(",") |> Enum.map(&String.to_integer/1)

    boards =
      Enum.map(board_strings, fn boardString ->
        boardString
        |> String.split("\n")
        |> Enum.reject(&(&1 == ""))
        |> Enum.map(fn board_row ->
          board_row
          |> String.split()
          |> Enum.reject(&(&1 == " "))
          |> Enum.map(&String.to_integer/1)
        end)
      end)

    {draws, boards}
  end
end
