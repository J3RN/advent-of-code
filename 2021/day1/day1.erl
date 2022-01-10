-module(day1).
-export([problem1/0, problem2/0]).

problem1() ->
    count_increasing_depths(read_file()).

problem2() ->
    count_increasing_depths(chunk_by(3, read_file())).

chunk_by(2, [_Hd | Tl] = List) ->
    lists:zip(lists:droplast(List), Tl);
chunk_by(3, [_Hd | [_TlHd | TlTl] = Tl] = List) ->
    lists:zip3(lists:droplast(lists:droplast(List)), lists:droplast(Tl), TlTl).

count_increasing_depths(Depths) ->
    length(lists:filter(fun({X, Y}) -> Y > X end, chunk_by(2, Depths))).

read_file() ->
    {ok, Contents} = file:read_file("input.txt"),
    Depths = lists:filter(fun(Line) -> Line /= "" end, string:split(binary_to_list(Contents), "\n", all)),
    lists:map(fun(Line) ->
                      {Int, _} = string:to_integer(Line),
                      Int
              end, Depths).
