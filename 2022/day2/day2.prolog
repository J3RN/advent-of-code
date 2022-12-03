%% Rules of the game

result(rock, paper, win).
result(rock, scissors, loss).
result(paper, rock, loss).
result(paper, scissors, win).
result(scissors, rock, win).
result(scissors, paper, loss).
result(X, X, draw).

%% Scoring

play_score(rock, 1).
play_score(paper, 2).
play_score(scissors, 3).

result_score(loss, 0).
result_score(draw, 3).
result_score(win, 6).

%% Known

opponent_play('A', rock).
opponent_play('B', paper).
opponent_play('C', scissors).

%% Part 1

my_play('X', rock).
my_play('Y', paper).
my_play('Z', scissors).

part1(Score) :-
    read_file(Contents),
    round_scores(Contents, Score),
    format("~d\n", [Score]).

round_scores([], 0).
round_scores([[Theirs, Mine] | Rest], Score) :-
    round_scores(Rest, RestScore),
    round_score(Theirs, Mine, RoundScore),
    Score is RoundScore + RestScore.

round_score(Theirs, Mine, Score) :-
    my_play(Mine, MyPlay),
    opponent_play(Theirs, TheirPlay),
    result(TheirPlay, MyPlay, Result),
    play_score(MyPlay, PlayScore),
    result_score(Result, ResultScore),
    Score is PlayScore + ResultScore.

%% Part 2

outcome('X', loss).
outcome('Y', draw).
outcome('Z', win).

part2(Score) :-
    read_file(Contents),
    new_round_scores(Contents, Score),
    format("~d\n", [Score]).

new_round_scores([], 0).
new_round_scores([[Theirs, Outcome] | Rest], Score) :-
    new_round_scores(Rest, RestScore),
    new_round_score(Theirs, Outcome, RoundScore),
    Score is RoundScore + RestScore.

new_round_score(Theirs, Outcome, Score) :-
    opponent_play(Theirs, TheirPlay),
    outcome(Outcome, Result),
    result(TheirPlay, MyPlay, Result),
    play_score(MyPlay, PlayScore),
    result_score(Result, ResultScore),
    Score is PlayScore + ResultScore.

%% Util

read_file(Contents) :-
    open('input', read, Stream),
    read_turns(Stream, Contents).

read_turns(Stream, Contents) :-
    get_char(Stream, A), get_char(Stream, ' '), get_char(Stream, B), get_char(Stream, '\n'), read_turns(Stream, Rest), Contents = [[A, B] | Rest];
    get_char(Stream, end_of_file), Contents = [].

%% Run

:- initialization(part1(Score)).
:- initialization(part2(Score)).
