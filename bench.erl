%%%-------------------------------------------------------------------
%%% @author huangzewu
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. Aug 2016 13:38
%%%-------------------------------------------------------------------
-module(bench).
-author("huangzewu").

%% API
-export([run/5]).

run(V1, V2, MT, EF, N) ->
  P1 = V1:start(board, MT, EF),
  P2 = V2:start(board, MT, EF),
  play_game(0, N,
            {V1, make_player(V1, P1)},
            {V2, make_player(V2, P2)},
            {0,0}).

make_player(V, Pid) ->
  fun(S) ->
    V:update(Pid, S),
    {ok, M} = V:get_move(Pid),
    M
  end.

play_game(N, N, {V1, _},{V2, _},{W1, W2}) ->
  show_stats(V1, W1, V2, W2, N),
  io:format("-------------------------------~n"),
  io:format("~p wins ~p, ~p wins ~p~n",[V1, W1, V2, W2]),
  ok;

play_game(Cnt, N, {V1, P1}, {V2, P2}, {W1, W2}) ->
  show_stats(V1, W1, V2, W2, Cnt),
  Inits = board:start(),
  Winner = play_once(P1, P2, Inits),
  case Winner of
    1 -> play_game(Cnt+1, N, {V1, P1},{V2, P2},{W1+1, W2});
    2 -> play_game(Cnt+1, N, {V1, P1},{V2, P2},{W1, W2+1});
    _ -> play_game(Cnt+1, N, {V1, P1},{V2, P2},{W1, W2})
  end.

play_once(P1, P2, State) ->
  Move = P1(State),
  NewStats = board:next_state(State, Move),
  case board:winner(NewStats) of
    on_going -> play_once(P2, P1, NewStats);
    Winner -> Winner
  end.

win_rate(_W, 0) -> 0.0;
win_rate(W, Cnt) -> 100*W/Cnt.

show_stats(V1, W1, V2, W2, Cnt) ->
  io:format("total:~p, win rate:~.2f%(~p), ~.2f%(~p)~n", [Cnt, win_rate(W1, Cnt), V1, win_rate(W2, Cnt), V2]).