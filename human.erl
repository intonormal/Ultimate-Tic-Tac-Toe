%%%-------------------------------------------------------------------
%%% @author huangzewu
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. Aug 2016 16:26
%%%-------------------------------------------------------------------
-module(human).
-author("huangzewu").

%% API
-export([start/1, update/2, display/3, get_move/1]).

start(Board) ->
  io:format("human start init~n"),
  put(board, Board),
  human.

update(_Player, _GameState) ->
  void.

display(_Player, GameState, Move) ->
  Board = get(board),
  io:format("~ts~n",[Board:display(GameState, Move)]).

get_move(Player) ->
  Board = get(board),
  Move = io:get_line("Your move (R C r c):"),
  case Board:parse(Move) of
    none -> get_move(Player);
    P -> {ok, P}
  end.
