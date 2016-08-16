%%%-------------------------------------------------------------------
%%% @author huangzewu
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. Aug 2016 16:24
%%%-------------------------------------------------------------------
-module(player).
-author("huangzewu").

%% API
-export([start/2, update/3, display/4, get_move/2]).

start(human, Board) -> human:start(Board);
start(ucb_mcts, Board) -> ucb_mcts:start(Board, 1000, 1.4);
start(mcts, Board) -> mcts:start(Board, 1000, 1.4);
start(mcts_p, Board) -> mcts_p:start(Board, 1000, 1.4).

update(human, Player, GameState) -> human:update(Player, GameState);
update(ucb_mcts, Player, GameState) -> ucb_mcts:update(Player, GameState);
update(mcts, Player, GameState) -> mcts:update(Player, GameState);
update(mcts_p, Player, GameState) -> mcts_p:update(Player, GameState).

display(human, Player, GameState, Move) -> human:display(Player, GameState, Move);
display(ucb_mcts, Player, GameState, Move) -> ucb_mcts:display(Player, GameState, Move);
display(mcts, Player, GameState, Move) -> mcts:display(Player, GameState, Move);
display(mcts_p, Player, GameState, Move) -> mcts_p:display(Player, GameState, Move).

get_move(human, Player) -> human:get_move(Player);
get_move(ucb_mcts, Player) -> ucb_mcts:get_move(Player);
get_move(mcts, Player) -> mcts:get_move(Player);
get_move(mcts_p, Player) -> mcts_p:get_move(Player).
