%%%-------------------------------------------------------------------
%%% @author huangzewu
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. Aug 2016 08:31
%%%-------------------------------------------------------------------
-module(game).
-author("huangzewu").

%% API
-export([start/0]).

start() ->
  room:start(board),
  tcp_server:start(8081, player_agent).