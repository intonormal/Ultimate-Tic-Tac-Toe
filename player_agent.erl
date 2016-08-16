%%%-------------------------------------------------------------------
%%% @author huangzewu
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. Aug 2016 13:46
%%%-------------------------------------------------------------------
-module(player_agent).
-author("huangzewu").
-export([init/1, handle_tcp_data/2, handle_info/2]).
-record(state, {sock}).
%% API

init(Socket) ->
  io:format("CallBackModule init~n"),
  {ok, #state{sock=Socket}}.

handle_tcp_data(TcpData, State) ->
  case binary_to_term(TcpData) of
    {echo, Msg} ->
      io:format("CallBackModule receive {echo, ~p}~n",[Msg]),
      send_message({echo, Msg}, State);
    {enter_room, NickName} ->
      io:format("CallBackModule receive {enter_room, ~p}~n",[NickName]),
      room:enter(self(), NickName);
    {leave_room, NickName} ->
      io:format("CallBackModule receive {leave_room, ~p}~n",[NickName]),
      room:leave(self());
    {play, Move} ->
      io:format("CallBackModule receive {play, ~p}~n",[Move]),
      room:play(self(), Move)
  end,
  {ok, State}.

handle_info(Msg, State) ->
  send_message(Msg, State),
  {ok, State}.

send_message(Message, State) ->
  TcpData = term_to_binary(Message),
  gen_tcp:send(State#state.sock, TcpData),
  ok.