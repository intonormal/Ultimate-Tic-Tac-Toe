%%%-------------------------------------------------------------------
%%% @author huangzewu
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. Aug 2016 10:53
%%%-------------------------------------------------------------------
-module(tcp_server).
-author("huangzewu").
-behavior(gen_server).

%% API
-export([start/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {port, lsock, agent}).

%%%===================================================================
%%% API
%%%===================================================================
start(LPort, Agent) ->
  gen_server:start({local, ?MODULE}, ?MODULE, [LPort, Agent], []).

init([LPort, Agent]) ->
  case gen_tcp:listen(LPort, [binary, {packet, 2}, {active, true}, {reuseaddr, true}]) of
      {ok, LSock} ->
        spawn_acceptor(LSock, Agent, 20),
        io:format("tcp_server start @[~p]~n", [LPort]),
        {ok, #state{port=LPort, lsock=LSock, agent=Agent}};
      {error, Reason} ->
        io:format("tcp_server not started, reason:~p~n",[Reason]),
        {stop, Reason}
    end.

spawn_acceptor(LSock, Agent, Num) ->
  io:format("spawn ~p acceptors ~n", [Num]),
  [tcp_acceptor:start(LSock, Agent) || _ <-lists:seq(1, Num)].

handle_call(_Request, _From, State) -> {reply, ok, State}.

handle_cast(_Request, State) -> {noreply, State}.

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.