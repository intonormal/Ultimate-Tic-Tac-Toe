%%%-------------------------------------------------------------------
%%% @author huangzewu
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. Aug 2016 11:26
%%%-------------------------------------------------------------------
-module(tcp_acceptor).
-author("huangzewu").
-behavior(gen_server).
%% API
-export([start/2]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {lsock, cb_module, cb_state}).

start(LSock, CallbackModule) ->
   gen_server:start(?MODULE, [LSock, CallbackModule], []).

init([LScok, CallbackModule]) ->
  io:format("acceptor init with timeout(0) to handle_info~n"),
  {ok, #state{lsock = LScok, cb_module = CallbackModule}, 0}.

handle_info(timeout, #state{lsock = LScok, cb_module = CallbackModule} = State) ->
  {ok, Sock} = gen_tcp:accept(LScok),
  io:format("server accept socket connect~n"),
  tcp_acceptor:start(LScok, CallbackModule),
  {ok, CbState} = CallbackModule:init(Sock),
  {noreply, State#state{cb_state = CbState}};

handle_info({tcp, _, RcvData}, State=#state{cb_module = CBM, cb_state = CBS}) ->
  case CBM:handle_tcp_data(RcvData, CBS) of
    {ok, NewSBS} -> {noreply, State#state{cb_state = NewSBS}};
    stop -> {stop, normal, State}
  end;

handle_info(Msg, State=#state{cb_module=CBM, cb_state=CBS}) ->
  case CBM:handle_info(Msg, CBS) of
    {ok, NewSBS} -> {noreply, State#state{cb_state = NewSBS}};
    stop -> {stop, normal, State}
  end.

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Request, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.