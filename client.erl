%%%-------------------------------------------------------------------
%%% @author huangzewu
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. Aug 2016 13:39
%%%-------------------------------------------------------------------
-module(client).
-author("huangzewu").

%% API
-export([start/2, start/5, enter_room/1, leave_room/1, echo/2]).

-record(state, {nickname, type, player, board, socket}).

start(NickName, PlayerType) ->
	{ok, Pid} = start(NickName, PlayerType, board, "127.0.0.1", 8081),
	enter_room(Pid),
	Pid.

start(NickName, PlayerType, Board, SIp, SPort) ->
	Pid = spawn(fun() -> init(NickName, PlayerType, Board, SIp, SPort) end),
	{ok, Pid}.

echo(Pid, Msg) ->
	Pid ! {echo, Msg},
	ok.

enter_room(Pid) ->
	Pid ! enter_room,
	ok.
leave_room(Pid) ->
	Pid ! leave_room,
	ok.

init(NickName, PlayerType, Board, SIp, SPort) ->
	io:format("connect to ~p~n", [{SIp, SPort}]),
	{ok, Sock} = gen_tcp:connect(SIp, SPort, [binary, {packet, 2}, {active, true}]),
	Player = player:start(PlayerType, Board),
	loop(#state{nickname = NickName, type=PlayerType, player = Player, board = Board, socket = Sock}).

loop(State = #state{nickname = NickName, type=PlayerType, player = Player, socket=Sock}) ->
	receive
		{echo, Msg} ->
			io:format("client send {echo, ~p} to server~n",[Msg]),
			gen_tcp:send(Sock, term_to_binary({echo, Msg})),
			loop(State);
		enter_room ->
			io:format("client send enter_room to server~n"),
			gen_tcp:send(Sock, term_to_binary({enter_room, NickName})),
			loop(State);
		leave_room ->
			io:format("client send leave_room to server~n"),
			gen_tcp:send(Sock, term_to_binary({leave_room, NickName})),
			loop(State);
 		{tcp, _, TcpData} ->
 			case binary_to_term(TcpData) of
 				{echo, Msg} -> io:format("echo:~p~n", [Msg]);
				{notify, Msg} -> io:format("~s~n", [Msg]);
				{notify, Msg, NickName} -> io:format(Msg, [NickName]);
				{update, none, GameState} ->
					io:format("Game begining~n"),
					player:update(PlayerType, Player, GameState);
				{update, Move, GameState} ->
					player:update(PlayerType, Player, GameState),
					player:display(PlayerType, Player, GameState, Move);
				play ->
					{ok, Move} = player:get_move(PlayerType, Player),
					io:format("move is: ~p~n",[Move]),
					gen_tcp:send(Sock, term_to_binary({play, Move}));
				Unexpected ->
					io:format("client receive Unexpected tcp ~p~n",[Unexpected])
 			end,
			loop(State)
 	end.