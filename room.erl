%%%-------------------------------------------------------------------
%%% @author huangzewu
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. Aug 2016 14:30
%%%-------------------------------------------------------------------
-module(room).
-author("huangzewu").

%% API
-export([start/1]).
-export([enter/2, leave/1, play/2]).
-export([reset/0]).

-record(state, {board,
                status = waiting, %status=waiting|playing
                current_player =  none,
                players=[], %players=[{pid, nick_name, monitor_ref}]
                game_state}).

start(Board) ->
  io:format("room start and register~n"),
  Pid = spawn(fun() -> init(Board) end),
  register(room, Pid),
  {ok, Pid}.

enter(Pid, NickName) -> room ! {enter, Pid, NickName}.

leave(Pid) -> room ! {leave, Pid}.

play(Pid, Move) -> room ! {play, Pid, Move}.

reset() -> room ! reset.

init(Board) ->
  <<A:32, B:32, C:32>> = crypto:rand_bytes(12),
  random:seed({A,B,C}),
  loop(#state{board=Board}).


select_player(Players) ->
  N = random:uniform(2),
  %lists:foreach(fun(Input) -> io:format("~p~n", [Input]) end, Players),
  {Pid, NickName, _} = lists:nth(N, Players),
  io:format("the first player is ~p~n",[NickName]),
  {Pid, NickName}.

loop(State=#state{status=waiting, board=Board, players=Players}) ->
  receive
    {enter, Pid, NickName} ->
      io:format("room receive {enter_room ~p}~n",[NickName]),
      case Players of
        [] ->
          notify_user(Pid,"~p Welcome, Your are the first player!~n", NickName),
          Ref=erlang:monitor(process, Pid),
          loop(State#state{players=[{Pid, NickName, Ref}]});
        [{Pid,_,_}] ->
          notify_user(Pid, "~p, Your are aleardy enter room!~n", NickName),
          loop(State);
        [{Pid2, NickName2, _}] ->
          notify_user(Pid, "~p, Welcom! Players all had standby in room!~n", NickName),
          notify_user(Pid2, "~p, enter room. Players all had standby in room!", NickName2),
          Ref = erlang:monitor(process, Pid),
          NewPlayers=[{Pid, NickName, Ref}|Players],
          First=select_player(NewPlayers),
          GameState = Board:start(),
          io:format("~p~n", [GameState]),
          self() ! begin_game,
          io:format("board started and send begin_game to self~n"),
          loop(State#state{status = playing,
                          current_player = First,
                          players = NewPlayers,
                          game_state = GameState})
      end;
    {leave, Pid} ->
      case lists:keyfind(Pid, 1, Players) of
        {Pid, NickName, Ref} ->
          NewPlayers = [{Pid2, _, _}]=lists:keydelete(Pid, 1, Players),
          io:format("leave room success!"),
          notify_user(Pid, "~p, You had leaved room!~n", NickName),
          notify_user(Pid2, "your competition ~p leaved room!~n",NickName),
          erlang:demonitor(Ref),
          loop(State#state{players = NewPlayers});
        _ -> loop(State)
      end;
    {'Down', _, process, Pid, Reason} ->
      io:format("~p down @waiting for ~p~n", [Pid, Reason]),
      self() ! {leave, Pid},
      loop(State);
    Unexpected ->
      io:format("unexpected @waiting ~p~n", [Unexpected]),
      loop(State)
  end;

loop(State=#state{status=playing,
                  current_player = {Current, CurrentNickName},
                  players=Players,
                  board = Board,
                  game_state = GameState}) ->
  receive
    {enter, Pid, NickName} ->
      io:format("game had started, can't not enter~n"),
      notify_user(Pid, "~p, Game had stared, you can't play!~n",NickName),
      loop(State);
    {leaving, Pid} ->
      case lists:keyfind(Pid, 1, Players) of
        {Pid, NickName, Ref} ->
          NewPlayers = [{Pid2, _, _}] = lists:keydelete(Pid, 1, Players),
          io:format("~p leave room success!~n",[NickName]),
          notify_user(Pid, "~p, You had leaved room!~n", NickName),
          notify_user(Pid2, "your competition ~p leaved room!~n", NickName),
          erlang:demonitor(Ref),
          loop(State#state{status = waiting, current_player = none, players=NewPlayers});
        _ ->
          notify_user(Pid, "Oops, You aren't in room!~n"),
          loop(State)
      end;
      begin_game ->
        {Next, _} = next_player(Current, Players),
        update(Current, GameState),
        update(Next, GameState),
        play(Current),
        loop(State);
      {play, Current, Move} ->
        case Board:is_legal(GameState, Move) of
          false ->
            io:format("illegal move~n"),
            notify_user(Current, "~p, Bad move, please try again!~n",CurrentNickName),
            play(Current),
            loop(State);
          true ->
            GameState2 = Board:next_state(GameState, Move),
            NextPlayer = {Next, _} = next_player(Current, Players),
            %update(Current, Move, GameState2),
            update(Next, Move, GameState2),
            case Board:winner(GameState2) of
              on_going ->
                play(Next),
                loop(State#state{game_state = GameState2, current_player = NextPlayer});
              draw ->
                loop(State#state{status = waiting, players = [], current_player = none});
              _ ->
                notify_user(Current, "~p Congrulations!, You wins",CurrentNickName),
                notify_user(Next, "Oops! You lost"),
                loop(State#state{status = waiting, players = [], current_player = none})
            end
        end;
      {'Down', _, process, Pid, Reason} ->
        io:format("~p down@playing ~p~n", [Pid, Reason]),
        self() ! {leave, Pid},
        loop(State);
      Unexpected ->
        io:format("unexpected @playing ~p~n", [Unexpected]),
        loop(State)
  end.

notify_user(Pid, Msg) -> Pid ! {notify, Msg}.
notify_user(Pid, Msg, NickName) -> Pid ! {notify, Msg, NickName}.

next_player(Pid, [{Pid, _, _}, {Pid2, NickName, _}]) -> {Pid2, NickName};
next_player(Pid, [{Pid2, NickName, _}, {Pid, _, _}]) -> {Pid2, NickName}.

update(Pid, GameState) -> Pid ! {update, none, GameState}.
update(Pid, Move, GameState) -> Pid ! {update, Move, GameState}.

play(Pid) -> Pid ! play.