%%%-------------------------------------------------------------------
%%% @author huangzewu
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. Aug 2016 11:11
%%%-------------------------------------------------------------------
-module(mcts).
-author("huangzewu").

%% API
-export([start/0, start/3]).
-export([get_move/1, update/2, display/3]).

-record(state, {board=board,
                max_time=2000,%milliseconds
                exploration_factor=1.4,
                plays_wins,%ets, key={player, game_state}
                           %value={plays::integer(), wins::integer}
                game_states=[]
                  %pair0-8 player1-little-borad-Bitmasks,player2-little-borad-Bitmasks
                  %pair9   player1-big-bord-win-state, player2-big-win-state
                  %pair10  big-board-row-to-move, big-board-clumn-to-move
                  %last    player-to-move
                }).


start() -> start(board, 5000, 1.4).

start(Board, MaxTime, ExplorationFactor) ->
  io:format("mcts spawn process with MaxTime=~p, ExplorationFactor=~p~n",[MaxTime, ExplorationFactor]),
  spawn(fun() -> init([Board, MaxTime, ExplorationFactor]) end).

update(Pid, GameState) -> call(Pid, {update, GameState}).

display(Pid, GameState, Move) -> call(Pid, {display, GameState, Move}).

get_move(Pid) -> call(Pid, get_move).

init([Board, MaxTime, ExplorationFactor]) ->
  io:format("mcts create ets table to store PlaysWins~n"),
  PlaysWins = ets:new(play_wins, [set, protected, {read_concurrency, true}]),
  State = #state{board=Board, max_time=MaxTime,
                exploration_factor=ExplorationFactor, plays_wins=PlaysWins},
  loop(State).

call(Pid, Msg) ->
  Ref = make_ref(),
  Pid ! {call, Ref, self(), Msg},
  receive
    {Ref, Result} -> Result
  end.

loop(State) ->
  receive
    {call, Ref, From, Msg} ->
      case handle_call(Msg, State) of
        {reply, Reply, NewState} ->
          From ! {Ref, Reply},
          loop(NewState);
        stop -> stop
      end
  end.

handle_call({update, GameState}, State=#state{game_states = GSs}) ->
  {reply, ok, State#state{game_states = [GameState|GSs]}};
handle_call({display, GameState, Move}, State=#state{board = Board}) ->
  io:format("~ts~n",[Board:display(GameState, Move)]),
  {reply, ok, State};

handle_call(get_move, State=#state{board=Board, game_states = GSs}) ->
  GS = hd(GSs),
  {Player, LegalStates} = player_legal_states(Board, GS),
  NextMove =
    case LegalStates of
      [] -> illegal;
      [{Move,_}] -> Move;
      _ ->
        {Games, MaxDepth, Time} = run_simulation(Player, LegalStates, State),
        io:format("Games:~p Time:~pms~n",[Games, Time]),
        io:format("Maximum depth searched:~p~n", [MaxDepth]),
        %[{move, percent,wins, plays}]
        Stats = make_stats(Player, LegalStates, State#state.plays_wins),
        SortedStats = lists:reverse(lists:keysort(2, Stats)),%2=Percent
        %[io:format("~p:~.2f% (~p/~p)~n", [Move, Percent, Wins, Plays]) || {Move, Percent, Wins, Plays} <- SortedStats],
        [{Move, _, _,_}|_] = SortedStats,
        Move
    end,
  {reply, {ok, NextMove}, State}.

player_legal_states(Board, CurGameSate) ->
  Player = Board:current_player(CurGameSate),
  Moves = Board:legal_moves(CurGameSate),
  LegalStates = [{Move, Board:next_state(CurGameSate, Move)}|| Move <-Moves],
  {Player, LegalStates}.

run_simulation(Player, LegalStates, State) ->
  BeginTime = os:timestamp(),
  run_simulation(Player, LegalStates, State, {BeginTime, 0, 0}).

run_simulation(Player, LegalStates, State, {BeginTime, Games, MaxDepth}) ->
  TimeComsumed = timer:now_diff(os:timestamp(), BeginTime) div 1000,
  case TimeComsumed < State#state.max_time of
    true ->
      {Winner, Expand, NeedUpdates, Depth} = random_game(Player, LegalStates, State),
      propagate_back(Winner, Expand, NeedUpdates, State#state.plays_wins),
      run_simulation(Player, LegalStates, State, {BeginTime, Games+1, max(Depth, MaxDepth)});
    false -> {Games, MaxDepth, TimeComsumed}
  end.

random_game(Player, LegalStates, State=#state{board=Board}) ->
  <<A:32, B:32, C:32>> = crypto:rand_bytes(12),
  random:seed({A,B,C}),
  MaxMoves = Board:max_moves(),
  random_game(Player, LegalStates, 1, MaxMoves, {none, [], 0}, State).

random_game(_, [], _, _, {Expand, NeedUpdateds, MaxDepth}, _) ->
  {draw, Expand, NeedUpdateds, MaxDepth};

random_game(_, _, IterCount, MaxMoves, {Expand, NeedUpdateds, MaxDepth}, _)
  when IterCount =:=MaxMoves+1 -> {draw, Expand, NeedUpdateds, MaxDepth};

random_game(Player, LegalStates, IterCount, MaxMoves, {Expand, NeedUpdateds, MaxDepth}, State) ->
  {GS, Existed} = select_one(Player, LegalStates, State),
  {Expand2, NeedUpdateds2, MaxDepth2} =
    case {Expand, Existed} of
      {none, false} -> {{Player, GS}, NeedUpdateds, IterCount};     % expand The First random_game
      {_, true} -> {Expand, [{Player, GS}|NeedUpdateds], MaxDepth}; % selection
      {_, false} -> {Expand, NeedUpdateds, MaxDepth}
    end,
  case get_winner(State#state.board, GS) of
    on_going ->
      {Player2, LegalStates2} = player_legal_states(State#state.board, GS),
      %io:format("iterCount=~p, maxdepth=~p~n", [IterCount,MaxDepth2]),
      random_game(Player2, LegalStates2, IterCount+1, MaxMoves, {Expand2, NeedUpdateds2, MaxDepth2}, State);
    Winner ->
      {Winner, Expand2, NeedUpdateds2, MaxDepth2}
  end.

choise(L) -> lists:nth(random:uniform(length(L)), L).

select_one(Player, LegalStates, #state{plays_wins = PlaysWins}) ->
  GSs =[I || {_, I} <- LegalStates],
  RandomGS=choise(GSs),
  {RandomGS, lookup(PlaysWins, {Player, RandomGS}) =/= none}.

propagate_back(Winner, none, NeedUpdateds, PlaysWins) ->
  update_play_wins(Winner, NeedUpdateds, PlaysWins);

propagate_back(Winner, Expand, NeedUpdateds, PlaysWins) ->
  insert(PlaysWins, Expand, {0, 0}),
  update_play_wins(Winner, [Expand|NeedUpdateds], PlaysWins).

update_play_wins(Winner, Updateds, PlayWinners) ->
  [begin
     case lookup(PlayWinners, Key) of
       {Ps, Ws} ->
         Ws2 = if
                 Winner =:= Player -> Ws+1;
                 true -> Ws
               end,
         insert(PlayWinners, Key, {Ps+1, Ws2});
       none -> none
     end
   end|| {Player, _} = Key <- Updateds].

get_winner(Board, Gs) -> Board:winner(Gs).

make_stats(Player, LegalStates, PlaysWins) ->
  [list_to_tuple([Move | get_plays_wins(PlaysWins, Player, GameState)]) || {Move, GameState} <- LegalStates].

get_plays_wins(Tid, Player, GameState) ->
  case lookup(Tid, {Player, GameState}) of
    none -> [0.0, 0, 0];
    {Plays, Wins} -> [100*Wins/Plays, Wins, Plays]
  end.

lookup(Tid, Key) ->
  case ets:lookup(Tid, Key) of
    [{_, Value}] -> Value;
    [] -> none
  end.

insert(Tid, Key, Value) -> ets:insert(Tid, {Key, Value}).