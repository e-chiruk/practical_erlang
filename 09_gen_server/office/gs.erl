%%%-------------------------------------------------------------------
%%% @author echiruk
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. Apr 2019 19.31
%%%-------------------------------------------------------------------
-module(gs).
-author("echiruk").

%% API
-export([start/0, add/3, remove/2, check/2, stop/1]).
-export([loop/1]).

start() ->
  InitialState = #{},
  spawn(?MODULE, loop, [InitialState]).

add(Pid, Key, Value) ->
  call(Pid, add, {Key, Value}).

remove(Pid, Key) ->
  call(Pid, remove, Key).

check(Pid, Key) ->
  call(Pid, check, Key).

call(Pid, Command, Msg) ->
  QueryRef = erlang:monitor(process, Pid),
  Pid ! {Command, QueryRef, self(), Msg},
  receive
    {reply, QueryRef, Reply} ->
      erlang:demonitor(QueryRef, [flush]),
      Reply;
    {'DOWN', QueryRef, process, Pid, Reason} ->
      io:format("Server crashed with Reason:~p~n", [Reason]),
      {error, Reason}
  after 5000 ->
    erlang:demonitor(QueryRef, [flush]),
    {error, no_reply}
  end.

stop(Pid) ->
  Pid ! stop.

loop(State) ->
  io:format("V3 loop ~p Current State:~p~n", [self(), State]),
  receive
    {Command, QueryRef, ClientPid, Msg} -> {Reply, NewState} = handle_call(Command, Msg, State),
      ClientPid ! {reply, QueryRef, Reply},
      ?MODULE:loop(NewState);
    stop -> io:format("~p stops now ~n", [self()]);
    Msg -> io:format("~p got msg ~p~n", [self(), Msg]),
      ?MODULE:loop(State)
  end.

handle_call(add, {K, V}, State) ->
  State2 = State#{K => V},
  {ok, State2};
handle_call(remove, K, State) ->
  State2 = maps:remove(K, State),
  {ok, State2};
handle_call(check, 42, State) ->
  42/0,
  {ok, State};
handle_call(check, K, State) ->
  Reply = case maps:find(K, State) of
    {ok, V} -> {ok, V};
    error -> {error, not_found}
  end,
  {Reply, State}.