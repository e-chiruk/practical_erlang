%%%-------------------------------------------------------------------
%%% @author echiruk
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. Apr 2019 19.55
%%%-------------------------------------------------------------------
-module(lesson11).
-author("echiruk").

%% API
-export([run/0]).


run() ->
  process_flag(trap_exit, true),
  lists:foreach(
    fun(ID) -> spawn_link(fun() -> worker(ID) end) end,
    lists:seq(1, 10)
  ),
  ok.

worker(ID) ->
  io:format("Worker ID:~p, Pid:~p, start~n", [ID, self()]),
  if
    ID rem 5 == 0 -> throw({worker, self(), die});
    true -> ok
  end,
  timer:sleep(rand:uniform(1000)),
  io:format("Worker ID:~p, Pid:~p, stop~n", [ID, self()]),
  ok.