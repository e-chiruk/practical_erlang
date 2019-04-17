%%%-------------------------------------------------------------------
%%% @author echiruk
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. Apr 2019 18.21
%%%-------------------------------------------------------------------
-module(lesson15).
-author("echiruk").

%% API
-export([run/0]).


run() ->
  try
    {ok, Res} = do_something(),
      Res
  catch
      throw:SomeError  ->
        ST = erlang:get_stacktrace(),
        io:format("Error: ~p~n~p~n", [SomeError, ST]),
        {error, SomeError};
      error:SomeError ->
        {big_error, SomeError};
      EType:Error ->
        io:format("EType: ~p, Error: ~p~n", [EType, Error])
  end.

do_something() ->
  10 + fun_a().

fun_a() ->
  10 + fun_b().

fun_b() ->
  %%throw(something_bad_happend).
  %%error(something_bad_happend).
  exit(b).