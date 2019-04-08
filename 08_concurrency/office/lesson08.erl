%%%-------------------------------------------------------------------
%%% @author echiruk
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. Apr 2019 18.08
%%%-------------------------------------------------------------------
-module(lesson08).
-author("echiruk").

%% API
-export([flatten/1, run_processes/1, run_process/1, mailbox_checker/0, test_mailbox/0]).


flatten(List) ->
  flatten(List, []).

flatten([], Acc) -> lists:reverse(Acc);
flatten([Head | Tail], Acc) ->
  case Head of
    [] -> flatten(Tail, Acc);
    [Head2 | Tail2] -> flatten([Tail2 | Tail], [Head2 | Acc]);
    B -> flatten(Tail, [B | Acc])
  end.



run_processes(Num) ->
  lists:foreach(
    fun(ID) -> spawn(?MODULE, run_process, [ID]) end,
    lists:seq(1, Num)
  ).

run_process(ID) ->
  timer:sleep(rand:uniform(500)),
  io:format("I am ~p with ID:~p~n", [self(), ID]),
  ok.

mailbox_checker() ->
  io:format("I am mailbox checker, send me messeges ~p~n", [self()]),
  loop().

loop() ->
  receive
    stop -> io:format("mailbox checker ~p stops now~n", [self()]),
            ok;
    Msg -> io:format("mailbox checker ~p got ~p~n", [self(), Msg]),
            loop()
  after
    30000 -> io:format("mailbox checker ~p, no messages~n", [self()]),
              loop()
  end.


test_mailbox() ->
  spawn(?MODULE, mailbox_checker, []).