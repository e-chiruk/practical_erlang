%%%-------------------------------------------------------------------
%%% @author echiruk
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. Apr 2019 18.32
%%%-------------------------------------------------------------------
-module(lesson16).
-author("echiruk").

%% API
-export([start/0, start/2, start_server/2, start_acceptor/2]).

start() ->
  start(1234, 2).

start(Port, NumAcceptors) ->

  spawn(?MODULE, start_server, [Port, NumAcceptors]).

start_server(Port, NumAcceptors) ->
  io:format("Starting server at port ~p~n", [Port]),
  {ok, ListenSocket} = gen_tcp:listen(Port, [binary, {active, false}, {reuseaddr, true}, {packet, 2}]),
  [spawn(?MODULE, start_acceptor, [ID, ListenSocket]) || ID <- lists:seq(1, NumAcceptors)],
  timer:sleep(infinity),
  ok.

start_acceptor(ID, ListenSocket) ->
  io:format("Acceptor: ~p ~p~n", [self(), ID]),
  io:format("Waiting for client...~n"),
  {ok, AcceptSocket} = gen_tcp:accept(ListenSocket),
  io:format("Got client ~p~n", [AcceptSocket]),
  spawn(?MODULE, start_acceptor, [ID, ListenSocket]),
  handle_connection(ID, ListenSocket, AcceptSocket).

handle_connection(ID, ListenSocket, AcceptSocket) ->
  case gen_tcp:recv(AcceptSocket, 0) of
    {ok, Msg} ->
        io:format("Got msg from client:~p, client: ~p:~p~n", [Msg, self(), ID]),
        BinId = list_to_binary(integer_to_list(ID)),
        gen_tcp:send(AcceptSocket, <<"ECHO from", BinId/binary, ": ", Msg/binary>>),
        handle_connection(ID, ListenSocket, AcceptSocket);
    {error, closed} ->
        io:format("Connection closed~n"),
        start_acceptor(ID, ListenSocket)
  end.