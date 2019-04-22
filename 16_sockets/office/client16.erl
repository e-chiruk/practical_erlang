%%%-------------------------------------------------------------------
%%% @author echiruk
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. Apr 2019 19.41
%%%-------------------------------------------------------------------
-module(client16).
-behavior(gen_server).

-export([start/0, start/1, send/2, stop/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
  socket :: gen_tcp:socket()
}).

send(ClientPid, Msg) ->
  gen_server:call(ClientPid, {msg, Msg}).

stop(ClientPid) ->
  gen_server:call(ClientPid, stop).

start() ->
  start(1234).


start(Port) ->
  Options = [{host, {127,0,0,1}},{port, Port}],
  gen_server:start_link(?MODULE, Options, []).


%%% gen_server API

init(Options) ->
  io:format("~p init, Options: ~p~n", [self(), Options]),
  Host = proplists:get_value(host, Options),
  Port = proplists:get_value(port, Options),
  {ok, Socket} = gen_tcp:connect(Host, Port, [binary, {active, true}, {packet, 2}]),
  io:format("connected ~p~n", [Socket]),
  State = #state{
    socket = Socket
  },
  {ok, State}.

handle_call({msg, Msg}, _From, #state{socket = Socket} = State) ->
  gen_tcp:send(Socket, Msg),
  {reply, ok, State};

handle_call(stop, _From, #state{socket = Socket} = State) ->
  gen_tcp:close(Socket),
  {stop, normal, ok, State};

handle_call(_Request, _From, #state{} = State) ->
  {reply, ok, State}.

handle_cast(_Request, #state{} = State) ->
  {noreply, State}.

handle_info(Request, State) ->
  io:format("client ~p, handle_info, ~p~n", [self(), Request]),
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVersion, State, _Extra) ->
  {ok, State}.