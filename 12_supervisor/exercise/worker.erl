-module(worker).
-behavior(gen_server).

-export([start_link/1, ping/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
}).

start_link(WorkerId) ->
  gen_server:start_link(?MODULE, [WorkerId], []).


%%% gen_server API

ping(Pid) ->
  gen_server:call(Pid, ping).


init([WorkerId]) ->
  {ok, WorkerId}.


handle_call(ping, _From, State) ->
  Reply = {State, self()},
  {reply, Reply, State};

handle_call(_Request, _From, #state{} = State) ->
  {reply, ok, State}.

handle_cast(_Request, #state{} = State) ->
  {noreply, State}.

handle_info(_Request, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVersion, State, _Extra) ->
  {ok, State}.
