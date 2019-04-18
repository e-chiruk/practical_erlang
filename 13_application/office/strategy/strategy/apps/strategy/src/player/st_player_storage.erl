-module(st_player_storage).
-behavior(gen_server).

-export([start_link/0, add_player/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, no_args, []).

-record(state, {
}).

%%% gen_server API

init(no_args) ->
  lager:info("~p init", [?MODULE]),
  ets:new(?MODULE, [named_table]),
  State = #state{},
  {ok, State}.

add_player(Socket, PlayerSrv) ->
  gen_server:call(?MODULE, {add_player, Socket, PlayerSrv}).

handle_call({add_player, Socket, PlayerSrv}, _From, State) ->
  ets:insert(?MODULE, {Socket, PlayerSrv}),
  State = #state{},
  {reply, ok, State};

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(_Request, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVersion, State, _Extra) ->
  {ok, State}.
