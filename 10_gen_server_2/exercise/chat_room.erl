-module(chat_room).
-behavior(gen_server).

-export([start_link/0, add_user/3, remove_user/2, get_users/1, add_message/3, get_history/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
  users,
  history = []
}).

init([]) ->
  {ok, #state{users = maps:new()}}.

handle_call(Arg0, Arg1, _Arg2) ->
  erlang:error(not_implemented).

handle_cast(Arg0, Arg1) ->
  erlang:error(not_implemented).

handle_info(_Info, State) ->
  {noreply, State}.