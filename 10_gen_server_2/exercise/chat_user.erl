-module(chat_user).
-behavior(gen_server).

-export([start_link/0, add_message/3, get_messages/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
  messages = []
}).

start_link() ->
  gen_server:start_link(?MODULE, [], []).

init([]) ->
  {ok, #state{}}.

get_messages(Server) ->
  gen_server:call(Server, get_messages).

add_message(Server, UserName, Message) ->
  gen_server:cast(Server, {add_message, {UserName, Message}}),
  ok.

handle_call(get_messages, _From, State) ->
  #state{messages = Messages} = State,
  Reply = lists:reverse(Messages),
  {reply, Reply, State}.

handle_cast({add_message, Data}, State) ->
  #state{messages = Messages} = State,
  {noreply, State#state{messages = [Data | Messages]}}.


handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.


