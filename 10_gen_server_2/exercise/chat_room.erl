-module(chat_room).
-behavior(gen_server).

-export([start_link/0, add_user/3, remove_user/2, get_users/1, add_message/3, get_history/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
  users,
  history = []
}).

start_link() ->
  gen_server:start_link(?MODULE, [], []).

add_user(RoomPid, UserName, UserPid) ->
  gen_server:cast(RoomPid, {add_user, {UserName, UserPid}}), ok.

remove_user(RoomPid, UserPid) ->
  gen_server:call(RoomPid, {remove_user, UserPid}).

get_users(RoomPid) ->
  gen_server:call(RoomPid, get_users).

add_message(RoomPid, UserName, Message) ->
  gen_server:cast(RoomPid, {add_message, {UserName, Message}}), ok.

get_history(RoomPid) ->
  gen_server:call(RoomPid, get_history).


init([]) ->
  {ok, #state{users = maps:new()}}.


handle_call({remove_user, UserPid}, _From, State) ->
  #state{users = Users} = State,
  case maps:find(UserPid, Users) of
    {ok, _} -> Users2 = maps:remove(UserPid, Users),
      {reply, ok, State#state{users = Users2}};
    error -> {reply, {error, user_not_found}, State}
  end;

handle_call(get_users, _From, State) ->
  #state{users = Users} = State,
  Reply = maps:values(Users),
  {reply, Reply, State};

handle_call(get_history, _From, #state{history = Messages} = State) ->
  Reply = lists:reverse(Messages),
  {reply, Reply, State}.


handle_cast({add_user, User}, State) ->
  #state{users = Users} = State,
  {_, UserPid} = User,
  Users2 = maps:put(UserPid, User, Users),
  {noreply, State#state{users = Users2}};

handle_cast({add_message, {Name, Message}}, State) ->
  #state{users = Users, history = Messages} = State,
  lists:foreach(fun(UserPid) ->
    chat_user:add_message(UserPid, Name, Message)
                end, maps:keys(Users)),
  {noreply, State#state{history = [{Name, Message} | Messages]}}.


handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.