-module(chat_room_manager).
-behavior(gen_server).

-export([start_link/0,
         create_room/1, get_rooms/0,
         add_user/3, remove_user/2, get_users/1,
         send_message/3,  get_history/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
  rooms
}).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
  {ok, #state{rooms = maps:new()}}.

create_room(RoomName) ->
  {ok, RoomPid} = chat_room:start_link(),
  Room = {RoomName, RoomPid},
  gen_server:cast(?MODULE, {add_room, Room}),
  Room.

get_rooms() ->
  gen_server:call(?MODULE, get_rooms).

add_user(RoomPid, UserName, UserPid) ->
  gen_server:call(?MODULE, {add_user, {RoomPid, UserName, UserPid}}).

remove_user(RoomPid, UserPid) ->
  gen_server:call(?MODULE, {remove_user, RoomPid, UserPid}).

get_users(RoomPid) ->
  gen_server:call(?MODULE, {get_users, RoomPid}).

send_message(RoomPid, UserName, Message) ->
  gen_server:call(?MODULE, {send_message, {RoomPid, UserName, Message}}).

get_history(RoomPid) ->
  gen_server:call(?MODULE, {get_history, RoomPid}).


handle_call(get_rooms, _From, #state{rooms = Rooms} = State) ->
  Reply = maps:values(Rooms),
  {reply, Reply, State};

handle_call({add_user, {RoomPid, UserName, UserPid}}, _From, State) ->
  #state{rooms = Rooms} = State,
  Reply = case maps:find(RoomPid, Rooms) of
            {ok, _Room} -> chat_room:add_user(RoomPid, UserName, UserPid), ok;
            error -> {error, room_not_found}
          end,
  {reply, Reply, State};

handle_call({remove_user, RoomPid, UserPid}, _From, State) ->
  #state{rooms = Rooms} = State,
  Reply = case maps:find(RoomPid, Rooms) of
            {ok, _Room} -> chat_room:remove_user(RoomPid, UserPid);
            error -> {error, room_not_found}
          end,
  {reply, Reply, State};

handle_call({get_users, RoomPid}, _From, State) ->
  #state{rooms = Rooms} = State,
  Reply = case maps:find(RoomPid, Rooms) of
            {ok, _Room} -> {ok, chat_room:get_users(RoomPid)};
            error -> {error, room_not_found}
          end,
  {reply, Reply, State};

handle_call({send_message, {RoomPid, UserName, Message}}, _From, State) ->
  #state{rooms = Rooms} = State,
  Reply = case maps:find(RoomPid, Rooms) of
            {ok, _Room} -> chat_room:add_message(RoomPid, UserName, Message);
            error -> {error, room_not_found}
          end,
  {reply, Reply, State};

handle_call({get_history, RoomPid}, _From, State) ->
  #state{rooms = Rooms} = State,
  Reply = case maps:find(RoomPid, Rooms) of
            {ok, _Room} -> {ok, chat_room:get_history(RoomPid)};
            error -> {error, room_not_found}
          end,
  {reply, Reply, State}.


handle_cast({add_room, Room}, State) ->
  #state{rooms = Rooms} = State,
  {_, RoomPid} = Room,
  Rooms2 = maps:put(RoomPid, Room, Rooms),
  {noreply, State#state{rooms = Rooms2}}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.