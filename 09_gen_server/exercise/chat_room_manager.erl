-module(chat_room_manager).

-export([start/0,
         create_room/2, remove_room/2, get_rooms/1,
         add_user/3, remove_user/3, get_users_list/2,
         send_message/4,  get_messages_history/2]).
-export([loop/1, handle_call/2]).

-record(room, {
              id,
              name,
              users = [],
              history = []
}).

-record(state, {
                max_rooms = 5,
                rooms = maps:new()
}).

start() ->
    spawn(?MODULE, loop, [#state{}]).


loop(State) ->
    receive
      {Msg, From, Ref} ->
        {Reply, State2} = ?MODULE:handle_call(Msg, State),
        From ! {reply, Ref, Reply},
        ?MODULE:loop(State2);
      stop -> ok;
      _Any -> ?MODULE:loop(State)
    end.

call(Server, Msg) ->
    Ref = erlang:monitor(process, Server),
    Server ! {Msg, self(), Ref},
    receive
      {reply, Ref, Reply} ->
        erlang:demonitor(Ref, [flush]),
        Reply;
      {'DOWN', Ref, process, Server, Reason} ->
        {error, Reason}
    after 5000 ->
      erlang:demonitor(Ref, [flush]),
      noreply
    end.


create_room(Server, RoomName) ->
    call(Server, {create_room, RoomName}).


remove_room(Server, RoomId) ->
    call(Server, {remove_room, RoomId}).


get_rooms(Server) ->
    call(Server, {get_rooms}).


add_user(Server, RoomId, UserName) ->
    call(Server, {add_user, RoomId, UserName}).


remove_user(Server, RoomId, UserName) ->
    call(Server, {remove_user, RoomId, UserName}).


get_users_list(Server, RoomId) ->
    call(Server, {get_users_list, RoomId}).


send_message(Server, RoomId, UserName, Message) ->
    call(Server, {send_message, RoomId, UserName, Message}).


get_messages_history(Server, RoomId) ->
    call(Server, {messages_history, RoomId}).

handle_call({send_message, RoomId, UserName, Message}, State) ->
    #state{rooms = Rooms} = State,
    case maps:find(RoomId, Rooms) of
        {ok, Room} -> #room{users = Users, history = History} = Room,
                      case lists:member(UserName, Users) of
                        false -> {{error, user_not_in_room}, State};
                        true ->
                          Msg = {UserName, Message},
                          Room2 = Room#room{history = [Msg | History]},
                          {ok, State#state{rooms = maps:update(RoomId, Room2, Rooms)}}
                      end;
        error -> {{error, room_not_found}, State}
    end;

handle_call({messages_history, RoomId}, State) ->
    #state{rooms = Rooms} = State,
    case maps:find(RoomId, Rooms) of
          {ok, Room} -> #room{history = History} = Room,
                        {{ok, History}, State};
          error -> {{error, room_not_found}, State}
    end;

handle_call({create_room, RoomName}, State) ->
    #state{max_rooms = MaxRooms, rooms = Rooms} = State,
    case maps:size(Rooms) of
      Size when Size >= MaxRooms -> {{error, room_limit}, State};
      _ -> RoomId = make_ref(),
           NewRoom = #room{id = RoomId, name = RoomName},
           {{ok, RoomId}, State#state{rooms = maps:put(RoomId, NewRoom, Rooms)}}
    end;

handle_call({remove_room, RoomId}, State) ->
    #state{rooms = Rooms} = State,
    case maps:find(RoomId, Rooms) of
      {ok, _Room} -> Rooms2 = maps:remove(RoomId, Rooms),
                     {ok, State#state{rooms = Rooms2}};
      error -> {{error, room_not_found}, State}
    end;

handle_call({get_rooms}, State) ->
    #state{rooms = Rooms} = State,
    Reply = maps:fold(fun(RoomId, #room{name = RoomName}, Acc) ->
                      [{RoomId, RoomName} | Acc]
                      end,
                      [], Rooms),
    {Reply, State};

handle_call({add_user, RoomId, UserName}, State) ->
    #state{rooms = Rooms} = State,
    case maps:find(RoomId, Rooms) of
        {ok, Room} -> #room{users = Users} = Room,
                      case lists:member(UserName, Users) of
                           true -> {{error, user_is_in_room}, State};
                           false -> Room2 = Room#room{users = [UserName | Users]},
                                    Rooms2 = maps:update(RoomId, Room2, Rooms),
                                    State2 = State#state{rooms = Rooms2},
                                    {ok, State2}
                      end;
        error -> {{error, room_not_found}, State}
    end;

handle_call({remove_user, RoomId, UserName}, State) ->
    #state{rooms = Rooms} = State,
    case maps:find(RoomId, Rooms) of
        {ok, Room} -> #room{users = Users} = Room,
            case lists:member(UserName, Users) of
                false -> {{error, user_not_in_room}, State};
                true -> Room2 = Room#room{users = lists:delete(UserName, Users)},
                        Rooms2 = maps:update(RoomId, Room2, Rooms),
                        State2 = State#state{rooms = Rooms2},
                        {ok, State2}
            end;
        error -> {{error, room_not_found}, State}
    end;

handle_call({get_users_list, RoomId}, State) ->
    #state{rooms = Rooms} = State,
    case maps:find(RoomId, Rooms) of
        {ok, Room} -> #room{users = Users} = Room,
            {{ok, Users}, State};
        error -> {{error, room_not_found}, State}
    end.
