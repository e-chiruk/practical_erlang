%%%-------------------------------------------------------------------
%%% @author echiruk
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. Apr 2019 18.27
%%%-------------------------------------------------------------------
-module(lesson06).
-export([init/0, get_users/0, get_young/0, generate_data/1, count_records/1, delete_table/1, move/2]).

-include_lib("stdlib/include/ms_transform.hrl").

-record(user, {
  name,
  age,
  gender
}).

get_users() ->
  [#user{name = "Bob", age = 24, gender = male},
    #user{name = "Bill", age = 21, gender = male},
    #user{name = "Kate", age = 20, gender = female},
    #user{name = "Ann", age = 19, gender = female}
  ].

init() ->
  delete_table(t_data),
  delete_table(t_dest),
  ets:new(t_data, [{keypos, 1}, named_table, set]),
  ets:new(t_dest, [{keypos, 1}, named_table, set]),
  ok.

delete_table(Name) ->
  delete_table(ets:all(), Name).

get_young() ->
  MS = ets:fun2ms(fun(#user{age = Age} = User) when Age < 23 -> User end),
  ets:select(my_table, MS).


generate_data(Limit) ->
  ets:delete_all_objects(t_data),
  insert_value(1, Limit).

insert_value(CurrId, Limit) when CurrId == Limit -> ok;
insert_value(CurrId, Limit) ->
  Sources = [source1, source2, source3, source4, source5],
  ets:insert(t_data, {CurrId, lists:nth(rand:uniform(length(Sources)), Sources), rand:uniform(1000000)}),
  insert_value(CurrId + 1, Limit).

count_records(Source) ->
  MS = ets:fun2ms(fun({_Id, CurSource, _Value} = Rec) when CurSource =:= Source -> Rec end),
  length(ets:select(t_data, MS)).


delete_table([], _Name) -> ok;
delete_table([Table | Tail], Name) ->
  case Table == Name of
      true -> ets:delete(Table);
      false -> delete_table(Tail, Name)
  end.

move(Min, Max) ->
  ets:delete_all_objects(t_dest),
  MS = ets:fun2ms(fun({_Id, _CurSource, Value} = Rec) when Value >= Min andalso Value =< Max -> Rec end),
  Records = ets:select(t_data, MS),
  ets:insert(t_dest, Records),
  ok.
