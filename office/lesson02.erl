-module(lesson02).

-export([init/0, get_names/1, filter_females/1, filter_by_gender/2, partition_by_age/2]).
-export([filter_females2/1]).

init() ->
  [
    { user, "Bob", 29, male},
    { user, "Bill", 21, male},
    { user, "Kate", 24, female},
    { user, "Lora", 23, female}
  ].

get_names(Users) ->
  get_names(Users, []).

get_names([], Acc) -> Acc;
get_names([User|Tail], Acc) ->
  {user, Name,_,_} = User,
  get_names(Tail, [Name|Acc]).

filter_females(Users) ->
  filter_females(Users, []).

filter_females([], Acc) -> lists:reverse(Acc);
filter_females([User|Tail], Acc) ->
  case User of
    {user, _, _, female} -> filter_females(Tail, [User | Acc]);
    {user, _, _, male} -> filter_females(Tail, Acc)
  end.
filter_females2([]) -> [];
filter_females2([User | Tail]) ->
  case User of
    {user, _, _, female} -> [User | filter_females2(Tail)];
    {user, _, _, male} -> filter_females2(Tail)
  end.

filter_by_gender(Users, Gender) when Gender == male orelse Gender == female ->
  filter_by_gender(Users, Gender, []).



filter_by_gender([], _Gender, Acc) -> lists:reverse(Acc);
filter_by_gender([User | Tail], Gender, Acc) ->
  {user, _Name, _Age, UserGender} = User,
  if
    Gender == UserGender -> filter_by_gender(Tail, Gender, [User | Acc]);
    true -> filter_by_gender(Tail, Gender, Acc)
  end.



partition_by_age(Users, Age) ->
  partition_by_age(Users, Age, {[], []}).

partition_by_age([], _Age, {Older, Younger}) ->
  {lists:reverse(Older), lists:reverse(Younger)};
partition_by_age([User | Tail], Age, {Older, Younger}) ->
  {user, _Name, UserAge, _Gender} = User,
  NewAcc = if
    UserAge > Age -> {[User | Older], Younger};
    UserAge =< Age -> {Older, [User | Younger]}
  end,
  partition_by_age(Tail, Age, NewAcc).

