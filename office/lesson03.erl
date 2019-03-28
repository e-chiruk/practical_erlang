%%%-------------------------------------------------------------------
%%% @author echiruk
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 27. Mar 2019 18.15
%%%-------------------------------------------------------------------
-module(lesson03).
-author("echiruk").

%% API
-export([
          takewhile/2,
          init/0,
          get_names/1,
          greet/1,
          filter_by_age/2,
          filter_by_gender/2,
          greet_females/1,
          greet_females2/1,
          get_stat/1,
          partition_by_age/2
]).
-record(stat, {
  total_users = 0,
  total_male = 0,
  total_female = 0,
  total_age = 0
}).
init() ->
  [
    { user, "Bob", 29, male},
    { user, "Bill", 21, male},
    { user, "Kate", 24, female},
    { user, "Lora", 23, female}
  ].

filter_by_age(Users, Age) ->
  F = fun({user, _, CurrAge, _}) -> CurrAge > Age end,
  lists:filter(F, Users).

filter_by_gender(Users, Gender) ->
  F = fun({user, _, _, CurrGender}) -> CurrGender == Gender end,
  lists:filter(F, Users).

greet_females(Users) ->
  F = fun({user, _, _, Gender}) -> Gender == female end,
  Females = lists:filter(F, Users),
  lists:map(fun({user, Name, _, _}) -> greet(Name) end, Females).

greet_females2(Users) ->
  lists:filtermap(
    fun({user, Name, _Age, Gender}) ->
        case Gender of
          female -> Greet = greet(Name),
            {true, Greet};
          male -> false
        end
    end,
    Users).

get_stat(Users) ->
  lists:foldl(
    fun(User, Acc) ->
      {user, _, Age, Gender} = User,
      #stat{total_users = TU, total_male = TM, total_female = TF, total_age = TA} = Acc,
      Acc1 = Acc#stat{total_users = TU + 1, total_age = TA + Age},
      Acc2 = case Gender of
                male -> Acc1#stat{total_male = TM + 1};
                female -> Acc1#stat{total_female = TF + 1}
             end,
      Acc2
    end,
    #stat{},
    Users
  ).

get_names(Users) ->
  F = fun({user, Name, _, _}) -> Name end,
  Names = lists:map(F, Users),
  UpperNames = lists:map(fun string:to_upper/1, Names),
  lists:map(fun greet/1, UpperNames).

takewhile(Pred, List) ->
  takewhile(Pred, List, []).

takewhile(_Pred, [], _Acc) -> [];
takewhile(Pred, [Head | Tail], Acc) ->
  case Pred(Head) of
    true -> takewhile(Pred, Tail, [Head | Acc]);
    false -> lists:reverse(Acc)
  end.

greet(Name) -> "Hello, " ++ Name ++ "!".



partition_by_age(Users, Age) ->
  F = fun(User, Acc) ->
    {Younger, Older} = Acc,
    {user, _, CurrAge, _} = User,
    if
      CurrAge > Age -> {Younger, [User | Older]};
      CurrAge =< Age -> {[User | Younger], Older}
    end
  end,
  lists:foldl(F, {[], []}, Users).