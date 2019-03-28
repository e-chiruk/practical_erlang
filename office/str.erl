%%%-------------------------------------------------------------------
%%% @author echiruk
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. Mar 2019 19.36
%%%-------------------------------------------------------------------
-module(str).

%% API
-export([trim/1]).

-record(state, {
              met_non_space = false,
              result = [],
              spaces = []
              }).

-define(SPACE, 32).
%%trim(Str) ->
%%  Pred = fun(Char) -> Char == 32 end,
%%  Str2 = lists:dropwhile(Pred, Str),
%%  Str3 = lists:reverse(Str2),
%%  Str4 = lists:dropwhile(Pred, Str3),
%%  lists:reverse(Str4).

trim(Str) ->
  trim(Str, #state{}).

trim([], #state{result = Res}) -> lists:reverse(Res);
trim([?SPACE | Tail], #state{met_non_space = false} = State) ->
  trim(Tail, State);
trim([Char | Tail], #state{met_non_space = false, result = Res} = State) ->
  State1 = State#state{met_non_space = true, result = [Char | Res]},
  trim(Tail, State1);
trim([Char | Tail], #state{met_non_space = true, result = Res, spaces = Spaces} = State) ->
  State1 = if
             Char == ?SPACE ->
               State#state{spaces = [Char | Spaces]};
             Char /= ?SPACE ->
                State#state{result = [Char | Spaces ++ Res], spaces = []}
           end,
  trim(Tail, State1).
