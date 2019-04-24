%%%-------------------------------------------------------------------
%%% @author echiruk
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. Apr 2019 20.47
%%%-------------------------------------------------------------------
-module(tpl_manager).
-author("echiruk").

%% API
-export([get_template/1]).


get_template(Name) ->
  case cache:get(tpl_cache, Name) of
    undefined ->
      io:format("ssssss"),
      PrivDir = code:priv_dir(cow),
      {ok, Tpl} = file:read_file(PrivDir ++ "/tpl/"++ Name ++ ".html"),
      cache:put(tpl_cache, Name, Tpl),
      Tpl;
    Tpl ->
      io:format("fffff"),
      Tpl
  end.