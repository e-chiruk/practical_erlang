%%%-------------------------------------------------------------------
%%% @author echiruk
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. Apr 2019 20.16
%%%-------------------------------------------------------------------
-module(user_handler).
-author("echiruk").

%% API
-export([init/2]).

init(Req0, State) ->
  Bindings = cowboy_req:bindings(Req0),
  UserId = maps:get(user_id, Bindings, <<"0">>),
  Info = get_user_info(UserId),
  Tpl = tpl_manager:get_template("user"),
  Headers = #{<<"content-type">> => <<"text/html">>},
  Body = bbmustache:render(Tpl, Info),
  Req = cowboy_req:reply(200, Headers, Body, Req0),
  {ok, Req, State}.

get_user_info(UserId) ->
  #{"user_id" => UserId,
    "username" => <<"Bob">>,
    "attributes" =>
      [#{"attr_name" => "height", "attr_value" => 182},
       #{"attr_name" => "weight", "attr_value" => 70}
      ]}.