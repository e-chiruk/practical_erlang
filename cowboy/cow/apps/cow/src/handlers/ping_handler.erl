%%%-------------------------------------------------------------------
%%% @author echiruk
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. Apr 2019 19.10
%%%-------------------------------------------------------------------
-module(ping_handler).
-author("echiruk").

%% API
-export([init/2]).

init(Req0, State) ->
  Req = cowboy_req:reply(200,
    #{<<"content-type">> => <<"text/plain">>},
    <<"Hello from ping handler!">>,
    Req0),
  {ok, Req, State}.