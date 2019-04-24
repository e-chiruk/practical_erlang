%%%-------------------------------------------------------------------
%%% @author echiruk
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. Apr 2019 18.45
%%%-------------------------------------------------------------------
-module(root_handler).
-author("echiruk").

%% API
-export([init/2]).

init(Req0, State) ->
  io:format("~p~n~n", [Req0]),
  Bindings = cowboy_req:bindings(Req0),
  io:format("Bindings:~p~n~n", [Bindings]),
  Params = cowboy_req:bindings(Req0),
  io:format("Params:~p~n~n", [Params]),
  case cowboy_req:has_body(Req0) of
    true ->
      {ok, ReqBody, Req2} = cowboy_req:read_body(Req0),
      io:format("Req2:~p~n~n", [Req2]),
      io:format("ReqBody:~p~n~n", [ReqBody]);
    false -> ok

  end,
  Headers = #{<<"content-type">> => <<"text/html">>},
  Body = <<"<html><head>
            <title>Cow</title>
            <link href='/static/main.css' rel='stylesheet' type='text/css'/>
            </head><body>
            <h1>Cow Service</h1>
            <p>Hello from Cow Service</p>
            </body></html>">>,
  Req = cowboy_req:reply(200, Headers, Body, Req0),
  {ok, Req, State}.