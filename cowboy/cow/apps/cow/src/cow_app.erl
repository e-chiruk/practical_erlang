%%%-------------------------------------------------------------------
%% @doc cow public API
%% @end
%%%-------------------------------------------------------------------

-module(cow_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1, start_cowboy/0]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    start_cowboy(),
    {ok, _} = cache:start_link(tpl_cache, [{n, 10}, {ttl, 60}]),
    cow_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

start_cowboy() ->
    {ok, Port} = application:get_env(cow, port),
    Dispatch = cowboy_router:compile([
        {'_', [{"/", root_handler, []},
               {"/ping", ping_handler, []},
               {"/user/:a/:b/:c", root_handler, []},
               {"/user/[:user_id]", user_handler, []},
               {"/static/[...]", cowboy_static, {priv_dir, cow, "www"}}
        ]}

    ]),
    {ok, _} = cowboy:start_clear(my_http_listener,
        [{port, Port}],
        #{env => #{dispatch => Dispatch}}
    ),
    ok.