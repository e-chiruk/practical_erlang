%%%-------------------------------------------------------------------
%%% @author echiruk
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. Apr 2019 20.15
%%%-------------------------------------------------------------------
-module(st_player_sup).
-author("echiruk").

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).


start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).


init([]) ->
  SupervisorSpecification = #{
    strategy => simple_one_for_one,
    intensity => 10,
    period => 60},

  ChildSpecifications =
    [#{id => st_player_srv,
      start => {st_player_srv, start_link, []},
      restart => permanent,
      shutdown => 2000,
      type => worker,
      modules => [st_player_srv]
    }],
  {ok, {SupervisorSpecification, ChildSpecifications}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
