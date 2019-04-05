-module(short_link).

-export([init/0, create_short/2, get_long/2, rand_str/1, get_home/1]).

-define(SERVER, "http://hexlet.io/").

%%% module API

init() ->
    %% init randomizer
    <<A:32, B:32, C:32>> = crypto:strong_rand_bytes(12),
    rand:seed(exsp, {A,B,C}),
    {#{}, #{}}.

get_home(Link) ->
  Regex = "^(http:\/\/www\.|https:\/\/www\.|http:\/\/|https:\/\/)?[a-z0-9]+([\-\.]{1}[a-z0-9]+)*\.[a-z]{2,5}",
  case re:run(Link, Regex, [{capture, first, list}]) of
    {match, [HomeLink]} -> HomeLink;
    nomatch -> {error, "Incorrect link"}
  end.

create_short(LongLink, State) ->
  {LongLinks, ShortLinks} = State,
  case maps:find(LongLink, LongLinks) of
    {ok, ShortLink} -> {ShortLink, State};
    error -> ShortLink = ?SERVER ++ rand_str(8),
             LongLinks2 = maps:put(LongLink, ShortLink, LongLinks),
             ShortLinks2 = maps:put(ShortLink, LongLink, ShortLinks),
             {ShortLink, {LongLinks2, ShortLinks2}}
  end.

get_long(ShortLink, State) ->
  {_, ShortLinks} = State,
  case maps:find(ShortLink, ShortLinks) of
    {ok, LongLink} -> {ok, LongLink};
    error -> {error, not_found}
  end.


%% generates random string of chars [a-zA-Z0-9]
rand_str(Length) ->
    lists:map(fun(Char) when Char > 83 -> Char + 13;
                 (Char) when Char > 57 -> Char + 7;
                 (Char) -> Char
              end,
              [rand:uniform(110 - 48) + 47 || _ <- lists:seq(1, Length)]).
