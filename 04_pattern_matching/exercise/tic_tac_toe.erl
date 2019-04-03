-module(tic_tac_toe).

-export([new_game/0, win/1, move/3]).


new_game() ->
    {{f, f, f},
     {f, f, f},
     {f, f, f}}.


win(GameState) ->
    case GameState of
      {{C, C ,C},
       {_, _, _},
       {_, _, _}} when C /= f -> {win, C};
      {{_, _, _},
       {C, C ,C},
       {_, _, _}} when C /= f -> {win, C};
      {{_, _, _},
       {_, _, _},
       {C, C ,C}} when C /= f -> {win, C};
      {{C, _, _},
       {C, _, _},
       {C, _, _}} when C /= f -> {win, C};
      {{_, C, _},
       {_, C, _},
       {_, C, _}} when C /= f -> {win, C};
      {{_, _, C},
       {_, _, C},
       {_, _, C}} when C /= f -> {win, C};
      {{C, _, _},
       {_, C, _},
       {_, _, C}} when C /= f -> {win, C};
      {{_, _, C},
       {_, C, _},
       {C, _, _}} when C /= f -> {win, C};
      _ -> no_win
    end.


move(Cell, Player, GameState) ->
    case {Cell, GameState} of
      {1, {{f, C2, C3}, Row2, Row3}} -> {ok, {{Player, C2, C3}, Row2, Row3}};
      {2, {{C1, f, C3}, Row2, Row3}} -> {ok, {{C1, Player, C3}, Row2, Row3}};
      {3, {{C1, C2, f}, Row2, Row3}} -> {ok, {{C1, C2, Player}, Row2, Row3}};
      {4, {Row1, {f, C2, C3}, Row3}} -> {ok, {Row1, {Player, C2, C3}, Row3}};
      {5, {Row1, {C1, f, C3}, Row3}} -> {ok, {Row1, {C1, Player, C3}, Row3}};
      {6, {Row1, {C1, C2, f}, Row3}} -> {ok, {Row1, {C1, C2, Player}, Row3}};
      {7, {Row1, Row2, {f, C2, C3}}} -> {ok, {Row1, Row2, {Player, C2, C3}}};
      {8, {Row1, Row2, {C1, f, C3}}} -> {ok, {Row1, Row2, {C1, Player, C3}}};
      {9, {Row1, Row2, {C1, C2, f}}} -> {ok, {Row1, Row2, {C1, C2, Player}}};
      _ -> {error,invalid_move}
    end.
