-module(template).

-export([parse/2]).

parse(Str, Data) when is_binary(Str) ->
    Data1 = maps:fold(fun(K,V, Acc)->
        K1 = iolist_to_binary([<<"{{">>, K, <<"}}">>]), Acc#{ K1 => V } end, #{}, Data),
    Str1 = maps:fold(fun(K,V, Acc) ->
        IsInt = is_integer(V),
        IsList = is_list(V),
        V1 = if
            IsInt -> integer_to_binary(V);
            IsList -> list_to_binary(V);
            true -> V
        end,
        binary:replace(Acc, K, V1) end, Str, Data1),
    iolist_to_binary(re:replace(Str1, "{{[a-zA-Z0-9]+}}", "", [global])).
