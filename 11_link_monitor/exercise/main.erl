-module(main).

-export([parse/1, worker/2]).

parse(Files) ->
    Workers = lists:foldl(
        fun(File, Acc) ->
            Pid = spawn(?MODULE, worker, [File, self()]),
            Ref = erlang:monitor(process, Pid),
            Acc#{{Pid, Ref} => File}
        end,
        #{},
        Files),
    loop(Workers, {#{}, #{}}).



parse_lines(Lines) ->
    lists:foldl(
        fun
            (<<>>, Acc) -> Acc;
            (Line, Acc) ->
                [_Id, Name, Quantity, _Price]
                    = binary:split(Line, <<",">>, [global]),
                Quantity2 = list_to_integer(binary_to_list(Quantity)),
                Acc#{Name => Quantity2}
        end,
        #{},
        Lines).

aggregate(Map1, Map2) ->
    maps:fold(
        fun(K, V, Acc) ->
            case maps:find(K, Acc) of
                {ok, VA} -> Acc#{K := VA + V};
                error -> Acc#{K => V}
            end
        end,
        Map1,
        Map2).

worker(File, RootPid) ->
    {ok, Bin} = file:read_file(File),
    Lines = binary:split(Bin, <<"\n">>, [global]),
    Data = parse_lines(Lines),
    RootPid ! {result, Data},
    ok.

loop(Workers, Acc) when map_size(Workers) == 0 -> Acc;
loop(Workers, {DataAcc, ErrAcc}) ->
    receive
        {result, Data} ->
            DataAcc2 = aggregate(Data, DataAcc),
            loop(Workers, {DataAcc2, ErrAcc});
        {'DOWN', Ref, process, Pid, Reason} ->
            Workers2 = maps:remove({Pid, Ref}, Workers),
            case Reason of
                normal -> loop(Workers2, {DataAcc, ErrAcc});
                _ ->
                    File = maps:get({Pid, Ref}, Workers),
                    ErrAcc2 = ErrAcc#{File => Reason},
                    loop(Workers2, {DataAcc, ErrAcc2})
            end
    after
        1000 -> {error, no_reply}
    end.
