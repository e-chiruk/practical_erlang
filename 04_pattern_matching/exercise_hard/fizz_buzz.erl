%% Реализовать Fizz Buzz
%% https://habr.com/ru/post/298134/
-module(fizz_buzz).

-export([init/0]).

init() ->
  init(lists:seq(1, 100), []).

init([], Acc) -> lists:reverse(Acc);
init([Head | Tail], Acc) when Head rem 3 =:= 0 andalso Head rem 5 =:= 0 -> init(Tail, ["FizzBuzz" | Acc]);
init([Head | Tail], Acc) when Head rem 3 =:= 0 -> init(Tail, ["Fizz" | Acc]);
init([Head | Tail], Acc) when Head rem 5 =:= 0 -> init(Tail, ["Buzz" | Acc]);
init([Head | Tail], Acc) -> init(Tail, [Head | Acc]).