-module(test).
-export([test/0]).

test() ->
    T = {1, 2, 3},
    {0, 2, 3} = T = miff:set_tuple_element(1, T, 0).
