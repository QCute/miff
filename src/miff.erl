%%%-------------------------------------------------------------------
%%% @doc
%%% miff - destructive set tuple element
%%% @end
%%%-------------------------------------------------------------------
-module(miff).
-on_load(on_load/0).
-export([set_tuple_element/3]).
%%%===================================================================
%%% API
%%%===================================================================
-spec on_load() -> ok.
on_load() ->
    case beam_opcodes:opcode(set_tuple_element, 3) of
        67 ->
            ok;
        _ ->
            error({instruction_not_supported, set_tuple_element})
    end.

%% @doc destructive set tuple element
-spec set_tuple_element(Index :: non_neg_integer(), Tuple :: tuple(), Element :: term()) -> tuple().
set_tuple_element(_Index, _Tuple, _Element) ->
    setelement(_Index, _Tuple, _Element).
