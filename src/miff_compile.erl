%%%-------------------------------------------------------------------
%%% @doc
%%% miff compile
%%% @end
%%%-------------------------------------------------------------------
-module(miff_compile).
-export([compile_asm/1]).
-export([validate_mutation/2, assert_type/3]).
-include_lib("stdlib/include/erl_compile.hrl").
%%%===================================================================
%%% API
%%%===================================================================
%% @doc compile_asm
-spec compile_asm(File :: file:filename()) -> 'ok' | 'error'.
compile_asm(File) ->
    {_, _, VMB} = hook:hook(beam_validator, validate_mutation, 2, ?MODULE),
    {_, _, VTB} = hook:hook(beam_validator, assert_type, 3, ?MODULE, assert_type, VMB),
    {_, _, VM1B} = exporter:export(beam_validator, vm_1, 2, VTB),
    {_, _, GTB} = exporter:export(beam_validator, get_movable_term_type, 2, VM1B),
    exporter:export(beam_validator, meet, 2, GTB),
    OutDir = filename:dirname(code:which(?MODULE)),
    compile:compile_asm(File, [], #options{outdir = OutDir}).

validate_mutation({set_tuple_element, _, _, _}, Vst) ->
    Vst;
validate_mutation(I, Vst) ->
    beam_validator:vm_1(I, Vst).

assert_type(RequiredType, Term, Vst) ->
    GivenType = beam_validator:get_movable_term_type(Term, Vst),
    case beam_validator:meet(RequiredType, GivenType) of
        GivenType ->
            ok;
        _RequiredType ->
            case {RequiredType, GivenType} of
                {{t_tuple, _, false, #{}}, {t_tuple, 0, false, #{}}} ->
                    ok;
                _ ->
                    error({bad_type, {needed, RequiredType},{actual, GivenType}})
            end
    end.
