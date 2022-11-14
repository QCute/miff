%%%-------------------------------------------------------------------
%%% @doc
%%% build set_tuple_element use set_tuple_element_sSP instruction
%%% @end
%%%-------------------------------------------------------------------
-module(miff_builder).
-export([build/1]).
%%%===================================================================
%%% API
%%%===================================================================
%% @doc build number range of set_tuple_element function index parameter
-spec build(Number :: non_neg_integer()) -> file:filename().
build(Number) ->
    Data = lists:concat([
        build_attribute(3 + Number + 1 + 4),
        "\n\n",
        build_function(1, 2, 3, Number),
        "\n\n",
        build_on_load(1 + 3 + Number),
        "\n\n",
        build_module_info(1 + 3 + 3 + Number)
    ]),
    File = filename:dirname(code:which(?MODULE)) ++ "/miff.S",
    file:write_file(File, Data).

build_attribute(Labels) ->
    lists:concat([
        "{module, miff}.  %% version = 0", "\n",
        "{exports, [{module_info,0},{module_info,1},{set_tuple_element,3}]}.", "\n",
        "{attributes, [{on_load,[{on_load,0}]}]}.", "\n",
        "{labels, ", Labels, "}."
    ]).

build_function(InfoLabel, SelectLabel, BeginLabel, Number) ->
    Info = lists:concat([
        "  {label,", InfoLabel, "}.", "\n",
        "    {line,[{location,\"src/miff.erl\",23}]}.", "\n",
        "    {func_info,{atom,miff},{atom,set_tuple_element},3}."
    ]),
    Select = build_select(InfoLabel, SelectLabel, BeginLabel, Number),
    Jump = string:join([build_jump_label(Index, BeginLabel, BeginLabel + Number + 1) || Index <- lists:seq(1, Number)], "\n"),
    Bad = build_bad_label(BeginLabel + Number + 1),
    lists:concat([
        "{function, set_tuple_element, 3, ", SelectLabel, "}.", "\n",
        Info, "\n",
        Select, "\n",
        Jump, "\n",
        Bad, "\n"
    ]).

build_select(InfoLabel, SelectLabel, BeginLabel, Number) ->
    Select = string:join([build_select_list(Index, BeginLabel) || Index <- lists:seq(1, Number)], ",\n"),
    lists:concat([
        "  {label,", SelectLabel, "}.", "\n",
        "    {select_val,{x,0},{f,", InfoLabel, "},{list,[", "\n",
        Select, "\n",
        "    ]}}."
    ]).

build_select_list(Index, BeginLabel) ->
    lists:concat([
        "      {integer,", Index, "},", "{f,", Index + BeginLabel, "}"
    ]).

build_jump_label(Index, BeginLabel, BadLabel) ->
    lists:concat([
        "  {label,", Index + BeginLabel, "}.", "\n",
        "    {test,is_tuple,{f,", BadLabel, "},[{x,1}]}.", "\n",
        "    {set_tuple_element,{x,2},{x,1},", Index - 1, "}.", "\n",
        "    {move,{x,1},{x,0}}.", "\n",
        "    return."
    ]).

build_bad_label(Label) ->
    lists:concat([
        "  {label,", Label, "}.", "\n",
        "    {move,{literal,badarg},{x,0}}.", "\n",
        "    {call_ext_only,1,{extfunc,erlang,error,1}}."
    ]).

build_on_load(Label) ->
    lists:concat([
        "{function, on_load, 0, ", Label + 2, "}." "\n"
        "  {label,", Label + 1, "}." "\n"
        "    {line,[{location,\"src/miff.erl\",13}]}." "\n"
        "    {func_info,{atom,miff},{atom,on_load},0}." "\n"
        "  {label,", Label + 2, "}." "\n"
        "    {allocate,0,0}." "\n"
        "    {move,{integer,3},{x,1}}." "\n"
        "    {move,{atom,set_tuple_element},{x,0}}." "\n"
        "    {line,[{location,\"src/miff.erl\",14}]}." "\n"
        "    {call_ext,2,{extfunc,beam_opcodes,opcode,2}}." "\n"
        "    {test,is_eq_exact,{f,", Label + 3, "},[{x,0},{integer,67}]}." "\n"
        "    {move,{atom,ok},{x,0}}." "\n"
        "    {deallocate,0}." "\n"
        "    return." "\n"
        "  {label,", Label + 3, "}." "\n"
        "    {move,{literal,{instruction_not_supported,set_tuple_element}},{x,0}}." "\n"
        "    {line,[{location,\"src/miff.erl\",18}]}." "\n"
        "    {call_ext_last,1,{extfunc,erlang,error,1},0}." "\n"
    ]).


build_module_info(Label) ->
    lists:concat([
        "{function, module_info, 0, ", Label + 2, "}." "\n"
        "  {label,", Label + 1, "}." "\n"
        "    {line,[]}." "\n"
        "    {func_info,{atom,miff},{atom,module_info},0}." "\n"
        "  {label,", Label + 2, "}." "\n"
        "    {move,{atom,miff},{x,0}}." "\n"
        "    {call_ext_only,1,{extfunc,erlang,get_module_info,1}}." "\n"
        "\n"
        "\n"
        "{function, module_info, 1, ", Label + 4, "}." "\n"
        "  {label,", Label + 3, "}." "\n"
        "    {line,[]}." "\n"
        "    {func_info,{atom,miff},{atom,module_info},1}." "\n"
        "  {label,", Label + 4, "}." "\n"
        "    {move,{x,0},{x,1}}." "\n"
        "    {move,{atom,miff},{x,0}}." "\n"
        "    {call_ext_only,2,{extfunc,erlang,get_module_info,2}}." "\n"
    ]).
