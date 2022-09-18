-module(werl_parse).

-export([
    param_to_integer/1,
    param_to_integer/2,

    query_to_integer/1,
    query_to_integer/2
]).

param_to_integer(Param) ->
    param_to_integer(Param, <<"NAN">>).

param_to_integer(Param, Fallback) ->
    Bin = werl_context:param(Param, Fallback),
    erlang:binary_to_integer(Bin).

query_to_integer(Param) ->
    query_to_integer(Param, <<"NAN">>).

query_to_integer(Param, Fallback) ->
    Bin = werl_context:query(Param, Fallback),
    erlang:binary_to_integer(Bin).
