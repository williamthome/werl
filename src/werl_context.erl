-module(werl_context).

-export([
    param/1,
    param/2,
    param/3,
    params/1,

    query/1,
    query/2,
    query/3,
    queries/1
]).

param(Param) ->
    param(Param, <<>>).

param(Param, Fallback) ->
    fun(#{'Context' := Context}) -> param(Param, Fallback, Context) end.

param(Param, Fallback, #{params := Params}) ->
    maps:get(Param, Params, Fallback).

params(#{params := Params}) ->
    Params.

query(Query) ->
    query(Query, <<>>).

query(Query, Fallback) ->
    fun(#{'Context' := Context}) -> query(Query, Fallback, Context) end.

query(Query, Fallback, #{queries := Queries}) ->
    maps:get(Query, Queries, Fallback).

queries(#{queries := Queries}) ->
    Queries.
