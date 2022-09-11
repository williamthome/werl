-module(werl_router).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% API
-export([
    new_route/1,
    create_routes/1,
    route_info/3
]).

%% TYPES
-export_type([
    route/0
]).

-type route() :: #{
    method => binary(),
    path => binary(),
    controller => module(),
    action => atom(),
    args => map(),
    params => #{binary() => binary()} | undefined
}.

%% CALLBACKS
-callback routes() -> [route()].

%%%=============================================================================
%%% API
%%%=============================================================================

new_route({Method, Path, Controller, Action, Args}) ->
    #{
        method => Method,
        path => Path,
        controller => Controller,
        action => Action,
        args => Args,
        params => undefined
    }.

create_routes(Routes) ->
    lists:map(
        fun
            (Route) when is_tuple(Route) -> new_route(Route);
            (Route) when is_map(Route) -> Route
        end,
        Routes
    ).

route_info(Router, Method, Path) ->
    get_best_match(Method, Path, Router:routes()).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

get_best_match(Method, _, [#{method := Method, path := '_'} = Route | _]) ->
    {ok, Route};
get_best_match(Method, Path, [#{method := Method, path := RoutePath} = Route | Routes]) ->
    PathParts = split_path(Path),
    RouteParts = split_path(RoutePath),
    case route_match(PathParts, RouteParts, #{}) of
        {true, Params} -> {ok, Route#{params => Params}};
        false -> get_best_match(Method, Path, Routes)
    end;
get_best_match(Method, Path, [_Route | Routes]) ->
    get_best_match(Method, Path, Routes);
get_best_match(_, _, []) ->
    {error, no_match}.

split_path(Path) ->
    binary:split(Path, <<"/">>, [global, trim_all]).

route_match([Value | PathParts], [<<$:, Param/binary>> | RouteParts], Params) ->
    route_match(PathParts, RouteParts, Params#{Param => Value});
route_match([Part | PathParts], [Part | RouteParts], Params) ->
    route_match(PathParts, RouteParts, Params);
route_match([], [], Params) ->
    {true, Params};
route_match(_, _, _) ->
    false.

%%%=============================================================================
%%% Tests
%%%=============================================================================

-ifdef(TEST).

get_best_match_test() ->
    Routes = create_routes([
        {<<"GET">>, <<"/users/:user_id/posts">>, foo_controller, index, #{}},
        {<<"GET">>, <<"/users/:user_id/posts/:id/edit">>, foo_controller, edit, #{}},
        {<<"GET">>, <<"/users/:user_id/posts/new">>, foo_controller, new, #{}},
        {<<"GET">>, <<"/users/:user_id/posts/:id">>, foo_controller, show, #{}},
        {<<"POST">>, <<"/users/:user_id/posts">>, foo_controller, create, #{}},
        {<<"PATCH">>, <<"/users/:user_id/posts/:id">>, foo_controller, update, #{}},
        {<<"PUT">>, <<"/users/:user_id/posts/:id">>, foo_controller, update, #{}},
        {<<"DELETE">>, <<"/users/:user_id/posts/:id">>, foo_controller, delete, #{}}
    ]),
    MatchRoute =
        fun(Nth, Params) ->
            Route = lists:nth(Nth, Routes),
            {ok, Route#{params => Params}}
        end,
    GetBestMatch = fun(Method, Path) -> get_best_match(Method, Path, Routes) end,
    [
        ?assertEqual(
            MatchRoute(1, #{<<"user_id">> => <<"1">>}),
            GetBestMatch(<<"GET">>, <<"/users/1/posts">>)
        ),
        ?assertEqual(
            MatchRoute(2, #{<<"user_id">> => <<"1">>, <<"id">> => <<"2">>}),
            GetBestMatch(<<"GET">>, <<"/users/1/posts/2/edit">>)
        ),
        ?assertEqual(
            MatchRoute(3, #{<<"user_id">> => <<"1">>}),
            GetBestMatch(<<"GET">>, <<"/users/1/posts/new">>)
        ),
        ?assertEqual(
            MatchRoute(4, #{<<"user_id">> => <<"1">>, <<"id">> => <<"2">>}),
            GetBestMatch(<<"GET">>, <<"/users/1/posts/2">>)
        ),
        ?assertEqual(
            MatchRoute(5, #{<<"user_id">> => <<"1">>}),
            GetBestMatch(<<"POST">>, <<"/users/1/posts">>)
        ),
        ?assertEqual(
            MatchRoute(6, #{<<"user_id">> => <<"1">>, <<"id">> => <<"2">>}),
            GetBestMatch(<<"PATCH">>, <<"/users/1/posts/2">>)
        ),
        ?assertEqual(
            MatchRoute(7, #{<<"user_id">> => <<"1">>, <<"id">> => <<"2">>}),
            GetBestMatch(<<"PUT">>, <<"/users/1/posts/2">>)
        ),
        ?assertEqual(
            MatchRoute(8, #{<<"user_id">> => <<"1">>, <<"id">> => <<"2">>}),
            GetBestMatch(<<"DELETE">>, <<"/users/1/posts/2">>)
        ),
        ?assertEqual({error, no_match}, GetBestMatch(<<"GET">>, <<"/">>))
    ].

-endif.
