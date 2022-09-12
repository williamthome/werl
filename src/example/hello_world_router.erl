-module(hello_world_router).

-behaviour(werl_router).

-export([
    routes/0
]).

routes() ->
    werl_router:create_routes([
        {<<"GET">>, <<"/">>, hello_world_controller, index, #{}},
        {<<"GET">>, '_', hello_world_controller, not_found, #{}}
    ]).
