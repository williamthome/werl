%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2022, williamthome
%%% @doc werl server.
%%%
%%% @author William Fank Thomé [https://github.com/williamthome/]
%%% @since 2022
%%% @end
%%%-----------------------------------------------------------------------------
-module(werl_server).

%% API functions
-export([
    start/1
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

start(#{app := App, router := Router, idle_timeout := IdleTimeout}) ->
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/css/[...]", cowboy_static,
                {priv_dir, App, "static/css"}},
            {"/js/[...]", cowboy_static,
                {priv_dir, App, "static/js"}},
            {"/favicon.ico", cowboy_static,
                {priv_file, App, "static/favicon.ico"}},
            {"/websocket", werl_websocket,
                #{router => Router, idle_timeout => IdleTimeout}},
            {'_', werl_handler,
                #{router => Router}}
        ]}
    ]),
    {ok, _} = cowboy:start_clear(
        werl_http_listener,
        [{port, 8080}],
        #{env => #{dispatch => Dispatch}}
    ),
    ok.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

% nothing here yet!
