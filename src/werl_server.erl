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

start(#{dispatch := Dispatch0, app := App, router := Router, ws_options := WSOptions}) ->
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/js/[...]", cowboy_static,
                {priv_dir, werl, "static/js"}},
            {"/js/[...]", cowboy_static,
                {priv_dir, App, "static/js"}},
            {"/css/[...]", cowboy_static,
                {priv_dir, App, "static/css"}},
            {"/favicon.ico", cowboy_static,
                {priv_file, App, "static/favicon.ico"}},
            {"/websocket", werl_websocket,
                #{router => Router, ws_options => WSOptions}},
            {'_', werl_handler,
                #{router => Router}}
            | Dispatch0
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
