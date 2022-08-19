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
-export([start/0]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

start() ->
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/css/[...]", cowboy_static, {priv_dir, werl, "static/css"}},
            {"/js/[...]", cowboy_static, {priv_dir, werl, "static/js"}},
            {"/websocket", werl_websocket, []},
            {"/favicon.ico", cowboy_static, {priv_file, werl, "static/favicon.ico"}},
            {'_', werl_handler, []}
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

% Nothing here yet!
