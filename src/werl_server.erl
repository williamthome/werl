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

start(#{dispatcher := Dispatcher} = Args) ->
    Routes = routes(Args),
    case Dispatcher of
        undefined ->
            Dispatch = cowboy_router:compile([{'_', Routes}]),
            {ok, _} = cowboy:start_clear(
                werl_http_listener,
                [{port, 8080}],
                #{env => #{dispatch => Dispatch}}
            );
        {Mod, Fun} ->
            Mod:Fun(Routes);
        Dispatcher when is_function(Dispatcher, 1) ->
            Dispatcher(Routes)
    end,
    ok.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

routes(#{app := App, router := Router, ws_options := WSOptions}) ->
    [
        {"/js/[...]", cowboy_static, {priv_dir, werl, "static/js"}},
        {"/js/[...]", cowboy_static, {priv_dir, App, "static/js"}},
        {"/css/[...]", cowboy_static, {priv_dir, App, "static/css"}},
        {"/favicon.ico", cowboy_static, {priv_file, App, "static/favicon.ico"}},
        {"/websocket", werl_websocket, #{router => Router, ws_options => WSOptions}},
        {'_', werl_handler, #{router => Router}}
    ].
