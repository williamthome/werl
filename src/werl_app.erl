%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2022, williamthome
%%% @doc werl public API.
%%%
%%% @author William Fank Thomé [https://github.com/williamthome/]
%%% @since 2022
%%% @end
%%%-----------------------------------------------------------------------------
-module(werl_app).

-behaviour(application).

%% API functions
-export([
    start/2,
    stop/1,
    priv_dir/0
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

start(_StartType, _StartArgs) ->
    ok = werl_server:start(get_env()),
    werl_sup:start_link().

stop(_State) ->
    ok.

priv_dir() ->
    code:priv_dir(werl).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

get_env() ->
    #{
        app => get_env_app(),
        router => get_env_router(),
        idle_timeout => get_env_idle_timeout()
    }.

get_env_app() ->
    get_env_value(app).

get_env_router() ->
    get_env_value(router).

get_env_idle_timeout() ->
    get_env_value(idle_timeout, 60_000).

get_env_value(Env) ->
    {ok, Value} = application:get_env(werl, Env),
    Value.

get_env_value(Env, Default) ->
    application:get_env(werl, Env, Default).
