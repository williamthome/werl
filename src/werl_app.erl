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
    Env = get_env(),
    ok = werl_server:start(Env),
    case werl_sup:start_link() of
        {ok, Pid} ->
            load_templates(Env),
            {ok, Pid};
        {error, Reason} ->
            {error, Reason}
    end.

stop(_State) ->
    ok.

priv_dir() ->
    code:priv_dir(werl).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

get_env() ->
    #{
        app => get_env_value(app),
        router => get_env_value(router),
        renderer => get_env_value(renderer, undefined),
        ws_options => get_env_value(ws_options, #{}),
        dispatch => get_env_value(dispatch, [])
    }.

get_env_value(Env) ->
    {ok, Value} = application:get_env(werl, Env),
    Value.

get_env_value(Env, Default) ->
    application:get_env(werl, Env, Default).

load_templates(#{renderer := undefined}) ->
    [];
load_templates(#{app := App, renderer := Renderer}) ->
    werl_template_sup:load_templates(App, Renderer).
