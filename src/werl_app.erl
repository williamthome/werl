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
-export([start/2, stop/1, priv_dir/0]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

start(_StartType, _StartArgs) ->
    ok = werl_server:start(),
    werl_sup:start_link().

stop(_State) ->
    ok.

priv_dir() ->
    code:priv_dir(werl).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

% Nothing here yet!
