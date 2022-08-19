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
-export([start/2, stop/1]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

start(_StartType, _StartArgs) ->
    werl_sup:start_link().

stop(_State) ->
    ok.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

% Nothing here yet!
