%%%-------------------------------------------------------------------
%% @doc werl public API
%% @end
%%%-------------------------------------------------------------------

-module(werl_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    werl_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
