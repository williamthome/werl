-module(werl_handler).

-behaviour(cowboy_handler).

%% Callbacks
-export([
    init/2,
    terminate/3
]).

%%%=============================================================================
%%% Callbacks
%%%=============================================================================

-spec init(Req, term()) -> {ok | module(), Req, term()} | {module(), Req, term(), term()} when
    Req :: cowboy_req:req().

init(Req0, #{router := Router} = State0) ->
    io:format("init handler [~p]~n", [Req0]),

    Method = cowboy_req:method(Req0),
    Path = cowboy_req:path(Req0),
    case werl_router:route_info(Router, Method, Path) of
        {ok, #{controller := Controller, action := Action, params := Params}} ->
            io:format("router found [~p]~n", [{Controller, Action, Params}]),
            erlang:apply(Controller, Action, [Req0, State0, Params]);
        {error, no_match} ->
            Req = cowboy_req:reply(404, Req0),
            {ok, Req, State0}
    end.

-spec terminate(term(), map(), term()) -> ok.

terminate(_Reason, _Req, _State) ->
    ok.
