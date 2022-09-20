-module(hello_world_renderer).

-behaviour(werl_renderer).

% Behaviour callbacks
-export([
    templates/0
]).

% API
-export([
    req_render/6
]).

%%%=============================================================================
%%% Behaviour callbacks
%%%=============================================================================

templates() ->
    [
        {app, "app.html.eel"},
        {home, "index.html.eel"}
    ].

%%%=============================================================================
%%% API
%%%=============================================================================

req_render(Head, Body, Static, Req0, State, Context) ->
    {HTML, _, _, _} = werl_template:render(app, #{
        'Title' => <<"WErl">>,
        'Static' => werl_json:encode(Static),
        'InnerHead' => Head,
        'InnerContent' => Body,
        'Context' => Context
    }),
    Req = cowboy_req:reply(200, #{<<"content-type">> => <<"text/html">>}, HTML, Req0),
    {ok, Req, State}.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

% nothing here yet!
