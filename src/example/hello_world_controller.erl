-module(hello_world_controller).

-export([
    index/3,
    not_found/3
]).

-export([
    handle_event/3
]).

index(Req0, State0, Params) ->
    {Body, Static, _} = werl_ctrl_home:render(),
    req_render(Body, Static, Req0, State0, Params).

not_found(Req0, State0, Params) ->
    Body = <<"Oops! Page not found :(">>,
    req_render(Body, [], Req0, State0, Params).

handle_event(<<"increment">>, _Payload, _Params) ->
    io:format("Got increment~n"),

    {_HTML, _Static, Indexes} = werl_ctrl_home:increment(),
    {reply, <<"render">>, Indexes}.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

req_render(Body, Static, Req0, State, _Params) ->
    {HTML, _, _} = werl_template:render(app, #{
        'Title' => <<"WErl">>,
        'Static' => werl_json:encode(Static),
        'InnerContent' => Body
    }),
    Req = cowboy_req:reply(200, #{<<"content-type">> => <<"text/html">>}, HTML, Req0),
    {ok, Req, State}.
