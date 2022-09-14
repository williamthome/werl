-module(hello_world_controller).

-export([
    index/3,
    not_found/3
]).

-export([
    handle_event/4,
    handle_join/2
]).

index(Req0, State0, Params) ->
    Bindings = #{'Count' => 0},
    {Body, Static, _, _} = werl_template:render(home, Bindings),
    req_render(Body, Static, Req0, State0, Params).

not_found(Req0, State0, Params) ->
    Body = <<"Oops! Page not found :(">>,
    req_render(Body, [], Req0, State0, Params).

handle_event(<<"increment">>, _Payload, _Params, State0) ->
    io:format("Got increment~n"),

    Count =
        case maps:find(memo, State0) of
            {ok, #{bindings := #{'Count' := Count0}}} -> Count0;
            error -> 0
        end,
    Bindings = #{'Count' => Count + 1},
    {_HTML, _Static, Indexes, NewMemo} = werl_template:render(home, Bindings),

    State = State0#{memo => NewMemo},

    {reply, <<"render">>, Indexes, State}.

handle_join(_Topic, _Token) ->
    {ok, <<"Guest">>}.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

req_render(Body, Static, Req0, State, _Params) ->
    {HTML, _, _, _} = werl_template:render(app, #{
        'Title' => <<"WErl">>,
        'Static' => werl_json:encode(Static),
        'InnerContent' => Body
    }),
    Req = cowboy_req:reply(200, #{<<"content-type">> => <<"text/html">>}, HTML, Req0),
    {ok, Req, State}.
