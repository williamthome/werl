-module(hello_world_controller).

-behaviour(werl_controller).

-export([
    index/3,
    not_found/3
]).

-export([
    handle_event/3,
    handle_call/3,
    handle_join/2
]).

index(Req0, State0, Context) ->
    Bindings = #{'Count' => 0},
    {Body, Static, _, _} = werl_template:render(home, Bindings, Context),
    req_render(Body, Static, Req0, State0, Context).

not_found(Req0, State0, Context) ->
    Body = <<"Oops! Page not found :(">>,
    req_render(Body, [], Req0, State0, Context).

handle_event(<<"increment">>, _Payload, State0) ->
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

handle_call({Event, Payload}, _From, State) ->
    io:format("Got handle_call ~p~n", [{Event, Payload}]),

    Reply = <<"Hello, World!">>,
    {reply, Reply, State}.

handle_join(_Topic, _Token) ->
    case rand:uniform() < 0.8 of
        true ->
            Rand = erlang:integer_to_binary(rand:uniform(9_999)),
            Username = <<"User", Rand/binary>>,
            {ok, Username};
        false ->
            error
    end.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

req_render(Body, Static, Req0, State, Context) ->
    {HTML, _, _, _} = werl_template:render(app, #{
        'Title' => <<"WErl">>,
        'Static' => werl_json:encode(Static),
        'InnerContent' => Body
    }, Context),
    Req = cowboy_req:reply(200, #{<<"content-type">> => <<"text/html">>}, HTML, Req0),
    {ok, Req, State}.
