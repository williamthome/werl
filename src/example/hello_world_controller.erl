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
    Head = <<"<script src=\"js/main.js\" defer></script>">>,
    Bindings = #{'Count' => 0, 'Context' => Context},
    {Body, Static, _, Memo} = werl_template:render(home, Bindings),
    State = State0#{memo => Memo},
    hello_world_renderer:req_render(Head, Body, Static, Req0, State, Context).

not_found(Req0, State0, Context) ->
    Body = <<"Oops! Page not found :(">>,
    hello_world_renderer:req_render(Body, [], Req0, State0, Context).

handle_event(<<"increment">>, _Payload, State0) ->
    io:format("Got increment ~p~n", [State0]),

    {ok, Memo} = maps:find(memo, State0),
    #{bindings := #{'Count' := Count} = MemoBindings} = Memo,
    Bindings = MemoBindings#{'Count' => Count + 1},
    % TODO: werl_template:render/3 with Context as third arg
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

% nothing here yet!
