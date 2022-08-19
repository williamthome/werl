-module(werl_ws).

-behaviour(cowboy_websocket_handler).

%% Callbacks
-export([
    init/2,
    terminate/3,
    websocket_init/1,
    websocket_handle/2,
    websocket_info/2
]).

-type status() :: not_ready | ready.

-record(state, {
    status = not_ready :: status()
}).

%%%=============================================================================
%%% Callbacks
%%%=============================================================================

init(Req0, []) ->
    State = #state{},
    OneMinuteInMs = 60_000,
    WsConnTimeout = OneMinuteInMs * 1,
    Options = #{idle_timeout => WsConnTimeout},
    io:format("websocket connection initiated~n~p~n~nstate: ~p~n", [Req0, State]),
    case cowboy_req:parse_header(<<"sec-websocket-protocol">>, Req0) of
        undefined ->
            {cowboy_websocket, Req0, State, Options};
        Subprotocols ->
            case lists:keymember(<<"mqtt">>, 1, Subprotocols) of
                true ->
                    Req = cowboy_req:set_resp_header(
                        <<"sec-websocket-protocol">>,
                        <<"mqtt">>,
                        Req0
                    ),
                    {cowboy_websocket, Req, State, Options};
                false ->
                    Req = cowboy_req:reply(400, Req0),
                    {ok, Req, State}
            end
    end.

websocket_init(State) ->
    io:format("init websocket [~p]~n", [self()]),
    do_reply(<<"ready">>, State).

websocket_handle({text, Msg}, State) ->
    {ok, Data} = parse_msg(Msg),
    do_handle(Data, State).

websocket_info({notify, Event}, State) ->
    do_reply(Event, State);
websocket_info({notify, Event, Payload}, State) ->
    do_reply(Event, Payload, State).

terminate(Reason, Req, _State) ->
    io:format(
        "websocket connection terminated~n~p~nReason: ~p~n",
        [maps:get(peer, Req), Reason]
    ),
    ok.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

parse_msg(Msg) ->
    case decode(Msg) of
        {ok, Data} when is_map(Data) ->
            {ok, Data};
        {ok, Data} ->
            io:format("Map is expected. Received: ~p~n", [Data]),
            {error, invalid_data};
        {error, _Reason} = Error ->
            io:format("Invalid msg: ~p~nError: ~p~n", [Msg, Error]),
            Error
    end.

do_handle(
    #{<<"event">> := <<"ready">>},
    State0
) ->
    io:format("Got ready~n"),
    State = State0#state{
        status = ready
    },
    do_reply(State).

do_reply(State) ->
    {ok, State, hibernate}.

do_reply(Event, State) ->
    Payload = maps:new(),
    do_reply(Event, Payload, State).

do_reply(Event, Payload, State) ->
    Data = data(Event, Payload),
    Msg = encode(Data),
    {reply, {text, Msg}, State, hibernate}.

data(Event, Payload) ->
    #{
        <<"event">> => Event,
        <<"payload">> => Payload
    }.

%%------------------------------------------------------------------------------
%% JSON
%%------------------------------------------------------------------------------

encode(Data) ->
    thoas:encode(Data).

decode(Msg) ->
    thoas:decode(Msg).
