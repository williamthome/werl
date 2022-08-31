-module(werl_websocket).

-behaviour(cowboy_websocket).

%% Callbacks
-export([
    init/2,
    terminate/3,
    websocket_init/1,
    websocket_handle/2,
    websocket_info/2
]).

%% cowboy types
-type commands() :: cowboy_websocket:commands().
-type call_result(State) :: {commands(), State} | {commands(), State, hibernate}.

%% werl types
-type status() :: not_ready | ready.

-record(state, {
    status = not_ready :: status()
}).

%%%=============================================================================
%%% Callbacks
%%%=============================================================================

-spec init(Req, any()) ->
    {ok | module(), Req, any()}
    | {module(), Req, any(), any()}
when
    Req :: cowboy_req:req().

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

-spec websocket_init(State) -> call_result(State) when State :: any().

websocket_init(State) ->
    io:format("init websocket [~p]~n", [self()]),
    do_reply(<<"ready">>, State).

-spec websocket_handle(
    ping
    | pong
    | {text | binary | ping | pong, binary()},
    State
) ->
    call_result(State).

websocket_handle({text, Msg}, State) ->
    {ok, Data} = parse_msg(Msg),
    do_handle(Data, State).

-spec websocket_info(any(), State) -> call_result(State) when State :: any().

websocket_info({notify, Event}, State) ->
    do_reply(Event, State);
websocket_info({notify, Event, Payload}, State) ->
    do_reply(Event, Payload, State).

-spec terminate(any(), cowboy_req:req(), any()) -> ok.

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
    case werl_json:decode(Msg) of
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
    do_reply(State);
do_handle(
    #{<<"event">> := <<"increment">>},
    State
) ->
    io:format("Got increment~n"),
    Html = werl_ctrl_home:increment(),
    do_reply(<<"render">>, Html, State).

do_reply(State) ->
    {ok, State, hibernate}.

do_reply(Event, State) ->
    Payload = maps:new(),
    do_reply(Event, Payload, State).

do_reply(Event, Payload, State) ->
    Data = data(Event, Payload),
    Msg = werl_json:encode(Data),
    {reply, {text, Msg}, State, hibernate}.

data(Event, Payload) ->
    #{
        <<"event">> => Event,
        <<"payload">> => Payload
    }.
