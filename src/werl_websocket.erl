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

-record(state, {
    status :: undefined | ready,
    route :: map(),
    static :: eel_compile:static(),
    view_state = #{} :: map()
}).

%%%=============================================================================
%%% Callbacks
%%%=============================================================================

-spec init(Req, map()) -> {ok | module(), Req, term()} | {module(), Req, term(), term()} when
    Req :: cowboy_req:req().

init(Req0, #{router := Router, idle_timeout := WsConnTimeout} = Args) ->
    io:format("websocket connection initiated~n~p~n~nstate: ~p~n", [Req0, Args]),

    #{path := Path} = cowboy_req:match_qs([path], Req0),
    {ok, Route} = werl_router:route_info(Router, <<"GET">>, Path),
    State = #state{
        route = Route
    },
    Options = #{idle_timeout => WsConnTimeout},

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

-spec websocket_init(State) -> call_result(State) when State :: term().

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

-spec websocket_info(term(), State) -> call_result(State) when State :: term().

websocket_info({notify, Event}, State) ->
    do_reply(Event, State);
websocket_info({notify, Event, Payload}, State) ->
    do_reply(Event, Payload, State).

-spec terminate(term(), cowboy_req:req(), term()) -> ok.

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
    #{<<"event">> := <<"ready">>, <<"payload">> := Payload},
    #state{status = undefined} = State0
) ->
    io:format("Got ready ~p~n", [Payload]),
    #{<<"static">> := Static} = Payload,
    State = State0#state{
        status = ready,
        static = Static
    },
    do_reply(State);
do_handle(
    #{<<"event">> := Event, <<"payload">> := Payload},
    #state{route = Route, view_state = ViewState0} = State0
) ->
    #{controller := Controller, params := Params} = Route,
    case erlang:apply(Controller, handle_event, [Event, Payload, Params, ViewState0]) of
        {reply, ReEvent, RePayload, ViewState1} ->
            ViewState = maps:merge(ViewState0, ViewState1),
            State = State0#state{
                view_state = ViewState
            },
            do_reply(ReEvent, RePayload, State);
        noreply ->
            do_reply(State0)
    end.

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
