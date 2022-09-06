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

-spec init(Req, any()) ->
    {ok | module(), Req, any()}
    | {module(), Req, any(), any()}
when
    Req :: cowboy_req:req().

init(Req, State) ->
    Method = cowboy_req:method(Req),
    Path = cowboy_req:path(Req),
    handle(Method, Path, Req, State).

-spec terminate(any(), map(), any()) -> ok.

terminate(_Reason, _Req, _State) ->
    ok.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

handle(<<"GET">>, <<"/">>, Req0, State) ->
    Html = build_home_html(),
    Req = cowboy_req:reply(
        200,
        #{<<"content-type">> => <<"text/html">>},
        Html,
        Req0
    ),
    {ok, Req, State}.

build_home_html() ->
    [Body, _] = werl_ctrl_home:render(),
    [App, _] = werl_template:render(app, #{
        'Title' => <<"WErl">>,
        'InnerContent' => Body
    }),
    App.
