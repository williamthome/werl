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
    Body = werl_ctrl_home:render(),
    build_html(Body).

build_html(Body) ->
    <<
        "<!DOCTYPE html>"
        "<html lang=\"en\">"
        "<head>"
        "<meta charset=\"UTF-8\">"
        "<meta http-equiv=\"X-UA-Compatible\" content=\"IE=edge\">"
        "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">"
        "<title>WErl</title>"
        "<link rel=\"stylesheet\" href=\"css/style.css\">"
        "<script src=\"js/broker.js\" async></script>"
        "<script src=\"js/morphdom.min.js\" async></script>"
        "<script src=\"js/main.js\" defer></script>"
        "</head>"
        "<body>",
        Body/binary,
        "</body>"
        "</html>"
    >>.
