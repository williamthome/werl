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
    Bindings = #{
        <<"count">> => <<"0">>
    },
    Body =
        case werl_template:render(home_template, Bindings) of
            {ok, Html} -> Html;
            {error, _Reason} -> <<"An error ocurred!">>
        end,
    Static = werl_template:get_static(home_template),
    Dynamic = werl_template:get_dynamic(home_template),
    Indexes = werl_template:get_indexes(home_template),
    build_html(Body, Static, Dynamic, Bindings, Indexes).

build_html(Body, Static, Dynamic, Bindings, Indexes) ->
    io:format("!! ~p~n", [{Static, Dynamic}]),

    Payload = werl_json:encode(#{
        static => Static,
        dynamic => Dynamic,
        bindings => Bindings,
        indexes => Indexes
    }),
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
        "<script>window.werl = ",
        Payload/binary,
        "</script>"
        "</body>"
        "</html>"
    >>.
