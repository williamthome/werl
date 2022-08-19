-module(werl_template).

-behaviour(gen_server).

%% API functions
-export([start_link/3, start_link/4, render/2, get_static/1]).

%% GenServer callbacks
-export([init/1, handle_call/3, handle_cast/2]).

-record(state, {
    static :: [binary()],
    vars :: [{binary(), integer()}],
    dynamic :: #{integer() => binary()} | [binary()],
    renders = [] :: [binary()]
}).

%%%=============================================================================
%%% API functions
%%%=============================================================================
-spec start_link(atom(), string() | binary(), map()) -> gen:start_ret().

start_link(TemplateId, TemplateFilename, Dynamic) when is_list(TemplateFilename) ->
    BaseDir = werl_app:priv_dir(),
    Template = filename:join([BaseDir, "templates", TemplateFilename]),
    case file:read_file(Template) of
        {ok, Html} -> start_link(TemplateId, Html, Dynamic);
        {error, Reason} -> {error, Reason}
    end;
start_link(TemplateId, Html, Dynamic) when is_binary(Html) ->
    {Static, Vars} = werl_template_scan:scan(Html),
    start_link(TemplateId, Static, Vars, Dynamic).

-spec start_link(atom(), binary(), list(), map()) -> gen:start_ret().

start_link(TemplateId, Static, Vars, Dynamic) when
    is_atom(TemplateId),
    is_list(Static),
    is_list(Vars),
    is_map(Dynamic)
->
    InitArgs = [Static, Vars, Dynamic],
    gen_server:start_link({local, TemplateId}, ?MODULE, InitArgs, []).

%%------------------------------------------------------------------------------
%% @doc Render.
%% @end
%%------------------------------------------------------------------------------
-spec render(atom(), map()) -> {ok, binary()} | {error, term()}.

render(TemplateId, Bindings) ->
    gen_server:call(TemplateId, {render, Bindings}).

get_static(TemplateId) ->
    gen_server:call(TemplateId, get_static).

%%%=============================================================================
%%% GenServer callbacks
%%%=============================================================================
-spec init(list()) -> {ok, #state{}}.

init([Static, Vars, Dynamic]) ->
    State = #state{
        static = Static,
        vars = Vars,
        dynamic = Dynamic
    },
    {ok, State}.

handle_call(
    {render, Bindings},
    _From,
    #state{
        static = Static,
        vars = Vars,
        dynamic = Dynamic,
        renders = Renders
    } = State0
) ->
    case werl_template_render:render(Static, Vars, Dynamic, Bindings) of
        {ok, {Render, NewDynamic}} ->
            State = State0#state{
                dynamic = NewDynamic,
                renders = [Render | Renders]
            },
            {reply, {ok, Render}, State};
        {error, Reason} ->
            {reply, {error, Reason}, State0}
    end;
handle_call(get_static, _From, #state{static = Static} = State) ->
    {reply, Static, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

% Nothing here yet!
