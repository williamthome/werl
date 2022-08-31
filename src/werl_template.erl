-module(werl_template).

-behaviour(gen_server).

%% API functions
-export([start_link/2]).
-export([render/2, bindings/1]).

%% GenServer callbacks
-export([init/1, handle_call/3, handle_cast/2]).

-record(state, {
    static    = [] :: list(),
    dynamic   = [] :: list(),
    snapshots = [] :: list(),
    bindings  = #{} :: map()
}).

%%%=============================================================================
%%% API functions
%%%=============================================================================
-spec start_link(atom(), string() | binary()) -> gen:start_ret().

start_link(TemplateId, TemplateFilename) when is_list(TemplateFilename) ->
    BaseDir = werl_app:priv_dir(),
    Template = filename:join([BaseDir, "templates", TemplateFilename]),
    case file:read_file(Template) of
        {ok, Html} -> start_link(TemplateId, Html);
        {error, Reason} -> {error, Reason}
    end;
start_link(TemplateId, Html) when is_binary(Html) ->
    {Static, Dynamic} = eel:compile(Html),
    InitArgs = [Static, Dynamic],
    gen_server:start_link({local, TemplateId}, ?MODULE, InitArgs, []).

%%------------------------------------------------------------------------------
%% @doc Render.
%% @end
%%------------------------------------------------------------------------------
-spec render(atom(), map()) -> {ok, binary()} | {error, term()}.

render(TemplateId, Bindings) ->
    gen_server:call(TemplateId, {render, Bindings}).

%%------------------------------------------------------------------------------
%% @doc Bindings.
%% @end
%%------------------------------------------------------------------------------
-spec bindings(atom()) -> map().

bindings(TemplateId) ->
    gen_server:call(TemplateId, bindings).

%%%=============================================================================
%%% GenServer callbacks
%%%=============================================================================
-spec init(list()) -> {ok, #state{}}.

init([Static, Dynamic]) ->
    State = #state{
        static = Static,
        dynamic = Dynamic
    },
    {ok, State}.

handle_call(
    {render, Bindings},
    _From,
    #state{
        static = Static,
        dynamic = Dynamic,
        snapshots = Renders,
        bindings = Bindings0
    } = State0
) ->
    Render = eel:render({Static, Dynamic}, Bindings),
    State = State0#state{
        snapshots = [Render | Renders],
        bindings = maps:merge(Bindings0, Bindings)
    },
    {reply, Render, State};
handle_call(bindings, _From, #state{bindings = Bindings} = State) ->
    {reply, Bindings, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

% Nothing here yet!
