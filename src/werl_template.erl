-module(werl_template).

-behaviour(gen_server).

%% API functions
-export([start_link/2]).
-export([render/2, static/1, bindings/1]).

%% GenServer callbacks
-export([init/1, handle_call/3, handle_cast/2]).

-record(state, {
    compiled = []  :: eel_compile:result(),
    memo     = #{} :: eel_render:memo()
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
    Compiled = eel:compile(Html),
    InitArgs = [Compiled],
    gen_server:start_link({local, TemplateId}, ?MODULE, InitArgs, []).

%%------------------------------------------------------------------------------
%% @doc Render.
%% @end
%%------------------------------------------------------------------------------
-spec render(atom(), map()) -> {ok, binary()} | {error, term()}.

render(TemplateId, Bindings) ->
    gen_server:call(TemplateId, {render, Bindings}).

%%------------------------------------------------------------------------------
%% @doc Static.
%% @end
%%------------------------------------------------------------------------------
-spec static(atom()) -> binary().

static(TemplateId) ->
    gen_server:call(TemplateId, static).

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

init([Compiled]) ->
    State = #state{compiled = Compiled},
    {ok, State}.

handle_call(
    {render, Bindings},
    _From,
    #state{compiled = {Static, _AST} = Compiled, memo = Memo} = State0
) ->
    {Render, NewMemo, Indexes} = eel:render(Compiled, Memo, Bindings),
    State = State0#state{memo = NewMemo},
    {reply, {Render, Static, Indexes}, State};
handle_call(static, _From, #state{compiled = {Static, _}} = State) ->
    {reply, Static, State};
handle_call(bindings, _From, #state{memo = #{bindings := Bindings}} = State) ->
    {reply, Bindings, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

% Nothing here yet!
