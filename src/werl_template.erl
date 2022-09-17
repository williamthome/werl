-module(werl_template).

-behaviour(gen_server).

%% API functions
-export([
    start_link/2,
    start_link/3,
    render/3,
    render/4,
    compiled/1,
    static/1,
    ast/1
]).

%% GenServer callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2
]).

-record(state, {
    compiled = [] :: eel_compile:result()
}).

%%%=============================================================================
%%% API functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Start link compiling a binary.
%% @end
%%------------------------------------------------------------------------------
-spec start_link(atom(), binary()) -> gen:start_ret().

start_link(TemplateId, Bin) when is_binary(Bin) ->
    Compiled = eel:compile(Bin),
    InitArgs = [Compiled],
    gen_server:start_link({local, TemplateId}, ?MODULE, InitArgs, []).

%%------------------------------------------------------------------------------
%% @doc Start link compiling a file.
%% @end
%%------------------------------------------------------------------------------
-spec start_link(atom(), atom(), file:filename_all()) -> gen:start_ret().

start_link(App, TemplateId, TemplateFilename) when is_list(TemplateFilename) ->
    BaseDir = code:priv_dir(App),
    Template = filename:join([BaseDir, "templates", TemplateFilename]),
    case file:read_file(Template) of
        {ok, Bin} -> start_link(TemplateId, Bin);
        {error, Reason} -> {error, Reason}
    end.

%%------------------------------------------------------------------------------
%% @doc Render.
%% @end
%%------------------------------------------------------------------------------
-spec render(atom(), eel_render:memo(), map()) ->
    {binary(), eel_compile:static(), eel_render:bindings_indexes(), eel_render:memo()}.

render(TemplateId, Bindings, Context) ->
    Memo = maps:new(),
    render(TemplateId, Bindings, Memo, Context).

%%------------------------------------------------------------------------------
%% @doc Render with memo.
%% @end
%%------------------------------------------------------------------------------
-spec render(atom(), eel_render:bindings(), eel_render:memo(), map()) ->
    {binary(), eel_compile:static(), map(), map()}.

render(TemplateId, Bindings, Memo, Context) ->
    gen_server:call(TemplateId, {render, Bindings, Memo, Context}).

%%------------------------------------------------------------------------------
%% @doc Compiled.
%% @end
%%------------------------------------------------------------------------------
-spec compiled(atom()) -> eel_compile:result().

compiled(TemplateId) ->
    gen_server:call(TemplateId, compiled).

%%------------------------------------------------------------------------------
%% @doc Static.
%% @end
%%------------------------------------------------------------------------------
-spec static(atom()) -> eel_compile:static().

static(TemplateId) ->
    gen_server:call(TemplateId, static).

%%------------------------------------------------------------------------------
%% @doc AST.
%% @end
%%------------------------------------------------------------------------------
-spec ast(atom()) -> eel_compile:ast().

ast(TemplateId) ->
    gen_server:call(TemplateId, ast).

%%%=============================================================================
%%% GenServer callbacks
%%%=============================================================================
-spec init(list()) -> {ok, #state{}}.

init([Compiled]) ->
    State = #state{compiled = Compiled},
    {ok, State}.

handle_call({render, Bindings0, Memo, Context}, _From, #state{compiled = Compiled} = State) ->
    Bindings = Bindings0#{'Context' => Context},
    {Render, NewMemo, Indexes} = eel:render(Compiled, Memo, Bindings),
    {Static, _AST} = Compiled,

io:format("Got render ~p~n", [{Compiled}]),

    {reply, {Render, Static, Indexes, NewMemo}, State};
handle_call(compiled, _From, #state{compiled = Compiled} = State) ->
    {reply, Compiled, State};
handle_call(static, _From, #state{compiled = {Static, _}} = State) ->
    {reply, Static, State};
handle_call(ast, _From, #state{compiled = {_, AST}} = State) ->
    {reply, AST, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

% nothing here yet!
