-module(werl_template_sup).

-behaviour(supervisor).

%% API functions
-export([
    start_link/0,
    start_child/2,
    start_child/3,
    load_templates/2
]).

%% Supervisor callbacks
-export([init/1]).

%% Defines
-define(SERVER, ?MODULE).

%%%=============================================================================
%%% API functions
%%%=============================================================================
-spec start_link() -> supervisor:startlink_ret().

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

-spec start_child(atom(), binary()) -> supervisor:startchild_ret().

start_child(Id, Bin) ->
    supervisor:start_child(?MODULE, [Id, Bin]).

-spec start_child(atom(), atom(), file:filename_all()) -> supervisor:startchild_ret().

start_child(App, Id, FileName) ->
    supervisor:start_child(?MODULE, [App, Id, FileName]).

load_templates(App, Renderer) ->
    lists:map(
        fun({Id, FileName}) -> {ok, _} = start_child(App, Id, FileName) end,
        Renderer:templates()
    ).

%%%=============================================================================
%%% Supervisor callbacks
%%%=============================================================================
-spec init([]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.

init([]) ->
    SupFlags = #{
        strategy => simple_one_for_one,
        intensity => 0,
        period => 1
    },
    ChildSpecs = [
        #{
            id => werl_template,
            start => {werl_template, start_link, []}
        }
    ],
    {ok, {SupFlags, ChildSpecs}}.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

% nothing here yet!
