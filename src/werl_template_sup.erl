-module(werl_template_sup).

-behaviour(supervisor).

%% API functions
-export([start_link/0]).

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

%%%=============================================================================
%%% Supervisor callbacks
%%%=============================================================================
-spec init([]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.

init([]) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 0,
        period => 1
    },
    Home = #{
        id => home_template,
        start =>
            {werl_template, start_link, [
                home_template,
                "index.html.eel",
                #{<<"count">> => <<"0">>}
            ]}
    },
    ChildSpecs = [Home],
    {ok, {SupFlags, ChildSpecs}}.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

% Nothing here yet!
