-module(hello_world_controller_404).

-behaviour(werl_controller).

-export([
    index/3
]).

index(Req0, State0, Context) ->
    Head = <<>>,
    Body = <<"Oops! Page not found :(">>,
    hello_world_renderer:req_render(Head, Body, [], Req0, State0, Context).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

% nothing here yet!
