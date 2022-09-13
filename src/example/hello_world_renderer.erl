-module(hello_world_renderer).

-behaviour(werl_renderer).

-export([
    templates/0
]).

templates() ->
    [
        {app, "app.html.eel"},
        {home, "index.html.eel"}
    ].
