-module(werl_ctrl_home).

-export([
    render/0,
    render/1,
    bindings/0,
    increment/0
]).

render() ->
    Bindings = #{'Count' => 0},
    render(Bindings).

render(Bindings) ->
    werl_template:render(home_template, Bindings).

bindings() ->
    werl_template:bindings(home_template).

increment() ->
    #{'Count' := Count} = bindings(),
    Bindings = #{'Count' => Count + 1},
    render(Bindings).
