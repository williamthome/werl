-module(werl_template_render).

-export([render/2, render/4]).

render(Static, Vars, Dynamic0, Bindings0) when is_list(Vars) ->
    Bindings1 = maps:merge(Dynamic0, Bindings0),
    Bindings =
        lists:foldl(
            fun({Var, Value}, Acc) ->
                case proplists:get_value(Var, Vars) of
                    undefined ->
                        Acc;
                    Indexes ->
                        lists:foldl(
                            fun(Index, IndexAcc) ->
                                IndexAcc#{Index => Value}
                            end,
                            Acc,
                            Indexes
                        )
                end
            end,
            #{},
            maps:to_list(Bindings1)
        ),
    Dynamic1 = maps:to_list(Bindings),
    {_, Dynamic} = lists:unzip(Dynamic1),
    case render(Static, Dynamic) of
        {ok, Render} -> {ok, {Render, Bindings}};
        {error, Reason} -> {error, Reason}
    end.

render(Static, Dynamic) when
    is_list(Static),
    is_list(Dynamic),
    length(Dynamic) =< length(Static)
->
    do_render(Static, Dynamic, <<>>).

do_render([S | Static], [D | Dynamic], Acc) ->
    do_render(Static, Dynamic, <<Acc/binary, S/binary, D/binary>>);
do_render([S | Static], [], Acc) ->
    do_render(Static, [], <<Acc/binary, S/binary>>);
do_render([], [], Acc) ->
    {ok, Acc}.
