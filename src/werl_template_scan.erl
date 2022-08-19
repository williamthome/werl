-module(werl_template_scan).

-export([scan/1]).

scan(Html) ->
    do_scan(Html, text, [<<>>], <<>>, {[<<>>], {0, []}}).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

do_scan(<<"<%", Sign, T/binary>>, text, Acc, Buffer, {Static, Vars}) ->
    do_scan(T, expr, [<<>> | Acc], <<Buffer/binary, "<%", Sign>>, {[<<>> | Static], Vars});
do_scan(<<"%>", T/binary>>, expr, [_ | Acc], Buffer, Context) ->
    do_scan(T, text, Acc, <<Buffer/binary, "%>">>, Context);
do_scan(<<"@", T/binary>>, expr, [_ | Acc], Buffer, Context) ->
    do_scan(T, var, [<<>> | Acc], <<Buffer/binary, "@">>, Context);
do_scan(<<" ", T/binary>>, var, [Var | Acc], Buffer, {Static, {VarIndex, Vars}}) ->
    do_scan(
        T,
        expr,
        [<<>> | Acc],
        <<Buffer/binary, " ">>,
        {Static, {VarIndex + 1, [{Var, VarIndex} | Vars]}}
    );
do_scan(<<"%>", T/binary>>, var, [_ | Acc], Buffer, Context) ->
    do_scan(T, text, Acc, <<Buffer/binary, "%>">>, Context);
do_scan(<<H, T/binary>>, Cursor, [HAcc | Acc], Buffer, {[HStatic | TStatic] = Static0, Vars}) ->
    Static =
        case Cursor of
            text -> [<<HStatic/binary, H>> | TStatic];
            _ -> Static0
        end,
    do_scan(T, Cursor, [<<HAcc/binary, H>> | Acc], <<Buffer/binary, H>>, {Static, Vars});
do_scan(<<>>, _Cursor, _Acc, _Buffer, {Static, {_VarIndex, Vars}}) ->
    {lists:reverse(Static), lists:reverse(Vars)}.
