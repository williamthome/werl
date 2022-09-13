-module(werl_js).

-export([
    script/1
]).

script(Static) ->
    Bin =
        <<
            "<script src=\"js/morphdom.min.js\" async></script>"
            "<script>window.werlStatic = <%= Static .%></script>",
            "<script src=\"js/werl.broker.js\" defer></script>"
        >>,
    eel:eval(Bin, #{'Static' => Static}).
