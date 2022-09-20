-module(werl_controller).

-type event() :: binary().
-type payload() :: term().
-type params() :: map().
-type state() :: map().
-type topic() :: binary().
-type token() :: binary().

-callback handle_event(event(), payload(), params(), state()) ->
    {reply, event(), payload(), state()} | noreply.

-callback handle_join(topic(), token()) -> {ok, term()} | error.

-optional_callbacks([
    handle_event/4,
    handle_join/2
]).
