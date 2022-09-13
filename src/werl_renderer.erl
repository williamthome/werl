-module(werl_renderer).

%% TYPES
-export_type([
    template_def/0
]).

-type template_def() :: {Id :: atom(), FileName :: string()}.

%% CALLBACKS
-callback templates() -> [template_def()].
