%% TODO: rename FAIL to ABORT once we require at least R13B04 for
%% building rebar. Macros with different arity were not supported by the
%% compiler before 13B04.
-define(FAIL, rebar_utils:abort()).
-define(ABORT(Str, Args), rebar_utils:abort(Str, Args)).

-define(CONSOLE(Str, Args), io:format(Str, Args)).

-define(INFO(Str, Args), ?CONSOLE(Str, Args)).
-define(WARN(Str, Args), ?CONSOLE(Str, Args)).
-define(ERROR(Str, Args), io:format(standard_error, Str, Args)).

-define(FMT(Str, Args), lists:flatten(io_lib:format(Str, Args))).
