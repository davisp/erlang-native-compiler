%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
%% -------------------------------------------------------------------
%%
%% rebar: Erlang Build Tools
%%
%% Copyright (c) 2009 Dave Smith (dizzyd@dizzyd.com)
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%% THE SOFTWARE.
%% -------------------------------------------------------------------
-module(rebar_config).

-export([new/0,
         new/1,
         base_config/1,
         consult_file/1,
         get/3,
         get_local/3,
         get_list/3,
         set_global/3,
         get_global/3,
         save_env/3,
         get_env/2,
         set_xconf/3,
         get_xconf/2,
         get_xconf/3]).

-include("rebar.hrl").

-ifdef(namespaced_types).
%% dict:dict() exists starting from Erlang 17.
-type rebar_dict() :: dict:dict(term(), term()).
-else.
%% dict() has been obsoleted in Erlang 17 and deprecated in 18.
-type rebar_dict() :: dict().
-endif.

-type key() :: atom().

-record(config, { dir :: file:filename(),
                  opts = [] :: list(),
                  globals = new_globals() :: rebar_dict(),
                  envs = new_env() :: rebar_dict(),
                  %% cross-directory/-command config
                  skip_dirs = new_skip_dirs() :: rebar_dict(),
                  xconf = new_xconf() :: rebar_dict() }).

-export_type([config/0]).

-opaque config() :: #config{}.

-define(DEFAULT_NAME, "rebar.config").

%% ===================================================================
%% Public API
%% ===================================================================

-spec base_config(config()) -> config().
base_config(GlobalConfig) ->
    ConfName = rebar_config:get_global(GlobalConfig, config, ?DEFAULT_NAME),
    new(GlobalConfig, ConfName).

-spec new() -> config().
new() ->
    #config{dir = rebar_utils:get_cwd()}.

-spec new(file:filename() | config()) -> config().
new(ConfigFile) when is_list(ConfigFile) ->
    case consult_file(ConfigFile) of
        {ok, Opts} ->
            #config { dir = rebar_utils:get_cwd(),
                      opts = Opts };
        Other ->
            ?ABORT("Failed to load ~s: ~p~n", [ConfigFile, Other])
    end;
new(_ParentConfig=#config{opts=Opts0, globals=Globals, skip_dirs=SkipDirs,
                          xconf=Xconf}) ->
    new(#config{opts=Opts0, globals=Globals, skip_dirs=SkipDirs, xconf=Xconf},
        ?DEFAULT_NAME).

-spec get(config(), key(), term()) -> term().
get(Config, Key, Default) ->
    proplists:get_value(Key, Config#config.opts, Default).

-spec get_list(config(), key(), term()) -> term().
get_list(Config, Key, Default) ->
    get(Config, Key, Default).

-spec get_local(config(), key(), term()) -> term().
get_local(Config, Key, Default) ->
    proplists:get_value(Key, local_opts(Config#config.opts, []), Default).

-spec set_global(config(), key(), term()) -> config().
set_global(Config, jobs=Key, Value) when is_list(Value) ->
    set_global(Config, Key, list_to_integer(Value));
set_global(Config, jobs=Key, Value) when is_integer(Value) ->
    NewGlobals = dict:store(Key, erlang:max(1, Value), Config#config.globals),
    Config#config{globals = NewGlobals};
set_global(Config, Key, Value) ->
    NewGlobals = dict:store(Key, Value, Config#config.globals),
    Config#config{globals = NewGlobals}.

-spec get_global(config(), key(), term()) -> term().
get_global(Config, Key, Default) ->
    case dict:find(Key, Config#config.globals) of
        error ->
            Default;
        {ok, Value} ->
            Value
    end.

-spec consult_file(file:filename()) -> term().
consult_file(File) ->
    case filename:extension(File) of
        ".script" ->
            consult_and_eval(remove_script_ext(File), File);
        _ ->
            Script = File ++ ".script",
            case filelib:is_regular(Script) of
                true ->
                    consult_and_eval(File, Script);
                false ->
                    file:consult(File)
            end
    end.

-spec save_env(config(), module(), nonempty_list()) -> config().
save_env(Config, Mod, Env) ->
    NewEnvs = dict:store(Mod, Env, Config#config.envs),
    Config#config{envs = NewEnvs}.

-spec get_env(config(), module()) -> term().
get_env(Config, Mod) ->
    dict:fetch(Mod, Config#config.envs).

-spec set_xconf(config(), term(), term()) -> config().
set_xconf(Config, Key, Value) ->
    NewXconf = dict:store(Key, Value, Config#config.xconf),
    Config#config{xconf=NewXconf}.

-spec get_xconf(config(), term()) -> term().
get_xconf(Config, Key) ->
    {ok, Value} = dict:find(Key, Config#config.xconf),
    Value.

-spec get_xconf(config(), term(), term()) -> term().
get_xconf(Config, Key, Default) ->
    case dict:find(Key, Config#config.xconf) of
        error ->
            Default;
        {ok, Value} ->
            Value
    end.

%% ===================================================================
%% Internal functions
%% ===================================================================

-spec new(config(), file:filename()) -> config().
new(ParentConfig, ConfName) ->
    %% Load terms from rebar.config, if it exists
    Dir = rebar_utils:get_cwd(),
    ConfigFile = filename:join([Dir, ConfName]),
    Opts0 = ParentConfig#config.opts,
    Opts = case consult_file(ConfigFile) of
               {ok, Terms} ->
                   %% Found a config file with some terms. We need to
                   %% be able to distinguish between local definitions
                   %% (i.e. from the file in the cwd) and inherited
                   %% definitions. To accomplish this, we use a marker
                   %% in the proplist (since order matters) between
                   %% the new and old defs.
                   Terms ++ [local] ++
                       [Opt || Opt <- Opts0, Opt /= local];
               {error, enoent} ->
                   [local] ++
                       [Opt || Opt <- Opts0, Opt /= local];
               Other ->
                   ?ABORT("Failed to load ~s: ~p\n", [ConfigFile, Other])
           end,

    ParentConfig#config{dir = Dir, opts = Opts}.

-spec consult_and_eval(file:filename(), file:filename()) -> {ok, term()}.
consult_and_eval(File, Script) ->
    ConfigData = try_consult(File),
    file:script(Script, bs([{'CONFIG', ConfigData}, {'SCRIPT', Script}])).

-spec remove_script_ext(file:filename()) -> file:filename().
remove_script_ext(F) ->
    "tpircs." ++ Rev = lists:reverse(F),
    lists:reverse(Rev).

-spec try_consult(file:filename()) -> term().
try_consult(File) ->
    case file:consult(File) of
        {ok, Terms} ->
            Terms;
        {error, enoent} ->
            [];
        {error, Reason} ->
            ?ABORT("Failed to read config file ~s: ~p~n", [File, Reason])
    end.

-type bs_vars() :: [{term(), term()}].
-spec bs(bs_vars()) -> bs_vars().
bs(Vars) ->
    lists:foldl(fun({K,V}, Bs) ->
                        erl_eval:add_binding(K, V, Bs)
                end, erl_eval:new_bindings(), Vars).

-spec local_opts(list(), list()) -> list().
local_opts([], Acc) ->
    lists:reverse(Acc);
local_opts([local | _Rest], Acc) ->
    lists:reverse(Acc);
local_opts([Item | Rest], Acc) ->
    local_opts(Rest, [Item | Acc]).

-spec new_globals() -> rebar_dict().
new_globals() -> dict:new().

-spec new_env() -> rebar_dict().
new_env() -> dict:new().

-spec new_skip_dirs() -> rebar_dict().
new_skip_dirs() -> dict:new().

-spec new_xconf() -> rebar_dict().
new_xconf() -> dict:new().
