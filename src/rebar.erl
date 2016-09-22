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
-module(rebar).

-export([main/1]).

-include("rebar.hrl").

-ifndef(BUILD_TIME).
-define(BUILD_TIME, "undefined").
-endif.

-ifndef(VCS_INFO).
-define(VCS_INFO, "undefined").
-endif.

-ifndef(OTP_INFO).
-define(OTP_INFO, "undefined").
-endif.

-define(DEFAULT_JOBS, 3).

%% ====================================================================
%% Public API
%% ====================================================================

%% escript Entry point
main(Args) ->
    case catch(run(Args)) of
        ok ->
            ok;
        rebar_abort ->
            rebar_utils:delayed_halt(1);
        Error ->
            %% Nothing should percolate up from rebar_core;
            %% Dump this error to console
            io:format("Uncaught error in rebar_core: ~p\n", [Error]),
            rebar_utils:delayed_halt(1)
    end.

%% ====================================================================
%% Internal functions
%% ====================================================================

run(["help"]) ->
    usage(),
    help(compile);
run(["help" | RawCommands]) ->
    lists:foreach(fun help/1, [list_to_atom(C) || C <- RawCommands]);
run(["version"]) ->
    ok = load_rebar_app(),
    %% Display vsn and build time info
    version();
run(RawArgs) ->
    ok = load_rebar_app(),
    Args = parse_args(RawArgs),
    BaseConfig = init_config(Args),
    {BaseConfig1, Cmds} = save_options(BaseConfig, Args),
    run_aux(BaseConfig1, Cmds).

load_rebar_app() ->
    %% Pre-load the rebar app so that we get default configuration
    ok = application:load(enc).

help(compile) ->
    rebar_port_compiler:info(help, compile);

help(clean) ->
    rebar_port_compiler:info(help, clean);

help(Command) ->
    ?CONSOLE("No help available for \"~p\"~n", [Command]).


parse_args([]) ->
    {[], []};

parse_args(["-h" | _]) ->
    usage(),
    help(compile),
    rebar_utils:delayed_halt(0);

parse_args(["--help" | _]) ->
    usage(),
    help(compile),
    rebar_utils:delayed_halt(0);

parse_args(["-v" | _]) ->
    version(),
    rebar_utils:delayed_halt(0);

parse_args(["--version" | _]) ->
    version(),
    rebar_utils:delayed_halt(0);

parse_args(["-c", FileName | Rest]) ->
    {Opts, NonOpts} = parse_args(Rest),
    {[{config, FileName} | Opts], NonOpts};

parse_args(["--config", FileName | Rest]) ->
    parse_args(["-c", FileName | Rest]);

parse_args([NonOpt | Rest]) ->
    {Opts, NonOpts} = parse_args(Rest),
    {Opts, [NonOpt | NonOpts]}.


usage() ->
    ?CONSOLE("enc [-hv] [-c CONFIG_FILE] COMMAND [COMMAND ...]~n", []).


init_config({Options, _NonOptArgs}) ->
    %% If $HOME/.rebar/config exists load and use as global config
    GlobalConfigFile = filename:join([os:getenv("HOME"), ".rebar", "config"]),
    GlobalConfig = case filelib:is_regular(GlobalConfigFile) of
                       true ->
                           rebar_config:new(GlobalConfigFile);
                       false ->
                           rebar_config:new()
                   end,

    %% Set the rebar config to use
    GlobalConfig1 = case proplists:get_value(config, Options) of
                        undefined ->
                            GlobalConfig;
                        Conf ->
                            rebar_config:set_global(GlobalConfig, config, Conf)
                    end,

    BaseConfig = rebar_config:base_config(GlobalConfig1),

    %% Keep track of how many operations we do, so we can detect bad commands
    BaseConfig1 = rebar_config:set_xconf(BaseConfig, operations, 0),
    %% Initialize vsn cache
    rebar_utils:init_vsn_cache(BaseConfig1).

init_config1(BaseConfig) ->
    %% Determine the location of the rebar executable; important for pulling
    %% resources out of the escript
    ScriptName = filename:absname(escript:script_name()),
    BaseConfig1 = rebar_config:set_xconf(BaseConfig, escript, ScriptName),
    %% Note the top-level directory for reference
    AbsCwd = filename:absname(rebar_utils:get_cwd()),
    rebar_config:set_xconf(BaseConfig1, base_dir, AbsCwd).

run_aux(BaseConfig, Commands) ->
    %% Make sure crypto is running
    case crypto:start() of
        ok -> ok;
        {error,{already_started,crypto}} -> ok
    end,

    %% Convert command strings to atoms
    CommandAtoms = [list_to_atom(C) || C <- Commands],

    BaseConfig1 = init_config1(BaseConfig),

    %% Make sure we're an app directory
    AppFile = case rebar_utils:is_app_dir() of
        {true, AppFile0} ->
            AppFile0;
        false ->
            rebar_utils:delayed_halt(1)
    end,

    % Setup our environment
    BaseConfig2 = setup_envs(BaseConfig1, [rebar_port_compiler]),

    %% Process each command, resetting any state between each one
    lists:foreach(fun(Command) ->
        process_command(Command, BaseConfig2, AppFile)
    end, CommandAtoms).


setup_envs(Config, Modules) ->
    lists:foldl(fun(Module, CfgAcc) ->
        Env = Module:setup_env(CfgAcc),
        rebar_config:save_env(CfgAcc, Module, Env)
    end, Config, Modules).


process_command(compile, Config, AppFile) ->
    rebar_port_compiler:compile(Config, AppFile);

process_command(clean, Config, AppFile) ->
    rebar_port_compiler:clean(Config, AppFile);

process_command(escriptize, Config, AppFile) ->
    rebar_escripter:escriptize(Config, AppFile);

process_command(Other, _, _) ->
    ?CONSOLE("Unknown command: ~s~n", [Other]),
    rebar_utils:delayed_halt(1).


save_options(Config, {Options, NonOptArgs}) ->
    GlobalDefines = proplists:get_all_values(defines, Options),
    Config1 = rebar_config:set_xconf(Config, defines, GlobalDefines),
    filter_flags(Config1, NonOptArgs, []).


%%
%% show version information and halt
%%
version() ->
    {ok, Vsn} = application:get_key(enc, vsn),
    ?CONSOLE("enc ~s ~s ~s ~s\n",
             [Vsn, ?OTP_INFO, ?BUILD_TIME, ?VCS_INFO]).

%%
%% Seperate all commands (single-words) from flags (key=value) and store
%% values into the rebar_config global storage.
%%
filter_flags(Config, [], Commands) ->
    {Config, lists:reverse(Commands)};
filter_flags(Config, [Item | Rest], Commands) ->
    case string:tokens(Item, "=") of
        [Command] ->
            filter_flags(Config, Rest, [Command | Commands]);
        [KeyStr, RawValue] ->
            Key = list_to_atom(KeyStr),
            Value = case Key of
                        verbose ->
                            list_to_integer(RawValue);
                        _ ->
                            RawValue
                    end,
            Config1 = rebar_config:set_global(Config, Key, Value),
            filter_flags(Config1, Rest, Commands);
        Other ->
            ?CONSOLE("Ignoring command line argument: ~p\n", [Other]),
            filter_flags(Config, Rest, Commands)
    end.
