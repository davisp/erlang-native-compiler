%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
%% -------------------------------------------------------------------
%%
%% rebar: Erlang Build Tools
%%
%% Copyright (c) 2009, 2010 Dave Smith (dizzyd@dizzyd.com)
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
-module(rebar_utils).

-export([get_cwd/0,
         is_arch/1,
         get_arch/0,
         is_app_dir/0,
         app_name/2,
         wordsize/0,
         sh/2,
         abort/0,
         abort/2,
         expand_env_variable/3,
         delayed_halt/1,
         base_dir/1,
         processing_base_dir/1,
         init_vsn_cache/1,
         delete_each/1
        ]).

-include("rebar.hrl").

%% ====================================================================
%% Public API
%% ====================================================================

get_cwd() ->
    {ok, Dir} = file:get_cwd(),
    Dir.

is_arch(ArchRegex) ->
    case re:run(get_arch(), ArchRegex, [{capture, none}]) of
        match ->
            true;
        nomatch ->
            false
    end.

%%
%% REBAR_TARGET_ARCH, if used, should be set to the "standard"
%% target string. That is a prefix for binutils tools.
%% "x86_64-linux-gnu" or "arm-linux-gnueabi" are good candidates
%% ${REBAR_TARGET_ARCH}-gcc, ${REBAR_TARGET_ARCH}-ld ...
%%
get_arch() ->
    Arch = os:getenv("REBAR_TARGET_ARCH"),
    Words = wordsize(Arch),
    otp_release() ++ "-" ++ get_system_arch(Arch) ++ "-" ++ Words.

get_system_arch(Arch) when Arch =:= false; Arch =:= "" ->
    erlang:system_info(system_architecture);
get_system_arch(Arch) ->
    Arch.

is_app_dir() ->
    is_app_dir(rebar_utils:get_cwd()).

is_app_dir(Dir) ->
	SrcDir = filename:join([Dir, "src"]),
	AppSrcScript = filename:join([SrcDir, "*.app.src.script"]),
	AppSrc = filename:join([SrcDir, "*.app.src"]),
	case {filelib:wildcard(AppSrcScript), filelib:wildcard(AppSrc)} of
		{[AppSrcScriptFile], _} ->
			{true, AppSrcScriptFile};
		{[], [AppSrcFile]} ->
			{true, AppSrcFile};
		{[],[]} ->
			EbinDir = filename:join([Dir, "ebin"]),
			App = filename:join([EbinDir, "*.app"]),
			case filelib:wildcard(App) of
				[AppFile] ->
					{true, AppFile};
				[] ->
					false;
				_ ->
					?ERROR("More than one .app file in ~s~n", [EbinDir]),
					false
			end;
		{_, _} ->
			?ERROR("More than one .app.src file in ~s~n", [SrcDir]),
			false
	end.


app_name(Config, AppFile) ->
    case load_app_file(Config, AppFile) of
        {ok, NewConfig, AppName, _} ->
            {NewConfig, AppName};
        {error, Reason} ->
            ?ABORT("Failed to extract name from ~s: ~p\n",
                   [AppFile, Reason])
    end.


load_app_file(Config, Filename) ->
    AppFile = {app_file, Filename},
    case rebar_config:get_xconf(Config, {appfile, AppFile}, undefined) of
        undefined ->
            case consult_app_file(Filename) of
                {ok, {application, AppName, AppData}} ->
                    Config1 = rebar_config:set_xconf(Config,
                                                     {appfile, AppFile},
                                                     {AppName, AppData}),
                    {ok, Config1, AppName, AppData};
                {error, _} = Error ->
                    {error, {error, Error}};
                Other ->
                    {error, {unexpected_terms, Other}}
            end;
        {AppName, AppData} ->
            {ok, Config, AppName, AppData}
    end.


consult_app_file(Filename) ->
    Result = case lists:suffix(".app", Filename) of
                 true ->
                     file:consult(Filename);
                 false ->
                     rebar_config:consult_file(Filename)
             end,
    case Result of
        {ok, [Term]} ->
            {ok, Term};
        _ ->
            Result
    end.

wordsize() ->
    wordsize(os:getenv("REBAR_TARGET_ARCH")).

%%
%% Options = [Option] -- defaults to [use_stdout, abort_on_error]
%% Option = ErrorOption | OutputOption | {cd, string()} | {env, Env}
%% ErrorOption = return_on_error | abort_on_error | {abort_on_error, string()}
%% OutputOption = use_stdout | {use_stdout, bool()}
%% Env = [{string(), Val}]
%% Val = string() | false
%%
sh(Command0, Options0) ->
    ?INFO("sh info:\n\tcwd: ~p\n\tcmd: ~s\n", [get_cwd(), Command0]),

    DefaultOptions = [use_stdout, abort_on_error],
    Options = [expand_sh_flag(V)
               || V <- proplists:compact(Options0 ++ DefaultOptions)],

    ErrorHandler = proplists:get_value(error_handler, Options),
    OutputHandler = proplists:get_value(output_handler, Options),

    Command = patch_on_windows(Command0, proplists:get_value(env, Options, [])),
    PortSettings = proplists:get_all_values(port_settings, Options) ++
        [exit_status, {line, 16384}, use_stdio, stderr_to_stdout, hide],
    Port = open_port({spawn, Command}, PortSettings),

    case sh_loop(Port, OutputHandler, []) of
        {ok, _Output} = Ok ->
            Ok;
        {error, {_Rc, _Output}=Err} ->
            ErrorHandler(Command, Err)
    end.

-spec abort() -> no_return().
abort() ->
    throw(rebar_abort).

-spec abort(string(), [term()]) -> no_return().
abort(String, Args) ->
    ?ERROR(String, Args),
    abort().

%%
%% Given env. variable FOO we want to expand all references to
%% it in InStr. References can have two forms: $FOO and ${FOO}
%% The end of form $FOO is delimited with whitespace or eol
%%
expand_env_variable(InStr, VarName, RawVarValue) ->
    case string:chr(InStr, $$) of
        0 ->
            %% No variables to expand
            InStr;
        _ ->
            ReOpts = [global, unicode, {return, list}],
            VarValue = re:replace(RawVarValue, "\\\\", "\\\\\\\\", ReOpts),
            %% Use a regex to match/replace:
            %% Given variable "FOO", match $FOO\W | $FOOeol | ${FOO}.
            RegEx = io_lib:format("\\\$(~s(\\W|$)|{~s})", [VarName, VarName]),
            re:replace(InStr, RegEx, [VarValue, "\\2"], ReOpts)
    end.

init_vsn_cache(Config) ->
    init_vsn_cache(Config, os:getenv("REBAR_VSN_CACHE_FILE")).
init_vsn_cache(Config, false) ->
    rebar_config:set_xconf(Config, vsn_cache, dict:new());
init_vsn_cache(Config, CacheFile) ->
    {ok, CacheList} = file:consult(CacheFile),
    CacheDict = dict:from_list(CacheList),
    rebar_config:set_xconf(Config, vsn_cache, CacheDict).

-spec delayed_halt(integer()) -> no_return().
delayed_halt(Code) ->
    %% Work around buffer flushing issue in erlang:halt if OTP older
    %% than R15B01.
    %% TODO: remove workaround once we require R15B01 or newer
    %% R15B01 introduced erlang:halt/2
    case erlang:is_builtin(erlang, halt, 2) of
        true ->
            halt(Code);
        false ->
            case os:type() of
                {win32, nt} ->
                    timer:sleep(100),
                    halt(Code);
                _ ->
                    halt(Code),
                    %% workaround to delay exit until all output is written
                    receive after infinity -> ok end
            end
    end.

delete_each([]) ->
    ok;
delete_each([File | Rest]) ->
    case file:delete(File) of
        ok ->
            delete_each(Rest);
        {error, enoent} ->
            delete_each(Rest);
        {error, Reason} ->
            ?ERROR("Failed to delete file ~s: ~p\n", [File, Reason]),
            ?FAIL
    end.

base_dir(Config) ->
    rebar_config:get_xconf(Config, base_dir).

processing_base_dir(Config) ->
    Cwd = rebar_utils:get_cwd(),
    processing_base_dir(Config, Cwd).

processing_base_dir(Config, Dir) ->
    AbsDir = filename:absname(Dir),
    AbsDir =:= base_dir(Config).

otp_release() ->
    case application:get_env(enc, memoized_otp_release) of
        {ok, Return} ->
            Return;
        undefined ->
            Return = otp_release_1(erlang:system_info(otp_release)),
            application:set_env(enc, memoized_otp_release, Return),
            Return
    end.

%% If OTP <= R16, otp_release is already what we want.
otp_release_1([$R,N|_]=Rel) when is_integer(N) ->
    Rel;
%% If OTP >= 17.x, erlang:system_info(otp_release) returns just the
%% major version number, we have to read the full version from
%% a file. See http://www.erlang.org/doc/system_principles/versions.html
otp_release_1(Rel) ->
    Files = [
             filename:join([code:root_dir(), "releases", Rel, "OTP_VERSION"]),
             filename:join([code:root_dir(), "OTP_VERSION"])
            ],

    %% It's possible that none of the above files exist on the filesystem, in
    %% which case, we're just going to rely on the provided "Rel" (which should
    %% just be the value of `erlang:system_info(otp_release)`).
    case read_otp_version_files(Files) of
        undefined ->
            warn_missing_otp_version_file(Rel),
            Rel;
        Vsn ->
            Vsn
    end.

warn_missing_otp_version_file(Rel) ->
    ?WARN("No OTP_VERSION file found. Using version string ~p.~n", [Rel]).

%% Try to open each file path provided, and if any of them exist on the
%% filesystem, read their contents and return the value of the first one found.
read_otp_version_files([]) ->
    undefined;
read_otp_version_files([File | Rest]) ->
    case file:read_file(File) of
        {ok, Vsn} -> normalize_otp_version(Vsn);
        {error, enoent} -> read_otp_version_files(Rest)
    end.

%% Takes the Version binary as read from the OTP_VERSION file and strips any
%% trailing "**" and trailing "\n", returning the string as a list.
normalize_otp_version(Vsn) ->
    %% It's fine to rely on the binary module here because we can
    %% be sure that it's available when the otp_release string does
    %% not begin with $R.
    Size = byte_size(Vsn),
    %% The shortest vsn string consists of at least two digits
    %% followed by "\n". Therefore, it's safe to assume Size >= 3.
    case binary:part(Vsn, {Size, -3}) of
        <<"**\n">> ->
            %% The OTP documentation mentions that a system patched
            %% using the otp_patch_apply tool available to licensed
            %% customers will leave a '**' suffix in the version as a
            %% flag saying the system consists of application versions
            %% from multiple OTP versions. We ignore this flag and
            %% drop the suffix, given for all intents and purposes, we
            %% cannot obtain relevant information from it as far as
            %% tooling is concerned.
            binary:bin_to_list(Vsn, {0, Size - 3});
        _ ->
            binary:bin_to_list(Vsn, {0, Size - 1})
    end.

%% We do the shell variable substitution ourselves on Windows and hope that the
%% command doesn't use any other shell magic.
patch_on_windows(Cmd, Env) ->
    case os:type() of
        {win32,nt} ->
            Cmd1 = "cmd /q /c "
                ++ lists:foldl(fun({Key, Value}, Acc) ->
                                       expand_env_variable(Acc, Key, Value)
                               end, Cmd, Env),
            %% Remove left-over vars
            re:replace(Cmd1, "\\\$\\w+|\\\${\\w+}", "",
                       [global, {return, list}]);
        _ ->
            Cmd
    end.

expand_sh_flag(return_on_error) ->
    {error_handler,
     fun(_Command, Err) ->
             {error, Err}
     end};
expand_sh_flag({abort_on_error, Message}) ->
    {error_handler,
     log_msg_and_abort(Message)};
expand_sh_flag(abort_on_error) ->
    {error_handler,
     fun log_and_abort/2};
expand_sh_flag(use_stdout) ->
    {output_handler,
     fun(Line, Acc) ->
             ?CONSOLE("~s", [Line]),
             [Line | Acc]
     end};
expand_sh_flag({use_stdout, false}) ->
    {output_handler,
     fun(Line, Acc) ->
             [Line | Acc]
     end};
expand_sh_flag({cd, _CdArg} = Cd) ->
    {port_settings, Cd};
expand_sh_flag({env, _EnvArg} = Env) ->
    {port_settings, Env}.

-type err_handler() :: fun((string(), {integer(), string()}) -> no_return()).
-spec log_msg_and_abort(string()) -> err_handler().
log_msg_and_abort(Message) ->
    fun(_Command, {_Rc, _Output}) ->
            ?ABORT(Message, [])
    end.

-spec log_and_abort(string(), {integer(), string()}) -> no_return().
log_and_abort(Command, {Rc, Output}) ->
    ?ABORT("sh(~s)~n"
           "failed with return code ~w and the following output:~n"
           "~s~n", [Command, Rc, Output]).

sh_loop(Port, Fun, Acc) ->
    receive
        {Port, {data, {eol, Line}}} ->
            sh_loop(Port, Fun, Fun(Line ++ "\n", Acc));
        {Port, {data, {noeol, Line}}} ->
            sh_loop(Port, Fun, Fun(Line, Acc));
        {Port, {exit_status, 0}} ->
            {ok, lists:flatten(lists:reverse(Acc))};
        {Port, {exit_status, Rc}} ->
            {error, {Rc, lists:flatten(lists:reverse(Acc))}}
    end.

wordsize(Arch) when Arch =:= false; Arch =:= "" ->
    native_wordsize();
wordsize(Arch) ->
    AllArchs = [
                {"i686","32"},
                {"i386","32"},
                {"arm","32"},
                {"aarch64", "64"},
                {"x86_64","64"}
               ],
    case match_wordsize(Arch, AllArchs) of
        false ->
            case cross_wordsize(Arch) of
                "" ->
                    env_wordsize(os:getenv("REBAR_TARGET_ARCH_WORDSIZE"));
                WordSize ->
                    WordSize
            end;
        {_, Wordsize} ->
            Wordsize
    end.

match_wordsize(Arch, [V={Match,_Bits}|Vs]) ->
    case re:run(Arch, Match, [{capture, none}]) of
        match ->
            V;
        nomatch ->
            match_wordsize(Arch, Vs)
    end;
match_wordsize(_Arch, []) ->
    false.

env_wordsize(Wordsize) when Wordsize =:= false;
                            Wordsize =:= "" ->
    ?WARN("REBAR_TARGET_ARCH_WORDSIZE not set, assuming 32\n", []),
    "32";
env_wordsize(Wordsize) ->
    case Wordsize of
        "16" -> Wordsize;
        "32" -> Wordsize;
        "64" -> Wordsize;
        _ ->
            ?WARN("REBAR_TARGET_ARCH_WORDSIZE bad value: ~p\n", [Wordsize]),
            "32"
    end.

%%
%% Find out the word size of the target by using Arch-gcc
%%
cross_wordsize(Arch) ->
    cross_sizeof(Arch, "void*").

%%
%% Find the size of target Type using a specially crafted C file
%% that will report an error on the line of the byte size of the type.
%%
cross_sizeof(Arch, Type) ->
    Compiler = if Arch =:= "" -> "cc";
                  true -> Arch ++ "-gcc"
               end,
    TempFile = mktempfile(".c"),
    ok = file:write_file(TempFile,
                         <<"int t01 [1 - 2*(((long) (sizeof (TYPE))) == 1)];\n"
                           "int t02 [1 - 2*(((long) (sizeof (TYPE))) == 2)];\n"
                           "int t03 [1 - 2*(((long) (sizeof (TYPE))) == 3)];\n"
                           "int t04 [1 - 2*(((long) (sizeof (TYPE))) == 4)];\n"
                           "int t05 [1 - 2*(((long) (sizeof (TYPE))) == 5)];\n"
                           "int t06 [1 - 2*(((long) (sizeof (TYPE))) == 6)];\n"
                           "int t07 [1 - 2*(((long) (sizeof (TYPE))) == 7)];\n"
                           "int t08 [1 - 2*(((long) (sizeof (TYPE))) == 8)];\n"
                           "int t09 [1 - 2*(((long) (sizeof (TYPE))) == 9)];\n"
                           "int t10 [1 - 2*(((long) (sizeof (TYPE))) == 10)];\n"
                           "int t11 [1 - 2*(((long) (sizeof (TYPE))) == 11)];\n"
                           "int t12 [1 - 2*(((long) (sizeof (TYPE))) == 12)];\n"
                           "int t13 [1 - 2*(((long) (sizeof (TYPE))) == 13)];\n"
                           "int t14 [1 - 2*(((long) (sizeof (TYPE))) == 14)];\n"
                           "int t15 [1 - 2*(((long) (sizeof (TYPE))) == 15)];\n"
                           "int t16 [1 - 2*(((long) (sizeof (TYPE))) == 16)];\n"
                         >>),
    Cmd = Compiler ++ " -DTYPE=\""++Type++"\" " ++ TempFile,
    ShOpts = [{use_stdout, false}, return_on_error],
    {error, {_,Res}} = sh(Cmd, ShOpts),
    ok = file:delete(TempFile),
    case string:tokens(Res, ":") of
        [_, Ln | _] ->
            try list_to_integer(Ln) of
                NumBytes -> integer_to_list(NumBytes*8)
            catch
                error:_ ->
                    ""
            end;
        _ ->
            ""
    end.

mktempfile(Suffix) ->
    {A,B,C} = rebar_now(),
    Dir = temp_dir(),
    File = "rebar_"++os:getpid()++
        integer_to_list(A)++"_"++
        integer_to_list(B)++"_"++
        integer_to_list(C)++Suffix,
    filename:join(Dir, File).

temp_dir() ->
    case os:type() of
        {win32, _} -> windows_temp_dir();
        _ -> "/tmp"
    end.

windows_temp_dir() ->
    case os:getenv("TEMP") of
        false ->
            case os:getenv("TMP") of
                false -> "C:/WINDOWS/TEMP";
                TMP -> TMP
            end;
        TEMP -> TEMP
    end.

rebar_now() ->
    case erlang:function_exported(erlang, timestamp, 0) of
        true ->
            apply(erlang, timestamp, []);
        false ->
            %% erlang:now/0 was deprecated in 18.0. One solution to avoid the
            %% deprecation warning is to use
            %% -compile({nowarn_deprecated_function, [{erlang, now, 0}]}), but
            %% that would raise a warning in versions older than 18.0.  Calling
            %% erlang:now/0 via apply/3 avoids that.
            apply(erlang, now, [])
    end.

native_wordsize() ->
    try erlang:system_info({wordsize, external}) of
        Val ->
            integer_to_list(8 * Val)
    catch
        error:badarg ->
            integer_to_list(8 * erlang:system_info(wordsize))
    end.
