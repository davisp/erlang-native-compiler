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
-module(rebar_app_utils).

-export([is_app_dir/0,
         app_name/2]).


-include("rebar.hrl").

%% ===================================================================
%% Public API
%% ===================================================================

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


%% ===================================================================
%% Internal functions
%% ===================================================================

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

%% In the case of *.app.src we want to give the user the ability to
%% dynamically script the application resource file (think dynamic version
%% string, etc.), in a way similar to what can be done with the rebar
%% config. However, in the case of *.app, rebar should not manipulate
%% that file. This enforces that dichotomy between app and app.src.
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
