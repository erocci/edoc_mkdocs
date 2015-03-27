%%%-------------------------------------------------------------------
%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2015, Jean Parpaillon
%%% 
%%% This file is provided to you under the Apache License,
%%% Version 2.0 (the "License"); you may not use this file
%%% except in compliance with the License.  You may obtain
%%% a copy of the License at
%%% 
%%%   http://www.apache.org/licenses/LICENSE-2.0
%%% 
%%% Unless required by applicable law or agreed to in writing,
%%% software distributed under the License is distributed on an
%%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%%% KIND, either express or implied.  See the License for the
%%% specific language governing permissions and limitations
%%% under the License.
%%% 
%%% @doc edoc doclet module for generating mkdocs
%%% @end
%%% Created : 26 Mar 2015 by Jean Parpaillon <jean.parpaillon@free.fr>
%%%-------------------------------------------------------------------

-module(edoc_doclet_mkdocs).

-export([run/2]).

-import(edoc_report, [report/2, warning/2]).

%% @headerfile "edoc_doclet.hrl"
-include_lib("edoc/include/edoc_doclet.hrl").
-include_lib("xmerl/include/xmerl.hrl").

-define(DEFAULT_FILE_SUFFIX, ".md").
-define(DEFAULT_LAYOUT, edoc_layout_mkdocs).

%% @spec (Command::doclet_gen() | doclet_toc(), edoc_context()) -> ok
%% @doc Main doclet entry point. See the file <a
%% href="../include/edoc_doclet.hrl">`edoc_doclet.hrl'</a> for the data
%% structures used for passing parameters.

run(#doclet_gen{}=Cmd, #context{opts=Opts}=Ctxt) ->
    gen(Cmd#doclet_gen.sources,
	Cmd#doclet_gen.app,
	Cmd#doclet_gen.modules,
	Ctxt#context{opts=[{layout, ?DEFAULT_LAYOUT} | Opts]});
run(#doclet_toc{}, _Ctxt) ->
    ok.

gen(Sources, App, Modules, Ctxt) ->
    Dir = filename:join(Ctxt#context.dir, atom_to_list(App)),
    Env = Ctxt#context.env,
    Options = Ctxt#context.opts,
    {_Modules1, Error} = sources(Sources, Dir, Modules, Env, Options),
    %% handle postponed error during processing of source files
    case Error of
	true -> exit(error);
	false -> ok
    end.


%% Processing the individual source files.

%% NEW-OPTIONS: file_suffix, private, hidden
%% INHERIT-OPTIONS: edoc:layout/2
%% INHERIT-OPTIONS: edoc:get_doc/3
%% DEFER-OPTIONS: run/2

sources(Sources, Dir, Modules, Env, Options) ->
    Suffix = proplists:get_value(file_suffix, Options,
				 ?DEFAULT_FILE_SUFFIX),
    Private = proplists:get_bool(private, Options),
    Hidden = proplists:get_bool(hidden, Options),
    {Ms, E} = lists:foldl(fun (Src, {Set, Error}) ->
				  source(Src, Dir, Suffix, Env, Set,
					 Private, Hidden, Error, Options)
			  end,
			  {sets:new(), false}, Sources),
    {[M || M <- Modules, sets:is_element(M, Ms)], E}.


%% Generating documentation for a source file, adding its name to the
%% set if it was successful. Errors are just flagged at this stage,
%% allowing all source files to be processed even if some of them fail.

source({M, P, Name, Path}, Dir, Suffix, Env, Set, Private, Hidden,
       Error, Options) ->
    File = filename:join(Path, Name),
    try edoc:get_doc(File, Env, Options) of
	{Module, Doc} ->
	    check_name(Module, M, P, File),
	    case ((not is_private(Doc)) orelse Private)
		andalso ((not is_hidden(Doc)) orelse Hidden) of
		true ->
		    Text = edoc:layout(Doc, Options),
		    Name1 = atom_to_list(M) ++ Suffix,
                    Encoding = [{encoding,encoding(Doc)}],
		    edoc_lib:write_file(Text, Dir, Name1, P, Encoding),
		    {sets:add_element(Module, Set), Error};
		false ->
		    {Set, Error}
	    end
    catch throw:R ->
	    report("skipping source file '~ts': ~P.", [File, R, 15]),
	    {Set, true}
    end.

check_name(M, M0, P0, File) ->
    P = '',
    N = M,
    N0 = M0,
    case N of
	[$? | _] ->
	    %% A module name of the form '?...' is assumed to be caused
	    %% by the epp_dodger parser when the module declaration has
	    %% the form '-module(?MACRO).'; skip the filename check.
	    ok;
	_ ->
	    if N =/= N0 ->
		    warning("file '~ts' actually contains module '~s'.",
			    [File, M]);
	       true ->
		    ok
	    end
    end,
    if P =/= P0 ->
	    warning("file '~ts' belongs to package '~s', not '~s'.",
		    [File, P, P0]);
       true ->
	    ok
    end.

is_private(E) ->
    case get_attrval(private, E) of
 	"yes" -> true;
 	_ -> false
    end.

is_hidden(E) ->
    case get_attrval(hidden, E) of
 	"yes" -> true;
 	_ -> false
    end.

encoding(E) ->
    case get_attrval(encoding, E) of
        "latin1" -> latin1;
        _ -> utf8
    end.

get_attrval(Name, #xmlElement{attributes = As}) ->
    case get_attr(Name, As) of
	[#xmlAttribute{value = V}] ->
	    V;
	[] -> ""
    end.

get_attr(Name, [#xmlAttribute{name = Name} = A | As]) ->
    [A | get_attr(Name, As)];
get_attr(Name, [_ | As]) ->
    get_attr(Name, As);
get_attr(_, []) ->
    [].
