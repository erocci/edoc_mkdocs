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
%%% @doc mkdocs default layout for edoc_mkdocs
%%% @end
%%% Created : 26 Mar 2015 by Jean Parpaillon <jean.parpaillon@free.fr>
%%%-------------------------------------------------------------------

-module(edoc_layout_mkdocs).

-export([module/2, 
	 package/2,
	 overview/2,
	 type/1
	]).

-import(edoc_report, [report/2]).

-include_lib("xmerl/include/xmerl.hrl").

-define(DEFAULT_XML_EXPORT, xmerl_text).
-define(NL, "\n").
-define(HR, "\n---\n").
-define(BR, "\n\n").
-define(TITLE, "Module").
-define(SUMMARY, "Description").
-define(SUMMARY_LABEL, "description").
-define(TYPES, "Data Types").
-define(TYPES_LABEL, "types").
-define(FN_IDX, "Function Index").
-define(FN_IDX_LABEL, "index").
-define(FN, "Function Details").
-define(FN_LABEL, "functions").

%% @doc The layout function.
%%

module(#xmlElement{name=module, content=Es}=E, Opts) ->
    Name = get_attrval(name, E),
    Desc = get_content(description, Es),
    ShortDesc = to_text(get_content(briefDescription, Desc)),
    FullDesc = to_text(get_content(fullDescription, Desc)),
    Functions = [ {function_name(El), El} || El <- get_content(functions, Es) ],
    SortedFs = lists:sort(Functions),
    Types = [ {type_name(El), El} || El <- get_content(typedecls, Es) ],
    Head = [ 
	     doc_index(FullDesc, Functions, Types), 
	     module_head(Es, Name, ShortDesc)
	   ],
    Body = [section([?TITLE, " ", get_attrval(name, E)], Head), ?BR,
	    section({?SUMMARY, ?SUMMARY_LABEL}, FullDesc), ?BR,
	    section({?TYPES, ?TYPES_LABEL}, types(lists:sort(Types), Opts)), ?BR,
	    section({?FN_IDX, ?FN_IDX_LABEL}, function_index(SortedFs)), ?BR,
	    section({?FN, ?FN_LABEL}, functions(SortedFs, Opts)), ?BR,
	    ?HR,
	    timestamp()
	   ],
    lists:flatten(Body).

package(#xmlElement{name=package, content=_Es}, _Opts) ->
    "".

overview(#xmlElement{name=overview, content=_Es}, _Opts) ->
    "".

type(_E) ->
    "".

%%%
%%% internals
%%%
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

get_elem(Name, [#xmlElement{name = Name} = E | Es]) ->
    [E | get_elem(Name, Es)];
get_elem(Name, [_ | Es]) ->
    get_elem(Name, Es);
get_elem(_, []) ->
    [].

to_text([]) ->
    [];
to_text([#xmlText{}=E]) ->
    to_text(E);
to_text(#xmlText{value=V}) ->
    V;
to_text(E) ->
    xmerl:export_simple(E, ?DEFAULT_XML_EXPORT).

get_content(Name, Es) ->
    case get_elem(Name, Es) of
	[#xmlElement{content=Es1}] -> Es1;
	[] -> []
    end.

seq(F, Es) ->
    seq(F, Es, []).

seq(F, Es, Tail) ->
    seq(F, Es, ", ", Tail).

seq(F, [E], _Sep, Tail) ->
    F(E) ++ Tail;
seq(F, [E | Es], Sep, Tail) ->
    F(E) ++ [Sep] ++ seq(F, Es, Sep, Tail);
seq(_F, [], _Sep, Tail) ->
    Tail.

atom(String) ->
    io_lib:write_atom(list_to_atom(String)).

t_name([E]) ->
    N = get_attrval(name, E),
    case get_attrval(module, E) of
	"" -> atom(N);
	M ->
	    S = atom(M) ++ ":" ++ atom(N),
	    case get_attrval(app, E) of
		"" -> S;
		A -> "//" ++ atom(A) ++ "/" ++ S
	    end
    end.

section(_, []) ->
    [];
section({Title, Label}, Content) ->
    ["## <a name=\"", Label, "\" ></a>", Title, ?NL, ?NL, Content, ?NL, ?NL];
section(Title, Content) ->
    ["## ", Title, ?NL, ?NL, Content, ?NL, ?NL].

module_head(E, Name, Desc) ->
    [Desc, ?NL, 
     copyright(E), ?NL,
     deprecated(E, "module"), ?NL,
     version(E), ?NL,
     since(E), ?NL,
     behaviours(E, Name), ?NL,
     authors(E), ?NL,
     references(E), ?NL,
     sees(E), ?NL,
     todos(E), ?NL
    ].

since(Es) ->
    case to_text(get_content(since, Es)) of
	[] -> [];
	Es1 ->
	    ["_Introduced in_: ", Es1]
    end.

behaviours(Es, Name) ->
    [case get_elem(behaviour, Es) of
	 [] -> [];
	 Es1 ->
	     ["_Behaviours_: ", seq(fun behaviour/1, Es1, ["."])]
     end, ?BR,
     case get_content(callbacks, Es) of
	 [] -> [];
	 Es1 ->
	     ["_This module defines the `", Name, "` behaviour_.", ?BR,
	      "Required callback functions: ", seq(fun callback/1, Es1, ["."])]
     end, ?BR].

behaviour(E=#xmlElement{content=Es}) ->
    see(E, ["`", to_text(Es), "`"]).

sees(Es) ->
    case get_elem(see, Es) of
	[] -> [];
	Es1 ->
	    [ "_See also_: ", seq(fun see/1, Es1, [", "]), ?NL]
    end.

see(E=#xmlElement{content=Es}) ->
    see(E, Es).

see(E, Es) ->
    case href(E) of
	[] -> Es;
	Ref -> Ref
    end.

href(E) ->
    case get_attrval(href, E) of
	"" -> [];
	URI -> ["[", URI, "](", URI, ")"]
    end.

references(Es) ->
    case get_elem(reference, Es) of
	[] -> [];
	Es1 ->
	    ["_References_", ?NL,
	     [ ["* ", C, ?NL] || #xmlElement{content=C} <- Es1 ],
	     ?NL
	    ]
    end.

todos(Es) ->
    case get_elem(todo, Es) of
	[] -> [];
	Es1 ->
	    Todos = [ ["* ", C, ?NL] || #xmlElement{content=C} <- Es1 ],
	    [ "_To do_", ?NL, Todos, ?NL ]
    end.

copyright(Es) ->
    case get_content(copyright, Es) of
	[] -> "";
	[#xmlText{value=S}] -> ["Copyright (c) ", S]
    end.

callback(E=#xmlElement{}) ->
    Name = get_attrval(name, E),
    Arity = get_attrval(arity, E),
    ["`", Name, "/", Arity, "`"].

authors(Es) ->
    case get_elem(author, Es) of
	[] -> [];
	Es1 -> ["_Authors_: ", seq(fun author/1, Es1), ?NL]
    end.

author(E=#xmlElement{}) ->
    Name = get_attrval(name, E),
    Mail = get_attrval(email, E),
    URI = get_attrval(website, E),
    ([if Name == Mail ->       author_email(Mail);
	true ->
	     if Mail == "" -> Name;
		true ->       [Name, " (", author_email(Mail), ")"]
	     end
      end,
      if URI == "" ->	[];
	 true ->	[" \[_website_: [", URI, "](", URI, ")\]"]
      end]).

author_email(Mail) ->
    ["<[", Mail, "](mailto:", Mail, ")>"].

function_name(E) ->
    atom(get_attrval(name, E)) ++ "/" ++ get_attrval(arity, E).

type_name(#xmlElement{content = Es}) ->
    t_name(get_elem(erlangName, get_content(typedef, Es))).

deprecated(Es, S) ->
    Es1 = get_content(description, get_content(deprecated, Es)),
    case to_text(get_content(fullDescription, Es1)) of
	[] -> [];
	Es2 ->
	    ["_This ", S, " is deprecated:_ ", Es2]
    end.

version(Es) ->
    case to_text(get_content(version, Es)) of
	[] -> [];
	Es1 ->
	    ["_Version_: ", Es1]
    end.

doc_index(FullDesc, Functions, Types) ->
    case doc_index_rows(FullDesc, Functions, Types) of
	[] -> [];
	Rs ->
	    [ [ ["* [", T, "](#", R, ")", ?NL] || {T, R} <- Rs ], ?NL ]
    end.

doc_index_rows(FullDesc, Functions, Types) ->
    (
      case FullDesc of
	  [] -> [];
	  _ -> [{?SUMMARY, ?SUMMARY_LABEL}]
      end 
      ++ case Types of
	     [] -> [];
	     _ -> [{?TYPES, ?TYPES_LABEL}]
	 end
      ++ case Functions of
	     [] -> [];
	     _ -> [{?FN, ?FN_LABEL}]
	 end
    ).

types([], _Opts) -> [];
types(Ts, Opts) ->
    [ 
      ?BR,
      lists:flatmap(fun ({Name, E}) -> typedecl(Name, E, Opts) end, Ts)
    ].

typedecl(Name, E=#xmlElement{content=Es}, Opts) ->
    [
     "### ", label_anchor([Name, "()"], E), ?BR,
     typedef(get_content(typedef, Es), Opts), ?BR,
     fulldesc(Es)
    ].

label_anchor(Content, E) ->
    case get_attrval(label, E) of
	"" -> Content;
	Ref -> [ "<a name=\"", Ref, "\" ></a>", Content ]
    end.

fulldesc(Es) ->
    case get_content(fullDescription, get_content(description, Es)) of
	[] -> [?BR];
	Desc -> [to_text(Desc), ?BR]
    end.

typedef(Es, Opts) ->
    Name = [
	    t_name(get_elem(erlangName, Es)),
	    "(", seq(fun t_utype_elem/1, get_content(argtypes, Es)), ")"
	   ],
    [ 
      case get_elem(type, Es) of
	  [] -> ["_abstract datatype_: `", Name, "`"];
	  Type -> format_type(Name, Name, Type, [], Opts)
      end,
      local_defs(get_elem(localdef, Es), Opts)
    ].


t_utype([E]) ->
    t_utype_elem(E).

t_utype_elem(E=#xmlElement{content = Es}) ->
    case get_attrval(name, E) of
	"" -> t_type(Es);
	Name ->
	    T = t_type(Es),
	    case T of
		[Name] -> T;    % avoid generating "Foo::Foo"
		T -> [Name, "::", T]
	    end
    end.

t_type([E=#xmlElement{name = typevar}]) ->
    t_var(E);
t_type([E=#xmlElement{name = atom}]) ->
    t_atom(E);
t_type([E=#xmlElement{name = integer}]) ->
    t_integer(E);
t_type([E=#xmlElement{name = range}]) ->
    t_range(E);
t_type([E=#xmlElement{name = binary}]) ->
    t_binary(E);
t_type([E=#xmlElement{name = float}]) ->
    t_float(E);
t_type([#xmlElement{name = nil}]) ->
    t_nil();
t_type([#xmlElement{name = paren, content = Es}]) ->
    t_paren(Es);
t_type([#xmlElement{name = list, content = Es}]) ->
    t_list(Es);
t_type([#xmlElement{name = nonempty_list, content = Es}]) ->
    t_nonempty_list(Es);
t_type([#xmlElement{name = map, content = Es}]) ->
    t_map(Es);
t_type([#xmlElement{name = tuple, content = Es}]) ->
    t_tuple(Es);
t_type([#xmlElement{name = 'fun', content = Es}]) ->
    ["fun(", t_fun(Es), ")"];
t_type([E = #xmlElement{name = record, content = Es}]) ->
    t_record(E, Es);
t_type([E = #xmlElement{name = abstype, content = Es}]) ->
    t_abstype(E, Es);
t_type([#xmlElement{name = union, content = Es}]) ->
    t_union(Es).

t_var(E) ->
    [get_attrval(name, E)].

t_atom(E) ->
    [get_attrval(value, E)].

t_integer(E) ->
    [get_attrval(value, E)].

t_range(E) ->
    [get_attrval(value, E)].

t_binary(E) ->
    [get_attrval(value, E)].

t_float(E) ->
    [get_attrval(value, E)].

t_nil() ->
    ["[]"].

t_paren(Es) ->
    ["(", t_utype(get_elem(type, Es)), ")"].

t_list(Es) ->
    ["[", t_utype(get_elem(type, Es)), "]"].

t_nonempty_list(Es) ->
    ["[", t_utype(get_elem(type, Es)), ", ...]"].

t_tuple(Es) ->
    ["{", seq(fun t_utype_elem/1, Es, ["}"]) ].

t_fun(Es) ->
    ["(", seq(fun t_utype_elem/1, get_content(argtypes, Es),
	      [") -> ", t_utype(get_elem(type, Es))]) ].

t_map(Es) ->
    Fs = get_elem(map_field, Es),
    ["#{", seq(fun t_map_field/1, Fs, ["}"]) ].

t_map_field(#xmlElement{content = [K,V]}) ->
    [ t_utype_elem(K), " => ", t_utype_elem(V) ].

t_record(E, Es) ->
    Name = ["#", t_type(get_elem(atom, Es)) ],
    case get_elem(field, Es) of
        [] ->
            see(E, [Name, "{}"]);
        Fs ->
            [ see(E, Name), "{", seq(fun t_field/1, Fs, ["}"]) ]
    end.

t_field(#xmlElement{content = Es}) ->
    t_type(get_elem(atom, Es)) ++ [" = "] ++ t_utype(get_elem(type, Es)).

t_abstype(E, Es) ->
    Name = t_name(get_elem(erlangName, Es)),
    case get_elem(type, Es) of
        [] ->
            see(E, [Name, "()"]);
        Ts ->
            see(E, [Name]) ++ ["("] ++ seq(fun t_utype_elem/1, Ts, [")"])
    end.

t_abstype(Es) ->
    ([t_name(get_elem(erlangName, Es)), "("]
     ++ seq(fun t_utype_elem/1, get_elem(type, Es), [")"])).

t_union(Es) ->
    seq(fun t_utype_elem/1, Es, " | ", []).

format_type(Prefix, _Name, Type, Last, _Opts) ->
    [ "`", Prefix, " = ", t_utype(Type), Last ].

local_defs(Es, Opts) ->
    local_defs(Es, [], Opts).

local_defs([], _, _Opts) -> [];
local_defs(Es0, Last, Opts) ->
    [E | Es] = lists:reverse(Es0),
    [?BR,
     lists:reverse(lists:append([localdef(E1, [], Opts) || E1 <- Es]),
		   localdef(E, Last, Opts))
     ].

localdef(#xmlElement{content=Es}=E, Last, Opts) ->
    {Name, N} = case get_elem(typevar, Es) of
		     [] ->
			 N0 = t_abstype(get_content(abstype, Es)),
			 {label_anchor(N0, E), N0};
		     [V] ->
			 N0 = t_var(V),
			 {N0, N0}
		 end,
    [ "* ", format_type(Name, N, get_elem(type, Es), Last, Opts) ].

function_index(Fs) ->
    case function_index_rows(Fs) of
	[] -> [];
	Rows -> 
	    [ "| Function | Description | ", ?NL,
	      "| -------- | ------------| ", ?NL,
	      Rows,
	      ?NL ]
    end.

function_index_rows(Fs) ->
    lists:map(fun ({Name, #xmlElement{content=Es}=F}) ->
		      [ "| ",
			trim(label_href(function_header(Name, F, "*"), F)), " | ",
			trim(index_desc(Es)), " |", ?NL
		      ]
	      end, Fs).

index_desc(Es) ->
    Desc = get_content(description, Es),
    [case get_content(deprecated, Es) of
 	 [] -> [];
 	 _ -> ["(_Deprecated_.)"]
     end,
     case get_content(briefDescription, Desc) of
	 [] -> equiv(Es);    % no description at all if no equiv
	 ShortDesc -> to_text(ShortDesc)
     end].

function_header(Name, E, Private) ->
    case is_exported(E) of
	true -> [Name];
	false -> [Name, Private]
    end.

is_exported(E) ->
    case get_attrval(exported, E) of
 	"yes" -> true;
 	_ -> false
    end.

label_href(Content, F) ->
    case get_attrval(label, F) of
	"" -> Content;
	Ref -> [ "[", Content, "](", local_label(Ref), ")"]
    end.

equiv_p(Es) ->
    equiv(Es, true).

equiv(Es) ->
    equiv(Es, false).

equiv(Es, P) ->
    case get_content(equiv, Es) of
	[] -> [];
	Es1 ->
	    case get_content(expr, Es1) of
		[] -> [];
		[Expr] ->
		    Expr1 = [{tt, [Expr]}],
		    Expr2 = case get_elem(see, Es1) of
				[] ->
				    Expr1;
				[E=#xmlElement{}] ->
				    see(E, Expr1)
			    end,
		    Txt = ["Equivalent to "] ++ Expr2 ++ ["."],
		    (case P of
			 true -> [{p, Txt}];
			 false -> Txt
		     end
		     ++ [?NL])
	    end
    end.

local_label(R) ->
    ["#", R].

functions(Fs, Opts) ->
    lists:flatmap(fun ({Name, E}) -> function(Name, E, Opts) end, Fs).

function(Name, #xmlElement{content=Es}=E, Opts) ->
    [
     "### ", label_anchor(function_header(Name, E, " *"), E), ?BR,
     case typespec(get_content(typespec, Es), Opts) of
	 [] ->
	     signature(get_content(args, Es),
		       get_attrval(name, E));
	 Spec -> Spec
     end, ?BR,
     case params(get_content(args, Es)) of
	 [] -> [];
	 Ps -> [ Ps, ?BR ]
     end, ?BR,
     case returns(get_content(returns, Es)) of
	 [] -> [];
	 Rs -> [ Rs, ?BR ]
     end,
     throws(Es, Opts),
     equiv_p(Es),
     deprecated(Es, "function"),
     fulldesc(Es),
     since(Es),
     sees(Es),
     todos(Es)
    ].

typespec([], _Opts) -> [];
typespec(Es, Opts) ->
    Name = t_name(get_elem(erlangName, Es)),
    Defs = get_elem(localdef, Es),
    [Type] = get_elem(type, Es),
    [ format_spec(Name, Type, Defs, Opts), local_defs(Defs, Opts) ].

format_spec(Sep, Type, Defs, _Opts) ->
    Br = case Defs of [] -> ?BR; _ -> [] end,
    [ "`", t_clause(Sep, Type), "`", Br ].

t_clause(Name, #xmlElement{content=[#xmlElement{name='fun', content=C}]}) ->
    [ Name, t_fun(C) ].

signature(Es, Name) ->
    [ "`", Name, "(", seq(fun arg/1, Es), ") -> any()`" ].

arg(#xmlElement{content=Es}) ->
    [get_text(argName, Es)].

get_text(Name, Es) ->
    case get_content(Name, Es) of
	[#xmlText{value=Text}] -> Text;
	[] -> ""
    end.

params(Es) ->
    As = [
	  {get_text(argName, Es1), get_content(fullDescription, get_content(description, Es1))}
	  || #xmlElement{content=Es1} <- Es],
    As1 = [A || A <- As, element(2, A) /= []],
    case As1 of
	[] -> [];
	_ -> [ ["`", A, "`: ", D, ?BR ] || {A, D} <- As1 ]
    end.

returns(Es) ->
    case get_content(fullDescription, get_content(description, Es)) of
	[] -> [];
	D -> [ "returns: ",  D]
    end.

throws(Es, Opts) ->
    case get_content(throws, Es) of
	[] -> [];
	Es1 ->
            %% Doesn't use format_type; keep it short!
	    [
	     "throws ", "`", t_utype(get_elem(type, Es1)), "`",
	     local_defs(get_elem(localdef, Es1), Opts), ?BR
	    ]
    end.

timestamp() ->
    [ ?BR,
      io_lib:format("Generated by EDoc (mkdocs layout), ~s, ~s.", 
		    [edoc_lib:datestr(date()),
		     edoc_lib:timestr(time())]),
      ?BR
    ].

trim(S) -> 
    trim(lists:concat(S), true).

trim([], _R) ->             [];
trim([ $\s | Tail ], R) -> trim(Tail, R);
trim([ $\n | Tail ], R) -> trim(Tail, R);
trim(S, true) ->           trim(lists:reverse(S), false);
trim(S, false) ->          lists:reverse(S).
