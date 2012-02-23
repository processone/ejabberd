%%%----------------------------------------------------------------------
%%% File    : funrelg.erl
%%% Author  : Badlop <badlop@process-one.net>
%%% Purpose : Function Relation Graph
%%% Created : 3 Apr 2007 by Badlop <badlop@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2012   ProcessOne
%%%
%%% This program is free software; you can redistribute it and/or
%%% modify it under the terms of the GNU General Public License as
%%% published by the Free Software Foundation; either version 2 of the
%%% License, or (at your option) any later version.
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%%% General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with this program; if not, write to the Free Software
%%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
%%% 02111-1307 USA
%%%
%%%----------------------------------------------------------------------

-module(funrelg).

-author('badlop@process-one.net').

-export([dir/1, file/1, g/0, g/1, gc/0, gc/1, d/0]).

-record(mfa, {mod, func, arity}).
-record(arcs, {id, from, to, occ}).

%% The functions that are not in export and don't call anybody are not drawn
%% The calls to this module that use Mod:Func are drawn as external calls

%%-----------------------------
%% Module handlers
%%-----------------------------

dir([SrcDir, OutDir]) ->
    SrcDirAbs = filename:absname(SrcDir),
    OutDirAbs = filename:absname(OutDir),
    Files = get_files([SrcDirAbs]),
    lists:foreach(
      fun(F) ->
	      file(F, OutDirAbs)
      end,
      Files).

file([SrcDir, OutDir]) ->
    SrcFilAbs = filename:absname(SrcDir),
    OutDirAbs = filename:absname(OutDir),
    file(SrcFilAbs, OutDirAbs).

file(SrcFile, OutDir) ->
    case {filename:extension(SrcFile), filename:basename(SrcFile)} of
	%% The file must by *.erl, and the first character must be a-z
	{".erl", [FirstChar | _]} when (FirstChar >= 97) and (FirstChar =< 122) ->
	    make_file(SrcFile, OutDir),
	    d();
	_ ->
	    ok
    end.

get_files([]) ->
    [];
get_files([FHead | FTail]) ->
    case catch file:list_dir(FHead) of
	{ok, Files} ->
	    FilesHead = [filename:join(FHead, FilesN) || FilesN <- Files],
	    get_files(FilesHead ++ FTail);
	{error, enotdir} ->
	    [FHead] ++ get_files(FTail)
    end.

make_file(File, OutDir) ->
    FB = filename:basename(File),
    FileDot = filename:join(OutDir, FB ++ ".dot"),
    Text = gc(File),
    {ok,FO}=file:open(FileDot,[write]),
    try
	io:fwrite(FO,"~s",[Text])
	after
	    file:close(FO)
	end,
    FileSvg = filename:join(OutDir, filename:basename(File, ".erl") ++ ".svg"),
    case os:cmd("dot -Tsvg " ++ FileDot ++ " -o " ++ FileSvg) of
	"" -> ok;
	ShellResult ->
	    io:format("Trying to run 'dot', we got this result:~n  ~s~n"
		      "Remember that you need to have Graphviz 'dot' installed.~n", [ShellResult])
    end,
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

g() ->
    g("a.erl").
g(Filename) ->
    gc(Filename),
    halt().

gc() ->
    gc("a.erl").
gc(Filename) ->
    {ok, File} = epp_dodger:parse_file(Filename),

    ets:new(arcs, [set, public, named_table, {keypos, 2}]),
    ets:new(mfa_conversion, [set, public, named_table, {keypos, 1}]),
    ets:new(counters, [set, public, named_table]),
    ets:insert(counters, {arcs_id, 0}),

    ModuleName = get_module_name(File),
    Exports = lists:usort(get_exports(File)),
    Functions = get_functions(File),
    FunctionsParsed = lists:usort(parse_functions(Functions)), % side effects: stores on ets
    Privates = FunctionsParsed -- Exports,
    %%Externals = get_externals(),

    MFAs = get_func_calls(),

    AppModules1 = os:cmd("ls -1 ../doc/devdoc/*.html | tr \".\" \" \" | tr \"/\" \" \" | awk '{print $3}'"),
    AppModules = [list_to_atom(Str) || Str <- string:tokens(AppModules1, [10])],

    ExternalTypes1 = [mfa_to_externaltype(MFA, ModuleName, AppModules, FunctionsParsed) || MFA <- MFAs],
    ExternalTypes = lists:usort(ExternalTypes1),
    AppExternals = [Function || {Type, Function} <- ExternalTypes, Type == app],
    ExmppExternals = [Function || {Type, Function} <- ExternalTypes, Type == exmpp],
    OTPExternals = [Function || {Type, Function} <- ExternalTypes, Type == other],

    textize(ModuleName, Exports, Privates, AppExternals, ExmppExternals, OTPExternals).

get_arcs_id() ->
    ets:update_counter(counters, arcs_id, 1).

insert_arc_normal(FromFunc, FromArity, ToFunc, ToArity) ->
    From = #mfa{mod = -1, func = FromFunc, arity = FromArity},
    To = #mfa{mod = -1, func = ToFunc, arity = ToArity},
    insert_arc(From, To).

insert_arc_external(FromFunc, FromArity, ToMod, ToFunc, ToArity) ->
    From = #mfa{mod = -1, func = FromFunc, arity = FromArity},
    To = #mfa{mod = ToMod, func = ToFunc, arity = ToArity},
    insert_arc(From, To).

insert_arc(From, To) ->
    Match = #arcs{
      from = From,
      to = To,
      id='$1',
      _='_'},
    Select = [{Match, [], ['$1']}],
    Ids = ets:select(arcs, Select),
    case Ids of
	[] ->
	    ets:insert(arcs,
		       #arcs{
			 id = get_arcs_id(),
			 from = From,
			 to = To,
			 occ = 1
			}
		      );
	[Id] ->
	    [Arc] = ets:lookup(arcs, Id),
	    Arc2 = Arc#arcs{occ = Arc#arcs.occ+1},
	    ets:insert(arcs, Arc2)
    end.

read_arc(Id) ->
    ets:lookup(arcs, Id).

d() ->
    ets:delete(arcs),
    ets:delete(mfa_conversion),
    ets:delete(counters).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Get
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_module_name(File) ->
    Attributes = get_elements(attribute, File),
    get_attribute(module, Attributes).

get_exports(File) ->
    Attributes = get_elements(attribute, File),
    Exports = get_attribute(export, Attributes),
    parse_export(Exports).

get_functions(File) ->
    get_elements(function, File).

get_elements(Type, L) -> get_elements(Type, L, []).
get_elements(_, [], Res) -> Res;
get_elements(Type, [{tree, Type, _, A} | L], Res) ->
    get_elements(Type, L, Res++[A]);
get_elements(Type, [_ | L], Res) ->
    get_elements(Type, L, Res).

get_attribute(_, []) -> [];
get_attribute(Name, [{attribute, {tree, atom, _, Name}, [Al]} | As]) ->
    case Name of
	module ->
	    {tree, atom, _, Mn} = Al,
	    Mn;
	export ->
	    {tree, list, _, {list, Exports, _}} = Al,
	    Exports++get_attribute(Name, As);
	_ -> ok
    end;
get_attribute(Name, [_ | As]) ->
    get_attribute(Name, As).

parse_export(Ex) ->
    lists:foldl(
      fun(Aq, Res) ->
	      {tree, arity_qualifier, _, {arity_qualifier, Atom, Inte}} = Aq,
	      {tree, atom, _, An} = Atom,
	      {tree, integer, _, In} = Inte,
	      Res++[{An, In}]
      end,
      [],
      Ex).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Parse
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

parse_functions(Functions) ->
    lists:map(
      fun(E) ->
	      {function, {_, _, _, Fn}, Clauses} = E,
	      Arity = get_function_arity(Clauses),
	      parse_clauses(Fn, Clauses),
	      {Fn, Arity}
      end,
      Functions).

parse_clauses(Fn, Clauses) ->
    lists:foreach(
      fun(E) ->
	      {tree, clause, L, C} = E,
	      {attr, _Ln, _, _} = L,
	      {clause, Parameters, _, Contents} = C,
	      Fa = length(Parameters),
	      parse_contents({Fn, Fa}, Contents)
      end,
      Clauses).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% OLD
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

parse_contents(F, Contents) ->
    {_, Res} = lists:foldl(
		 fun(C, {Fu, R}) ->
			 {Fu, R++parse_content(F, C)}
		 end,
		 {F, []},
		 Contents),
    Res.

parse_content(F, {tree, application, L, A}) ->
    {attr, _Ln, _, _} = L,
    parse_application(F, A);
parse_content(F, {tree, list, _, {list, [L], Ls}}) ->
    RL = parse_content(F, L),
    RL++parse_content(F, Ls);
parse_content(F, {tree, tuple, _, T}) ->
    parse_contents(F, T);
parse_content(F, {tree, match_expr, _, M}) ->
    {match_expr, _, Value} = M,
    parse_content(F, Value);
parse_content(F, {tree, try_expr, _, T}) ->
    {try_expr, As, Bs, Cs, Ds} = T,
    RAs = parse_contents(F, As),
    RBs = parse_contents(F, Bs),
    RDs = parse_contents(F, Ds),
    RAs++RBs++RDs++parse_contents(F, Cs);
parse_content(F, {tree, block_expr, _, Ts}) ->
    parse_contents(F, Ts);
parse_content(F, {tree, catch_expr, _, C}) ->
    parse_content(F, C);
parse_content(F, {tree, case_expr, _, C}) ->
    {case_expr, Case, Cases} = C,
    R2 = parse_content(F, Case),
    R2++parse_contents(F, Cases);
parse_content(F, {tree, if_expr, _, Cases}) ->
    parse_contents(F, Cases);
parse_content(F, {tree, disjunction, _, Cases}) ->
    parse_contents(F, Cases);
parse_content(F, {tree, conjunction, _, Cases}) ->
    parse_contents(F, Cases);
parse_content(F, {tree, list_comp, _, L}) ->
    {list_comp, A, Bs} = L,
    RA = parse_content(F, A),
    RA++parse_contents(F, Bs);
parse_content(F, {tree, generator, _, G}) ->
    {generator, _, Gen} = G,
    parse_content(F, Gen);
parse_content(F, {tree, clause, _, C}) ->
    {clause, Clause, When, Second} = C,
    R2 = parse_contents(F, Clause),
    R3 = parse_content(F, When),
    R2++R3++parse_contents(F, Second);
parse_content(F, {tree, fun_expr, _, E}) ->
    parse_contents(F, E);
parse_content(F, {tree, record_expr, _, E}) ->
    {record_expr, _, _, Records} = E,
    parse_contents(F, Records);
parse_content(F, {tree, record_field, _, R}) ->
    {record_field, F1, F2} = R,
    R2 = parse_content(F, F1),
    R2++parse_content(F, F2);
parse_content(F, {tree, receive_expr, _, E}) ->
    {receive_expr, E1, _, _} = E,
    parse_contents(F, E1);
parse_content(F, {tree, infix_expr, _, I}) ->
    {infix_expr, _Operator, T1, T2} = I,
    R = parse_content(F, T1),
    R++parse_content(F, T2);
parse_content(_F, {tree, prefix_expr, _, _P}) ->
    [];
parse_content(_F, {tree, class_qualifier, _, _}) -> [];
parse_content(_F, {tree, implicit_fun, _, _}) -> [];
parse_content(_F, {tree, record_access, _, _}) -> [];
parse_content(_F, {tree, record_index_expr, _, _}) -> [];
parse_content(_F, {tree, macro, _, _}) -> [];
parse_content(_F, {tree, binary, _, _}) -> [];
parse_content(_F, {tree, binary_generator, _, _}) -> [];
parse_content(_F, {tree, binary_comp, _, _}) -> [];
parse_content(_F, {integer, _, _I}) -> [];
parse_content(_F, {float, _, _I}) -> [];
parse_content(_F, {string, _, _S}) -> [];
parse_content(_F, {char, _, _S}) -> [];
parse_content(_F, {atom, _, _A}) -> [];
parse_content(_F, {var, _, _V}) -> [];
parse_content(_F, {nil, _}) -> [];
parse_content(_F, none) -> [];
parse_content(_F, C) -> io:format("Unknown content: ~p~n", [C]), [].

parse_application({Fn, Fa}, {application, {atom, _, Name}, Valores}) ->
    Arity = length(Valores),
    insert_arc_normal(Fn, Fa, Name, Arity),
    Ra = [{arc_normal, Fn, Name}],
    Ra++parse_contents({Fn, Fa}, Valores);
parse_application({Fn, Fa}, {application, {tree, module_qualifier, _, M}, Tree2}) ->
    case M of
	{module_qualifier, {_, _, Tf1}, {_, _, Tf2}} ->
	    ToArity = length(Tree2),
	    insert_arc_external(Fn, Fa, Tf1, Tf2, ToArity),
	    parse_contents({Fn, Fa}, Tree2);
	_Other ->
	    %%io:format("Unknown application module_qualifier: ~p~n", [M]),
	    parse_contents({Fn, Fa}, Tree2)
    end;
parse_application({Fn, Fa}, {application, {tree, record_access, _, _}, Tree2}) ->
    parse_contents({Fn, Fa}, Tree2);
parse_application({Fn, Fa}, {application, {var, _, _}, Tree2}) ->
    parse_contents({Fn, Fa}, Tree2);
parse_application({Fn, Fa}, {application, Other, Tree2}) ->
    io:format("Unknown application tree: ~p~n", [Other]),
    parse_contents({Fn, Fa}, Tree2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Textize
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

textize(ModuleName, Exports, Privates, AppExternals, ExmppExternals, OTPExternals) ->
    ExportsS = textize_exports(Exports),
    PrivatesS = textize_exports(Privates),
    AppExternalsS = textize_externals(AppExternals),
    ExmppExternalsS = textize_externals(ExmppExternals),
    OTPExternalsS = textize_externals(OTPExternals),
    Arcs = textize_arcs(),
    io_lib:format(
      "digraph ~p {~n"++
      "  label = \"Module ~p\";~n"++
      "  labelloc = \"t\";~n"++
      "  fontname = Helvetica;~n"++
      "  rankdir = LR;~n"++
      "  ratio = fill;~n"++
      %%"  node [shape = box, style = filled, color = olivedrab1, fontsize=40];~s~n"++
      %%"  node [shape = parallelogram, style = filled, color = rosybrown1, fontsize=40];~s~n"++
      %%"  node [shape = ellipse, style = filled, color = slategray1, peripheries=3, fontsize=40];~n"++
      "  node [shape = box, color=darkgreen, fillcolor=Honeydew,    style=filled, href=\"PRI~p.html#\\N\"];~s~n"++
      "  node [shape = box, color=darkgreen, fillcolor=PaleGreen,   style=filled, href=\"EXP~p.html#\\N\"];~s~n"++
      "  node [shape = box, color=darkgreen, fillcolor=Aquamarine,  style=filled, href=\"APP\\N\"];~s~n"++
      "  node [shape = box, color=darkblue,  fillcolor=LightSkyBlue,style=filled, href=\"EXM\\N\"];~s~n"++
      "  node [shape = box, color=darkred,   fillcolor=PeachPuff,   style=filled, href=\"OTP\\N\"];~s~n"++
      "  node [shape = box];~n"++
      "~s"++
      "}~n",
      [ModuleName, ModuleName,
       ModuleName, PrivatesS,
       ModuleName, ExportsS,
       AppExternalsS,
       ExmppExternalsS,
       OTPExternalsS,
       Arcs]).

textize_exports(Exports) ->
    lists:foldl(
      fun({Func, Arity}, R) ->
	      string:concat(R, io_lib:format(" \"~p/~p\";", [Func, Arity]))
      end,
      "",
      Exports).

textize_externals(Externals) ->
    lists:foldl(
      fun({Mod, Func, Arity}, R) ->
	      string:concat(R, io_lib:format(" \"~p:~p/~p\";", [Mod, Func, Arity]));
         ({Func, Arity}, R) ->
	      string:concat(R, io_lib:format(" \"~p/~p\";", [Func, Arity]))
      end,
      "",
      Externals).

textize_arcs() ->
    Id = ets:first(arcs),
    textize_arcs(Id, []).

textize_arcs('$end_of_table', Res) -> Res;
textize_arcs(Id, Res) ->
    [Arc] = read_arc(Id),
    From = mfa_prepare(Arc#arcs.from),
    To = mfa_prepare(Arc#arcs.to),
    Edge_attrs = check_loop(From, To)++check_occu(Arc#arcs.occ)++check_external(Arc#arcs.to),
    Res2 = io_lib:format("  ~s -> ~s~s;~n", [From, To, Edge_attrs]),
    Id_next = ets:next(arcs, Id),
    textize_arcs(Id_next, string:concat(Res2, Res)).

get_func_calls() ->
    Id = ets:first(arcs),
    get_func_calls(Id, []).

get_func_calls('$end_of_table', Res) -> Res;
get_func_calls(Id, Res) ->
    [Arc] = read_arc(Id),
    MFA = Arc#arcs.to,
    Call = MFA,
    Id_next = ets:next(arcs, Id),
    get_func_calls(Id_next, [Call | Res]).

mfa_prepare(MFAinitial) ->
    MFA = try_mfa_conversion(MFAinitial),
    Mod = case MFA#mfa.mod of
	      -1 -> "\"";
	      M -> io_lib:format("\"~p:", [M])
	  end,
    Func = io_lib:format("~p", [MFA#mfa.func]),
    Arity = case MFA#mfa.arity of
		-1 -> "\"";
		A -> io_lib:format("/~p\"", [A])
	    end,
    string:concat(Mod, string:concat(Func, Arity)).

%% noexternal | app | exmpp | other
mfa_to_externaltype(MFA, ModuleName, AppModules, Functions) ->
    case MFA of
	{mfa, -1, Fu, Ar} ->
	    case {is_internal, lists:member({Fu, Ar}, Functions)} of
		{is_internal, true} ->
		    {noexternal, {Fu, Ar}};
		{is_internal, false} ->
		    store_mfa_conversion(MFA, {mfa, -1, Fu, Ar}),
		    {other, {Fu, Ar}}
	    end;
	{mfa, Mo, Fu, Ar} when Mo == ModuleName ->
	    store_mfa_conversion(MFA, {mfa, -1, Fu, Ar}),
	    {noexternal, {Fu, Ar}};
	{mfa, Mo, Fu, Ar} ->
	    Type = case {is_app, lists:member(Mo, AppModules)} of
		       {is_app, true} -> app;
		       {is_app, false} ->
			   case string:str(atom_to_list(Mo), "exmpp") of
			       0 -> other;
			       N when is_integer(N) -> exmpp
			   end
		   end,
	    {Type, {Mo, Fu, Ar}}
    end.

get_function_arity([{tree, clause, _, {clause, Vars, _, _}} | _]) ->
    length(Vars);
get_function_arity(E) ->
    io:format("Unknown function arity: ~p~n", [E]).

store_mfa_conversion(MFA1, MFA2) ->
    ets:insert(mfa_conversion, {MFA1, MFA2}).

try_mfa_conversion(MFA) ->
    case ets:lookup(mfa_conversion, MFA) of
	[{MFA,MFA2}] ->
	    MFA2;
	[] ->
	    MFA
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Utils
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%check_loop(I, I) -> " [color = sandybrown]";
check_loop(I, I) -> "";
check_loop(_, _) -> "".

check_occu(1) -> "";
check_occu(Occ) -> io_lib:format(" [label=\"~p\"]", [Occ]).

check_external(To) ->
    case To#mfa.mod of
	-1 -> " [style = bold]";
	_ -> ""
    end.
