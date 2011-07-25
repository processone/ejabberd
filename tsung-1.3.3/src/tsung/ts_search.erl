%%%  This program is free software; you can redistribute it and/or modify
%%%  it under the terms of the GNU General Public License as published by
%%%  the Free Software Foundation; either version 2 of the License, or
%%%  (at your option) any later version.
%%%
%%%  This program is distributed in the hope that it will be useful,
%%%  but WITHOUT ANY WARRANTY; without even the implied warranty of
%%%  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%%  GNU General Public License for more details.
%%%
%%%  You should have received a copy of the GNU General Public License
%%%  along with this program; if not, write to the Free Software
%%%  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307, USA.
%%%

%%%  In addition, as a special exception, you have the permission to
%%%  link the code of this program with any library released under
%%%  the EPL license and distribute linked combinations including
%%%  the two.

%%% File    : ts_search.erl
%%% Author  : Mickael Remond <mickael.remond@erlang-fr.org>
%%% Description : Add dynamic / Differenciated parameters in tsung
%%%               request and response
%%%               The function subst is intended to be called for each
%%%               relevant field in ts_protocol implementation.
%%% Created : 22 Mar 2004 by Mickael Remond <mickael.remond@erlang-fr.org>

%%% Nicolas Niclausse: add dynamic variable and matching

-module(ts_search).
-vc('$Id$ ').

-export([subst/2, match/4, parse_dynvar/2]).

-include("ts_profile.hrl").

%% ----------------------------------------------------------------------
%% Function: subst/2
%% Purpose: search into a given string and replace %%Mod:Fun%% strings
%%          by the result of the call to Mod:Fun(Pid) where Pid is the
%%          Pid of the client The substitution tag are intended to
%%          be used in tsung.xml scenarii files.
%% Returns: new string
%% ----------------------------------------------------------------------
subst(Int, _DynVar) when is_integer(Int) ->
    Int;
subst(Atom, _DynVar) when is_atom(Atom) ->
    Atom;
subst(Binary, DynVar) when is_binary(Binary) ->
    list_to_binary(subst(binary_to_list(Binary), DynVar));
subst(String, DynVar) ->
    subst(String, DynVar, []).

subst([], _DynVar, Acc) ->
    lists:reverse(Acc);
subst([$%,$%,$_|Rest], DynVar, Acc) ->
    extract_variable(Rest, DynVar, Acc, []);
subst([$%,$%|Rest], DynVar, Acc) ->
    extract_module(Rest,  DynVar, Acc, []);
subst([H|Tail], DynVar, Acc) ->
    subst(Tail, DynVar, [H|Acc]).

%% Search for the module string in the subst markup
extract_module([],_DynVar, Acc,_) ->
    lists:reverse(Acc);
extract_module([$:|Tail],DynVar, Acc, Mod) ->
    ?DebugF("found module name: ~p~n",[lists:reverse(Mod)]),
    extract_function(Tail,DynVar, Acc,lists:reverse(Mod),[]);
extract_module([H|Tail],DynVar, Acc, Mod) ->
    extract_module(Tail,DynVar, Acc,[H|Mod]).

%% Search for the module string in the subst markup
extract_variable([],_DynVar,Acc,_) ->
    lists:reverse(Acc);
extract_variable([$%,$%|Tail], DynVar, Acc, Var) ->
    VarName = list_to_atom(lists:reverse(Var)),
    case ts_dynvars:lookup(VarName,DynVar) of
        {ok, ResultTmp} ->
            Result=ts_utils:term_to_list(ResultTmp),
            ?DebugF("found value ~p for name ~p~n",[Result,VarName]),
            subst(Tail, DynVar,lists:reverse(Result) ++ Acc);
        false ->
            ?LOGF("DynVar: no value found for var ~p~n",[VarName],?WARN),
            subst(Tail, DynVar,lists:reverse("undefined") ++ Acc)
    end;

extract_variable([H|Tail],DynVar,Acc,Mod) ->
    extract_variable(Tail,DynVar,Acc,[H|Mod]).

%% Search for the function string and do the real substitution before
%% keeping on the parsing
extract_function([], _DynVar, Acc, _Mod, _Fun) ->
    lists:reverse(Acc);
extract_function([$%,$%|Tail], DynVar, Acc, Mod, Fun) ->
    ?DebugF("found function name: ~p~n",[lists:reverse(Fun)]),
    Module = list_to_atom(Mod),
    Function = list_to_atom(lists:reverse(Fun)),
    Result = case Module:Function({self(), DynVar }) of
                 Int when is_integer(Int) ->
                     lists:reverse(integer_to_list(Int));
                 Str when is_list(Str) ->
                     Str;
                 _Val ->
                     ?LOGF("extract fun:bad result ~p~n",[_Val],?WARN),
                     []
             end,
    subst(Tail, DynVar, lists:reverse(Result) ++ Acc);

extract_function([H|Tail],DynVar,  Acc, Mod, Fun) ->
    extract_function(Tail, DynVar, Acc, Mod, [H|Fun]).

%%----------------------------------------------------------------------
%% @spec match(Match::#match{}, Data::binary() | list, {Counts::integer(),
%% Max::integer(), SessionId::integer(),UserId::integer()}, Dynvars::term()
%%  ) ->  Count::integer()
%% @doc search for regexp in Data; send result to ts_mon
%% @end
%%----------------------------------------------------------------------
match([], _Data, {Count, _MaxC, _SessionId, _UserId}, _DynVars) -> Count;
match([Match=#match{'skip_headers'=http}|Tail], Data, Counts,DynVars) when is_binary(Data)->
    %% keep http body only
    case re:run(Data,"\\r\\n\\r\\n(.*)",[{capture,all_but_first,binary},dotall]) of
        {match,[NewData]} ->
            match([Match#match{'skip_headers'=no}|Tail], NewData, Counts, DynVars);
        _ ->
            ?LOGF("Skip http headers failure, data was: ~p ~n",[Data], ?ERR),
            match([Match#match{'skip_headers'=no}|Tail], Data, Counts, DynVars)
        end;

match([Match=#match{'apply_to_content'=undefined}|Tail], Data, Counts,DynVars) when is_binary(Data)->
    ?DebugF("Matching Data size ~p; apply undefined~n",[size(Data)]),
    match([Match|Tail], binary_to_list(Data), Counts, DynVars);
match([Match=#match{'apply_to_content'={Module,Fun}}|Tail], Data, Counts,DynVars) when is_binary(Data)->
    ?DebugF("Matching Data size ~p; apply ~p:~p~n",[size(Data),Module,Fun]),
    NewData = Module:Fun(Data),
    ?DebugF("Match: apply result =~p~n",[NewData]),
    case is_binary(NewData) of
        true ->
            match([Match|Tail], binary_to_list(NewData), Counts, DynVars);
        false->
            match([Match|Tail], NewData, Counts, DynVars)
    end;
match(Match, Data, Counts,DynVars)  when is_list(Data) ->
    match(Match, Data, Counts, [], DynVars).

%% @spec match(Match::#match{}, Data::binary() | list(), Count::tuple(),
%%             Stats::list(), DynVars::term()) -> Count::integer()
match([], _Data, {Count,_, _,_}, Stats, _) ->
    %% all matches done, add stats, and return Count unchanged (continue)
    ts_mon:add(Stats),
    Count;
match([Match=#match{regexp=RawRegExp,subst=Subst, do=Action, 'when'=When}
       |Tail], String,Counts,Stats,DynVars)->
    RegExp  = case Subst of
        true -> subst(RawRegExp, DynVars);
        _    -> RawRegExp
    end,
    ?DebugF("RegExp was ~p and now is ~p after substitution (~p)~n",[RawRegExp,RegExp,Subst]),
    case regexp:first_match(String, RegExp) of
        {When,_, _} ->
            ?LOGF("Ok Match (regexp=~p) do=~p~n",[RegExp,Action], ?INFO),
            setcount(Match, Counts, [{count, match}| Stats]);
        When -> % nomatch
            ?LOGF("Bad Match (regexp=~p) do=~p~n",[RegExp, Action], ?INFO),
            setcount(Match, Counts, [{count, nomatch} | Stats]);
        {match,_, _} -> % match but when=nomatch
            ?LOGF("Ok Match (regexp=~p)~n",[RegExp], ?INFO),
            case Action of
                loop    -> put(loop_count, 0);
                restart -> put(restart_count, 0);
                _       -> ok
            end,
            match(Tail, String, Counts, [{count, match} | Stats],DynVars);
        nomatch -> % nomatch but when=match
            ?LOGF("Bad Match (regexp=~p)~n",[RegExp], ?INFO),
            case Action of
                loop    -> put(loop_count, 0);
                restart -> put(restart_count, 0);
                _       -> ok
            end,
            match(Tail, String, Counts,[{count, nomatch} | Stats],DynVars);
        {error,_Error} ->
            ?LOGF("Error while matching: bad REGEXP (~p)~n", [RegExp], ?ERR),
            match(Tail, String, Counts,[{count, badregexp} | Stats],DynVars)
    end.

%%----------------------------------------------------------------------
%% Func: setcount/3
%% Args:  #match, Counts, Stats
%% Update the request counter after a match:
%%   - if loop is true, we must start again the same request, so add 1 to count
%%   - if restart is true, we must start again the whole session, set count to MaxCount
%%   - if stop is true, set count to 0
%%----------------------------------------------------------------------
setcount(#match{do=continue}, {Count, _MaxC, _SessionId, _UserId}, Stats)->
    ts_mon:add(Stats),
    Count;
setcount(#match{do=log}, {Count, MaxC, SessionId, UserId}, Stats)->
    ts_mon:add_match(Stats,{UserId,SessionId,MaxC-Count}),
    Count;
setcount(#match{do=restart, max_restart=MaxRestart}, {Count, MaxC,SessionId,UserId}, Stats)->
    CurRestart = get(restart_count),
    Ids={UserId,SessionId,MaxC-Count},
    ?LOGF("Restart on (no)match ~p~n",[CurRestart], ?INFO),
    case CurRestart of
        undefined ->
            put(restart_count,1),
            ts_mon:add_match([{count, match_restart} | Stats],Ids),
            MaxC ;
        Val when Val > MaxRestart ->
            ?LOG("Max restart reached, abort ! ~n", ?WARN),
            ts_mon:add_match([{count, match_restart_abort} | Stats],Ids),
            0;
        Val ->
            put(restart_count, Val +1),
            ts_mon:add_match([{count, match_restart} | Stats],Ids),
            MaxC
    end;
setcount(#match{do=loop,loop_back=Back,max_loop=MaxLoop,sleep_loop=Sleep},{Count,_MaxC,_SessionId,_UserId},Stats)->
    CurLoop = get(loop_count),
    ?LOGF("Loop on (no)match ~p~n",[CurLoop], ?INFO),
    ts_mon:add([{count, match_loop} | Stats]),
    case CurLoop of
        undefined ->
            put(loop_count,1),
            timer:sleep(Sleep),
            Count +1 + Back ;
        Val when Val >= MaxLoop ->
            ?LOG("Max Loop reached, abort loop on request! ~n", ?WARN),
            put(loop_count, 0),
            Count;
        Val ->
            put(loop_count, Val +1),
            timer:sleep(Sleep),
            Count + 1 + Back
    end;
setcount(#match{do=abort}, {Count,MaxC,SessionId,UserId}, Stats) ->
    ts_mon:add_match([{count, match_stop} | Stats],{UserId,SessionId,MaxC-Count}),
    0.

%%----------------------------------------------------------------------
%% Func: parse_dynvar/2
%% Args: DynVarSpecs, Data
%% Purpose: look for dynamic variables in Data
%% Returns: DynVars (List)
%%----------------------------------------------------------------------
parse_dynvar([], _Data) -> ts_dynvars:new();
parse_dynvar(DynVarSpecs, Data)  when is_binary(Data) ->
    ?DebugF("Parsing Dyn Variable (specs=~p); data is ~p~n",[DynVarSpecs,Data]),
    parse_dynvar(DynVarSpecs,Data, undefined,undefined,[]);
parse_dynvar(DynVarSpecs, _Data)  ->
    ?LOGF("Error while Parsing dyn Variable(~p)~n",[DynVarSpecs],?WARN),
    ts_dynvars:new().

% parse_dynvar(DynVars,BinaryData,ListData,TreeData,Accum)
%            ListData and TreeData are lazy computed when needed by
%            regexp or xpath variables respectively
parse_dynvar([],_Binary , _String,_Tree, DynVars) -> DynVars;

parse_dynvar(D=[{regexp,_VarName, _RegExp}| _DynVarsSpecs],
                Binary,undefined,Tree, DynVars) ->
    parse_dynvar(D,Binary,binary_to_list(Binary),Tree,DynVars);

parse_dynvar([{regexp,VarName, RegExp}| DynVarsSpecs],
                Binary,String,Tree, DynVars) ->
    case gregexp:groups(String, RegExp) of
        {match,[Value|_]} ->
            ?LOGF("DynVar: Match (~p=~p) ~n",[VarName, Value], ?INFO),
            parse_dynvar(DynVarsSpecs, Binary,String,Tree,
                            ts_dynvars:set(VarName,Value,DynVars));
        nomatch ->
            ?LOGF("Dyn Var: no Match (varname=~p), ~n",[VarName], ?WARN),
            parse_dynvar(DynVarsSpecs, Binary,String,Tree, ts_dynvars:set(VarName,"",DynVars));
        {match, []} ->
            ?LOGF("Dyn Var: empty Match (varname=~p), ~n",[VarName], ?NOTICE),
            parse_dynvar(DynVarsSpecs, Binary,String,Tree, ts_dynvars:set(VarName,"",DynVars))
    end;

parse_dynvar(D=[{xpath,_VarName, _Expr}| _DynVarsSpecs],
                Binary,String,undefined,DynVars) ->
    HTML = extract_body(Binary),
    try mochiweb_html:parse(HTML) of
        Tree ->
            parse_dynvar(D,Binary,String,Tree,DynVars)
    catch
        Type:Exp ->
            ?LOGF("Page couldn't be parsed:(~p:~p) ~n Page:~p~n",
                    [Type,Exp,Binary],?ERR),
            parse_dynvar(D,Binary,String,xpath_error,DynVars)
    end;

parse_dynvar(D=[{jsonpath,_VarName, _Expr}| _DynVarsSpecs],
                Binary,String,undefined,DynVars) ->
    Body = extract_body(Binary),
    try mochijson2:decode(Body) of
        JSON ->
            ?LOGF("JSON decode: ~p~n", [JSON],?DEB),
            parse_dynvar(D,Binary,String,JSON,DynVars)
    catch
        Type:Exp ->
            ?LOGF("JSON couldn't be parsed:(~p:~p) ~n Page:~p~n",
                    [Type,Exp,Binary],?ERR),
            parse_dynvar(D,Binary,String,json_error,DynVars)
    end;

parse_dynvar(D=[{pgsql_expr,_VarName, _Expr}| _DynVarsSpecs],
                Binary,String,undefined,DynVars) ->
    Pairs=ts_pgsql:to_pairs(Binary),
    parse_dynvar(D,Binary,String,Pairs,DynVars);

parse_dynvar([{xpath,VarName,_Expr}|DynVarsSpecs],Binary,String,xpath_error,DynVars)->
    ?LOGF("Couldn't execute XPath: page not parsed (varname=~p)~n",
          [VarName],?ERR),
    parse_dynvar(DynVarsSpecs, Binary,String,xpath_error,DynVars);

parse_dynvar([{jsonpath,VarName,_Expr}|DynVarsSpecs],Binary,String,json_error,DynVars)->
    ?LOGF("Couldn't execute JSONPath: page not parsed (varname=~p)~n",
          [VarName],?ERR),
    parse_dynvar(DynVarsSpecs, Binary,String,json_error,DynVars);

parse_dynvar([{pgsql_expr,VarName,_Expr}|DynVarsSpecs],Binary,String,pgsql_error,DynVars)->
    ?LOGF("Couldn't decode pgsql expr from PGSQL binary (varname=~p)~n", [VarName],?ERR),
    parse_dynvar(DynVarsSpecs, Binary,String,json_error,DynVars);

parse_dynvar([{xpath,VarName, Expr}| DynVarsSpecs],Binary,String,Tree,DynVars)->
    Result = mochiweb_xpath:execute(Expr,Tree),
    Value  = mochiweb_xpath_utils:string_value(Result),
    ListValue = binary_to_list(Value),
    case ListValue of
        [] -> ?LOGF("Dyn Var: no Match (varname=~p), ~n",[VarName],?WARN);
        _  -> ?LOGF("Dyn Var: Match (~p=~p), ~n",[VarName,ListValue],?INFO)
    end,
    parse_dynvar(DynVarsSpecs, Binary,String,Tree,ts_dynvars:set(VarName,ListValue,DynVars));

parse_dynvar([{jsonpath,VarName, Expr}| DynVarsSpecs],Binary,String,JSON,DynVars)->
    Values = ts_utils:jsonpath(Expr,JSON),
    case Values of
        undefined -> ?LOGF("Dyn Var: no Match (varname=~p), ~n",[VarName],?WARN);
        _  -> ?LOGF("Dyn Var: Match (~p=~p), ~n",[VarName,Values],?INFO)
    end,
    parse_dynvar(DynVarsSpecs, Binary,String,JSON,ts_dynvars:set(VarName,Values,DynVars));

parse_dynvar([{pgsql_expr,VarName, Expr}| DynVarsSpecs],Binary,String,PGSQL,DynVars)->
    Values = ts_pgsql:find_pair(Expr,PGSQL),
    case Values of
        undefined -> ?LOGF("Dyn Var: no Match (varname=~p), ~n",[VarName],?WARN);
        _  -> ?LOGF("Dyn Var: Match (~p=~p), ~n",[VarName,Values],?INFO)
    end,
    parse_dynvar(DynVarsSpecs, Binary,String,PGSQL,ts_dynvars:set(VarName,Values,DynVars));

parse_dynvar(Args, _Binary,_String,_Tree, _DynVars) ->
    ?LOGF("Bad args while parsing Dyn Var (~p)~n", [Args], ?ERR),
    [].

extract_body(<<"\r\n\r\n",Rest/binary>>) ->
    Rest;

extract_body(<<_:1/binary,Rest/binary>>) ->
    extract_body(Rest);

extract_body(<<>>) ->
    <<>>.
