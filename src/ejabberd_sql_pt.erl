%%%-------------------------------------------------------------------
%%% File    : ejabberd_sql_pt.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Description : Parse transform for SQL queries
%%% Created : 20 Jan 2016 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2017   ProcessOne
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
%%% You should have received a copy of the GNU General Public License along
%%% with this program; if not, write to the Free Software Foundation, Inc.,
%%% 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
%%%
%%%----------------------------------------------------------------------

-module(ejabberd_sql_pt).

%% API
-export([parse_transform/2]).

-export([parse/2]).

-include("ejabberd_sql_pt.hrl").

-record(state, {loc,
                'query' = [],
                params = [],
                param_pos = 0,
                args = [],
                res = [],
                res_vars = [],
                res_pos = 0}).

-define(QUERY_RECORD, "sql_query").

-define(ESCAPE_RECORD, "sql_escape").
-define(ESCAPE_VAR, "__SQLEscape").

-define(MOD, sql__module_).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function:
%% Description:
%%--------------------------------------------------------------------
parse_transform(AST, _Options) ->
    %io:format("PT: ~p~nOpts: ~p~n", [AST, Options]),
    NewAST = top_transform(AST),
    %io:format("NewPT: ~p~n", [NewAST]),
    NewAST.



%%====================================================================
%% Internal functions
%%====================================================================


transform(Form) ->
    case erl_syntax:type(Form) of
        application ->
            case erl_syntax_lib:analyze_application(Form) of
                {?SQL_MARK, 1} ->
                    case erl_syntax:application_arguments(Form) of
                        [Arg] ->
                            case erl_syntax:type(Arg) of
                                string ->
                                    S = erl_syntax:string_value(Arg),
                                    Pos = erl_syntax:get_pos(Arg),
                                    ParseRes = parse(S, Pos),
                                    set_pos(make_sql_query(ParseRes), Pos);
                                _ ->
                                    throw({error, erl_syntax:get_pos(Form),
                                           "?SQL argument must be "
                                           "a constant string"})
                            end;
                        _ ->
                            throw({error, erl_syntax:get_pos(Form),
                                   "wrong number of ?SQL args"})
                    end;
                {?SQL_UPSERT_MARK, 2} ->
                    case erl_syntax:application_arguments(Form) of
                        [TableArg, FieldsArg] ->
                            case {erl_syntax:type(TableArg),
                                  erl_syntax:is_proper_list(FieldsArg)}of
                                {string, true} ->
                                    Table = erl_syntax:string_value(TableArg),
                                    ParseRes =
                                        parse_upsert(
                                          erl_syntax:list_elements(FieldsArg)),
                                    Pos = erl_syntax:get_pos(Form),
                                    set_pos(
                                      make_sql_upsert(Table, ParseRes, Pos),
                                      Pos);
                                _ ->
                                    throw({error, erl_syntax:get_pos(Form),
                                           "?SQL_UPSERT arguments must be "
                                           "a constant string and a list"})
                            end;
                        _ ->
                            throw({error, erl_syntax:get_pos(Form),
                                   "wrong number of ?SQL_UPSERT args"})
                    end;
                _ ->
                    Form
            end;
        attribute ->
            case erl_syntax:atom_value(erl_syntax:attribute_name(Form)) of
                module ->
                    case erl_syntax:attribute_arguments(Form) of
                        [M | _] ->
                            Module = erl_syntax:atom_value(M),
                            %io:format("module ~p~n", [Module]),
                            put(?MOD, Module),
                            Form;
                        _ ->
                            Form
                    end;
                _ ->
                    Form
            end;
        _ ->
            Form
    end.

top_transform(Forms) when is_list(Forms) ->
    lists:map(
      fun(Form) ->
              try
                  Form2 = erl_syntax_lib:map(
                            fun(Node) ->
                                                %io:format("asd ~p~n", [Node]),
                                    transform(Node)
                            end, Form),
                  Form3 = erl_syntax:revert(Form2),
                  Form3
	      catch
		  throw:{error, Line, Error} ->
		      {error, {Line, erl_parse, Error}}
	      end
      end, Forms).

parse(S, Loc) ->
    parse1(S, [], #state{loc = Loc}).

parse(S, ParamPos, Loc) ->
    parse1(S, [], #state{loc = Loc, param_pos = ParamPos}).

parse1([], Acc, State) ->
    State1 = append_string(lists:reverse(Acc), State),
    State1#state{'query' = lists:reverse(State1#state.'query'),
                 params = lists:reverse(State1#state.params),
                 args = lists:reverse(State1#state.args),
                 res = lists:reverse(State1#state.res),
                 res_vars = lists:reverse(State1#state.res_vars)
                };
parse1([$@, $( | S], Acc, State) ->
    State1 = append_string(lists:reverse(Acc), State),
    {Name, Type, S1, State2} = parse_name(S, State1),
    Var = "__V" ++ integer_to_list(State2#state.res_pos),
    EVar = erl_syntax:variable(Var),
    Convert =
        case Type of
            integer ->
                erl_syntax:application(
                  erl_syntax:atom(binary_to_integer),
                  [EVar]);
            string ->
                EVar;
            boolean ->
                erl_syntax:application(
                  erl_syntax:atom(ejabberd_sql),
                  erl_syntax:atom(to_bool),
                  [EVar])
        end,
    State3 = append_string(Name, State2),
    State4 = State3#state{res_pos = State3#state.res_pos + 1,
                          res = [Convert | State3#state.res],
                          res_vars = [EVar | State3#state.res_vars]},
    parse1(S1, [], State4);
parse1([$%, $( | S], Acc, State) ->
    State1 = append_string(lists:reverse(Acc), State),
    {Name, Type, S1, State2} = parse_name(S, State1),
    Var = State2#state.param_pos,
    Convert =
        erl_syntax:application(
          erl_syntax:record_access(
            erl_syntax:variable(?ESCAPE_VAR),
            erl_syntax:atom(?ESCAPE_RECORD),
            erl_syntax:atom(Type)),
          [erl_syntax:variable(Name)]),
    State3 = State2,
    State4 =
        State3#state{'query' = [{var, Var} | State3#state.'query'],
                     args = [Convert | State3#state.args],
                     params = [Var | State3#state.params],
                     param_pos = State3#state.param_pos + 1},
    parse1(S1, [], State4);
parse1([C | S], Acc, State) ->
    parse1(S, [C | Acc], State).

append_string([], State) ->
    State;
append_string(S, State) ->
    State#state{query = [{str, S} | State#state.query]}.

parse_name(S, State) ->
    parse_name(S, [], 0, State).

parse_name([], _Acc, _Depth, State) ->
    throw({error, State#state.loc,
           "expected ')', found end of string"});
parse_name([$), T | S], Acc, 0, State) ->
    Type =
        case T of
            $d -> integer;
            $s -> string;
            $b -> boolean;
            _ ->
                throw({error, State#state.loc,
                       ["unknown type specifier '", T, "'"]})
        end,
    {lists:reverse(Acc), Type, S, State};
parse_name([$)], _Acc, 0, State) ->
    throw({error, State#state.loc,
           "expected type specifier, found end of string"});
parse_name([$( = C | S], Acc, Depth, State) ->
    parse_name(S, [C | Acc], Depth + 1, State);
parse_name([$) = C | S], Acc, Depth, State) ->
    parse_name(S, [C | Acc], Depth - 1, State);
parse_name([C | S], Acc, Depth, State) ->
    parse_name(S, [C | Acc], Depth, State).


make_var(V) ->
    Var = "__V" ++ integer_to_list(V),
    erl_syntax:variable(Var).


make_sql_query(State) ->
    Hash = erlang:phash2(State#state{loc = undefined}),
    SHash = <<"Q", (integer_to_binary(Hash))/binary>>,
    Query = pack_query(State#state.'query'),
    EQuery =
        lists:map(
          fun({str, S}) ->
                  erl_syntax:binary(
                    [erl_syntax:binary_field(
                       erl_syntax:string(S))]);
             ({var, V}) -> make_var(V)
          end, Query),
    erl_syntax:record_expr(
     erl_syntax:atom(?QUERY_RECORD),
     [erl_syntax:record_field(
       erl_syntax:atom(hash),
        %erl_syntax:abstract(SHash)
        erl_syntax:binary(
          [erl_syntax:binary_field(
             erl_syntax:string(binary_to_list(SHash)))])),
      erl_syntax:record_field(
       erl_syntax:atom(args),
        erl_syntax:fun_expr(
          [erl_syntax:clause(
             [erl_syntax:variable(?ESCAPE_VAR)],
             none,
             [erl_syntax:list(State#state.args)]
            )])),
      erl_syntax:record_field(
       erl_syntax:atom(format_query),
        erl_syntax:fun_expr(
          [erl_syntax:clause(
             [erl_syntax:list(lists:map(fun make_var/1, State#state.params))],
             none,
             [erl_syntax:list(EQuery)]
            )])),
      erl_syntax:record_field(
       erl_syntax:atom(format_res),
        erl_syntax:fun_expr(
          [erl_syntax:clause(
             [erl_syntax:list(State#state.res_vars)],
             none,
             [erl_syntax:tuple(State#state.res)]
            )])),
      erl_syntax:record_field(
       erl_syntax:atom(loc),
        erl_syntax:abstract({get(?MOD), State#state.loc}))
     ]).

pack_query([]) ->
    [];
pack_query([{str, S1}, {str, S2} | Rest]) ->
    pack_query([{str, S1 ++ S2} | Rest]);
pack_query([X | Rest]) ->
    [X | pack_query(Rest)].


parse_upsert(Fields) ->
    {Fs, _} =
        lists:foldr(
          fun(F, {Acc, Param}) ->
                  case erl_syntax:type(F) of
                      string ->
                          V = erl_syntax:string_value(F),
                          {_, _, State} = Res =
                              parse_upsert_field(
                                V, Param, erl_syntax:get_pos(F)),
                          {[Res | Acc], State#state.param_pos};
                      _ ->
                          throw({error, erl_syntax:get_pos(F),
                                 "?SQL_UPSERT field must be "
                                 "a constant string"})
                  end
          end, {[], 0}, Fields),
    %io:format("upsert ~p~n", [{Fields, Fs}]),
    Fs.

%% key | {Update}
parse_upsert_field([$! | S], ParamPos, Loc) ->
    {Name, ParseState} = parse_upsert_field1(S, [], ParamPos, Loc),
    {Name, key, ParseState};
parse_upsert_field([$- | S], ParamPos, Loc) ->
    {Name, ParseState} = parse_upsert_field1(S, [], ParamPos, Loc),
    {Name, {false}, ParseState};
parse_upsert_field(S, ParamPos, Loc) ->
    {Name, ParseState} = parse_upsert_field1(S, [], ParamPos, Loc),
    {Name, {true}, ParseState}.

parse_upsert_field1([], _Acc, _ParamPos, Loc) ->
    throw({error, Loc,
           "?SQL_UPSERT fields must have the "
           "following form: \"[!-]name=value\""});
parse_upsert_field1([$= | S], Acc, ParamPos, Loc) ->
    {lists:reverse(Acc), parse(S, ParamPos, Loc)};
parse_upsert_field1([C | S], Acc, ParamPos, Loc) ->
    parse_upsert_field1(S, [C | Acc], ParamPos, Loc).


make_sql_upsert(Table, ParseRes, Pos) ->
    check_upsert(ParseRes, Pos),
    erl_syntax:fun_expr(
      [erl_syntax:clause(
         [erl_syntax:atom(pgsql), erl_syntax:variable("__Version")],
         [erl_syntax:infix_expr(
            erl_syntax:variable("__Version"),
            erl_syntax:operator('>='),
            erl_syntax:integer(90100))],
         [make_sql_upsert_pgsql901(Table, ParseRes),
          erl_syntax:atom(ok)]),
       erl_syntax:clause(
         [erl_syntax:underscore(), erl_syntax:underscore()],
         none,
         [make_sql_upsert_generic(Table, ParseRes)])
      ]).

make_sql_upsert_generic(Table, ParseRes) ->
    Update = make_sql_query(make_sql_upsert_update(Table, ParseRes)),
    Insert = make_sql_query(make_sql_upsert_insert(Table, ParseRes)),
    InsertBranch =
        erl_syntax:case_expr(
          erl_syntax:application(
            erl_syntax:atom(ejabberd_sql),
            erl_syntax:atom(sql_query_t),
            [Insert]),
          [erl_syntax:clause(
             [erl_syntax:abstract({updated, 1})],
             none,
             [erl_syntax:atom(ok)]),
           erl_syntax:clause(
             [erl_syntax:variable("__UpdateRes")],
             none,
             [erl_syntax:variable("__UpdateRes")])]),
    erl_syntax:case_expr(
      erl_syntax:application(
        erl_syntax:atom(ejabberd_sql),
        erl_syntax:atom(sql_query_t),
        [Update]),
      [erl_syntax:clause(
         [erl_syntax:abstract({updated, 1})],
         none,
         [erl_syntax:atom(ok)]),
       erl_syntax:clause(
         [erl_syntax:underscore()],
         none,
         [InsertBranch])]).

make_sql_upsert_update(Table, ParseRes) ->
    WPairs =
        lists:flatmap(
          fun({_Field, {_}, _ST}) ->
                  [];
             ({Field, key, ST}) ->
                  [ST#state{
                     'query' = [{str, Field}, {str, "="}] ++ ST#state.'query'
                    }]
          end, ParseRes),
    Where = join_states(WPairs, " AND "),
    SPairs =
        lists:flatmap(
          fun({_Field, key, _ST}) ->
                  [];
             ({_Field, {false}, _ST}) ->
                  [];
             ({Field, {true}, ST}) ->
                  [ST#state{
                     'query' = [{str, Field}, {str, "="}] ++ ST#state.'query'
                    }]
          end, ParseRes),
    Set = join_states(SPairs, ", "),
    State =
        concat_states(
          [#state{'query' = [{str, "UPDATE "}, {str, Table}, {str, " SET "}]},
           Set,
           #state{'query' = [{str, " WHERE "}]},
           Where
          ]),
    State.

make_sql_upsert_insert(Table, ParseRes) ->
    Vals =
        lists:map(
          fun({_Field, _, ST}) ->
                  ST
          end, ParseRes),
    Fields =
        lists:map(
          fun({Field, _, _ST}) ->
                  #state{'query' = [{str, Field}]}
          end, ParseRes),
    State =
        concat_states(
          [#state{'query' = [{str, "INSERT INTO "}, {str, Table}, {str, "("}]},
           join_states(Fields, ", "),
           #state{'query' = [{str, ") VALUES ("}]},
           join_states(Vals, ", "),
           #state{'query' = [{str, ")"}]}
          ]),
    State.

make_sql_upsert_pgsql901(Table, ParseRes) ->
    Update = make_sql_upsert_update(Table, ParseRes),
    Vals =
        lists:map(
          fun({_Field, _, ST}) ->
                  ST
          end, ParseRes),
    Fields =
        lists:map(
          fun({Field, _, _ST}) ->
                  #state{'query' = [{str, Field}]}
          end, ParseRes),
    Insert =
        concat_states(
          [#state{'query' = [{str, "INSERT INTO "}, {str, Table}, {str, "("}]},
           join_states(Fields, ", "),
           #state{'query' = [{str, ") SELECT "}]},
           join_states(Vals, ", "),
           #state{'query' = [{str, " WHERE NOT EXISTS (SELECT * FROM upsert)"}]}
          ]),
    State =
        concat_states(
          [#state{'query' = [{str, "WITH upsert AS ("}]},
           Update,
           #state{'query' = [{str, " RETURNING *) "}]},
           Insert
          ]),
    Upsert = make_sql_query(State),
    erl_syntax:application(
      erl_syntax:atom(ejabberd_sql),
      erl_syntax:atom(sql_query_t),
      [Upsert]).


check_upsert(ParseRes, Pos) ->
    Set =
        lists:filter(
          fun({_Field, Match, _ST}) ->
                  Match /= key
          end, ParseRes),
    case Set of
        [] ->
            throw({error, Pos,
                   "No ?SQL_UPSERT fields to set, use INSERT instead"});
        _ ->
            ok
    end,
    ok.


concat_states(States) ->
    lists:foldr(
      fun(ST11, ST2) ->
              ST1 = resolve_vars(ST11, ST2),
              ST1#state{
                'query' = ST1#state.'query' ++ ST2#state.'query',
                params = ST1#state.params ++ ST2#state.params,
                args = ST1#state.args ++ ST2#state.args,
                res = ST1#state.res ++ ST2#state.res,
                res_vars = ST1#state.res_vars ++ ST2#state.res_vars,
                loc = case ST1#state.loc of
                          undefined -> ST2#state.loc;
                          _ -> ST1#state.loc
                      end
               }
      end, #state{}, States).

resolve_vars(ST1, ST2) ->
    Max = lists:max([0 | ST1#state.params ++ ST2#state.params]),
    {Map, _} =
        lists:foldl(
          fun(Var, {Acc, New}) ->
                  case lists:member(Var, ST2#state.params) of
                      true ->
                          {dict:store(Var, New, Acc), New + 1};
                      false ->
                          {Acc, New}
                  end
          end, {dict:new(), Max + 1}, ST1#state.params),
    NewParams =
        lists:map(
          fun(Var) ->
                  case dict:find(Var, Map) of
                      {ok, New} ->
                          New;
                      error ->
                          Var
                  end
          end, ST1#state.params),
    NewQuery =
        lists:map(
          fun({var, Var}) ->
                  case dict:find(Var, Map) of
                      {ok, New} ->
                          {var, New};
                      error ->
                          {var, Var}
                  end;
             (S) -> S
          end, ST1#state.'query'),
    ST1#state{params = NewParams, 'query' = NewQuery}.


join_states([], _Sep) ->
    #state{};
join_states([H | T], Sep) ->
    J = [[H] | [[#state{'query' = [{str, Sep}]}, X] || X <- T]],
    concat_states(lists:append(J)).


set_pos(Tree, Pos) ->
    erl_syntax_lib:map(
      fun(Node) ->
              case erl_syntax:get_pos(Node) of
                  0 -> erl_syntax:set_pos(Node, Pos);
                  _ -> Node
              end
      end, Tree).
