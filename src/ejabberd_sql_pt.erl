%%%-------------------------------------------------------------------
%%% File    : ejabberd_sql_pt.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Description : Parse transform for SQL queries
%%%
%%% Created : 20 Jan 2016 by Alexey Shchepin <alexey@process-one.net>
%%%-------------------------------------------------------------------
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
                                    ParseRes =
                                        parse(S, erl_syntax:get_pos(Arg)),
                                    make_sql_query(ParseRes);
                                _ ->
                                    throw({error, erl_syntax:get_pos(Form),
                                           "?SQL argument must be "
                                           "a constant string"})
                            end;
                        _ ->
                            throw({error, erl_syntax:get_pos(Form),
                                   "wrong number of ?SQL args"})
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
                  erl_syntax:atom(ejabberd_odbc),
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
    Var = "__V" ++ integer_to_list(State2#state.param_pos),
    EVar = erl_syntax:variable(Var),
    Convert =
        erl_syntax:application(
          erl_syntax:record_access(
            erl_syntax:variable(?ESCAPE_VAR),
            erl_syntax:atom(?ESCAPE_RECORD),
            erl_syntax:atom(Type)),
          [erl_syntax:variable(Name)]),
    State3 = State2,
    State4 =
        State3#state{'query' = [{var, EVar} | State3#state.'query'],
                     args = [Convert | State3#state.args],
                     params = [EVar | State3#state.params],
                     param_pos = State3#state.param_pos + 1},
    parse1(S1, [], State4);
parse1([C | S], Acc, State) ->
    parse1(S, [C | Acc], State).

append_string([], State) ->
    State;
append_string(S, State) ->
    State#state{query = [{str, S} | State#state.query]}.

parse_name(S, State) ->
    parse_name(S, [], State).

parse_name([], Acc, State) ->
    throw({error, State#state.loc,
           "expected ')', found end of string"});
parse_name([$), T | S], Acc, State) ->
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
parse_name([$)], Acc, State) ->
    throw({error, State#state.loc,
           "expected type specifier, found end of string"});
parse_name([C | S], Acc, State) ->
    parse_name(S, [C | Acc], State).


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
             ({var, V}) -> V
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
             [erl_syntax:list(State#state.params)],
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

