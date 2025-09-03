%%%----------------------------------------------------------------------
%%% File    : nodetree_tree_sql.erl
%%% Author  : Christophe Romain <christophe.romain@process-one.net>
%%% Purpose : Standard node tree plugin with ODBC backend
%%% Created :  1 Dec 2007 by Christophe Romain <christophe.romain@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2025   ProcessOne
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

%%% @doc The module <strong>{@module}</strong> is the default PubSub node tree plugin.
%%% <p>It is used as a default for all unknown PubSub node type.  It can serve
%%% as a developer basis and reference to build its own custom pubsub node tree
%%% types.</p>
%%% <p>PubSub node tree plugins are using the {@link gen_nodetree} behaviour.</p>
%%% <p><strong>The API isn't stabilized yet</strong>. The pubsub plugin
%%% development is still a work in progress. However, the system is already
%%% usable and useful as is. Please, send us comments, feedback and
%%% improvements.</p>

-module(nodetree_tree_sql).
-behaviour(gen_pubsub_nodetree).
-author('christophe.romain@process-one.net').

-include("pubsub.hrl").

-include_lib("xmpp/include/xmpp.hrl").

-include("ejabberd_sql_pt.hrl").
-include("translate.hrl").

-export([init/3,
         terminate/2,
         options/0,
         set_node/1,
         get_node/3, get_node/2, get_node/1,
         get_nodes/2, get_nodes/1,
         get_all_nodes/1,
         get_parentnodes/3,
         get_parentnodes_tree/3,
         get_subnodes/3,
         get_subnodes_tree/3,
         create_node/6,
         delete_node/2]).

-export([raw_to_node/2]).


init(_Host, _ServerHost, _Opts) ->
    ok.


terminate(_Host, _ServerHost) ->
    ok.


options() ->
    [{sql, true} | nodetree_tree:options()].


set_node(Record) when is_record(Record, pubsub_node) ->
    {Host, Node} = Record#pubsub_node.nodeid,
    Parent = case Record#pubsub_node.parents of
                 [] -> <<>>;
                 [First | _] -> First
             end,
    Type = Record#pubsub_node.type,
    H = node_flat_sql:encode_host(Host),
    Nidx = case nodeidx(Host, Node) of
               {result, OldNidx} ->
                   catch ejabberd_sql:sql_query_t(
                           ?SQL("delete from pubsub_node_option "
                                "where nodeid=%(OldNidx)d")),
                   catch ejabberd_sql:sql_query_t(
                           ?SQL("update pubsub_node set"
                                " host=%(H)s, node=%(Node)s,"
                                " parent=%(Parent)s, plugin=%(Type)s "
                                "where nodeid=%(OldNidx)d")),
                   OldNidx;
               {error, not_found} ->
                   catch ejabberd_sql:sql_query_t(
                           ?SQL("insert into pubsub_node(host, node, parent, plugin) "
                                "values(%(H)s, %(Node)s, %(Parent)s, %(Type)s)")),
                   case nodeidx(Host, Node) of
                       {result, NewNidx} -> NewNidx;
                       {error, not_found} -> none;  % this should not happen
                       {error, _} -> db_error
                   end;
               {error, _} ->
                   db_error
           end,
    case Nidx of
        db_error ->
            {error, xmpp:err_internal_server_error(?T("Database failure"), ejabberd_option:language())};
        none ->
            Txt = ?T("Node index not found"),
            {error, xmpp:err_internal_server_error(Txt, ejabberd_option:language())};
        _ ->
            lists:foreach(fun({Key, Value}) ->
                                  SKey = iolist_to_binary(atom_to_list(Key)),
                                  SValue = misc:term_to_expr(Value),
                                  catch ejabberd_sql:sql_query_t(
                                          ?SQL("insert into pubsub_node_option(nodeid, name, val) "
                                               "values (%(Nidx)d, %(SKey)s, %(SValue)s)"))
                          end,
                          Record#pubsub_node.options),
            {result, Nidx}
    end.


get_node(Host, Node, _From) ->
    get_node(Host, Node).


get_node(Host, Node) ->
    H = node_flat_sql:encode_host(Host),
    case catch ejabberd_sql:sql_query_t(
                 ?SQL("select @(node)s, @(parent)s, @(plugin)s, @(nodeid)d from pubsub_node "
                      "where host=%(H)s and node=%(Node)s")) of
        {selected, [RItem]} ->
            raw_to_node(Host, RItem);
        {'EXIT', _Reason} ->
            {error, xmpp:err_internal_server_error(?T("Database failure"), ejabberd_option:language())};
        _ ->
            {error, xmpp:err_item_not_found(?T("Node not found"), ejabberd_option:language())}
    end.


get_node(Nidx) ->
    case catch ejabberd_sql:sql_query_t(
                 ?SQL("select @(host)s, @(node)s, @(parent)s, @(plugin)s from pubsub_node "
                      "where nodeid=%(Nidx)d")) of
        {selected, [{Host, Node, Parent, Type}]} ->
            raw_to_node(Host, {Node, Parent, Type, Nidx});
        {'EXIT', _Reason} ->
            {error, xmpp:err_internal_server_error(?T("Database failure"), ejabberd_option:language())};
        _ ->
            {error, xmpp:err_item_not_found(?T("Node not found"), ejabberd_option:language())}
    end.


get_nodes(Host) ->
    get_nodes(Host, infinity).


get_nodes(Host, Limit) ->
    H = node_flat_sql:encode_host(Host),
    Query = fun(mssql, _) when is_integer(Limit), Limit >= 0 ->
                    ejabberd_sql:sql_query_t(
                      ?SQL("select top %(Limit)d @(node)s, @(parent)s, @(plugin)s, @(nodeid)d "
                           "from pubsub_node where host=%(H)s"));
               (_, _) when is_integer(Limit), Limit >= 0 ->
                    ejabberd_sql:sql_query_t(
                      ?SQL("select @(node)s, @(parent)s, @(plugin)s, @(nodeid)d "
                           "from pubsub_node where host=%(H)s limit %(Limit)d"));
               (_, _) ->
                    ejabberd_sql:sql_query_t(
                      ?SQL("select @(node)s, @(parent)s, @(plugin)s, @(nodeid)d "
                           "from pubsub_node where host=%(H)s"))
            end,
    case ejabberd_sql:sql_query_t(Query) of
        {selected, RItems} ->
            [ raw_to_node(Host, Item) || Item <- RItems ];
        _ ->
            []
    end.


get_all_nodes({_U, _S, _R} = JID) ->
    SubKey = jid:tolower(JID),
    GenKey = jid:remove_resource(SubKey),
    EncKey = node_flat_sql:encode_jid(GenKey),
    Pattern = <<(node_flat_sql:encode_jid_like(GenKey))/binary, "/%">>,
    case ejabberd_sql:sql_query_t(
           ?SQL("select @(node)s, @(parent)s, @(plugin)s, @(nodeid)d "
                "from pubsub_node where host=%(EncKey)s "
                "or host like %(Pattern)s %ESCAPE")) of
        {selected, RItems} ->
            [ raw_to_node(GenKey, Item) || Item <- RItems ];
        _ ->
            []
    end;
get_all_nodes(Host) ->
    Pattern1 = <<"%@", Host/binary>>,
    Pattern2 = <<"%@", Host/binary, "/%">>,
    case ejabberd_sql:sql_query_t(
           ?SQL("select @(node)s, @(parent)s, @(plugin)s, @(nodeid)d "
                "from pubsub_node where host=%(Host)s "
                "or host like %(Pattern1)s "
                "or host like %(Pattern2)s %ESCAPE")) of
        {selected, RItems} ->
            [ raw_to_node(Host, Item) || Item <- RItems ];
        _ ->
            []
    end.


get_parentnodes(Host, Node, _From) ->
    case get_node(Host, Node) of
        Record when is_record(Record, pubsub_node) ->
            Record#pubsub_node.parents;
        _ ->
            []
    end.


get_parentnodes_tree(Host, Node, _From) ->
    get_parentnodes_tree(Host, Node, 0, []).


get_parentnodes_tree(Host, Node, Level, Acc) ->
    case get_node(Host, Node) of
        Record when is_record(Record, pubsub_node) ->
            Tree = [{Level, [Record]} | Acc],
            case Record#pubsub_node.parents of
                [Parent] -> get_parentnodes_tree(Host, Parent, Level + 1, Tree);
                _ -> Tree
            end;
        _ ->
            Acc
    end.


get_subnodes(Host, Node, Limit) ->
    H = node_flat_sql:encode_host(Host),
    Query = fun(mssql, _) when is_integer(Limit), Limit >= 0 ->
                    ejabberd_sql:sql_query_t(
                      ?SQL("select top %(Limit)d @(node)s, @(parent)s, @(plugin)s, @(nodeid)d "
                           "from pubsub_node where host=%(H)s and parent=%(Node)s"));
               (_, _) when is_integer(Limit), Limit >= 0 ->
                    ejabberd_sql:sql_query_t(
                      ?SQL("select @(node)s, @(parent)s, @(plugin)s, @(nodeid)d "
                           "from pubsub_node where host=%(H)s and parent=%(Node)s "
                           "limit %(Limit)d"));
               (_, _) ->
                    ejabberd_sql:sql_query_t(
                      ?SQL("select @(node)s, @(parent)s, @(plugin)s, @(nodeid)d "
                           "from pubsub_node where host=%(H)s and parent=%(Node)s"))
            end,
    case ejabberd_sql:sql_query_t(Query) of
        {selected, RItems} ->
            [ raw_to_node(Host, Item) || Item <- RItems ];
        _ ->
            []
    end.


get_subnodes_tree(Host, Node, _From) ->
    get_subnodes_tree(Host, Node).


get_subnodes_tree(Host, Node) ->
    case get_node(Host, Node) of
        {error, _} ->
            [];
        Rec ->
            Type = Rec#pubsub_node.type,
            H = node_flat_sql:encode_host(Host),
            N = <<(ejabberd_sql:escape_like_arg(Node))/binary, "/%">>,
            Sub = case catch ejabberd_sql:sql_query_t(
                               ?SQL("select @(node)s, @(parent)s, @(plugin)s, @(nodeid)d from pubsub_node "
                                    "where host=%(H)s and plugin=%(Type)s and"
                                    " (parent=%(Node)s or parent like %(N)s %ESCAPE)")) of
                      {selected, RItems} ->
                          [ raw_to_node(Host, Item) || Item <- RItems ];
                      _ ->
                          []
                  end,
            [Rec | Sub]
    end.


create_node(Host, Node, Type, Owner, Options, Parents) ->
    BJID = jid:tolower(jid:remove_resource(Owner)),
    case nodeidx(Host, Node) of
        {error, not_found} ->
            ParentExists = case Host of
                               {_U, _S, _R} ->
                                   %% This is special case for PEP handling
                                   %% PEP does not uses hierarchy
                                   true;
                               _ ->
                                   case Parents of
                                       [] ->
                                           true;
                                       [Parent | _] ->
                                           case nodeidx(Host, Parent) of
                                               {result, PNode} ->
                                                   case nodeowners(PNode) of
                                                       [{<<>>, Host, <<>>}] -> true;
                                                       Owners -> lists:member(BJID, Owners)
                                                   end;
                                               _ ->
                                                   false
                                           end;
                                       _ ->
                                           false
                                   end
                           end,
            case ParentExists of
                true ->
                    case set_node(#pubsub_node{
                                    nodeid = {Host, Node},
                                    parents = Parents,
                                    type = Type,
                                    options = Options
                                   }) of
                        {result, Nidx} -> {ok, Nidx};
                        Other -> Other
                    end;
                false ->
                    {error, xmpp:err_forbidden()}
            end;
        {result, _} ->
            {error, xmpp:err_conflict(?T("Node already exists"), ejabberd_option:language())};
        {error, db_fail} ->
            {error, xmpp:err_internal_server_error(?T("Database failure"), ejabberd_option:language())}
    end.


delete_node(Host, Node) ->
    lists:map(
      fun(Rec) ->
              Nidx = Rec#pubsub_node.id,
              catch ejabberd_sql:sql_query_t(
                      ?SQL("delete from pubsub_node where nodeid=%(Nidx)d")),
              Rec
      end,
      get_subnodes_tree(Host, Node)).


%% helpers
raw_to_node(Host, [Node, Parent, Type, Nidx]) ->
    raw_to_node(Host, {Node, Parent, Type, binary_to_integer(Nidx)});
raw_to_node(Host, {Node, Parent, Type, Nidx}) ->
    Options = case catch ejabberd_sql:sql_query_t(
                           ?SQL("select @(name)s, @(val)s from pubsub_node_option "
                                "where nodeid=%(Nidx)d")) of
                  {selected, ROptions} ->
                      DbOpts = lists:map(
                                 fun({<<"max_items">>, <<"infinity">>}) ->
                                         {max_items, max};
                                    ({Key, Value}) ->
                                         RKey = misc:binary_to_atom(Key),
                                         Tokens = element(2, erl_scan:string(binary_to_list(<<Value/binary, ".">>))),
                                         RValue = element(2, erl_parse:parse_term(Tokens)),
                                         {RKey, RValue}
                                 end,
                                 ROptions),
                      Module = misc:binary_to_atom(<<"node_", Type/binary, "_sql">>),
                      StdOpts = Module:options(),
                      lists:foldl(fun({Key, Value}, Acc) ->
                                          lists:keystore(Key, 1, Acc, {Key, Value})
                                  end,
                                  StdOpts,
                                  DbOpts);
                  _ ->
                      []
              end,
    Parents = case Parent of
                  <<>> -> [];
                  _ -> [Parent]
              end,
    #pubsub_node{
      nodeid = {Host, Node},
      id = Nidx,
      parents = Parents,
      type = Type,
      options = Options
     }.


nodeidx(Host, Node) ->
    H = node_flat_sql:encode_host(Host),
    case catch ejabberd_sql:sql_query_t(
                 ?SQL("select @(nodeid)d from pubsub_node "
                      "where host=%(H)s and node=%(Node)s")) of
        {selected, [{Nidx}]} ->
            {result, Nidx};
        {'EXIT', _Reason} ->
            {error, db_fail};
        _ ->
            {error, not_found}
    end.


nodeowners(Nidx) ->
    {result, Res} = node_flat_sql:get_node_affiliations(Nidx),
    [ LJID || {LJID, Aff} <- Res, Aff =:= owner ].
