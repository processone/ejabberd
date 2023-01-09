%%%----------------------------------------------------------------------
%%% File    : mod_stats.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Basic statistics.
%%% Created : 11 Jan 2003 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2023   ProcessOne
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

-module(mod_stats).

-author('alexey@process-one.net').

-protocol({xep, 39, '0.6.0'}).

-behaviour(gen_mod).

-export([start/2, stop/1, reload/3, process_iq/1,
	 mod_options/1, depends/2, mod_doc/0]).

-include("logger.hrl").
-include_lib("xmpp/include/xmpp.hrl").
-include("translate.hrl").

start(Host, _Opts) ->
    gen_iq_handler:add_iq_handler(ejabberd_local, Host, ?NS_STATS,
				  ?MODULE, process_iq).

stop(Host) ->
    gen_iq_handler:remove_iq_handler(ejabberd_local, Host, ?NS_STATS).

reload(Host, NewOpts, _OldOpts) ->
    start(Host, NewOpts).

depends(_Host, _Opts) ->
    [].

process_iq(#iq{type = set, lang = Lang} = IQ) ->
    Txt = ?T("Value 'set' of 'type' attribute is not allowed"),
    xmpp:make_error(IQ, xmpp:err_not_allowed(Txt, Lang));
process_iq(#iq{type = get, to = To, lang = Lang,
	       sub_els = [#stats{} = Stats]} = IQ) ->
    Node = str:tokens(Stats#stats.node, <<"/">>),
    Names = [Name || #stat{name = Name} <- Stats#stats.list],
    case get_local_stats(To#jid.server, Node, Names, Lang) of
	{result, List} ->
	    xmpp:make_iq_result(IQ, Stats#stats{list = List});
	{error, Error} ->
	    xmpp:make_error(IQ, Error)
    end.

-define(STAT(Name), #stat{name = Name}).

get_local_stats(_Server, [], [], _Lang) ->
    {result,
     [?STAT(<<"users/online">>), ?STAT(<<"users/total">>),
      ?STAT(<<"users/all-hosts/online">>),
      ?STAT(<<"users/all-hosts/total">>)]};
get_local_stats(Server, [], Names, _Lang) ->
    {result,
     lists:map(fun (Name) -> get_local_stat(Server, [], Name)
	       end,
	       Names)};
get_local_stats(_Server, [<<"running nodes">>, _],
		[], _Lang) ->
    {result,
     [?STAT(<<"time/uptime">>), ?STAT(<<"time/cputime">>),
      ?STAT(<<"users/online">>),
      ?STAT(<<"transactions/committed">>),
      ?STAT(<<"transactions/aborted">>),
      ?STAT(<<"transactions/restarted">>),
      ?STAT(<<"transactions/logged">>)]};
get_local_stats(_Server, [<<"running nodes">>, ENode],
		Names, Lang) ->
    case search_running_node(ENode) of
      false ->
	    Txt = ?T("No running node found"),
	    {error, xmpp:err_item_not_found(Txt, Lang)};
      Node ->
	  {result,
	   lists:map(fun (Name) -> get_node_stat(Node, Name) end,
		     Names)}
    end;
get_local_stats(_Server, _, _, Lang) ->
    Txt = ?T("No statistics found for this item"),
    {error, xmpp:err_feature_not_implemented(Txt, Lang)}.

-define(STATVAL(Val, Unit), #stat{name = Name, units = Unit, value = Val}).

-define(STATERR(Code, Desc),
	#stat{name = Name,
	      error = #stat_error{code = Code, reason = Desc}}).

get_local_stat(Server, [], Name)
    when Name == <<"users/online">> ->
    case catch ejabberd_sm:get_vh_session_list(Server) of
      {'EXIT', _Reason} ->
	  ?STATERR(500, <<"Internal Server Error">>);
      Users ->
	  ?STATVAL((integer_to_binary(length(Users))),
		   <<"users">>)
    end;
get_local_stat(Server, [], Name)
    when Name == <<"users/total">> ->
    case catch
	   ejabberd_auth:count_users(Server)
	of
      {'EXIT', _Reason} ->
	  ?STATERR(500, <<"Internal Server Error">>);
      NUsers ->
	  ?STATVAL((integer_to_binary(NUsers)),
		   <<"users">>)
    end;
get_local_stat(_Server, [], Name)
    when Name == <<"users/all-hosts/online">> ->
    Users = ejabberd_sm:connected_users_number(),
    ?STATVAL((integer_to_binary(Users)), <<"users">>);
get_local_stat(_Server, [], Name)
    when Name == <<"users/all-hosts/total">> ->
    NumUsers = lists:foldl(fun (Host, Total) ->
				   ejabberd_auth:count_users(Host)
				     + Total
			   end,
			   0, ejabberd_option:hosts()),
    ?STATVAL((integer_to_binary(NumUsers)),
	     <<"users">>);
get_local_stat(_Server, _, Name) ->
    ?STATERR(404, <<"Not Found">>).

get_node_stat(Node, Name)
    when Name == <<"time/uptime">> ->
    case catch ejabberd_cluster:call(Node, erlang, statistics,
			[wall_clock])
	of
      {badrpc, _Reason} ->
	  ?STATERR(500, <<"Internal Server Error">>);
      CPUTime ->
	    ?STATVAL(str:format("~.3f",	[element(1, CPUTime) / 1000]),
		   <<"seconds">>)
    end;
get_node_stat(Node, Name)
    when Name == <<"time/cputime">> ->
    case catch ejabberd_cluster:call(Node, erlang, statistics, [runtime])
	of
      {badrpc, _Reason} ->
	  ?STATERR(500, <<"Internal Server Error">>);
      RunTime ->
	  ?STATVAL(str:format("~.3f", [element(1, RunTime) / 1000]),
		   <<"seconds">>)
    end;
get_node_stat(Node, Name)
    when Name == <<"users/online">> ->
    case catch ejabberd_cluster:call(Node, ejabberd_sm,
			dirty_get_my_sessions_list, [])
	of
      {badrpc, _Reason} ->
	  ?STATERR(500, <<"Internal Server Error">>);
      Users ->
	  ?STATVAL((integer_to_binary(length(Users))),
		   <<"users">>)
    end;
get_node_stat(Node, Name)
    when Name == <<"transactions/committed">> ->
    case catch ejabberd_cluster:call(Node, mnesia, system_info,
			[transaction_commits])
	of
      {badrpc, _Reason} ->
	  ?STATERR(500, <<"Internal Server Error">>);
      Transactions ->
	  ?STATVAL((integer_to_binary(Transactions)),
		   <<"transactions">>)
    end;
get_node_stat(Node, Name)
    when Name == <<"transactions/aborted">> ->
    case catch ejabberd_cluster:call(Node, mnesia, system_info,
			[transaction_failures])
	of
      {badrpc, _Reason} ->
	  ?STATERR(500, <<"Internal Server Error">>);
      Transactions ->
	  ?STATVAL((integer_to_binary(Transactions)),
		   <<"transactions">>)
    end;
get_node_stat(Node, Name)
    when Name == <<"transactions/restarted">> ->
    case catch ejabberd_cluster:call(Node, mnesia, system_info,
			[transaction_restarts])
	of
      {badrpc, _Reason} ->
	  ?STATERR(500, <<"Internal Server Error">>);
      Transactions ->
	  ?STATVAL((integer_to_binary(Transactions)),
		   <<"transactions">>)
    end;
get_node_stat(Node, Name)
    when Name == <<"transactions/logged">> ->
    case catch ejabberd_cluster:call(Node, mnesia, system_info,
			[transaction_log_writes])
	of
      {badrpc, _Reason} ->
	  ?STATERR(500, <<"Internal Server Error">>);
      Transactions ->
	  ?STATVAL((integer_to_binary(Transactions)),
		   <<"transactions">>)
    end;
get_node_stat(_, Name) ->
    ?STATERR(404, <<"Not Found">>).

search_running_node(SNode) ->
    search_running_node(SNode,
			mnesia:system_info(running_db_nodes)).

search_running_node(_, []) -> false;
search_running_node(SNode, [Node | Nodes]) ->
    case iolist_to_binary(atom_to_list(Node)) of
      SNode -> Node;
      _ -> search_running_node(SNode, Nodes)
    end.

mod_options(_Host) ->
    [].

mod_doc() ->
    #{desc =>
          [?T("This module adds support for "
              "https://xmpp.org/extensions/xep-0039.html"
              "[XEP-0039: Statistics Gathering]. This protocol "
              "allows you to retrieve the following statistics "
              "from your ejabberd server:"), "",
           ?T("- Total number of registered users on the current "
              "virtual host (users/total)."), "",
           ?T("- Total number of registered users on all virtual "
              "hosts (users/all-hosts/total)."), "",
           ?T("- Total number of online users on the current "
              "virtual host (users/online)."), "",
           ?T("- Total number of online users on all virtual "
              "hosts (users/all-hosts/online)."), "",
           ?T("NOTE: The protocol extension is deferred and seems "
              "like even a few clients that were supporting it "
              "are now abandoned. So using this module makes "
              "very little sense.")]}.
