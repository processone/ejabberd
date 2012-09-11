%%%-------------------------------------------------------------------
%%% File    : mod_bosh.erl
%%% Author  : Evgeniy Khramtsov <ekhramtsov@process-one.net>
%%% Purpose : This module acts as a bridge to ejabberd_bosh which implements
%%%           the real stuff, this is to handle the new pluggable architecture
%%%           for extending ejabberd's http service.
%%% Created : 20 Jul 2011 by Evgeniy Khramtsov <ekhramtsov@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2011   ProcessOne
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
%%%-------------------------------------------------------------------
-module(mod_bosh).

-author('steve@zeank.in-berlin.de').

%%-define(ejabberd_debug, true).

-behaviour(gen_mod).

-export([start/2, stop/1, process/2, open_session/2,
	 close_session/1, find_session/1, node_up/1, node_down/1,
	 migrate/3]).

-include("ejabberd.hrl").

-include("jlib.hrl").

-include("ejabberd_http.hrl").

-include("bosh.hrl").

-record(bosh, {sid = <<"">> :: binary() | '$1',
               pid = self() :: pid() | '$2'}).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------

process([], #request{method = 'POST', data = <<>>}) ->
    ?DEBUG("Bad Request: no data", []),
    {400, ?HEADER(?CT_XML),
     #xmlel{name = <<"h1">>, attrs = [],
	    children = [{xmlcdata, <<"400 Bad Request">>}]}};
process([],
	#request{method = 'POST', data = Data, ip = IP, headers = Hdrs}) ->
    ?DEBUG("Incoming data: ~p", [Data]),
    Type = get_type(Hdrs),
    ejabberd_bosh:process_request(Data, IP, Type);
process([], #request{method = 'GET', data = <<>>}) ->
    {200, ?HEADER(?CT_XML), get_human_html_xmlel()};
process([], #request{method = 'OPTIONS', data = <<>>}) ->
    {200, ?OPTIONS_HEADER, []};
process(_Path, _Request) ->
    ?DEBUG("Bad Request: ~p", [_Request]),
    {400, ?HEADER(?CT_XML),
     #xmlel{name = <<"h1">>, attrs = [],
	    children = [{xmlcdata, <<"400 Bad Request">>}]}}.

get_human_html_xmlel() ->
    Heading = <<"ejabberd ", (jlib:atom_to_binary(?MODULE))/binary>>,
    #xmlel{name = <<"html">>,
	   attrs =
	       [{<<"xmlns">>, <<"http://www.w3.org/1999/xhtml">>}],
	   children =
	       [#xmlel{name = <<"head">>, attrs = [],
		       children =
			   [#xmlel{name = <<"title">>, attrs = [],
				   children = [{xmlcdata, Heading}]}]},
		#xmlel{name = <<"body">>, attrs = [],
		       children =
			   [#xmlel{name = <<"h1">>, attrs = [],
				   children = [{xmlcdata, Heading}]},
			    #xmlel{name = <<"p">>, attrs = [],
				   children =
				       [{xmlcdata, <<"An implementation of ">>},
					#xmlel{name = <<"a">>,
					       attrs =
						   [{<<"href">>,
						     <<"http://xmpp.org/extensions/xep-0206.html">>}],
					       children =
						   [{xmlcdata,
						     <<"XMPP over BOSH (XEP-0206)">>}]}]},
			    #xmlel{name = <<"p">>, attrs = [],
				   children =
				       [{xmlcdata,
					 <<"This web page is only informative. To "
					   "use HTTP-Bind you need a Jabber/XMPP "
					   "client that supports it.">>}]}]}]}.

open_session(SID, Pid) ->
    mnesia:dirty_write(#bosh{sid = SID, pid = Pid}).

close_session(SID) -> mnesia:dirty_delete(bosh, SID).

find_session(SID) ->
    Node = ejabberd_cluster:get_node(SID),
    case rpc:call(Node, mnesia, dirty_read, [bosh, SID],
		  5000)
	of
      [#bosh{pid = Pid}] -> {ok, Pid};
      _ -> error
    end.

migrate(_Node, _UpOrDown, After) ->
    Rs = mnesia:dirty_select(bosh,
			     [{#bosh{sid = '$1', pid = '$2', _ = '_'}, [],
			       ['$$']}]),
    lists:foreach(fun ([SID, Pid]) ->
			  case ejabberd_cluster:get_node(SID) of
			    Node when Node /= node() ->
				ejabberd_bosh:migrate(Pid, Node,
						      random:uniform(After));
			    _ -> ok
			  end
		  end,
		  Rs).

node_up(_Node) ->
    copy_entries(mnesia:dirty_first(bosh)).

node_down(Node) when Node == node() ->
    copy_entries(mnesia:dirty_first(bosh));
node_down(_) -> ok.

copy_entries('$end_of_table') -> ok;
copy_entries(Key) ->
    case mnesia:dirty_read(bosh, Key) of
      [#bosh{sid = SID} = Entry] ->
	  case ejabberd_cluster:get_node_new(SID) of
	    Node when node() /= Node ->
		rpc:cast(Node, mnesia, dirty_write, [Entry]);
	    _ -> ok
	  end;
      _ -> ok
    end,
    copy_entries(mnesia:dirty_next(bosh, Key)).

start(Host, Opts) ->
    start_hook_handler(),
    setup_database(),
    start_jiffy(Opts),
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    ChildSpec = {Proc,
		 {ejabberd_tmp_sup, start_link, [Proc, ejabberd_bosh]},
		 permanent, infinity, supervisor, [ejabberd_tmp_sup]},
    supervisor:start_child(ejabberd_sup, ChildSpec).

stop(Host) ->
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    supervisor:terminate_child(ejabberd_sup, Proc),
    supervisor:delete_child(ejabberd_sup, Proc).

setup_database() ->
    mnesia:create_table(bosh,
			[{ram_copies, [node()]}, {local_content, true},
			 {attributes, record_info(fields, bosh)}]),
    mnesia:add_table_copy(bosh, node(), ram_copies).

start_jiffy(Opts) ->
    case gen_mod:get_opt(json, Opts,
                         fun(false) -> false;
                            (true) -> true
                         end, false) of
        false ->
            ok;
        true ->
            case application:start(jiffy) of
                ok ->
                    ok;
                {error, {already_started, _}} ->
                    ok;
                Err ->
                    ?WARNING_MSG("Failed to start JSON codec (jiffy): ~p. "
                                 "JSON support will be disabled", [Err])
            end
    end.

start_hook_handler() -> spawn(fun hook_handler/0).

hook_handler() ->
    case catch register(ejabberd_bosh_hook_handler, self())
	of
      true ->
	  ejabberd_hooks:add(node_up, ?MODULE, node_up, 100),
	  ejabberd_hooks:add(node_down, ?MODULE, node_down, 100),
	  ejabberd_hooks:add(node_hash_update, ?MODULE, migrate,
			     100),
	  MRef = erlang:monitor(process, ejabberd_sup),
	  hook_handler_loop(MRef);
      _ -> ok
    end.

hook_handler_loop(MRef) ->
    receive
      {'DOWN', MRef, _Type, _Object, _Info} ->
	  catch ejabberd_hooks:delete(node_up, ?MODULE, node_up,
				      100),
	  catch ejabberd_hooks:delete(node_down, ?MODULE,
				      node_down, 100),
	  catch ejabberd_hooks:delete(node_hash_update, ?MODULE,
				      migrate, 100),
	  ok;
      _ -> hook_handler_loop(MRef)
    end.

get_type(Hdrs) ->
    try
        {_, S} = lists:keyfind('Content-Type', 1, Hdrs),
        [T|_] = str:tokens(S, <<";">>),
        [_, <<"json">>] = str:tokens(T, <<"/">>),
        json
    catch _:_ ->
            xml
    end.
