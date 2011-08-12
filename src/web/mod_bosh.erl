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

-export([
         start/2,
         stop/1,
         process/2,
         open_session/2,
         close_session/1,
         find_session/1,
         node_up/1,
         migrate/1
	]).

-include("ejabberd.hrl").
-include("jlib.hrl").
-include("ejabberd_http.hrl").
-include("bosh.hrl").

-record(bosh, {sid, pid}).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------

process([], #request{method = 'POST',
                     data = []}) ->
    ?DEBUG("Bad Request: no data", []),
    {400, ?HEADER, {xmlelement, "h1", [],
                    [{xmlcdata, "400 Bad Request"}]}};
process([], #request{method = 'POST',
                     data = Data,
                     ip = IP}) ->
    ?DEBUG("Incoming data: ~s", [Data]),
    ejabberd_bosh:process_request(Data, IP);
process([], #request{method = 'GET',
                     data = []}) ->
    {200, ?HEADER, get_human_html_xmlel()};
process([], #request{method = 'OPTIONS',
                     data = []}) ->
    {200, ?OPTIONS_HEADER, []};
process(_Path, _Request) ->
    ?DEBUG("Bad Request: ~p", [_Request]),
    {400, ?HEADER, {xmlelement, "h1", [],
                    [{xmlcdata, "400 Bad Request"}]}}.

get_human_html_xmlel() ->
    Heading = "ejabberd " ++ atom_to_list(?MODULE),
    {xmlelement, "html", [{"xmlns", "http://www.w3.org/1999/xhtml"}],
     [{xmlelement, "head", [],
       [{xmlelement, "title", [], [{xmlcdata, Heading}]}]},
      {xmlelement, "body", [],
       [{xmlelement, "h1", [], [{xmlcdata, Heading}]},
        {xmlelement, "p", [],
         [{xmlcdata, "An implementation of "},
          {xmlelement, "a",
	   [{"href", "http://xmpp.org/extensions/xep-0206.html"}],
           [{xmlcdata, "XMPP over BOSH (XEP-0206)"}]}]},
        {xmlelement, "p", [],
         [{xmlcdata, "This web page is only informative. "
	   "To use HTTP-Bind you need a Jabber/XMPP client that supports it."}
	 ]}
       ]}]}.

open_session(SID, Pid) ->
    mnesia:dirty_write(#bosh{sid = SID, pid = Pid}).

close_session(SID) ->
    mnesia:dirty_delete(bosh, SID).

find_session(SID) ->
    Node = ejabberd_cluster:get_node(SID),
    case rpc:call(Node, mnesia, dirty_read, [bosh, SID], 5000) of
        [#bosh{pid = Pid}] ->
            {ok, Pid};
        _ ->
            error
    end.

migrate(After) ->
    Rs = mnesia:dirty_select(
           bosh,
           [{#bosh{sid = '$1', pid = '$2', _ = '_'},
             [],
             ['$$']}]),
    lists:foreach(
      fun([SID, Pid]) ->
              case ejabberd_cluster:get_node(SID) of
                  Node when Node /= node() ->
                      ejabberd_bosh:migrate(Pid, Node, 1000);%%random:uniform(After));
                  _ ->
                      ok
              end
      end, Rs).

node_up(_Node) ->
    copy_entries(mnesia:dirty_first(bosh)).

copy_entries('$end_of_table') ->
    ok;
copy_entries(Key) ->
    case mnesia:dirty_read(bosh, Key) of
	[#bosh{sid = SID} = Entry] ->
	    case ejabberd_cluster:get_node_new(SID) of
		Node when node() /= Node ->
		    rpc:cast(Node, mnesia, dirty_write, [Entry]);
		_ ->
		    ok
	    end;
	_ ->
	    ok
    end,
    copy_entries(mnesia:dirty_next(bosh, Key)).

%%%----------------------------------------------------------------------
%%% BEHAVIOUR CALLBACKS
%%%----------------------------------------------------------------------
start(Host, _Opts) ->
    start_hook_handler(),
    setup_database(),
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    ChildSpec =
        {Proc,
         {ejabberd_tmp_sup, start_link,
          [Proc, ejabberd_bosh]},
         permanent,
         infinity,
         supervisor,
         [ejabberd_tmp_sup]},
    supervisor:start_child(ejabberd_sup, ChildSpec).

stop(Host) ->
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    supervisor:terminate_child(ejabberd_sup, Proc),
    supervisor:delete_child(ejabberd_sup, Proc).

setup_database() ->
    mnesia:create_table(bosh,
                        [{ram_copies, [node()]},
                         {local_content, true},
                         {attributes, record_info(fields, bosh)}]),
    mnesia:add_table_copy(bosh, node(), ram_copies).

start_hook_handler() ->
    %% HACK: We need this in order to avoid
    %% hooks processing more than once
    %% when many vhosts are involved
    %% TODO: get rid of this stuff
    spawn(fun hook_handler/0).

hook_handler() ->
    case catch register(ejabberd_bosh_hook_handler, self()) of
        true ->
            ejabberd_hooks:add(node_up, ?MODULE, node_up, 100),
            ejabberd_hooks:add(node_hash_update, ?MODULE, migrate, 100),
            %% Stop if ejabberd_sup goes down
            %% (i.e. the whole ejabberd goes down)
            MRef = erlang:monitor(process, ejabberd_sup),
            hook_handler_loop(MRef);
        _ ->
            ok
    end.

hook_handler_loop(MRef) ->
    receive
        {'DOWN', MRef, _Type, _Object, _Info} ->
            %% Unregister the hooks. I think this is useless, thus 'catch'
            catch ejabberd_hooks:delete(node_up, ?MODULE, node_up, 100),
            catch ejabberd_hooks:delete(node_hash_update, ?MODULE, migrate, 100),
            ok;
        _ ->
            hook_handler_loop(MRef)
    end.
