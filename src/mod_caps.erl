%%%----------------------------------------------------------------------
%%% File    : mod_caps.erl
%%% Author  : Magnus Henoch <henoch@dtek.chalmers.se>
%%% Purpose : Request and cache Entity Capabilities (XEP-0115)
%%% Created : 7 Oct 2006 by Magnus Henoch <henoch@dtek.chalmers.se>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2010   ProcessOne
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
%%% 2009, improvements from ProcessOne to support correct PEP handling
%%% through s2s, use less memory, and speedup global caps handling
%%%----------------------------------------------------------------------

-module(mod_caps).
-author('henoch@dtek.chalmers.se').

-behaviour(gen_server).
-behaviour(gen_mod).

-export([read_caps/1,
	 get_features/1]).

%% gen_mod callbacks
-export([start/2, start_link/2,
	 stop/1]).

%% gen_server callbacks
-export([init/1,
	 handle_info/2,
	 handle_call/3,
	 handle_cast/2,
	 terminate/2,
	 code_change/3
	]).

%% hook handlers
-export([user_send_packet/3]).

-include("ejabberd.hrl").
-include("jlib.hrl").

-define(PROCNAME, ejabberd_mod_caps).

-record(caps, {node, version, exts}).
-record(caps_features, {node_pair, features = []}).

-record(state, {host}).

%%====================================================================
%% API
%%====================================================================
start_link(Host, Opts) ->
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    gen_server:start_link({local, Proc}, ?MODULE, [Host, Opts], []).

start(Host, Opts) ->
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    ChildSpec =
	{Proc,
	 {?MODULE, start_link, [Host, Opts]},
	 transient,
	 1000,
	 worker,
	 [?MODULE]},
    supervisor:start_child(ejabberd_sup, ChildSpec).

stop(Host) ->
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    gen_server:call(Proc, stop),
    supervisor:terminate_child(ejabberd_sup, Proc),
    supervisor:delete_child(ejabberd_sup, Proc).

%% get_features returns a list of features implied by the given caps
%% record (as extracted by read_caps) or 'unknown' if features are
%% not completely collected at the moment.
get_features(nothing) ->
    [];
get_features(#caps{node = Node, version = Version, exts = Exts}) ->
    SubNodes = [Version | Exts],
    lists:foldl(
      fun(SubNode, Acc) ->
	      case mnesia:dirty_read({caps_features,
				      node_to_binary(Node, SubNode)}) of
		  [] ->
		      Acc;
		  [#caps_features{features = Features}] ->
		      binary_to_features(Features) ++ Acc
	      end
      end, [], SubNodes).

%% read_caps takes a list of XML elements (the child elements of a
%% <presence/> stanza) and returns an opaque value representing the
%% Entity Capabilities contained therein, or the atom nothing if no
%% capabilities are advertised.
read_caps(Els) ->
    read_caps(Els, nothing).

read_caps([{xmlelement, "c", Attrs, _Els} | Tail], Result) ->
    case xml:get_attr_s("xmlns", Attrs) of
	?NS_CAPS ->
	    Node = xml:get_attr_s("node", Attrs),
	    Version = xml:get_attr_s("ver", Attrs),
	    Exts = string:tokens(xml:get_attr_s("ext", Attrs), " "),
	    read_caps(Tail, #caps{node = Node, version = Version, exts = Exts});
	_ ->
	    read_caps(Tail, Result)
    end;
read_caps([{xmlelement, "x", Attrs, _Els} | Tail], Result) ->
    case xml:get_attr_s("xmlns", Attrs) of
	?NS_MUC_USER ->
	    nothing;
	_ ->
	    read_caps(Tail, Result)
    end;
read_caps([_ | Tail], Result) ->
    read_caps(Tail, Result);
read_caps([], Result) ->
    Result.

%%====================================================================
%% Hooks
%%====================================================================
user_send_packet(#jid{luser = User, lserver = Server} = From,
		 #jid{luser = User, lserver = Server, lresource = ""},
		 {xmlelement, "presence", Attrs, Els}) ->
    Type = xml:get_attr_s("type", Attrs),
    if Type == ""; Type == "available" ->
	    case read_caps(Els) of
		nothing ->
		    ok;
		#caps{version = Version, exts = Exts} = Caps ->
		    feature_request(Server, From, Caps, [Version | Exts])
	    end;
       true ->
	    ok
    end;
user_send_packet(_From, _To, _Packet) ->
    ok.

%%====================================================================
%% gen_server callbacks
%%====================================================================
init([Host, _Opts]) ->
    mnesia:create_table(caps_features,
			[{disc_copies, [node()]},
			 {local_content, true},
			 {attributes, record_info(fields, caps_features)}]),
    mnesia:add_table_copy(caps_features, node(), disc_copies),
    ejabberd_hooks:add(user_send_packet, Host,
		       ?MODULE, user_send_packet, 75),
    {ok, #state{host = Host}}.

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(_Req, _From, State) ->
    {reply, {error, badarg}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    Host = State#state.host,
    ejabberd_hooks:delete(user_send_packet, Host,
			  ?MODULE, user_send_packet, 75),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Aux functions
%%====================================================================
feature_request(Host, From, Caps, [SubNode | Tail] = SubNodes) ->
    Node = Caps#caps.node,
    BinaryNode = node_to_binary(Node, SubNode),
    case mnesia:dirty_read({caps_features, BinaryNode}) of
	[] ->
	    IQ = #iq{type = get,
		     xmlns = ?NS_DISCO_INFO,
		     sub_el = [{xmlelement, "query",
				[{"xmlns", ?NS_DISCO_INFO},
				 {"node", Node ++ "#" ++ SubNode}],
				[]}]},
	    F = fun(IQReply) ->
			feature_response(
			  IQReply, Host, From, Caps, SubNodes)
		end,
	    ejabberd_local:route_iq(
	      jlib:make_jid("", Host, ""), From, IQ, F);
	_ ->
	    feature_request(Host, From, Caps, Tail)
    end;
feature_request(_Host, _From, _Caps, []) ->
    ok.

feature_response(#iq{type = result,
		     sub_el = [{xmlelement, _, _, Els}]},
		 Host, From, Caps, [SubNode | SubNodes]) ->
    Features = lists:flatmap(
		 fun({xmlelement, "feature", FAttrs, _}) ->
			 [xml:get_attr_s("var", FAttrs)];
		    (_) ->
			 []
		 end, Els),
    BinaryNode = node_to_binary(Caps#caps.node, SubNode),
    mnesia:dirty_write(
      #caps_features{node_pair = BinaryNode,
		     features = features_to_binary(Features)}),
    feature_request(Host, From, Caps, SubNodes);
feature_response(timeout, _Host, _From, _Caps, _SubNodes) ->
    ok;
feature_response(_IQResult, Host, From, Caps, [SubNode | SubNodes]) ->
    %% We got type=error or invalid type=result stanza, so
    %% we cache empty feature not to probe the client permanently
    BinaryNode = node_to_binary(Caps#caps.node, SubNode),
    mnesia:dirty_write(#caps_features{node_pair = BinaryNode}),
    feature_request(Host, From, Caps, SubNodes).

node_to_binary(Node, SubNode) ->
    {list_to_binary(Node), list_to_binary(SubNode)}.

features_to_binary(L) -> [list_to_binary(I) || I <- L].
binary_to_features(L) -> [binary_to_list(I) || I <- L].
