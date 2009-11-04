%%%----------------------------------------------------------------------
%%% File    : mod_caps.erl
%%% Author  : Magnus Henoch <henoch@dtek.chalmers.se>
%%% Purpose : Request and cache Entity Capabilities (XEP-0115)
%%% Created : 7 Oct 2006 by Magnus Henoch <henoch@dtek.chalmers.se>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2009   ProcessOne
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
	 get_caps/1,
	 note_caps/3,
	 wait_caps/2,
	 clear_caps/1,
	 get_features/2,
	 get_user_resources/2,
	 handle_disco_response/3]).

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

-include_lib("exmpp/include/exmpp.hrl").

%% hook handlers
-export([receive_packet/3,
	 receive_packet/4,
	 presence_probe/3,
	 remove_connection/3]).

-include("ejabberd.hrl").

-define(PROCNAME, ejabberd_mod_caps).
-define(DICT, dict).
-define(CAPS_QUERY_TIMEOUT, 60000). % 1mn without answer, consider client never answer

-record(caps, {node, version, exts}).
-record(caps_features, {node_pair, features = []}).
-record(user_caps, {jid, caps}).
-record(user_caps_resources, {uid, resource}).
-record(state, {host,
		disco_requests = ?DICT:new(),
		feature_queries = []}).

%% read_caps takes a list of XML elements (the child elements of a
%% <presence/> stanza) and returns an opaque value representing the
%% Entity Capabilities contained therein, or the atom nothing if no
%% capabilities are advertised.
read_caps(Els) ->
    read_caps(Els, nothing).
read_caps([#xmlel{ns = ?NS_CAPS, name = 'c'} = El | Tail], _Result) ->
    Node = exmpp_xml:get_attribute_as_list(El, 'node', ""),
    Version = exmpp_xml:get_attribute_as_list(El, 'ver', ""),
    Exts = string:tokens(exmpp_xml:get_attribute_as_list(El, 'ext', ""), " "),
    read_caps(Tail, #caps{node = Node, version = Version, exts = Exts});
read_caps([#xmlel{ns = ?NS_MUC_USER, name = 'x'} | _Tail], _Result) ->
    nothing;
read_caps([_ | Tail], Result) ->
    read_caps(Tail, Result);
read_caps([], Result) ->
    Result.

%% get_caps reads user caps from database
%% here we handle a simple retry loop, to avoid race condition
%% when asking caps while we still did not called note_caps
%% timeout is set to 10s
%% this is to be improved, but without altering performances.
%% if we did not get user presence 10s after getting presence_probe
%% we assume it has no caps
get_caps(LJID) ->
    get_caps(LJID, 5).
get_caps(_, 0) ->
    nothing;
get_caps({U, S, R}, Retry) ->
    BJID = exmpp_jid:to_binary(U, S, R),
    case catch mnesia:dirty_read({user_caps, BJID}) of
	[#user_caps{caps=waiting}] ->
	    timer:sleep(2000),
	    get_caps({U, S, R}, Retry-1);
	[#user_caps{caps=Caps}] ->
	    Caps;
	_ ->
	    nothing
    end.

%% clear_caps removes user caps from database
clear_caps(JID) ->
    R = exmpp_jid:prep_resource(JID),
    BJID = exmpp_jid:to_binary(JID),
    BUID = exmpp_jid:bare_to_binary(JID),
    catch mnesia:dirty_delete({user_caps, BJID}),
    catch mnesia:dirty_delete_object(#user_caps_resources{uid = BUID, resource = list_to_binary(R)}),
    ok.
  
%% give default user resource
get_user_resources(U, S) ->
    BUID = exmpp_jid:bare_to_binary(U, S),
    case catch mnesia:dirty_read({user_caps_resources, BUID}) of
	{'EXIT', _} ->
	    [];
	Resources ->
	    lists:map(fun(#user_caps_resources{resource=R}) -> binary_to_list(R) end, Resources)
    end.

%% note_caps should be called to make the module request disco
%% information.  Host is the host that asks, From is the full JID that
%% sent the caps packet, and Caps is what read_caps returned.
note_caps(Host, From, Caps) ->
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    gen_server:cast(Proc, {note_caps, From, Caps}).

%% wait_caps should be called just before note_caps
%% it allows to lock get_caps usage for code using presence_probe
%% that may run before we get any chance to note_caps.
wait_caps(Host, From) ->
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    gen_server:cast(Proc, {wait_caps, From}).

%% get_features returns a list of features implied by the given caps
%% record (as extracted by read_caps).  It may block, and may signal a
%% timeout error.
get_features(Host, Caps) ->
    case Caps of
	nothing -> 
	    [];
	#caps{} ->
	    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
	    gen_server:call(Proc, {get_features, Caps})
    end.

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
    gen_server:call(Proc, stop).

receive_packet(From, To, Packet) when ?IS_PRESENCE(Packet) ->
    case exmpp_presence:get_type(Packet) of
    'probe' ->
	ok;
    'error' ->
	ok;
    'invisible' ->
	ok;
    'subscribe' ->
	ok;
    'subscribed' ->
	ok;
    'unsubscribe' ->
	ok;
    'unsubscribed' ->
	ok;
    'unavailable' ->
	{_, S1, _} = jlib:short_prepd_jid(From),
	case jlib:short_prepd_jid(To) of
	{_, S1, _} -> ok;
	_ -> clear_caps(From)
	end;
	%% TODO: probe client, and clean only if no answers
	%% as far as protocol does not allow inter-server communication to
	%% let remote server handle it's own caps to decide which user is to be
	%% notified, we must keep a cache of online status of external contacts
	%% this is absolutely not scallable, but we have no choice for now
	%% we can only use unavailable presence, but if remote user just remove a local user
	%% from its roster, then it's considered as offline, so he does not receive local PEP
	%% anymore until he login again.
	%% This is tracked in EJAB-943
    _ ->
	ServerString = exmpp_jid:prep_domain_as_list(To),
	Els = Packet#xmlel.children,
	note_caps(ServerString, From, read_caps(Els))
    end;
receive_packet(_, _, _) ->
    ok.

receive_packet(_JID, From, To, Packet) ->
    receive_packet(From, To, Packet).

presence_probe(From, To, _) ->
    ServerString = exmpp_jid:prep_domain_as_list(To),
    wait_caps(ServerString, From).

remove_connection(_SID, JID, _Info) ->
    clear_caps(JID).

caps_to_binary(#caps{node = Node, version = Version, exts = Exts}) ->
    BExts = [list_to_binary(Ext) || Ext <- Exts],
    #caps{node = list_to_binary(Node), version = list_to_binary(Version), exts = BExts}.

node_to_binary(Node, SubNode) ->
    {list_to_binary(Node), list_to_binary(SubNode)}.

features_to_binary(L) -> [list_to_binary(I) || I <- L].
binary_to_features(L) -> [binary_to_list(I) || I <- L].

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([Host, _Opts]) ->
    mnesia:create_table(caps_features,
			[{disc_copies, [node()]},
			 {attributes, record_info(fields, caps_features)}]),
    mnesia:create_table(user_caps,
			[{ram_copies, [node()]},
			 {attributes, record_info(fields, user_caps)}]),
    mnesia:create_table(user_caps_resources,
			[{ram_copies, [node()]},
			 {type, bag},
			 {attributes, record_info(fields, user_caps_resources)}]),
    mnesia:delete_table(user_caps_default),
    mnesia:clear_table(user_caps),            % clean in case of explicitely set to disc_copies
    mnesia:clear_table(user_caps_resources),  % clean in case of explicitely set to disc_copies
    ejabberd_hooks:add(user_receive_packet, Host, ?MODULE, receive_packet, 30),
    ejabberd_hooks:add(s2s_receive_packet, Host, ?MODULE, receive_packet, 30),
    ejabberd_hooks:add(presence_probe_hook, Host, ?MODULE, presence_probe, 20),
    ejabberd_hooks:add(sm_remove_connection_hook, Host, ?MODULE, remove_connection, 20),
    {ok, #state{host = Host}}.

maybe_get_features(#caps{node = Node, version = Version, exts = Exts}) ->
    SubNodes = [Version | Exts],
    %% Make sure that we have all nodes we need to know.
    %% If a single one is missing, we wait for more disco
    %% responses.
    case lists:foldl(fun(SubNode, Acc) ->
			case Acc of
			    fail -> fail;
			    _ ->
				case mnesia:dirty_read({caps_features, {Node, SubNode}}) of
				    [] -> fail;
				    [#caps_features{features = Features}] -> Features ++ Acc %% TODO binary
				end
			end
		end, [], SubNodes) of
	fail -> wait;
	Features -> {ok, Features}
    end.

timestamp() ->
    {MegaSecs, Secs, _MicroSecs} = now(),
    MegaSecs * 1000000 + Secs.

handle_call({get_features, Caps}, From, State) ->
    case maybe_get_features(Caps) of
	{ok, Features} -> 
	    {reply, binary_to_features(Features), State};
	wait ->
	    gen_server:cast(self(), visit_feature_queries),
	    Timeout = timestamp() + 10,
	    FeatureQueries = State#state.feature_queries,
	    NewFeatureQueries = [{From, Caps, Timeout} | FeatureQueries],
	    NewState = State#state{feature_queries = NewFeatureQueries},
	    {noreply, NewState}
    end;

handle_call(stop, _From, State) ->
    {stop, normal, ok, State}.

handle_cast({note_caps, From, nothing}, State) ->
    BJID = exmpp_jid:to_binary(From),
    catch mnesia:dirty_delete({user_caps, BJID}),
    {noreply, State};
handle_cast({note_caps, From, 
	     #caps{node = Node, version = Version, exts = Exts} = Caps}, 
	    #state{host = Host, disco_requests = Requests} = State) ->
    %% XXX: this leads to race conditions where ejabberd will send
    %% lots of caps disco requests.
    %#jid{node = U, domain = S, resource = R} = From,
    U = exmpp_jid:prep_node(From),
    S = exmpp_jid:prep_domain(From),
    R = exmpp_jid:resource(From),
    BJID = exmpp_jid:to_binary(From),
    mnesia:transaction(fun() ->
	mnesia:dirty_write(#user_caps{jid = BJID, caps = caps_to_binary(Caps)}),
	case ejabberd_sm:get_user_resources(U, S) of
	    [] ->
		% only store resource of caps aware external contacts
		BUID = exmpp_jid:bare_to_binary(From),
		mnesia:dirty_write(#user_caps_resources{uid = BUID, resource = list_to_binary(R)});
	    _ -> 
		ok
	end
    end),
    %% Now, find which of these are not already in the database.
    SubNodes = [Version | Exts],
    case lists:foldl(fun(SubNode, Acc) ->
			case mnesia:dirty_read({caps_features, {Node, SubNode}}) of
			    [] ->
				[SubNode | Acc];
			    _ ->
				Acc
			end
		end, [], SubNodes) of
	[] ->
	    {noreply, State};
	Missing ->
	    %% For each unknown caps "subnode", we send a disco request.
	    NewRequests = lists:foldl(
		fun(SubNode, Dict) ->
			  ID = randoms:get_string(),
			  Query = exmpp_xml:set_attribute(
			    #xmlel{ns = ?NS_DISCO_INFO, name = 'query'},
			    'node', lists:concat([Node, "#", SubNode])),
			  Stanza = exmpp_iq:get(?NS_JABBER_CLIENT, Query, ID),
			  ejabberd_local:register_iq_response_handler
			    (list_to_binary(Host), ID, ?MODULE, handle_disco_response),
			  ejabberd_router:route(exmpp_jid:make(Host), From, Stanza),
			  timer:send_after(?CAPS_QUERY_TIMEOUT, self(), {disco_timeout, ID, BJID}),
			  ?DICT:store(ID, node_to_binary(Node, SubNode), Dict)
		  end, Requests, Missing),
	    {noreply, State#state{disco_requests = NewRequests}}
    end;
handle_cast({wait_caps, From}, State) ->
    BJID = exmpp_jid:to_binary(From),
    mnesia:dirty_write(#user_caps{jid = BJID, caps = waiting}),
    {noreply, State};
handle_cast({disco_response, From, _To, #iq{id = ID, type = Type, payload = Payload}},
	    #state{disco_requests = Requests} = State) ->
    case {Type, Payload} of
	{result, #xmlel{name = 'query', children = Els}} ->
	    case ?DICT:find(ID, Requests) of
		{ok, BinaryNode} ->
		    Features =
			lists:flatmap(fun(#xmlel{name = 'feature'} = F) ->
					      [exmpp_xml:get_attribute_as_list(F, 'var', "")];
					 (_) ->
					      []
				      end, Els),
		    mnesia:dirty_write(#caps_features{node_pair = BinaryNode, features = features_to_binary(Features)}),
		    gen_server:cast(self(), visit_feature_queries);
		error ->
		    ?DEBUG("ID '~s' matches no query", [ID])
	    end;
	{error, _} ->
	    %% XXX: if we get error, we cache empty feature not to probe the client continuously
	    case ?DICT:find(ID, Requests) of
		{ok, BinaryNode} ->
		    mnesia:dirty_write(#caps_features{node_pair = BinaryNode}),
		    gen_server:cast(self(), visit_feature_queries);
		error ->
		    ?DEBUG("ID '~s' matches no query", [ID])
	    end;
	    %gen_server:cast(self(), visit_feature_queries),
	    %?DEBUG("Error IQ reponse from ~s:~n~p", [exmpp_jid:to_list(From), SubEls]);
	{result, Payload} ->
	    ?DEBUG("Invalid IQ contents from ~s:~n~p", [exmpp_jid:to_binary(From), Payload]);
	_ ->
	    %% Can't do anything about errors
	    ok
    end,
    NewRequests = ?DICT:erase(ID, Requests),
    {noreply, State#state{disco_requests = NewRequests}};
handle_cast({disco_timeout, ID, BJID}, #state{host = Host, disco_requests = Requests} = State) ->
    %% do not wait a response anymore for this IQ, client certainly will never answer
    NewRequests = case ?DICT:is_key(ID, Requests) of
    true ->
	catch mnesia:dirty_delete({user_caps, BJID}),
	ejabberd_local:unregister_iq_response_handler(list_to_binary(Host), ID),
	?DICT:erase(ID, Requests);
    false ->
	Requests
    end,
    {noreply, State#state{disco_requests = NewRequests}};
handle_cast(visit_feature_queries, #state{feature_queries = FeatureQueries} = State) ->
    Timestamp = timestamp(),
    NewFeatureQueries =
	lists:foldl(fun({From, Caps, Timeout}, Acc) ->
			    case maybe_get_features(Caps) of
				wait when Timeout > Timestamp -> [{From, Caps, Timeout} | Acc];
				wait -> Acc;
				{ok, Features} ->
				    gen_server:reply(From, Features),
				    Acc
			    end
		    end, [], FeatureQueries),
    {noreply, State#state{feature_queries = NewFeatureQueries}}.

handle_disco_response(From, To, IQ_Rec) ->
    Host = exmpp_jid:prep_domain_as_list(To),
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    gen_server:cast(Proc, {disco_response, From, To, IQ_Rec}).

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    Host = State#state.host,
    ejabberd_hooks:delete(user_receive_packet, Host, ?MODULE, receive_packet, 30),
    ejabberd_hooks:delete(s2s_receive_packet, Host, ?MODULE, receive_packet, 30),
    ejabberd_hooks:delete(presence_probe_hook, Host, ?MODULE, presence_probe, 20),
    ejabberd_hooks:delete(sm_remove_connection_hook, Host, ?MODULE, remove_connection, 20),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
