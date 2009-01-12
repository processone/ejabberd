%%%----------------------------------------------------------------------
%%% File    : mod_caps.erl
%%% Author  : Magnus Henoch <henoch@dtek.chalmers.se>
%%% Purpose : Request and cache Entity Capabilities (XEP-0115)
%%% Created : 7 Oct 2006 by Magnus Henoch <henoch@dtek.chalmers.se>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2008   ProcessOne
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

-module(mod_caps).
-author('henoch@dtek.chalmers.se').

-behaviour(gen_server).
-behaviour(gen_mod).

-export([read_caps/1,
	 get_caps/1,
	 note_caps/3,
	 clear_caps/1,
	 get_features/2,
	 get_user_resource/2,
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

-include("ejabberd.hrl").

-define(PROCNAME, ejabberd_mod_caps).
-define(DICT, dict).
-define(CAPS_QUERY_TIMEOUT, 60000). % 1mn without answer, consider client never answer

-record(caps, {node, version, exts}).
-record(caps_features, {node_pair, features}).
-record(user_caps, {jid, caps}).
-record(user_caps_default, {uid, resource}).
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
    Node = exmpp_xml:get_attribute(El, 'node', ""),
    Version = exmpp_xml:get_attribute(El, 'ver', ""),
    Exts = string:tokens(exmpp_xml:get_attribute(El, 'ext', ""), " "),
    read_caps(Tail, #caps{node = Node, version = Version, exts = Exts});
read_caps([#xmlel{ns = ?NS_MUC_USER, name = 'x'} | _Tail], _Result) ->
    nothing;
read_caps([_ | Tail], Result) ->
    read_caps(Tail, Result);
read_caps([], Result) ->
    Result.

%% get_caps reads user caps from database
get_caps({U, S, R}) ->
    BJID = exmpp_jid:jid_to_binary(U, S, R),
    case catch mnesia:dirty_read({user_caps, BJID}) of
	[#user_caps{caps=Caps}] -> 
	    Caps;
	_ -> 
	    nothing
    end.

%% clear_caps removes user caps from database
clear_caps(JID) ->
    BJID = exmpp_jid:jid_to_binary(JID),
    BUID = exmpp_jid:bare_jid_to_binary(JID),
    catch mnesia:dirty_delete({user_caps, BJID}),
    case catch mnesia:dirty_read({user_caps_default, BUID}) of
	[#user_caps_default{resource=R}] ->
	    catch mnesia:dirty_delete({user_caps_default, BUID});
	_ ->
	    ok
    end.
  
%% give default user resource
get_user_resource(U, S) ->
    BUID = exmpp_jid:bare_jid_to_binary(U, S),
    case catch mnesia:dirty_read({user_caps_default, BUID}) of
	[#user_caps_default{resource=R}] ->
	    R;
	_ ->
	    []
    end.

%% note_caps should be called to make the module request disco
%% information.  Host is the host that asks, From is the full JID that
%% sent the caps packet, and Caps is what read_caps returned.
note_caps(Host, From, Caps) ->
    case Caps of
	nothing -> 
	    ok;
	_ ->
	    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
	    gen_server:cast(Proc, {note_caps, From, Caps})
    end.

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

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([Host, _Opts]) ->
    mnesia:create_table(caps_features,
			[{ram_copies, [node()]},
			 {attributes, record_info(fields, caps_features)}]),
    mnesia:create_table(user_caps,
			[{disc_copies, [node()]},
			 {attributes, record_info(fields, user_caps)}]),
    mnesia:create_table(user_caps_default,
			[{disc_copies, [node()]},
			 {attributes, record_info(fields, user_caps_default)}]),
    {ok, #state{host = Host}}.

maybe_get_features(#caps{node = Node, version = Version, exts = Exts}) ->
    SubNodes = [Version | Exts],
    F = fun() ->
		%% Make sure that we have all nodes we need to know.
		%% If a single one is missing, we wait for more disco
		%% responses.
		lists:foldl(fun(SubNode, Acc) ->
				    case Acc of
					fail -> fail;
					_ ->
					    case mnesia:read({caps_features, {Node, SubNode}}) of
						[] -> fail;
						[#caps_features{features = Features}] -> Features ++ Acc
					    end
				    end
			    end, [], SubNodes)
	end,
    case mnesia:transaction(F) of
	{atomic, fail} ->
	    wait;
	{atomic, Features} ->
	    {ok, Features}
    end.

timestamp() ->
    {MegaSecs, Secs, _MicroSecs} = now(),
    MegaSecs * 1000000 + Secs.

handle_call({get_features, Caps}, From, State) ->
    case maybe_get_features(Caps) of
	{ok, Features} -> 
	    {reply, Features, State};
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

handle_cast({note_caps, From, 
	     #caps{node = Node, version = Version, exts = Exts} = Caps}, 
	    #state{host = Host, disco_requests = Requests} = State) ->
    %% XXX: this leads to race conditions where ejabberd will send
    %% lots of caps disco requests.
    %#jid{node = U, domain = S, resource = R} = From,
    U = exmpp_jid:lnode(From),
    S = exmpp_jid:ldomain(From),
    R = exmpp_jid:resource(From),
    BJID = exmpp_jid:jid_to_binary(From),
    mnesia:dirty_write(#user_caps{jid = BJID, caps = Caps}),
    case ejabberd_sm:get_user_resources(U, S) of
	[] ->
	    ok;
	_ -> 
	    % only store default resource of external contacts
	    BUID = exmpp_jid:bare_jid_to_binary(From),
	    mnesia:dirty_write(#user_caps_default{uid = BUID, resource = R})
    end,
    SubNodes = [Version | Exts],
    %% Now, find which of these are not already in the database.
    Fun = fun() ->
		  lists:foldl(fun(SubNode, Acc) ->
				      case mnesia:read({caps_features, {Node, SubNode}}) of
					  [] ->
					      [SubNode | Acc];
					  _ ->
					      Acc
				      end
			      end, [], SubNodes)
	  end,
    case mnesia:transaction(Fun) of
	{atomic, Missing} ->
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
			  ejabberd_router:route(exmpp_jid:make_bare_jid(Host),
			    From, Stanza),
			  timer:send_after(?CAPS_QUERY_TIMEOUT, self(), {disco_timeout, ID}),
			  ?DICT:store(ID, {Node, SubNode}, Dict)
		  end, Requests, Missing),
	    {noreply, State#state{disco_requests = NewRequests}};
	Error ->
	    ?ERROR_MSG("Transaction failed: ~p", [Error]),
	    {noreply, State}
    end;
handle_cast({disco_response, From, _To, #iq{id = ID, type = Type, payload = Payload}},
	    #state{disco_requests = Requests} = State) ->
    case {Type, Payload} of
	{result, #xmlel{name = 'query', children = Els}} ->
	    case ?DICT:find(ID, Requests) of
		{ok, {Node, SubNode}} ->
		    Features =
			lists:flatmap(fun(#xmlel{name = 'feature'} = F) ->
					      [exmpp_xml:get_attribute(F, 'var', "")];
					 (_) ->
					      []
				      end, Els),
		    mnesia:transaction(
		      fun() ->
			      mnesia:write(#caps_features{node_pair = {Node, SubNode},
							  features = Features})
		      end),
		    gen_server:cast(self(), visit_feature_queries);
		error ->
		    ?ERROR_MSG("ID '~s' matches no query", [ID])
	    end;
	{error, _} ->
	    %% XXX: if we get error, we cache empty feature not to probe the client continuously
	    case ?DICT:find(ID, Requests) of
		{ok, {Node, SubNode}} ->
		    Features = [],
		    mnesia:transaction(
		      fun() ->
			      mnesia:write(#caps_features{node_pair = {Node, SubNode},
							  features = Features})
		      end),
		    gen_server:cast(self(), visit_feature_queries);
		error ->
		    ?ERROR_MSG("ID '~s' matches no query", [ID])
	    end;
	    %gen_server:cast(self(), visit_feature_queries),
	    %?DEBUG("Error IQ reponse from ~s:~n~p", [exmpp_jid:jid_to_list(From), SubEls]);
	{result, Payload} ->
	    ?DEBUG("Invalid IQ contents from ~s:~n~p", [exmpp_jid:jid_to_list(From), Payload]);
	_ ->
	    %% Can't do anything about errors
	    ok
    end,
    NewRequests = ?DICT:erase(ID, Requests),
    {noreply, State#state{disco_requests = NewRequests}};
handle_cast({disco_timeout, ID}, #state{host = Host, disco_requests = Requests} = State) ->
    %% do not wait a response anymore for this IQ, client certainly will never answer
    NewRequests = case ?DICT:is_key(ID, Requests) of
    true ->
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
    Host = exmpp_jid:ldomain_as_list(To),
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    gen_server:cast(Proc, {disco_response, From, To, IQ_Rec}).

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
