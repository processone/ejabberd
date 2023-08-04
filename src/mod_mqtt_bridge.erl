%%%-------------------------------------------------------------------
%%% @author Pawel Chmielowski <pawel@process-one.net>
%%% @copyright (C) 2002-2023 ProcessOne, SARL. All Rights Reserved.
%%%
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%%
%%%-------------------------------------------------------------------
-module(mod_mqtt_bridge).
-behaviour(gen_mod).

%% gen_mod API
-export([start/2, stop/1, reload/3, depends/2, mod_options/1, mod_opt_type/1]).
-export([mod_doc/0]).

%% API
-export([mqtt_publish_hook/3]).

-include("logger.hrl").
-include("mqtt.hrl").
-include("translate.hrl").

%%%===================================================================
%%% API
%%%===================================================================
start(_Host, Opts) ->
    User = mod_mqtt_bridge_opt:replication_user(Opts),
    start_servers(User, element(1, mod_mqtt_bridge_opt:servers(Opts))),
    {ok, [{hook, mqtt_publish, mqtt_publish_hook, 50}]}.

stop(Host) ->
    stop_servers(element(1, mod_mqtt_bridge_opt:servers(Host))),
    ok.

start_servers(User, Servers) ->
    lists:foldl(
	fun({Proc, Transport, HostAddr, Port, Path, Publish, Subscribe, Authentication}, Started) ->
	    case Started of
		#{Proc := _} ->
		    ?DEBUG("Already started ~p", [Proc]),
		    Started;
		_ ->
		    ChildSpec = {Proc,
				 {mod_mqtt_bridge_session, start_link,
				  [Proc, Transport, HostAddr, Port, Path, Publish, Subscribe, Authentication, User]},
				 transient,
				 1000,
				 worker,
				 [mod_mqtt_bridge_session]},
		    Res = supervisor:start_child(ejabberd_gen_mod_sup, ChildSpec),
		    ?DEBUG("Starting ~p ~p", [Proc, Res]),
		    Started#{Proc => true}
	    end
	end, #{}, Servers).

stop_servers(Servers) ->
    lists:foreach(
	fun({Proc, _Transport, _Host, _Port, _Path, _Publish, _Subscribe, _Authentication}) ->
	    try p1_server:call(Proc, stop)
	    catch _:_ -> ok
	    end,
	    supervisor:terminate_child(ejabberd_gen_mod_sup, Proc),
	    supervisor:delete_child(ejabberd_gen_mod_sup, Proc)
	end, Servers).

reload(_Host, NewOpts, OldOpts) ->
    OldServers = element(1, mod_mqtt_bridge_opt:servers(OldOpts)),
    NewServers = element(1, mod_mqtt_bridge_opt:servers(NewOpts)),
    Deleted = lists:filter(
	fun(E) -> not lists:keymember(element(1, E), 1, NewServers) end,
	OldServers),
    Added = lists:filter(
	fun(E) -> not lists:keymember(element(1, E), 1, OldServers) end,
	NewServers),
    stop_servers(Deleted),
    start_servers(mod_mqtt_bridge_opt:replication_user(NewOpts), Added),
    ok.

depends(_Host, _Opts) ->
    [{mod_mqtt, hard}].

proc_name(Proto, Host, Port, Path) ->
    HostB = list_to_binary(Host),
    TransportB = list_to_binary(Proto),
    PathB = case Path of
		V when is_list(V) ->
		    list_to_binary(V);
		_ -> <<>>
	    end,
    binary_to_atom(<<"mod_mqtt_bridge_", TransportB/binary, "_", HostB/binary,
		     "_", (integer_to_binary(Port))/binary, PathB/binary>>, utf8).

-spec mqtt_publish_hook(jid:ljid(), publish(), non_neg_integer()) -> ok.
mqtt_publish_hook({_, S, _}, #publish{topic = Topic} = Pkt, _ExpiryTime) ->
    {_, Publish} = mod_mqtt_bridge_opt:servers(S),
    case maps:find(Topic, Publish) of
	error -> ok;
	{ok, Procs} ->
	    lists:foreach(
		fun(Proc) ->
		    Proc ! {publish, Pkt}
		end, Procs)
    end.

%%%===================================================================
%%% Options
%%%===================================================================
-spec mod_options(binary()) ->
    [{servers,
      {[{atom(), mqtt | mqtts | mqtt5 | mqtt5s, binary(), non_neg_integer(),
	 #{binary() => binary()}, #{binary() => binary()}, map()}],
       #{binary() => [atom()]}}} |
    {atom(), any()}].
mod_options(Host) ->
    [{servers, []},
     {replication_user, jid:make(<<"admin">>, Host)}].

mod_opt_type(replication_user) ->
    econf:jid();
mod_opt_type(servers) ->
    econf:and_then(
	econf:map(econf:url([mqtt, mqtts, mqtt5, mqtt5s, ws, wss, ws5, wss5]),
	    econf:options(
		#{
		    publish => econf:map(econf:binary(), econf:binary(), [{return, map}]),
		    subscribe => econf:map(econf:binary(), econf:binary(), [{return, map}]),
		    authentication => econf:either(
			econf:options(
			    #{
				username => econf:binary(),
				password => econf:binary()
			    }, [{return, map}]),
			econf:options(
			    #{
				certfile => econf:pem()
			    }, [{return, map}])
		    )}, [{return, map}]),
		  [{return, map}]),
	fun(Servers) ->
	    maps:fold(
		fun(Url, Opts, {HAcc, PAcc}) ->
		    {ok, Scheme, _UserInfo, Host, Port, Path, _Query} =
		    misc:uri_parse(Url, [{mqtt, 1883}, {mqtts, 8883},
					 {mqtt5, 1883}, {mqtt5s, 8883},
					 {ws, 80}, {wss, 443},
					 {ws5, 80}, {wss5, 443}]),
		    Publish = maps:get(publish, Opts, #{}),
		    Subscribe = maps:get(subscribe, Opts, #{}),
		    Authentication = maps:get(authentication, Opts, []),
		    Proto = list_to_atom(Scheme),
		    Proc = proc_name(Scheme, Host, Port, Path),
		    PAcc2 = maps:fold(
			fun(Topic, _RemoteTopic, Acc) ->
			    maps:update_with(Topic, fun(V) -> [Proc | V] end, [Proc], Acc)
			end, PAcc, Publish),
		    {[{Proc, Proto, Host, Port, Path, Publish, Subscribe, Authentication} | HAcc], PAcc2}
		end, {[], #{}}, Servers)
	end
    ).

%%%===================================================================
%%% Doc
%%%===================================================================
mod_doc() ->
    #{desc =>
      [?T("This module adds ability to synchronize local MQTT topics with data on remote servers"),
       ?T("It can update topics on remote servers when local user updates local topic, or can subscribe "
	  "for changes on remote server, and update local copy when remote data is updated."),
      ?T("It is available since ejabberd 23.01.")],
      example =>
      ["modules:",
       "  ...",
       "  mod_mqtt_bridge:",
       "    servers:",
       "      \"mqtt://server.com\":",
       "        publish:",
       "          \"localA\": \"remoteA\" # local changes to 'localA' will be replicated on remote server as 'remoteA'",
       "          \"topicB\": \"topicB\"",
       "        subscribe:",
       "          \"remoteB\": \"localB\" # changes to 'remoteB' on remote server will be stored as 'localB' on local server",
       "        authentication:",
       "          certfile: \"/etc/ejabberd/mqtt_server.pem\"",
       "    replication_user: \"mqtt@xmpp.server.com\"",
       "  ..."],
      opts =>
      [{servers,
	#{value => "{ServerUrl: {publish: [TopicPairs], subscribe: [TopicPairs], authentication: [AuthInfo]}}",
	  desc =>
	  ?T("Declaration of data to share, must contain 'publish' or 'subscribe' or both, and 'authentication' "
	     "section with username/password field or certfile pointing to client certificate. "
	     "Accepted urls can use schema mqtt, mqtts (mqtt with tls), mqtt5, mqtt5s (both to trigger v5 protocol), "
	      "ws, wss, ws5, wss5. Certifcate authentication can be only used with mqtts, mqtt5s, wss, wss5.")}},
       {replication_user,
	#{value => "JID",
	  desc =>
	  ?T("Identifier of a user that will be assigned as owner of local changes.")}}]}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
