%%%-------------------------------------------------------------------
%%% @author Pawel Chmielowski <pawel@process-one.net>
%%% @copyright (C) 2002-2022 ProcessOne, SARL. All Rights Reserved.
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
start(Host, Opts) ->
    User = mod_mqtt_bridge_opt:replication_user(Opts),
    lists:foldl(
	fun({Proc, Transport, HostAddr, Port, Publish, Subscribe, Authentication}, Started) ->
	    case Started of
		#{Proc := _} ->
		    ?DEBUG("Already started ~p", [Proc]),
		    Started;
		_ ->
		    ChildSpec = {Proc,
				 {mod_mqtt_bridge_session, start_link,
				  [Proc, Transport, HostAddr, Port, Publish, Subscribe, Authentication, User]},
				 transient,
				 1000,
				 worker,
				 [mod_mqtt_bridge_session]},
		    Res = supervisor:start_child(ejabberd_gen_mod_sup, ChildSpec),
		    ?DEBUG("Starting ~p ~p", [Proc, Res]),
		    Started#{Proc => true}
	    end
	end, #{}, element(1, mod_mqtt_bridge_opt:servers(Opts))),
    ejabberd_hooks:add(mqtt_publish, Host, ?MODULE, mqtt_publish_hook, 50).

stop(Host) ->
    lists:foldl(
	fun({Proc, _Transport, _Host, _Port, _Publish, _Subscribe, _Authentication}, _) ->
	    try p1_server:call(Proc, stop)
	    catch _:_ -> ok
	    end,
	    supervisor:terminate_child(ejabberd_gen_mod_sup, Proc),
	    supervisor:delete_child(ejabberd_gen_mod_sup, Proc)
	end, #{}, element(1, mod_mqtt_bridge_opt:servers(Host))),
    ejabberd_hooks:delete(mqtt_publish, Host, ?MODULE, mqtt_publish_hook, 50).

reload(_Host, _NewOpts, _OldOpts) ->
    ok.

depends(_Host, _Opts) ->
    [{mod_mqtt, hard}].

proc_name(Transport, Host, Port) ->
    HostB = list_to_binary(Host),
    case Transport of
	gen_tcp ->
	    binary_to_atom(<<"mod_mqtt_bridge_mqtt_", HostB/binary, "_", (integer_to_binary(Port))/binary>>, utf8);
	_ -> binary_to_atom(<<"mod_mqtt_bridge_mqtts_", HostB/binary, "_", (integer_to_binary(Port))/binary>>, utf8)
    end.

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
      {[{atom(), gen_tcp | ssl, binary(), non_neg_integer(),
	 #{binary() => binary()}, #{binary() => binary()}, binary()}],
       #{binary() => [atom()]}}} |
    {atom(), any()}].
mod_options(Host) ->
    [{servers, []},
     {replication_user, jid:make(<<"admin">>, Host)}].

mod_opt_type(replication_user) ->
    econf:jid();
mod_opt_type(servers) ->
    econf:and_then(
	econf:map(econf:url([mqtt, mqtts]),
		  econf:options(#{
				    publish => econf:map(econf:binary(), econf:binary(), [{return, map}]),
				    subscribe => econf:map(econf:binary(), econf:binary(), [{return, map}]),
				    authentication => econf:binary()},
				[{return, map}]),
		  [{return, map}]),
	fun(Servers) ->
	    maps:fold(
		fun(Url, Opts, {HAcc, PAcc}) ->
		    {ok, Scheme, _UserInfo, Host, Port, _Path, _Query} = misc:uri_parse(Url),
		    Publish = maps:get(publish, Opts, #{}),
		    Subscribe = maps:get(subscribe, Opts, #{}),
		    Authentication = maps:get(authentication, Opts, []),
		    Transport = case Scheme of "mqtt" -> gen_tcp;
				    _ -> ssl
				end,
		    Proc = proc_name(Transport, Host, Port),
		    PAcc2 = maps:fold(
			fun(Topic, _RemoteTopic, Acc) ->
			    maps:update_with(Topic, fun(V) -> [Proc | V] end, [Proc], Acc)
			end, PAcc, Publish),
		    {[{Proc, Transport, Host, Port, Publish, Subscribe, Authentication} | HAcc], PAcc2}
		end, {[], #{}}, Servers)
	end
    ).

%%%===================================================================
%%% Doc
%%%===================================================================
mod_doc() ->
    #{desc =>
      ?T("This module adds ability to replicate data from or to external servers"),
      opts =>
      [{servers,
	#{value => "{ServerUrl: Replication informations}",
	  desc =>
	  ?T("Main entry point to define which topics should be replicated.")}},
       {replication_user,
	#{value => "JID",
	  desc =>
	  ?T("Identifier of a user which will own all local modifications.")}}]}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
