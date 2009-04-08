%%%----------------------------------------------------------------------
%%% File    : mod_register.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Inband registration support
%%% Created :  8 Dec 2002 by Alexey Shchepin <alexey@process-one.net>
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
%%%----------------------------------------------------------------------

-module(mod_register).
-author('alexey@process-one.net').

-behaviour(gen_mod).

-export([start/2,
	 stop/1,
	 stream_feature_register/1,
	 unauthenticated_iq_register/4,
	 process_iq/3]).

-include_lib("exmpp/include/exmpp.hrl").

-include("ejabberd.hrl").

start(Host, Opts) ->
    HostB = list_to_binary(Host),
    IQDisc = gen_mod:get_opt(iqdisc, Opts, one_queue),
    gen_iq_handler:add_iq_handler(ejabberd_local, HostB, ?NS_INBAND_REGISTER,
				  ?MODULE, process_iq, IQDisc),
    gen_iq_handler:add_iq_handler(ejabberd_sm, HostB, ?NS_INBAND_REGISTER,
				  ?MODULE, process_iq, IQDisc),
    ejabberd_hooks:add(c2s_stream_features, HostB,
 		       ?MODULE, stream_feature_register, 50),
    ejabberd_hooks:add(c2s_unauthenticated_iq, HostB,
 		       ?MODULE, unauthenticated_iq_register, 50),
    mnesia:create_table(mod_register_ip,
			[{ram_copies, [node()]},
			 {local_content, true},
			 {attributes, [key, value]}]),
    mnesia:add_table_copy(mod_register_ip, node(), ram_copies),
    ok.

stop(Host) ->
    HostB = list_to_binary(Host),
    ejabberd_hooks:delete(c2s_stream_features, HostB,
 			  ?MODULE, stream_feature_register, 50),
    ejabberd_hooks:delete(c2s_unauthenticated_iq, HostB,
			  ?MODULE, unauthenticated_iq_register, 50),
    gen_iq_handler:remove_iq_handler(ejabberd_local, HostB, ?NS_INBAND_REGISTER),
    gen_iq_handler:remove_iq_handler(ejabberd_sm, HostB, ?NS_INBAND_REGISTER).


stream_feature_register(Acc) ->
    [#xmlel{ns = ?NS_INBAND_REGISTER_FEAT, name = 'register'} | Acc].

unauthenticated_iq_register(_Acc,
			    Server, #iq{ns = ?NS_INBAND_REGISTER} = IQ_Rec, IP) ->
    Address = case IP of
		 {A, _Port} -> A;
		  _ -> undefined
	      end,
    BareJID = exmpp_jid:make_jid(Server),
    ResIQ = process_iq(exmpp_jid:make_jid(),
 		       BareJID,
 		       IQ_Rec,
		       Address),
    exmpp_iq:iq_to_xmlel(ResIQ, BareJID, undefined);

unauthenticated_iq_register(Acc, _Server, _IQ, _IP) ->
    Acc.

process_iq(From, To, IQ) ->
    process_iq(From, To, IQ, {exmpp_jid:lnode_as_list(From),
                              exmpp_jid:ldomain_as_list(From),
                              exmpp_jid:lresource_as_list(From)}).

process_iq(From, To,
	   #iq{type = Type, lang = Lang, payload = SubEl} = IQ_Rec,
	   Source) ->
    case Type of
	set ->
	    UTag = exmpp_xml:get_element(SubEl, 'username'),
	    PTag = exmpp_xml:get_element(SubEl, 'password'),
	    RTag = exmpp_xml:get_element(SubEl, 'remove'),
	    Server = exmpp_jid:ldomain_as_list(To),
	    if
		(UTag /= undefined) and (RTag /= undefined) ->
		    User = exmpp_xml:get_cdata_as_list(UTag),
		    case {exmpp_jid:node_as_list(From), exmpp_jid:ldomain_as_list(From)} of
		    {User, Server} ->
			    ejabberd_auth:remove_user(User, Server),
                            exmpp_iq:result(IQ_Rec, SubEl);
			_ ->
			    if
				PTag /= undefined ->
				    Password = exmpp_xml:get_cdata_as_list(PTag),
				    case ejabberd_auth:remove_user(User,
								   Server,
								   Password) of
					ok ->
                                            exmpp_iq:result(IQ_Rec, SubEl);
					%% TODO FIXME: This piece of
					%% code does not work since
					%% the code have been changed
					%% to allow several auth
					%% modules.  lists:foreach can
					%% only return ok:
					not_allowed ->
                                            exmpp_iq:error(IQ_Rec,
                                              'not-allowed');
					not_exists ->
                                            exmpp_iq:error(IQ_Rec,
                                              'item-not-found');
					_ ->
                                            exmpp_iq:error(IQ_Rec,
                                              'internal-server-error')
				    end;
				true ->
                                    exmpp_iq:error(IQ_Rec, 'bad-request')
			    end
		    end;
		(UTag == undefined) and (RTag /= undefined) ->
		    case {exmpp_jid:node_as_list(From),
                  exmpp_jid:ldomain_as_list(From),
                  exmpp_jid:resource_as_list(From)}of
			{User, Server, Resource} ->
			    ResIQ = exmpp_iq:result(IQ_Rec, SubEl),
			    ejabberd_router:route(
			      exmpp_jid:make_jid(User, 
                                     Server, 
                                     Resource),
			      exmpp_jid:make_jid(User, 
                                     Server, 
                                     Resource),
			      exmpp_iq:iq_to_xmlel(ResIQ)),
			    ejabberd_auth:remove_user(User, Server),
			    ignore;
			_ ->
                            exmpp_iq:error(IQ_Rec, 'not-allowed')
		    end;
		(UTag /= undefined) and (PTag /= undefined) ->
		    User = exmpp_xml:get_cdata_as_list(UTag),
		    Password = exmpp_xml:get_cdata_as_list(PTag),
		    case {exmpp_jid:node_as_list(From), exmpp_jid:ldomain_as_list(From)} of
			{User, Server} ->
			    try_set_password(User, Server, Password, IQ_Rec, SubEl);
			_ ->
			    case try_register(User, Server, Password,
					      Source, Lang) of
				ok ->
                                    exmpp_iq:result(IQ_Rec, SubEl);
				{error, Error} ->
                                    exmpp_iq:error(IQ_Rec, Error)
			    end
		    end;
		true ->
                    exmpp_iq:error(IQ_Rec, 'bad-request')
	    end;
	get ->
            Result = #xmlel{ns = ?NS_INBAND_REGISTER, name = 'query', children =
              [#xmlel{ns = ?NS_INBAND_REGISTER, name = 'instructions', children =
                  [#xmlcdata{cdata = list_to_binary(translate:translate(Lang,
                          "Choose a username and password "
                          "to register with this server"))}]},
                #xmlel{ns = ?NS_INBAND_REGISTER, name = 'username'},
                #xmlel{ns = ?NS_INBAND_REGISTER, name = 'password'}]},
            exmpp_iq:result(IQ_Rec, Result)
    end.

%% @doc Try to change password and return IQ response
try_set_password(User, Server, Password, IQ_Rec, SubEl) ->
    case ejabberd_auth:set_password(User, Server, Password) of
	ok ->
            exmpp_iq:result(IQ_Rec, SubEl);
	{error, empty_password} ->
            exmpp_iq:error(IQ_Rec, 'bad-request');
	{error, not_allowed} ->
            exmpp_iq:error(IQ_Rec, 'not-allowed');
	{error, invalid_jid} ->
            exmpp_iq:error(IQ_Rec, 'item-not-found');
	_ ->
            exmpp_iq:error(IQ_Rec, 'internal-server-error')
    end.

try_register(User, Server, Password, Source, Lang) ->
    case exmpp_stringprep:is_node(User) of
	false ->
	    {error, 'bad-request'};
	_ ->
	    JID = exmpp_jid:make_jid(User, 
                                      Server),
	    Access = gen_mod:get_module_opt(Server, ?MODULE, access, all),
	    case acl:match_rule(Server, Access, JID) of
		deny ->
		    {error, 'conflict'};
		allow ->
		    case check_timeout(Source) of
			true ->
			    case ejabberd_auth:try_register(User, Server, Password) of
				{atomic, ok} ->
				    send_welcome_message(JID),
				    send_registration_notifications(JID),
				    ok;
				Error ->
				    remove_timeout(Source),
				    case Error of
					{atomic, exists} ->
					    {error, 'conflict'};
					{error, invalid_jid} ->
					    {error, 'jid-malformed'};
					{error, not_allowed} ->
					    {error, 'not-allowed'};
					{error, _Reason} ->
					    {error, 'internal-server-error'}
				    end
			    end;
			false ->
			    ErrText = "Users are not allowed to register "
				"accounts so quickly",
                            {error, exmpp_stanza:error(?NS_JABBER_CLIENT, 'resource-constraint', {Lang, ErrText})}
		    end
	    end
    end.


send_welcome_message(JID) ->
    Host = exmpp_jid:ldomain_as_list(JID),
    case gen_mod:get_module_opt(Host, ?MODULE, welcome_message, {"", ""}) of
	{"", ""} ->
	    ok;
	{Subj, Body} ->
	    ejabberd_router:route(
	      exmpp_jid:make_jid(Host),
	      JID,
              exmpp_message:normal(Subj, Body));
	_ ->
	    ok
    end.

send_registration_notifications(UJID) ->
    Host = exmpp_jid:ldomain_as_list(UJID),
    case gen_mod:get_module_opt(Host, ?MODULE, registration_watchers, []) of
	[] -> ok;
	JIDs when is_list(JIDs) ->
	    Body = lists:flatten(
		     io_lib:format(
		       "The user '~s' was just created on node ~w.",
		       [exmpp_jid:jid_to_list(UJID), node()])),
	    lists:foreach(
	      fun(S) ->
                      try
                          JID = exmpp_jid:parse_jid(S),
                          ejabberd_router:route(
                            exmpp_jid:make_jid(Host),
                            JID,
                            exmpp_message:chat(Body))
                      catch
                          _ ->
                              ok
		      end
	      end, JIDs);
	_ ->
	    ok
    end.


check_timeout(undefined) ->
    true;
check_timeout(Source) ->
    Timeout = case ejabberd_config:get_local_option(registration_timeout) of
		  undefined -> 600;
		  TO -> TO
	      end,
    if
	is_integer(Timeout) ->
	    {MSec, Sec, _USec} = now(),
	    Priority = -(MSec * 1000000 + Sec),
	    CleanPriority = Priority + Timeout,
	    F = fun() ->
			Treap = case mnesia:read(mod_register_ip, treap,
						 write) of
				    [] ->
					treap:empty();
				    [{mod_register_ip, treap, T}] -> T
				end,
			Treap1 = clean_treap(Treap, CleanPriority),
			case treap:lookup(Source, Treap1) of
			    error ->
				Treap2 = treap:insert(Source, Priority, [],
						      Treap1),
				mnesia:write({mod_register_ip, treap, Treap2}),
				true;
			    {ok, _, _} ->
				mnesia:write({mod_register_ip, treap, Treap1}),
				false
			end
		end,
	    case mnesia:transaction(F) of
		{atomic, Res} ->
		    Res;
		{aborted, Reason} ->
		    ?ERROR_MSG("mod_register: timeout check error: ~p~n",
			       [Reason]),
		    true
	    end;
	true ->
	    true
    end.

clean_treap(Treap, CleanPriority) ->
    case treap:is_empty(Treap) of
	true ->
	    Treap;
	false ->
	    {_Key, Priority, _Value} = treap:get_root(Treap),
	    if
		Priority > CleanPriority ->
		    clean_treap(treap:delete_root(Treap), CleanPriority);
		true ->
		    Treap
	    end
    end.

remove_timeout(undefined) ->
    true;
remove_timeout(Source) ->
    Timeout = case ejabberd_config:get_local_option(registration_timeout) of
		  undefined -> 600;
		  TO -> TO
	      end,
    if
	is_integer(Timeout) ->
	    F = fun() ->
			Treap = case mnesia:read(mod_register_ip, treap,
						 write) of
				    [] ->
					treap:empty();
				    [{mod_register_ip, treap, T}] -> T
				end,
			Treap1 = treap:delete(Source, Treap),
			mnesia:write({mod_register_ip, treap, Treap1}),
			ok
		end,
	    case mnesia:transaction(F) of
		{atomic, ok} ->
		    ok;
		{aborted, Reason} ->
		    ?ERROR_MSG("mod_register: timeout remove error: ~p~n",
			       [Reason]),
		    ok
	    end;
	true ->
	    ok
    end.

