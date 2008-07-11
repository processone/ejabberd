%%%----------------------------------------------------------------------
%%% File    : mod_register.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Inband registration support
%%% Created :  8 Dec 2002 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2008   Process-one
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

-include("ejabberd.hrl").
-include("jlib.hrl").

start(Host, Opts) ->
    IQDisc = gen_mod:get_opt(iqdisc, Opts, one_queue),
    gen_iq_handler:add_iq_handler(ejabberd_local, Host, ?NS_REGISTER,
				  ?MODULE, process_iq, IQDisc),
    gen_iq_handler:add_iq_handler(ejabberd_sm, Host, ?NS_REGISTER,
				  ?MODULE, process_iq, IQDisc),
    ejabberd_hooks:add(c2s_stream_features, Host,
 		       ?MODULE, stream_feature_register, 50),
    ejabberd_hooks:add(c2s_unauthenticated_iq, Host,
 		       ?MODULE, unauthenticated_iq_register, 50),
    mnesia:create_table(mod_register_ip,
			[{ram_copies, [node()]},
			 {local_content, true},
			 {attributes, [key, value]}]),
    mnesia:add_table_copy(mod_register_ip, node(), ram_copies),
    ok.

stop(Host) ->
    ejabberd_hooks:delete(c2s_stream_features, Host,
 			  ?MODULE, stream_feature_register, 50),
    ejabberd_hooks:delete(c2s_unauthenticated_iq, Host,
			  ?MODULE, unauthenticated_iq_register, 50),
    gen_iq_handler:remove_iq_handler(ejabberd_local, Host, ?NS_REGISTER),
    gen_iq_handler:remove_iq_handler(ejabberd_sm, Host, ?NS_REGISTER).


stream_feature_register(Acc) ->
    [{xmlelement, "register",
      [{"xmlns", ?NS_FEATURE_IQREGISTER}], []} | Acc].

unauthenticated_iq_register(_Acc,
			    Server, #iq{xmlns = ?NS_REGISTER} = IQ, IP) ->
    Address = case IP of
		 {A, _Port} -> A;
		  _ -> undefined
	      end,
    ResIQ = process_iq(jlib:make_jid("", "", ""),
 		       jlib:make_jid("", Server, ""),
 		       IQ,
		       Address),
    Res1 = jlib:replace_from_to(jlib:make_jid("", Server, ""),
 				jlib:make_jid("", "", ""),
 				jlib:iq_to_xml(ResIQ)),
    jlib:remove_attr("to", Res1);

unauthenticated_iq_register(Acc, _Server, _IQ, _IP) ->
    Acc.

process_iq(From, To, IQ) ->
    process_iq(From, To, IQ, jlib:jid_tolower(jlib:jid_remove_resource(From))).

process_iq(From, To,
	   #iq{type = Type, lang = Lang, sub_el = SubEl, id = ID} = IQ,
	   Source) ->
    case Type of
	set ->
	    UTag = xml:get_subtag(SubEl, "username"),
	    PTag = xml:get_subtag(SubEl, "password"),
	    RTag = xml:get_subtag(SubEl, "remove"),
	    Server = To#jid.lserver,
	    if
		(UTag /= false) and (RTag /= false) ->
		    User = xml:get_tag_cdata(UTag),
		    case From of
			#jid{user = User, lserver = Server} ->
			    ejabberd_auth:remove_user(User, Server),
			    IQ#iq{type = result, sub_el = [SubEl]};
			_ ->
			    if
				PTag /= false ->
				    Password = xml:get_tag_cdata(PTag),
				    case ejabberd_auth:remove_user(User,
								   Server,
								   Password) of
					ok ->
					    IQ#iq{type = result,
						  sub_el = [SubEl]};
					%% TODO FIXME: This piece of
					%% code does not work since
					%% the code have been changed
					%% to allow several auth
					%% modules.  lists:foreach can
					%% only return ok:
					not_allowed ->
					    IQ#iq{type = error,
						  sub_el =
						  [SubEl, ?ERR_NOT_ALLOWED]};
					not_exists ->
					    IQ#iq{type = error,
						  sub_el =
						  [SubEl, ?ERR_ITEM_NOT_FOUND]};
					_ ->
					    IQ#iq{type = error,
						  sub_el =
						  [SubEl,
						   ?ERR_INTERNAL_SERVER_ERROR]}
				    end;
				true ->
				    IQ#iq{type = error,
					  sub_el = [SubEl, ?ERR_BAD_REQUEST]}
			    end
		    end;
		(UTag == false) and (RTag /= false) ->
		    case From of
			#jid{user = User,
			     lserver = Server,
			     resource = Resource} ->
			    ResIQ = #iq{type = result, xmlns = ?NS_REGISTER,
					id = ID,
					sub_el = [SubEl]},
			    ejabberd_router:route(
			      jlib:make_jid(User, Server, Resource),
			      jlib:make_jid(User, Server, Resource),
			      jlib:iq_to_xml(ResIQ)),
			    ejabberd_auth:remove_user(User, Server),
			    ignore;
			_ ->
			    IQ#iq{type = error,
				  sub_el = [SubEl, ?ERR_NOT_ALLOWED]}
		    end;
		(UTag /= false) and (PTag /= false) ->
		    User = xml:get_tag_cdata(UTag),
		    Password = xml:get_tag_cdata(PTag),
		    case From of
			#jid{user = User, lserver = Server} ->
			    ejabberd_auth:set_password(User, Server, Password),
			    IQ#iq{type = result, sub_el = [SubEl]};
			_ ->
			    case try_register(User, Server, Password,
					      Source) of
				ok ->
				    IQ#iq{type = result, sub_el = [SubEl]};
				{error, Error} ->
				    IQ#iq{type = error,
					  sub_el = [SubEl, Error]}
			    end
		    end;
		true ->
		    IQ#iq{type = error,
			  sub_el = [SubEl, ?ERR_BAD_REQUEST]}
	    end;
	get ->
	    IQ#iq{type = result,
		  sub_el = [{xmlelement,
			     "query",
			     [{"xmlns", "jabber:iq:register"}],
			     [{xmlelement, "instructions", [],
			       [{xmlcdata,
				 translate:translate(
				   Lang,
				   "Choose a username and password "
				   "to register with this server")}]},
			      {xmlelement, "username", [], []},
			      {xmlelement, "password", [], []}]}]}
    end.


try_register(User, Server, Password, Source) ->
    case jlib:is_nodename(User) of
	false ->
	    {error, ?ERR_BAD_REQUEST};
	_ ->
	    JID = jlib:make_jid(User, Server, ""),
	    Access = gen_mod:get_module_opt(Server, ?MODULE, access, all),
	    case acl:match_rule(Server, Access, JID) of
		deny ->
		    {error, ?ERR_CONFLICT};
		allow ->
		    case check_timeout(Source) of
			true ->
			    case ejabberd_auth:try_register(User, Server, Password) of
				{atomic, ok} ->
				    ejabberd_hooks:run(user_registered, Server,
						       [User, Server]),
				    send_welcome_message(JID),
				    send_registration_notifications(JID),
				    ok;
				Error ->
				    remove_timeout(Source),
				    case Error of
					{atomic, exists} ->
					    {error, ?ERR_CONFLICT};
					{error, invalid_jid} ->
					    {error, ?ERR_JID_MALFORMED};
					{error, not_allowed} ->
					    {error, ?ERR_NOT_ALLOWED};
					{error, _Reason} ->
					    {error, ?ERR_INTERNAL_SERVER_ERROR}
				    end
			    end;
			false ->
			    {error, ?ERR_RESOURCE_CONSTRAINT}
		    end
	    end
    end.


send_welcome_message(JID) ->
    Host = JID#jid.lserver,
    case gen_mod:get_module_opt(Host, ?MODULE, welcome_message, {"", ""}) of
	{"", ""} ->
	    ok;
	{Subj, Body} ->
	    BodyFormatted = io_lib:format(Body, []),
	    ejabberd_router:route(
	      jlib:make_jid("", Host, ""),
	      JID,
	      {xmlelement, "message", [{"type", "normal"}],
	       [{xmlelement, "subject", [], [{xmlcdata, Subj}]},
		{xmlelement, "body", [], [{xmlcdata, BodyFormatted}]}]});
	_ ->
	    ok
    end.

send_registration_notifications(UJID) ->
    Host = UJID#jid.lserver,
    case gen_mod:get_module_opt(Host, ?MODULE, registration_watchers, []) of
	[] -> ok;
	JIDs when is_list(JIDs) ->
	    Body = lists:flatten(
		     io_lib:format(
		       "The user '~s' was just created on node ~w.",
		       [jlib:jid_to_string(UJID), node()])),
	    lists:foreach(
	      fun(S) ->
		      case jlib:string_to_jid(S) of
			  error -> ok;
			  JID ->
			      ejabberd_router:route(
				jlib:make_jid("", Host, ""),
				JID,
				{xmlelement, "message", [{"type", "chat"}],
				 [{xmlelement, "body", [],
				   [{xmlcdata, Body}]}]})
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

