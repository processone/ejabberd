%%%----------------------------------------------------------------------
%%% File    : mod_register.erl
%%% Author  : Alexey Shchepin <alexey@sevcom.net>
%%% Purpose : 
%%% Created :  8 Dec 2002 by Alexey Shchepin <alexey@sevcom.net>
%%% Id      : $Id$
%%%----------------------------------------------------------------------

-module(mod_register).
-author('alexey@sevcom.net').
-vsn('$Revision$ ').

-behaviour(gen_mod).

-export([start/1, init/0, process_iq/3]).

-include("ejabberd.hrl").
-include("jlib.hrl").

start(Opts) ->
    IQDisc = gen_mod:get_opt(iqdisc, Opts, one_queue),
    gen_iq_handler:add_iq_handler(ejabberd_local, ?NS_REGISTER,
				  ?MODULE, process_iq, IQDisc),
    gen_iq_handler:add_iq_handler(ejabberd_sm, ?NS_REGISTER,
				  ?MODULE, process_iq, IQDisc),
    ok.

init() ->
    ok.

process_iq(From, _To, #iq{type = Type, lang = Lang, sub_el = SubEl} = IQ) ->
    case Type of
	set ->
	    UTag = xml:get_subtag(SubEl, "username"),
	    PTag = xml:get_subtag(SubEl, "password"),
	    RTag = xml:get_subtag(SubEl, "remove"),
	    Server = ?MYNAME,
	    if
		(UTag /= false) and (RTag /= false) ->
		    User = xml:get_tag_cdata(UTag),
		    case From of
			#jid{user = User, lserver = Server} ->
			    ejabberd_auth:remove_user(User),
			    IQ#iq{type = result, sub_el = [SubEl]};
			_ ->
			    if
				PTag /= false ->
				    Password = xml:get_tag_cdata(PTag),
				    case ejabberd_auth:remove_user(User,
								   Password) of
					ok ->
					    IQ#iq{type = result,
						  sub_el = [SubEl]};
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
			#jid{user = User, lserver = Server} ->
			    ejabberd_auth:remove_user(User),
			    IQ#iq{type = result, sub_el = [SubEl]};
			_ ->
			    IQ#iq{type = error,
				  sub_el = [SubEl, ?ERR_NOT_ALLOWED]}
		    end;
		(UTag /= false) and (PTag /= false) ->
		    User = xml:get_tag_cdata(UTag),
		    Password = xml:get_tag_cdata(PTag),
		    case From of
			#jid{user = User, lserver = Server} ->
			    ejabberd_auth:set_password(User, Password),
			    IQ#iq{type = result, sub_el = [SubEl]};
			_ ->
			    case try_register(User, Password) of
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


try_register(User, Password) ->
    case jlib:is_nodename(User) of
	false ->
	    {error, ?ERR_BAD_REQUEST};
	_ ->
	    JID = jlib:make_jid(User, ?MYNAME, ""),
	    case acl:match_rule(register, JID) of
		deny ->
		    {error, ?ERR_CONFLICT};
		allow ->
		    case ejabberd_auth:try_register(User, Password) of
			{atomic, ok} ->
			    send_welcome_message(JID),
			    send_registration_notifications(JID),
			    ok;
			{atomic, exists} ->
			    {error, ?ERR_CONFLICT};
			{error, invalid_jid} ->
			    {error, ?ERR_JID_MALFORMED};
			{error, not_allowed} ->
			    {error, ?ERR_NOT_ALLOWED};
			{error, _Reason} ->
			    {error, ?ERR_INTERNAL_SERVER_ERROR}
		    end
	    end
    end.


send_welcome_message(JID) ->
    case ejabberd_config:get_local_option(welcome_message) of
	{"", ""} ->
	    ok;
	{Subj, Body} ->
	    ejabberd_router:route(
	      jlib:make_jid("", ?MYNAME, ""),
	      JID,
	      {xmlelement, "message", [{"type", "normal"}],
	       [{xmlelement, "subject", [], [{xmlcdata, Subj}]},
		{xmlelement, "body", [], [{xmlcdata, Body}]}]});
	_ ->
	    ok
    end.

send_registration_notifications(UJID) ->
    case ejabberd_config:get_local_option(registration_watchers) of
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
				jlib:make_jid("", ?MYNAME, ""),
				JID,
				{xmlelement, "message", [{"type", "chat"}],
				 [{xmlelement, "body", [],
				   [{xmlcdata, Body}]}]})
		      end
	      end, JIDs);
	_ ->
	    ok
    end.

