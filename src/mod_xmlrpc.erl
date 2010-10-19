%%%----------------------------------------------------------------------
%%% File    : mod_xmlrpc.erl
%%% Author  : Badlop / Mickael Remond / Christophe Romain
%%% Purpose : XML-RPC server
%%% Created :
%%% Id      :
%%%----------------------------------------------------------------------

%%%/***************************************************************************
%%% *									      *
%%% *   This program is free software; you can redistribute it and/or modify  *
%%% *   it under the terms of the GNU General Public License as published by  *
%%% *   the Free Software Foundation; either version 2 of the License, or     *
%%% *   (at your option) any later version.				      *
%%% *									      *
%%% ***************************************************************************/
%%%
%%%
%%%	MOD_XMLRPC - an XML-RPC server module for ejabberd
%%%
%%%	v0.5 - 17 March 2008
%%%
%%%	http://ejabberd.jabber.ru/mod_xmlrpc
%%%
%%%	(C) 2005, Badlop
%%%	    2006, Process-one
%%%	    2007, Process-one
%%%	    2008, Process-one
%%%
%%%  Changelog:
%%%
%%%   0.7 - 02 April 2009 - cromain
%%%	 - add user nick change
%%%
%%%   0.6 - 02 June 2008 - cromain
%%%	 - add user existance checking
%%%	 - improve parameter checking
%%%	 - allow orderless parameter
%%%
%%%   0.5 - 17 March 2008 - cromain
%%%	 - add user changing and higher level methods
%%%
%%%   0.4 - 18 February 2008 - cromain
%%%	 - add roster handling
%%%	 - add message sending
%%%	 - code and api clean-up
%%%
%%%   0.3 - 18 October 2007 - cromain
%%%	 - presence improvement
%%%	 - add new functionality
%%%
%%%   0.2 - 4 March 2006 - mremond
%%%	 - Code clean-up
%%%	 - Made it compatible with current ejabberd SVN version
%%%
%%%   0.1.2 - 28 December 2005
%%%	 - Now compatible with ejabberd 1.0.0
%%%	 - The XMLRPC server is started only once, not once for every virtual host
%%%	 - Added comments for handlers. Every available handler must be explained
%%%

-module(mod_xmlrpc).
-author('Process-one').
-vsn('0.6').

-behaviour(gen_mod).

-export([start/2,
    handler/2,
    link_contacts/5,
    unlink_contacts/3,
    loop/1,
    stop/1]).

-export([add_rosteritem/6]).

-include("ejabberd.hrl").
-include("jlib.hrl").
-include("mod_roster.hrl").

-ifdef(EJABBERD1).
-record(session, {sid, usr, us, priority}). %% ejabberd 1.1.x
-else.
-record(session, {sid, usr, us, priority, info}). %% ejabberd 2.0.x
-endif.


-define(PROCNAME, ejabberd_mod_xmlrpc).
-define(PORT, 4560).
-define(TIMEOUT, 5000).

%% -----------------------------
%% Module interface
%% -----------------------------

start(_Host, Opts) ->
    case whereis(?PROCNAME) of
    undefined ->
	%% get options
	Port = gen_mod:get_opt(port, Opts, ?PORT),
	MaxSessions = 10,
	Timeout = gen_mod:get_opt(timeout, Opts, ?TIMEOUT),
	Handler = {mod_xmlrpc, handler},
	State = tryit,

	%% TODO: this option gives
	%% error_info: {function_clause,[{gen_tcp,mod,[{ip,{127,0,0,1}}]},
	%%case gen_mod:get_opt(listen_all, Opts, false) of
	%%	true -> Ip = all;
	%%	false -> Ip = {127, 0, 0, 1}
	%%end,
	Ip = all,

	%% start the XML-RPC server
	{ok, Pid} = xmlrpc:start_link(Ip, Port, MaxSessions, Timeout, Handler, State),

	%% start the loop process
	register(?PROCNAME, spawn(?MODULE, loop, [Pid])),
	ok;
    _ ->
	ok
    end.

loop(Pid) ->
    receive
    stop ->
	xmlrpc:stop(Pid)
    end.

stop(_Host) ->
    case whereis(?PROCNAME) of
    undefined ->
	ok;
    _Pid ->
	?PROCNAME ! stop,
	unregister(?PROCNAME)
    end.


%% -----------------------------
%% Handlers
%% -----------------------------

handler(tryit, Call) ->
    try handler(notry, Call) of
	Result -> Result
    catch
	A:B ->
	    ?ERROR_MSG("Problem '~p' in~nCall: ~p~nError: ~p", [A, Call, B]),
	    {false, {response, [-100]}}
    end;

% Call:		Arguments:	    Returns:

%% .............................
%%  Debug

%% echothis		String							  String
handler(_State, {call, echothis, [A]}) ->
    {false, {response, [A]}};

%% multhis		 struct[{a, Integer}, {b, Integer}]			  Integer
handler(_State, {call, multhis, [{struct, Struct}]}) ->
    [{a, A}, {b, B}] = lists:sort(Struct),
    {false, {response, [A*B]}};

%% .............................
%%  User administration

%% create_account  struct[{user, String}, {server, Server}, {password, String}]	  Integer
handler(_State, {call, create_account, [{struct, Struct}]}) ->
    [{password, P}, {server, S}, {user, U}] = lists:sort(Struct),
    case ejabberd_auth:try_register(U, S, P) of
    {atomic, ok} ->
	{false, {response, [0]}};
    {atomic, exists} ->
	{false, {response, [409]}};
    _ ->
	{false, {response, [1]}}
    end;

%% delete_account  struct[{user, String}, {server, Server}]	  Integer
handler(_State, {call, delete_account, [{struct, Struct}]}) ->
    [{server, S}, {user, U}] = lists:sort(Struct),
    Fun = fun() -> ejabberd_auth:remove_user(U, S) end,
    user_action(U, S, Fun, ok);

%% change_password struct[{user, String}, {server, String}, {newpass, String}]	   Integer
handler(_State, {call, change_password, [{struct, Struct}]}) ->
    [{newpass, P}, {server, S}, {user, U}] = lists:sort(Struct),
    Fun = fun() -> ejabberd_auth:set_password(U, S, P) end,
    user_action(U, S, Fun, ok);

%% set_nickname	struct[{user, String}, {server, String}, {nick, String}]	  Integer
handler(_State, {call, set_nickname, [{struct, Struct}]}) ->
    [{nick, N}, {server, S}, {user, U}] = lists:sort(Struct),
    Fun = fun() -> case mod_vcard:process_sm_iq(
	{jid, U, S, "", U, S, ""},
	{jid, U, S, "", U, S, ""},
	{iq, "", set, "", "en",
	{xmlelement, "vCard",
	    [{"xmlns", "vcard-temp"}], [
	    {xmlelement, "NICKNAME", [], [{xmlcdata, N}]}
	    ]
	}}) of
	{iq, [], result, [], _, []} -> ok;
	_ -> error
	end
    end,
    user_action(U, S, Fun, ok);

%% set_rosternick	struct[{user, String}, {server, String}, {nick, String}]	  Integer
handler(_State, {call, set_rosternick, [{struct, Struct}]}) ->
    [{nick, N}, {server, S}, {user, U}] = lists:sort(Struct),
    Fun = fun() -> change_rosternick(U, S, N) end,
    user_action(U, S, Fun, ok);

%% add_rosteritem  struct[{user, String}, {server, String},
%%			  {jid, String}, {group, String}, {nick, String}, {subs, String}]		 Integer
handler(_State, {call, add_rosteritem, [{struct, Struct}]}) ->
    [{group, G},{jid, JID},{nick, N},{server, S},{subs, Subs},{user, U}] = lists:sort(Struct),
    Fun = fun() -> add_rosteritem(U, S, JID, N, G, Subs) end,
    user_action(U, S, Fun, {atomic, ok});

%% link_contacts  struct[{jid1, String}, {nick1, String}, {jid2, String}, {nick2, String}]		Integer
handler(_State, {call, link_contacts, [{struct, Struct}]}) ->
    [{jid1, JID1}, {jid2, JID2}, {nick1, Nick1}, {nick2, Nick2}] = lists:sort(Struct),
    {U1, S1, _} = jlib:jid_tolower(jlib:string_to_jid(JID1)),
    {U2, S2, _} = jlib:jid_tolower(jlib:string_to_jid(JID2)),
    case {ejabberd_auth:is_user_exists(U1, S1), ejabberd_auth:is_user_exists(U2, S2)} of
    {true, true} ->
	case link_contacts(JID1, Nick1, JID2, Nick2) of
	{atomic, ok} ->
	    {false, {response, [0]}};
	_ ->
	    {false, {response, [1]}}
	end;
    _ ->
	{false, {response, [404]}}
    end;

%% delete_rosteritem  struct[{user, String}, {server, String}, {jid, String}]		 Integer
handler(_State, {call, delete_rosteritem, [{struct, Struct}]}) ->
    [{jid, JID}, {server, S}, {user, U}] = lists:sort(Struct),
    Fun = fun() -> del_rosteritem(U, S, JID) end,
    user_action(U, S, Fun, {atomic, ok});

%% unlink_contacts  struct[{jid1, String}, {jid2, String}]		Integer
handler(_State, {call, unlink_contacts, [{struct, Struct}]}) ->
    [{jid1, JID1}, {jid2, JID2}] = lists:sort(Struct),
    {U1, S1, _} = jlib:jid_tolower(jlib:string_to_jid(JID1)),
    {U2, S2, _} = jlib:jid_tolower(jlib:string_to_jid(JID2)),
    case {ejabberd_auth:is_user_exists(U1, S1), ejabberd_auth:is_user_exists(U2, S2)} of
    {true, true} ->
	case unlink_contacts(JID1, JID2) of
	{atomic, ok} ->
	    {false, {response, [0]}};
	_ ->
	    {false, {response, [1]}}
	end;
    _ ->
	{false, {response, [404]}}
    end;

%% get_roster  struct[{user, String}, {server, String}]
%%		array[struct[{jid, String}, {group, String}, {nick, String},
%%			   {subscription, String}, {pending, String}]]
handler(_State, {call, get_roster, [{struct, Struct}]}) ->
    [{server, S}, {user, U}] = lists:sort(Struct),
    case ejabberd_auth:is_user_exists(U, S) of
    true ->
	Roster = format_roster(get_roster(U, S)),
	{false, {response, [{array, Roster}]}};
    false ->
	{false, {response, [404]}}
    end;

%% get_roster_with_presence  struct[{user, String}, {server, String}]
%%		array[struct[{jid, String}, {resource, String}, {group, String}, {nick, String},
%%			  {subscription, String}, {pending, String},
%%			  {show, String}, {status, String}]]
handler(_State, {call, get_roster_with_presence, [{struct, Struct}]}) ->
    [{server, S}, {user, U}] = lists:sort(Struct),
    case ejabberd_auth:is_user_exists(U, S) of
    true ->
	Roster = format_roster_with_presence(get_roster(U, S)),
	{false, {response, [{array, Roster}]}};
    false ->
	{false, {response, [404]}}
    end;

%% get_presence  struct[{user, String}, {server, String}]
%%		array[struct[{jid, String}, {show, String}, {status, String}]]
handler(_State, {call, get_presence, [{struct, Struct}]}) ->
    [{server, S}, {user, U}] = lists:sort(Struct),
    case ejabberd_auth:is_user_exists(U, S) of
    true ->
	{Resource, Show, Status} = get_presence(U, S),
	FullJID = case Resource of
	[] ->
	    lists:flatten([U,"@",S]);
	_ ->
	    lists:flatten([U,"@",S,"/",Resource])
	end,
	R = {struct, [{jid, FullJID}, {show, Show}, {status, Status} ]},
	{false, {response, [R]}};
    false ->
	{false, {response, [404]}}
    end;

%% get_resources  struct[{user, String}, {server, String}]
%%		array[String]
handler(_State, {call, get_resources, [{struct, Struct}]}) ->
    [{server, S}, {user, U}] = lists:sort(Struct),
    case ejabberd_auth:is_user_exists(U, S) of
    true ->
	Resources = get_resources(U, S),
	{false, {response, [{array, Resources}]}};
    false ->
	{false, {response, [404]}}
    end;

%% send_chat  struct[{from, String}, {to, String}, {body, String}]
%%		Integer
handler(_State, {call, send_chat, [{struct, Struct}]}) ->
    [{body, Msg}, {from, FromJID}, {to, ToJID}] = lists:sort(Struct),
    From = jlib:string_to_jid(FromJID),
    To = jlib:string_to_jid(ToJID),
    Stanza = {xmlelement, "message", [{"type", "chat"}],
		[{xmlelement, "body", [], [{xmlcdata, Msg}]}]},
    ejabberd_router:route(From, To, Stanza),
    {false, {response, [0]}};

%% send_message  struct[{from, String}, {to, String}, {subject, String}, {body, String}]
%%		Integer
handler(_State, {call, send_message, [{struct, Struct}]}) ->
    [{body, Msg}, {from, FromJID}, {subject, Sub}, {to, ToJID}] = lists:sort(Struct),
    From = jlib:string_to_jid(FromJID),
    To = jlib:string_to_jid(ToJID),
    Stanza = {xmlelement, "message", [{"type", "normal"}],
		[{xmlelement, "subject", [], [{xmlcdata, Sub}]},
		    {xmlelement, "body", [], [{xmlcdata, Msg}]}]},
    ejabberd_router:route(From, To, Stanza),
    {false, {response, [0]}};

%% send_stanza  struct[{from, String}, {to, String}, {stanza, String}]
%%		Integer
handler(_State, {call, send_stanza, [{struct, Struct}]}) ->
    [{from, FromJID}, {stanza, StanzaStr}, {to, ToJID}] = lists:sort(Struct),
    case xml_stream:parse_element(StanzaStr) of
    {error, _} ->
	{false, {response, [1]}};
    Stanza ->
	{xmlelement, _, Attrs, _} = Stanza,
	From = jlib:string_to_jid(proplists:get_value("from", Attrs, FromJID)),
	To = jlib:string_to_jid(proplists:get_value("to", Attrs, ToJID)),
	ejabberd_router:route(From, To, Stanza),
	{false, {response, [0]}}
    end;

%% rename_account  struct[{user, String}, {server, String}, {newuser, String}, {newserver, String}]
%%		Integer
handler(_State, {call, rename_account, [{struct, Struct}]}) ->
    [{newserver, NS}, {newuser, NU}, {server, S}, {user, U}] = lists:sort(Struct),
    case ejabberd_auth:is_user_exists(U, S) of
    true ->
	case ejabberd_auth:get_password(U, S) of
	false ->
	    {false, {response, [1]}};
	Password ->
	    case ejabberd_auth:try_register(NU, NS, Password) of
	    {atomic, ok} ->
		OldJID = jlib:jid_to_string({U, S, ""}),
		NewJID = jlib:jid_to_string({NU, NS, ""}),
		Roster = get_roster(U, S),
		lists:foreach(fun(#roster{jid={RU, RS, RE}, name=Nick, groups=Groups}) ->
		    NewGroup = extract_group(Groups),
		    {NewNick, Group} = case lists:filter(fun(#roster{jid={PU, PS, _}}) ->
			(PU == U) and (PS == S)
			end, get_roster(RU, RS)) of
		    [#roster{name=OldNick, groups=OldGroups}|_] -> {OldNick, extract_group(OldGroups)};
		    [] -> {NU, []}
		    end,
		    JIDStr = jlib:jid_to_string({RU, RS, RE}),
		    link_contacts(NewJID, NewNick, NewGroup, JIDStr, Nick, Group),
		    unlink_contacts(OldJID, JIDStr)
		end, Roster),
		ejabberd_auth:remove_user(U, S),
		{false, {response, [0]}};
	    {atomic, exists} ->
		{false, {response, [409]}};
	    _ ->
		{false, {response, [1]}}
	    end
	end;
    false ->
	{false, {response, [404]}}
    end;

%% add_contacts  struct[{user, String}, {server, String},
%%		array[struct[{jid, String}, {group, String}, {nick, String}]]]
%%		Integer
handler(_State, {call, add_contacts, [{struct, Struct}]}) ->
    [{array, Contacts}, {server, S}, {user, U}] = lists:sort(Struct),
    case ejabberd_auth:is_user_exists(U, S) of
    true ->
	JID1 = jlib:jid_to_string({U, S, ""}),
	Response = lists:foldl(fun({struct, Struct2}, Acc) ->
	    [{group, Group}, {jid, JID2}, {nick, Nick}] = lists:sort(Struct2),
	    {PU, PS, _} = jlib:jid_tolower(jlib:string_to_jid(JID2)),
	    case ejabberd_auth:is_user_exists(PU, PS) of
	    true ->
		case link_contacts(JID1, "", "", JID2, Nick, Group) of
		{atomic, ok} -> Acc;
		_ -> 1
		end;
	    false ->
		Acc
	    end
	end, 0, element(2, Contacts)),
	{false, {response, [Response]}};
    false ->
	{false, {response, [404]}}
    end;

%% remove_contacts  struct[{user, String}, {server, String}, array[String]]
%%		Integer
handler(_State, {call, remove_contacts, [{struct, Struct}]}) ->
    [{array, Contacts}, {server, S}, {user, U}] = lists:sort(Struct),
    case ejabberd_auth:is_user_exists(U, S) of
    true ->
	JID1 = jlib:jid_to_string({U, S, ""}),
	Response = lists:foldl(fun(JID2, Acc) ->
	    {PU, PS, _} = jlib:jid_tolower(jlib:string_to_jid(JID2)),
	    case ejabberd_auth:is_user_exists(PU, PS) of
	    true ->
		case unlink_contacts(JID1, JID2) of
		{atomic, ok} -> Acc;
		_ -> 1
		end;
	    false ->
		Acc
	    end
	end, 0, element(2, Contacts)),
	{false, {response, [Response]}};
    false ->
	{false, {response, [404]}}
    end;

%% check_users_registration  array[struct[{user, String}, {server, String}]]
%%		array[struct[{user, String}, {server, String}, {status, Integer}]]
handler(_State, {call, check_users_registration, [{array, Users}]}) ->
    Response = lists:map(fun({struct, Struct}) ->
	[{server, S}, {user, U}] = lists:sort(Struct),
	Registered = case ejabberd_auth:is_user_exists(U, S) of
	true -> 1;
	false -> 0
	end,
	{struct, [{user, U}, {server, S}, {status, Registered}]}
    end, Users),
    {false, {response, [{array, Response}]}};


%% If no other guard matches
handler(_State, Payload) ->
    FaultString = lists:flatten(io_lib:format("Unknown call: ~p", [Payload])),
    {false, {response, {fault, -1, FaultString}}}.


%% -----------------------------
%% Internal roster handling
%% -----------------------------

get_roster(User, Server) ->
    LUser = jlib:nodeprep(User),
    LServer = jlib:nameprep(Server),
    ejabberd_hooks:run_fold(roster_get, LServer, [], [{LUser, LServer}]).

change_rosternick(User, Server, Nick) ->
    LUser = jlib:nodeprep(User),
    LServer = jlib:nameprep(Server),
    LJID = {LUser, LServer, []},
    JID = jlib:jid_to_string(LJID),
    Push = fun(Subscription) ->
	jlib:iq_to_xml(#iq{type = set, xmlns = ?NS_ROSTER, id = "push",
			   sub_el = [{xmlelement, "query", [{"xmlns", ?NS_ROSTER}],
				     [{xmlelement, "item", [{"jid", JID}, {"name", Nick}, {"subscription", atom_to_list(Subscription)}],
				      []}]}]})
	end,
    Result = case roster_backend(Server) of
	mnesia ->
	    %% XXX This way of doing can not work with s2s
	    mnesia:transaction(
		fun() ->
		    lists:foreach(fun(Roster) ->
			{U, S} = Roster#roster.us,
			mnesia:write(Roster#roster{name = Nick}),
			lists:foreach(fun(R) ->
			    UJID = jlib:make_jid(U, S, R),
			    ejabberd_router:route(UJID, UJID, Push(Roster#roster.subscription))
			end, get_resources(U, S))
		    end, mnesia:match_object(#roster{jid = LJID, _ = '_'}))
		end);
	odbc ->
	    %%% XXX This way of doing does not work with several domains
	    ejabberd_odbc:sql_transaction(Server,
		fun() ->
		    SNick = ejabberd_odbc:escape(Nick),
		    SJID = ejabberd_odbc:escape(JID),
		    ejabberd_odbc:sql_query_t(
				["update rosterusers"
				 " set nick='", SNick, "'"
				 " where jid='", SJID, "';"]),
		    case ejabberd_odbc:sql_query_t(
			["select username from rosterusers"
			 " where jid='", SJID, "'"
			 " and subscription = 'B';"]) of
			{selected, ["username"], Users} ->
			    lists:foreach(fun({RU}) ->
				lists:foreach(fun(R) ->
				    UJID = jlib:make_jid(RU, Server, R),
				    ejabberd_router:route(UJID, UJID, Push(both))
				end, get_resources(RU, Server))
			    end, Users);
			_ ->
			    ok
		    end
		end);
	none ->
	    {error, no_roster}
    end,
    case Result of
	{atomic, ok} -> ok;
	_ -> error
    end.

add_rosteritem(User, Server, JID, Nick, Group, Subscription) ->
    add_rosteritem(User, Server, JID, Nick, Group, Subscription, true).
add_rosteritem(User, Server, JID, Nick, Group, Subscription, Push) ->
    {RU, RS, _} = jlib:jid_tolower(jlib:string_to_jid(JID)),
    LJID = {RU,RS,[]},
    Groups = case Group of
	[] -> [];
	_ -> [Group]
	end,
    Roster = #roster{
		usj = {User,Server,LJID},
		us  = {User,Server},
		jid = LJID,
		name = Nick,
		ask = none,
		subscription = list_to_atom(Subscription),
		groups = Groups},
    Result =
	case roster_backend(Server) of
	    mnesia ->
		mnesia:transaction(fun() ->
					   case mnesia:read({roster,{User,Server,LJID}}) of
					       [#roster{subscription=both}] ->
						   already_added;
					       _ ->
						   mnesia:write(Roster)
					   end
				   end);
	    odbc ->
		%% MREMOND: TODO: check if already_added
		case ejabberd_odbc:sql_transaction(Server,
						   fun() ->
							   Username = ejabberd_odbc:escape(User),
							   SJID = ejabberd_odbc:escape(jlib:jid_to_string(LJID)),
							   case ejabberd_odbc:sql_query_t(
								  ["select username from rosterusers "
								   "      where username='", Username, "' "
								   "        and jid='", SJID,
								   "' and subscription = 'B';"]) of
							       {selected, ["username"],[]} ->
								   ItemVals = record_to_string(Roster),
								   ItemGroups = groups_to_string(Roster),
								   odbc_queries:update_roster(Server, Username,
											      SJID, ItemVals,
											      ItemGroups);
							       _ ->
								   already_added
							   end
						   end) of
		    {atomic, already_added} -> {atomic, already_added};
		    {atomic, _} -> {atomic, ok};
		    Error -> Error
		end;
	    none ->
		{error, no_roster}
	end,
    case {Result, Push} of
	{{atomic, already_added}, _} -> ok;  %% No need for roster push
	{{atomic, ok}, true} -> roster_push(User, Server, JID, Nick, Subscription);
	{{error, no_roster}, true} -> roster_push(User, Server, JID, Nick, Subscription);
	{{atomic, ok}, false} -> ok;
	_ -> error
    end,
    Result.

del_rosteritem(User, Server, JID) ->
    del_rosteritem(User, Server, JID, true).
del_rosteritem(User, Server, JID, Push) ->
    {RU, RS, _} = jlib:jid_tolower(jlib:string_to_jid(JID)),
    LJID = {RU,RS,[]},
    Result = case roster_backend(Server) of
	mnesia ->
	    mnesia:transaction(fun() ->
		mnesia:delete({roster, {User,Server,LJID}})
	    end);
	odbc ->
	    case ejabberd_odbc:sql_transaction(Server, fun() ->
		Username = ejabberd_odbc:escape(User),
		SJID = ejabberd_odbc:escape(jlib:jid_to_string(LJID)),
		odbc_queries:del_roster(Server, Username, SJID)
		end) of
	    {atomic, _} -> {atomic, ok};
	    Error -> Error
	    end;
	none ->
	    {error, no_roster}
	end,
    case {Result, Push} of
    {{atomic, ok}, true} -> roster_push(User, Server, JID, "", "remove");
    {{error, no_roster}, true} -> roster_push(User, Server, JID, "", "remove");
    {{atomic, ok}, false} -> ok;
    _ -> error
    end,
    Result.

link_contacts(JID1, Nick1, JID2, Nick2) ->
    link_contacts(JID1, Nick1, JID2, Nick2, true).
link_contacts(JID1, Nick1, JID2, Nick2, Push) ->
    link_contacts(JID1, Nick1, [], JID2, Nick2, [], Push).

link_contacts(JID1, Nick1, Group1, JID2, Nick2, Group2) ->
    link_contacts(JID1, Nick1, Group1, JID2, Nick2, Group2, true).
link_contacts(JID1, Nick1, Group1, JID2, Nick2, Group2, Push) ->
    {U1, S1, _} = jlib:jid_tolower(jlib:string_to_jid(JID1)),
    {U2, S2, _} = jlib:jid_tolower(jlib:string_to_jid(JID2)),
    case add_rosteritem(U1, S1, JID2, Nick2, Group1, "both", Push) of
    {atomic, ok} -> add_rosteritem(U2, S2, JID1, Nick1, Group2, "both", Push);
    Error -> Error
    end.

unlink_contacts(JID1, JID2) ->
    unlink_contacts(JID1, JID2, true).
unlink_contacts(JID1, JID2, Push) ->
    {U1, S1, _} = jlib:jid_tolower(jlib:string_to_jid(JID1)),
    {U2, S2, _} = jlib:jid_tolower(jlib:string_to_jid(JID2)),
    case del_rosteritem(U1, S1, JID2, Push) of
    {atomic, ok} -> del_rosteritem(U2, S2, JID1, Push);
    Error -> Error
    end.

roster_push(User, Server, JID, Nick, Subscription) ->
    LJID = jlib:make_jid(User, Server, ""),
    TJID = jlib:string_to_jid(JID),
    {TU, TS, _} = jlib:jid_tolower(TJID),
    Presence = {xmlelement, "presence", [{"type",
	    case Subscription of
	    "remove" -> "unsubscribed";
	    "none" -> "unsubscribe";
	    "both" -> "subscribed";
	    _ -> "subscribe"
	    end}], []},
    Item = case Nick of
    "" -> [{"jid", JID}, {"subscription", Subscription}];
    _ -> [{"jid", JID}, {"name", Nick}, {"subscription", Subscription}]
    end,
    Result = jlib:iq_to_xml(#iq{type = set, xmlns = ?NS_ROSTER, id = "push",
    sub_el = [{xmlelement, "query", [{"xmlns", ?NS_ROSTER}],
    [{xmlelement, "item", Item, []}]}]}),
    ejabberd_router:route(TJID, LJID, Presence),
    ejabberd_router:route(LJID, LJID, Result),
    lists:foreach(fun(Resource) ->
	UJID = jlib:make_jid(User, Server, Resource),
	ejabberd_router:route(TJID, UJID, Presence),
	ejabberd_router:route(UJID, UJID, Result),
	case Subscription of
	"remove" -> none;
	_ ->
	    lists:foreach(fun(TR) ->
		ejabberd_router:route(jlib:make_jid(TU, TS, TR), UJID,
		{xmlelement, "presence", [], []})
	    end, get_resources(TU, TS))
	end
    end, [R || R <- get_resources(User, Server), Subscription =/= "remove"]).

roster_backend(Server) ->
    Modules = gen_mod:loaded_modules(Server),
    Mnesia = lists:member(mod_roster, Modules),
    Odbc = lists:member(mod_roster_odbc, Modules),
    if Mnesia -> mnesia;
    true ->
        if Odbc -> odbc;
	true -> none
	end
    end.

record_to_string(#roster{us = {User, _Server},
			jid = JID,
			name = Name,
			subscription = Subscription,
			ask = Ask,
			askmessage = AskMessage}) ->
    Username = ejabberd_odbc:escape(User),
    SJID = ejabberd_odbc:escape(jlib:jid_to_string(jlib:jid_tolower(JID))),
    Nick = ejabberd_odbc:escape(Name),
    SSubscription = case Subscription of
    both -> "B";
    to   -> "T";
    from -> "F";
    none -> "N"
    end,
    SAsk = case Ask of
    subscribe   -> "S";
    unsubscribe -> "U";
    both	   -> "B";
    out	   -> "O";
    in	   -> "I";
    none	   -> "N"
    end,
    SAskMessage = ejabberd_odbc:escape(AskMessage),
    ["'", Username, "',"
    "'", SJID, "',"
    "'", Nick, "',"
    "'", SSubscription, "',"
    "'", SAsk, "',"
    "'", SAskMessage, "',"
    "'N', '', 'item'"].

groups_to_string(#roster{us = {User, _Server},
			jid = JID,
			groups = Groups}) ->
    Username = ejabberd_odbc:escape(User),
    SJID = ejabberd_odbc:escape(jlib:jid_to_string(jlib:jid_tolower(JID))),
    %% Empty groups do not need to be converted to string to be inserted in
    %% the database
    lists:foldl(fun([], Acc) -> Acc;
		   (Group, Acc) ->
			String = ["'", Username, "',"
			"'", SJID, "',"
			"'", ejabberd_odbc:escape(Group), "'"],
			[String|Acc]
    end, [], Groups).

%% Format roster items as a list of:
%%  [{struct, [{jid, "test@localhost"},{group, "Friends"},{nick, "Nicktest"}]}]
format_roster([]) ->
    [];
format_roster(Items) ->
    format_roster(Items, []).
format_roster([], Structs) ->
    Structs;
format_roster([#roster{jid=JID, name=Nick, groups=Group,
			subscription=Subs, ask=Ask}|Items], Structs) ->
    {User,Server,_Resource} = JID,
    Struct = {struct, [{jid,lists:flatten([User,"@",Server])},
			{group, extract_group(Group)},
			{nick, Nick},
			{subscription, atom_to_list(Subs)},
			{pending, atom_to_list(Ask)}
			]},
    format_roster(Items, [Struct|Structs]).

%% Format roster items as a list of:
%%  [{struct, [{jid, "test@localhost"}, {resource, "Messenger"}, {group, "Friends"},
%%		 {nick, "Nicktest"},{show, "available"}, {status, "Currently at office"}]}]
%% Note: If user is connected several times, only keep the resource with the
%% highest non-negative priority
format_roster_with_presence([]) ->
    [];
format_roster_with_presence(Items) ->
    format_roster_with_presence(Items, []).
format_roster_with_presence([], Structs) ->
    Structs;
format_roster_with_presence([#roster{jid=JID, name=Nick, groups=Group,
				    subscription=Subs, ask=Ask}|Items], Structs) ->
    {User,Server,_R} = JID,
    Presence = case Subs of
    both -> get_presence(User, Server);
    from -> get_presence(User, Server);
    _Other -> {"", "unavailable", ""}
    end,
    {Resource, Show, Status} =
    case Presence of
    {_R, "invisible", _S} -> {"", "unavailable", ""};
    _Status -> Presence
    end,
    Struct = {struct, [{jid,lists:flatten([User,"@",Server])},
			{resource, Resource},
			{group, extract_group(Group)},
			{nick, Nick},
			{subscription, atom_to_list(Subs)},
			{pending, atom_to_list(Ask)},
			{show, Show},
			{status, Status}
			]},
    format_roster_with_presence(Items, [Struct|Structs]).

extract_group([]) -> [];
extract_group([Group|_Groups]) -> Group.

%% -----------------------------
%% Internal session handling
%% -----------------------------

%% This is inspired from ejabberd_sm.erl
get_presence(User, Server) ->
    case get_sessions(User, Server) of
    [] ->
	{"", "unavailable", ""};
    Ss ->
	Session = hd(Ss),
	if Session#session.priority >= 0 ->
	    Pid = element(2, Session#session.sid),
	    %{_User, _Resource, Show, Status} = rpc:call(node(Pid), ejabberd_c2s, get_presence, [Pid]),
	    {_User, Resource, Show, Status} = ejabberd_c2s:get_presence(Pid),
	    {Resource, Show, Status};
	true ->
	    {"", "unavailable", ""}
	    end
    end.

get_resources(User, Server) ->
    lists:map(fun(S) -> element(3, S#session.usr)
    end, get_sessions(User, Server)).

get_sessions(User, Server) ->
    LUser = jlib:nodeprep(User),
    LServer = jlib:nameprep(Server),
    case catch mnesia:dirty_index_read(session, {LUser, LServer}, #session.us) of
    {'EXIT', _Reason} -> [];
    [] -> [];
    Result -> lists:reverse(lists:keysort(#session.priority, clean_session_list(Result)))
    end.

clean_session_list(Ss) ->
    clean_session_list(lists:keysort(#session.usr, Ss), []).

clean_session_list([], Res) ->
    Res;
clean_session_list([S], Res) ->
    [S | Res];
clean_session_list([S1, S2 | Rest], Res) ->
    if
    S1#session.usr == S2#session.usr ->
	if
	S1#session.sid > S2#session.sid ->
	    clean_session_list([S1 | Rest], Res);
	true ->
	    clean_session_list([S2 | Rest], Res)
	end;
    true ->
	clean_session_list([S2 | Rest], [S1 | Res])
    end.


%% -----------------------------
%% Internal function pattern
%% -----------------------------

user_action(User, Server, Fun, OK) ->
    case ejabberd_auth:is_user_exists(User, Server) of
    true ->
	case catch Fun() of
	OK ->
	    {false, {response, [0]}};
	_ ->
	    {false, {response, [1]}}
	end;
    false ->
	{false, {response, [404]}}
    end.
