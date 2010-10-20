%%%-------------------------------------------------------------------
%%% File    : mod_admin_p1.erl
%%% Author  : Badlop / Mickael Remond / Christophe Romain
%%% Purpose : Administrative functions and commands for ProcessOne customers
%%% Created : 21 May 2008 by Badlop <badlop@process-one.net>
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
%%%-------------------------------------------------------------------

%%% @doc Administrative functions and commands for ProcessOne customers
%%%
%%% This ejabberd module defines and registers many ejabberd commands
%%% that can be used for performing administrative tasks in ejabberd.
%%%
%%% The documentation of all those commands can be read using ejabberdctl
%%% in the shell.
%%%
%%% The commands can be executed using any frontend to ejabberd commands.
%%% Currently ejabberd_xmlrpc and ejabberdctl. Using ejabberd_xmlrpc it is possible
%%% to call any ejabberd command. However using ejabberdctl not all commands
%%% can be called.

%%%  Changelog:
%%%
%%%   0.8 - 26 September 2008 - badlop
%%%	 - added patch for parameter 'Push'
%%%
%%%   0.7 - 20 August 2008 - badlop
%%%	 - module converted to ejabberd commands
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

-module(mod_admin_p1).
-author('ProcessOne').

-export([start/2, stop/1,
	 %% Erlang
	 restart_module/2,
	 %% Accounts
	 create_account/3,
	 delete_account/2,
	 change_password/3,
	 rename_account/4,
	 check_users_registration/1,
	 %% Sessions
	 get_presence/2,
	 get_resources/2,
	 %% Vcard
	 set_nickname/3,
	 %% Roster
	 add_rosteritem/6,
	 delete_rosteritem/3,
	 add_rosteritem_groups/5,
  	 del_rosteritem_groups/5,
  	 modify_rosteritem_groups/6,
	 link_contacts/6,
	 unlink_contacts/2,
	 link_contacts/7, unlink_contacts/3, % Versions with Push parameter
	 get_roster/2,
	 get_roster_with_presence/2,
	 add_contacts/3,
	 remove_contacts/3,
	 %% PubSub
	 update_status/4,
	 delete_status/3,
	 %% Transports
	 transport_register/5,
	 %% Stanza
	 send_chat/3,
	 send_message/4,
	 send_stanza/3
	]).

-include("ejabberd.hrl").
-include("ejabberd_commands.hrl").
-include("mod_roster.hrl").
-include("jlib.hrl").

-ifdef(EJABBERD1).
-record(session, {sid, usr, us, priority}). %% ejabberd 1.1.x
-else.
-record(session, {sid, usr, us, priority, info}). %% ejabberd 2.x.x
-endif.

start(_Host, _Opts) ->
    ejabberd_commands:register_commands(commands()).

stop(_Host) ->
    ejabberd_commands:unregister_commands(commands()).

%%%
%%% Register commands
%%%

commands() ->
    [
     #ejabberd_commands{name = restart_module, tags = [erlang],
			desc = "Stop an ejabberd module, reload code and start",
			module = ?MODULE, function = restart_module,
			args = [{module, string}, {host, string}],
			result = {res, rescode}},

     %% Similar to ejabberd_admin register
     #ejabberd_commands{name = create_account, tags = [accounts],
			desc = "Create an ejabberd user account",
			longdesc = "This command is similar to 'register'.",
			module = ?MODULE, function = create_account,
			args = [{user, string}, {server, string},
				{password, string}],
			result = {res, integer}},

     %% Similar to ejabberd_admin unregister
     #ejabberd_commands{name = delete_account, tags = [accounts],
			desc = "Remove an account from the server",
			longdesc = "This command is similar to 'unregister'.",
			module = ?MODULE, function = delete_account,
			args = [{user, string}, {server, string}],
			result = {res, integer}},

     #ejabberd_commands{name = rename_account, tags = [accounts],
			desc = "Change an acount name",
			longdesc = "Creates a new account "
			"and copies the roster from the old one, and updates the rosters of his contacts. "
			"Offline messages and private storage are lost.",
			module = ?MODULE, function = rename_account,
			args = [{user, string}, {server, string},
				{newuser, string}, {newserver, string}],
			result = {res, integer}},

     %% This command is also implemented in mod_admin_contrib
     #ejabberd_commands{name = change_password, tags = [accounts],
			desc = "Change the password on behalf of the given user",
			module = ?MODULE, function = change_password,
			args = [{user, string}, {server, string},
				{newpass, string}],
			result = {res, integer}},

     %% This command is also implemented in mod_admin_contrib
     #ejabberd_commands{name = set_nickname, tags = [vcard],
			desc = "Define user nickname",
			longdesc = "Set/updated nickname in the user Vcard. "
			"Other informations are unchanged.",
			module = ?MODULE, function = set_nickname,
			args = [{user, string}, {server, string}, {nick,string}],
			result = {res, integer}},

     %% This command is also implemented in mod_admin_contrib
     #ejabberd_commands{name = add_rosteritem, tags = [roster],
			desc = "Add an entry in a user's roster",
			longdesc = "Some arguments are:\n"
			" - jid: the JabberID of the user you would "
			"like to add in user roster on the server.\n"
			" - subs: the state of the roster item subscription.\n\n"
			"The allowed values of the 'subs' argument are: both, to, from or none.\n"
			" - none: presence packets are not sent between parties.\n"
			" - both: presence packets are sent in both direction.\n"
			" - to: the user sees the presence of the given JID.\n"
			" - from: the JID specified sees the user presence.\n\n"
			"ejabberd sends to the user's connected client both the roster item and the presence."
			"Don't forget that roster items should keep symmetric: "
			"when adding a roster item for a user, "
			"you have to do the symmetric roster item addition.\n\n",
			module = ?MODULE, function = add_rosteritem,
			args = [{user, string}, {server, string}, {jid, string},
				{group, string}, {nick, string}, {subs, string}],
			result = {res, integer}},

     %% This command is also implemented in mod_admin_contrib
     #ejabberd_commands{name = delete_rosteritem, tags = [roster],
			desc = "Remove a roster item from the user's roster",
			longdesc = "Roster items should be kept symmetric: "
			"when removing a roster item for a user you have to do "
			"the symmetric roster item removal. \n\n"
			"ejabberd sends to the user's connected client both the roster item removel and the presence unsubscription."
			"This mechanism bypass the standard roster approval "
			"addition mechanism and should only be used for server "
			"administration or server integration purpose.",
			module = ?MODULE, function = delete_rosteritem,
			args = [{user, string}, {server, string}, {jid, string}],
			result = {res, integer}},

     #ejabberd_commands{name = add_rosteritem_groups, tags = [roster],
			desc = "Add new groups in an existing roster item",
			longdesc = "The argument newgroups must be a string with group names separated by the character ;",
			module = ?MODULE, function = add_rosteritem_groups,
			args = [{user, string}, {server, string}, {jid, string},
				{newgroups, string}, {push, string}],
			result = {res, integer}},

     #ejabberd_commands{name = del_rosteritem_groups, tags = [roster],
			desc = "Delete groups in an existing roster item",
			longdesc = "The argument groups must be a string with group names separated by the character ;",
			module = ?MODULE, function = del_rosteritem_groups,
			args = [{user, string}, {server, string}, {jid, string},
				{groups, string}, {push, string}],
			result = {res, integer}},

     #ejabberd_commands{name = modify_rosteritem_groups, tags = [roster],
			desc = "Modify the groups of an existing roster item",
			longdesc = "The argument groups must be a string with group names separated by the character ;",
			module = ?MODULE, function = modify_rosteritem_groups,
			args = [{user, string}, {server, string}, {jid, string},
				{groups, string}, {subs, string}, {push, string}],
			result = {res, integer}},

     #ejabberd_commands{name = link_contacts, tags = [roster],
			desc = "Add a symmetrical entry in two users roster",
			longdesc = "jid1 is the JabberID of the user1 you would "
			"like to add in user2 roster on the server.\n"
			"nick1 is the nick of user1.\n"
			"group1 is the group name when adding user1 to user2 roster.\n"
			"jid2 is the JabberID of the user2 you would like to "
			"add in user1 roster on the server.\n"
			"nick2 is the nick of user2.\n"
			"group2 is the group name when adding user2 to user1 roster.\n\n"
			"This mechanism bypasses the standard roster approval "
			"addition mechanism "
			"and should only be userd for server administration or "
			"server integration purpose.",
			module = ?MODULE, function = link_contacts,
			args = [{jid1, string}, {nick1, string}, {group1, string}, {jid2, string}, {nick2, string}, {group2, string}],
			result = {res, integer}},

     #ejabberd_commands{name = unlink_contacts, tags = [roster],
			desc = "Remove a symmetrical entry in two users roster",
			longdesc = "jid1 is the JabberID of the user1.\n"
			"jid2 is the JabberID of the user2.\n\n"
			"This mechanism bypass the standard roster approval "
			"addition mechanism "
			"and should only be used for server administration or "
			"server integration purpose.",
			module = ?MODULE, function = unlink_contacts,
			args = [{jid1, string}, {jid2, string}],
			result = {res, integer}},

     %% TODO: test
     %% This command is not supported by ejabberdctl
     #ejabberd_commands{name = add_contacts, tags = [roster],
			desc = "Call add_rosteritem with subscription \"both\" "
			"for a given list of contacts",
			module = ?MODULE, function = add_contacts,
			args = [{user, string},
				{server, string},
				{contacts, {list,
					    {contact, {tuple, [
							       {jid, string},
							       {group, string},
							       {nick, string}
							      ]}}
					   }}
			       ],
			result = {res, integer}},
     %% xmlrpc:call({127, 0, 0, 1}, 4560, "/", {call, add_contacts, [{struct,
     %%  [{user, "badlop"},
     %%   {server, "localhost"},
     %%   {contacts, {array, [{struct, [
     %%    {contact, {array, [{struct, [
     %%     {group, "Friends"},
     %%     {jid, "tom@localhost"},
     %%     {nick, "Tom"}
     %%    ]}]}}
     %%   ]}]}}
     %%  ]
     %% }]}).

     %% TODO: test
     %% This command is not supported by ejabberdctl
     #ejabberd_commands{name = remove_contacts, tags = [roster],
			desc = "Call del_rosteritem for a list of contacts",
			module = ?MODULE, function = remove_contacts,
			args = [{user, string},
				{server, string},
				{contacts, {list,
					    {jid, string}
					   }}
			       ],
			result = {res, integer}},
     %% xmlrpc:call({127, 0, 0, 1}, 4560, "/", {call, remove_contacts, [{struct,
     %%  [{user, "badlop"},
     %%   {server, "localhost"},
     %%   {contacts, {array, [{struct, [
     %%    {jid, "tom@localhost"}
     %%   ]}]}}
     %%  ]
     %% }]}).

     %% TODO: test
     %% This command is not supported by ejabberdctl
     #ejabberd_commands{name = check_users_registration, tags = [roster],
			desc = "List registration status for a list of users",
			module = ?MODULE, function = check_users_registration,
			args = [{users, {list,
					 {auser, {tuple, [
							  {user, string},
							  {server, string}
							 ]}}
					}}
			       ],
			result = {users, {list,
					  {auser, {tuple, [
							   {user, string},
							   {server, string},
							   {status, integer}
							  ]}}
					 }}},
     %% xmlrpc:call({127, 0, 0, 1}, 4560, "/", {call, check_users_registration, [{struct,
     %%  [{users, {array, [{struct, [
     %%    {auser, {array, [{struct, [
     %%     {user, "badlop"},
     %%     {server, "localhost"}
     %%    ]}]}}
     %%   ]}]}}]
     %%  }]}).

     %% This command is also implemented in mod_admin_contrib
     #ejabberd_commands{name = get_roster, tags = [roster],
			desc = "Retrieve the roster for a given user",
			longdesc = "Returns a list of the contacts in a user "
			"roster.\n\n"
			"Also returns the state of the contact subscription. "
			"Subscription can be either "
			" \"none\", \"from\", \"to\", \"both\". "
			"Pending can be \"in\", \"out\" or \"none\".",
			module = ?MODULE, function = get_roster,
			args = [{user, string}, {server, string}],
			result = {contacts, {list, {contact, {tuple, [{jid, string}, {groups, {list, {group, string}}},
								      {nick, string}, {subscription, string}, {pending, string}]}}}}},

     #ejabberd_commands{name = get_roster_with_presence, tags = [roster],
			desc = "Retrieve the roster for a given user including "
			"presence information",
			longdesc = "The 'show' value contains the user presence. "
			"It can take limited values:\n"
			" - available\n"
			" - chat (Free for chat)\n"
			" - away\n"
			" - dnd (Do not disturb)\n"
			" - xa (Not available, extended away)\n"
			" - unavailable (Not connected)\n\n"
			"'status' is a free text defined by the user client.\n\n"
			"Also returns the state of the contact subscription. "
			"Subscription can be either "
			"\"none\", \"from\", \"to\", \"both\". "
			"Pending can be \"in\", \"out\" or \"none\".\n\n"
			"Note: If user is connected several times, only keep the"
			" resource with the highest non-negative priority.",
			module = ?MODULE, function = get_roster_with_presence,
			args = [{user, string}, {server, string}],
			result = {contacts, {list, {contact, {tuple, [{jid, string}, {resource, string}, {group, string}, {nick, string}, {subscription, string}, {pending, string}, {show, string}, {status, string}]}}}}},

     #ejabberd_commands{name = get_presence, tags = [session],
			desc = "Retrieve the resource with highest priority, "
			"and its presence (show and status message) for a given "
			"user.",
			longdesc = "The 'jid' value contains the user jid with "
			"resource.\n"
			"The 'show' value contains the user presence flag. "
			"It can take limited values:\n"
			" - available\n"
			" - chat (Free for chat)\n"
			" - away\n"
			" - dnd (Do not disturb)\n"
			" - xa (Not available, extended away)\n"
			" - unavailable (Not connected)\n\n"
			"'status' is a free text defined by the user client.",
			module = ?MODULE, function = get_presence,
			args = [{user, string}, {server, string}],
			result = {presence, {tuple, [{jid, string},
						     {show, string},
						     {status, string}]}}},

     #ejabberd_commands{name = get_resources, tags = [session],
			desc = "Get all available resources for a given user",
			module = ?MODULE, function = get_resources,
			args = [{user, string}, {server, string}],
			result = {resources, {list, {resource, string}}}},

     %% PubSub
     #ejabberd_commands{name = update_status, tags = [pubsub],
			desc = "Update the status on behalf of a user",
			longdesc =
			"jid: the JabberID of the user. Example: user@domain.\n\n"
			"node: the reference of the node to publish on.\n"
			"Example: http://process-one.net/protocol/availability\n\n"
			"itemid: the reference of the item (in our case profile ID).\n\n"
			"payload: the payload of the publish operation in XML.\n"
			"The string has to be properly escaped to comply with XML formalism of XML RPC.",
			module = ?MODULE, function = update_status,
			args = [{jid, string}, {node, string}, {itemid, string}, {payload, string}],
			result = {res, string}},

     #ejabberd_commands{name = delete_status, tags = [pubsub],
			desc = "Delete the status on behalf of a user",
			longdesc =
			"jid: the JabberID of the user. Example: user@domain.\n\n"
			"node: the reference of the node to publish on.\n"
			"Example: http://process-one.net/protocol/availability\n\n"
			"itemid: the reference of the item (in our case profile ID).",
			module = ?MODULE, function = delete_status,
			args = [{jid, string}, {node, string}, {itemid, string}],
			result = {res, string}},

     #ejabberd_commands{name = transport_register, tags = [transports],
			desc = "Register a user in a transport",
			module = ?MODULE, function = transport_register,
			args = [{host, string}, {transport, string},
				{jidstring, string}, {username, string}, {password, string}],
			result = {res, string}},

     %% Similar to mod_admin_contrib send_message which sends a headline
     #ejabberd_commands{name = send_chat, tags = [stanza],
			desc = "Send chat message to a given user",
			module = ?MODULE, function = send_chat,
			args = [{from, string}, {to, string}, {body, string}],
			result = {res, integer}},

     #ejabberd_commands{name = send_message, tags = [stanza],
			desc = "Send normal message to a given user",
			module = ?MODULE, function = send_message,
			args = [{from, string}, {to, string},
				{subject, string}, {body, string}],
			result = {res, integer}},

     #ejabberd_commands{name = send_stanza, tags = [stanza],
			desc = "Send stanza to a given user",
			longdesc = "If Stanza contains a \"from\" field, "
			"then it overrides the passed from argument."
			"If Stanza contains a \"to\" field, then it overrides "
			"the passed to argument.",
			module = ?MODULE, function = send_stanza,
			args = [{user, string}, {server, string},
				{stanza, string}],
			result = {res, integer}}
    ].


%%%
%%% Erlang
%%%

restart_module(ModuleString, Host) ->
    Module = list_to_atom(ModuleString),
    List = gen_mod:loaded_modules_with_opts(Host),
    Opts = case lists:keysearch(Module,1, List) of
	       {value, {_, O}} -> O;
	       _ -> []
	   end,
    gen_mod:stop_module(Host, Module),
    code:delete(Module),
    code:purge(Module),
    gen_mod:start_module(Host, Module, Opts),
    ok.


%%%
%%% Accounts
%%%

create_account(U, S, P) ->
    case ejabberd_auth:try_register(U, S, P) of
	{atomic, ok} ->
	    0;
	{atomic, exists} ->
	    409;
	_ ->
	    1
    end.

delete_account(U, S) ->
    Fun = fun() -> ejabberd_auth:remove_user(U, S) end,
    user_action(U, S, Fun, ok).

change_password(U, S, P) ->
    Fun = fun() -> ejabberd_auth:set_password(U, S, P) end,
    user_action(U, S, Fun, ok).

rename_account(U, S, NU, NS) ->
    case ejabberd_auth:is_user_exists(U, S) of
	true ->
	    case ejabberd_auth:get_password(U, S) of
		false ->
		    1;
		Password ->
		    case ejabberd_auth:try_register(NU, NS, Password) of
			{atomic, ok} ->
			    OldJID = jlib:jid_to_string({U, S, ""}),
			    NewJID = jlib:jid_to_string({NU, NS, ""}),
			    Roster = get_roster2(U, S),
			    lists:foreach(fun(#roster{jid={RU, RS, RE}, name=Nick, groups=Groups}) ->
						  NewGroup = extract_group(Groups),
						  {NewNick, Group} = case lists:filter(fun(#roster{jid={PU, PS, _}}) ->
											       (PU == U) and (PS == S)
										       end, get_roster2(RU, RS)) of
									 [#roster{name=OldNick, groups=OldGroups}|_] -> {OldNick, extract_group(OldGroups)};
									 [] -> {NU, []}
								     end,
						  JIDStr = jlib:jid_to_string({RU, RS, RE}),
						  link_contacts2(NewJID, NewNick, NewGroup, JIDStr, Nick, Group, true),
						  unlink_contacts2(OldJID, JIDStr, true)
					  end, Roster),
			    ejabberd_auth:remove_user(U, S),
			    0;
			{atomic, exists} ->
			    409;
			_ ->
			    1
		    end
	    end;
	false ->
	    404
    end.


%%%
%%% Sessions
%%%

get_presence(U, S) ->
    case ejabberd_auth:is_user_exists(U, S) of
	true ->
	    {Resource, Show, Status} = get_presence2(U, S),
	    FullJID = case Resource of
			  [] ->
			      lists:flatten([U,"@",S]);
			  _ ->
			      lists:flatten([U,"@",S,"/",Resource])
		      end,
	    {FullJID, Show, Status};
	false ->
	    404
    end.

get_resources(U, S) ->
    case ejabberd_auth:is_user_exists(U, S) of
	true ->
	    get_resources2(U, S);
	false ->
	    404
    end.


%%%
%%% Vcard
%%%

set_nickname(U, S, N) ->
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
    user_action(U, S, Fun, ok).


%%%
%%% Roster
%%%

add_rosteritem(U, S, JID, G, N, Subs) ->
    add_rosteritem(U, S, JID, G, N, Subs, true).

add_rosteritem(U, S, JID, G, N, Subs, Push) ->
    Fun = fun() -> add_rosteritem2(U, S, JID, N, G, Subs, Push) end,
    user_action(U, S, Fun, {atomic, ok}).

link_contacts(JID1, Nick1, Group1, JID2, Nick2, Group2) ->
    link_contacts(JID1, Nick1, Group1, JID2, Nick2, Group2, true).

link_contacts(JID1, Nick1, Group1, JID2, Nick2, Group2, Push) ->
    {U1, S1, _} = jlib:jid_tolower(jlib:string_to_jid(JID1)),
    {U2, S2, _} = jlib:jid_tolower(jlib:string_to_jid(JID2)),
    case {ejabberd_auth:is_user_exists(U1, S1), ejabberd_auth:is_user_exists(U2, S2)} of
	{true, true} ->
	    case link_contacts2(JID1, Nick1, Group1, JID2, Nick2, Group2, Push) of
		{atomic, ok} ->
		    0;
		_ ->
		    1
	    end;
	_ ->
	    404
    end.

delete_rosteritem(U, S, JID) ->
    Fun = fun() -> del_rosteritem(U, S, JID) end,
    user_action(U, S, Fun, {atomic, ok}).

unlink_contacts(JID1, JID2) ->
    unlink_contacts(JID1, JID2, true).

unlink_contacts(JID1, JID2, Push) ->
    {U1, S1, _} = jlib:jid_tolower(jlib:string_to_jid(JID1)),
    {U2, S2, _} = jlib:jid_tolower(jlib:string_to_jid(JID2)),
    case {ejabberd_auth:is_user_exists(U1, S1), ejabberd_auth:is_user_exists(U2, S2)} of
	{true, true} ->
	    case unlink_contacts2(JID1, JID2, Push) of
		{atomic, ok} ->
		    0;
		_ ->
		    1
	    end;
	_ ->
	    404
    end.

get_roster(U, S) ->
    case ejabberd_auth:is_user_exists(U, S) of
	true ->
	    format_roster(get_roster2(U, S));
	false ->
	    404
    end.

get_roster_with_presence(U, S) ->
    case ejabberd_auth:is_user_exists(U, S) of
	true ->
	    format_roster_with_presence(get_roster2(U, S));
	false ->
	    404
    end.

add_contacts(U, S, Contacts) ->
    case ejabberd_auth:is_user_exists(U, S) of
	true ->
	    JID1 = jlib:jid_to_string({U, S, ""}),
	    lists:foldl(fun({JID2, Group, Nick}, Acc) ->
				{PU, PS, _} = jlib:jid_tolower(jlib:string_to_jid(JID2)),
				case ejabberd_auth:is_user_exists(PU, PS) of
				    true ->
					case link_contacts2(JID1, "", Group, JID2, Nick, Group, true) of
					    {atomic, ok} -> Acc;
					    _ -> 1
					end;
				    false ->
					Acc
				end
			end, 0, Contacts);
	false ->
	    404
    end.

remove_contacts(U, S, Contacts) ->
    case ejabberd_auth:is_user_exists(U, S) of
	true ->
	    JID1 = jlib:jid_to_string({U, S, ""}),
	    lists:foldl(fun(JID2, Acc) ->
				{PU, PS, _} = jlib:jid_tolower(jlib:string_to_jid(JID2)),
				case ejabberd_auth:is_user_exists(PU, PS) of
				    true ->
					case unlink_contacts2(JID1, JID2, true) of
					    {atomic, ok} -> Acc;
					    _ -> 1
					end;
				    false ->
					Acc
				end
			end, 0, Contacts);
	false ->
	    404
    end.

check_users_registration(Users) ->
    lists:map(fun({U, S}) ->
		      Registered = case ejabberd_auth:is_user_exists(U, S) of
				       true -> 1;
				       false -> 0
				   end,
		      {U, S, Registered}
	      end, Users).

%%%
%%% Groups of Roster Item
%%%

add_rosteritem_groups(User, Server, JID, NewGroupsString, PushString) ->
    {U1, S1, _} = jlib:jid_tolower(jlib:string_to_jid(JID)),
    NewGroups = string:tokens(NewGroupsString, ";"),
    Push = list_to_atom(PushString),
    case {ejabberd_auth:is_user_exists(U1, S1), ejabberd_auth:is_user_exists(User, Server)} of
    {true, true} ->
	case add_rosteritem_groups2(User, Server, JID, NewGroups, Push) of
	ok ->
	    {false, {response, [0]}};
	Error ->
	    ?INFO_MSG("Error found: ~n~p", [Error]),
	    {false, {response, [1]}}
	end;
    _ ->
	{false, {response, [404]}}
    end.

del_rosteritem_groups(User, Server, JID, NewGroupsString, PushString) ->
    {U1, S1, _} = jlib:jid_tolower(jlib:string_to_jid(JID)),
    NewGroups = string:tokens(NewGroupsString, ";"),
    Push = list_to_atom(PushString),
    case {ejabberd_auth:is_user_exists(U1, S1), ejabberd_auth:is_user_exists(User, Server)} of
    {true, true} ->
	case del_rosteritem_groups2(User, Server, JID, NewGroups, Push) of
	ok ->
	    {false, {response, [0]}};
	Error ->
	    ?INFO_MSG("Error found: ~n~p", [Error]),
	    {false, {response, [1]}}
	end;
    _ ->
	{false, {response, [404]}}
    end.

modify_rosteritem_groups(User, Server, JID, NewGroupsString, SubsString, PushString) ->
    Nick = "", %% That information will not be used, anyway
    Subs = list_to_atom(SubsString),
    {U1, S1, _} = jlib:jid_tolower(jlib:string_to_jid(JID)),
    NewGroups = string:tokens(NewGroupsString, ";"),
    Push = list_to_atom(PushString),
    case {ejabberd_auth:is_user_exists(U1, S1), ejabberd_auth:is_user_exists(User, Server)} of
    {true, true} ->
	case modify_rosteritem_groups2(User, Server, JID, NewGroups, Push, Nick, Subs) of
	ok ->
	    {false, {response, [0]}};
	Error ->
	    ?INFO_MSG("Error found: ~n~p", [Error]),
	    {false, {response, [1]}}
	end;
    _ ->
	{false, {response, [404]}}
    end.

add_rosteritem_groups2(User, Server, JID, NewGroups, Push) ->
    GroupsFun =
	fun(Groups) ->
		lists:usort(NewGroups ++ Groups)
	end,
    change_rosteritem_group(User, Server, JID, GroupsFun, Push).

del_rosteritem_groups2(User, Server, JID, NewGroups, Push) ->
    GroupsFun =
	fun(Groups) ->
		Groups -- NewGroups
	end,
    change_rosteritem_group(User, Server, JID, GroupsFun, Push).

modify_rosteritem_groups2(User, Server, JID2, NewGroups, Push, Nick, Subs) when NewGroups == [] ->
    JID1 = jlib:jid_to_string(jlib:make_jid(User, Server, "")),
    case unlink_contacts(JID1, JID2) of
	{atomic, ok} ->
	    ok;
	Error ->
	    Error
    end;
modify_rosteritem_groups2(User, Server, JID, NewGroups, Push, Nick, Subs) ->
    GroupsFun =
	fun(_Groups) ->
		NewGroups
	end,
    change_rosteritem_group(User, Server, JID, GroupsFun, Push, NewGroups, Nick, Subs).

change_rosteritem_group(User, Server, JID, GroupsFun, Push) ->
    change_rosteritem_group(User, Server, JID, GroupsFun, Push, [], "", "both").

change_rosteritem_group(User, Server, JID, GroupsFun, Push, NewGroups, Nick, Subs) ->
    {RU, RS, _} = jlib:jid_tolower(jlib:string_to_jid(JID)),
    LJID = {RU,RS,[]},
    LUser = jlib:nodeprep(User),
    LServer = jlib:nameprep(Server),
    Result =
	case roster_backend(LServer) of
	    mnesia ->
		mnesia:transaction(
		  fun() ->
			  case mnesia:read({roster, {LUser, LServer, LJID}}) of
			      [#roster{} = Roster] ->
				  NewGroups2 = GroupsFun(Roster#roster.groups),
				  NewRoster = Roster#roster{groups = NewGroups2},
				  mnesia:write(NewRoster),
				  {ok, NewRoster#roster.name,
				   NewRoster#roster.subscription,
				   NewGroups2};
			      _ ->
				  not_in_roster
			  end
		  end);
	    odbc ->
		ejabberd_odbc:sql_transaction(
		  LServer,
		  fun() ->
			  Username = ejabberd_odbc:escape(User),
			  SJID = ejabberd_odbc:escape(jlib:jid_to_string(LJID)),
			  case ejabberd_odbc:sql_query_t(
				 ["select nick, subscription from rosterusers "
				  "      where username='", Username, "' "
				  "        and jid='", SJID, "';"]) of
			      {selected, ["nick", "subscription"],
			       [{Name, SSubscription}]} ->
				  Subscription =
				      case SSubscription of
					  "B" -> both;
					  "T" -> to;
					  "F" -> from;
					  _ -> none
				      end,
				  Groups =
				      case odbc_queries:get_roster_groups(
					     LServer, Username, SJID) of
					  {selected, ["grp"], JGrps}
					  when is_list(JGrps) ->
					      [JGrp || {JGrp} <- JGrps];
					  _ ->
					      []
				      end,
				  NewGroups2 = GroupsFun(Groups),
				  ejabberd_odbc:sql_query_t(
				    ["delete from rostergroups "
				     "      where username='", Username, "' "
				     "        and jid='", SJID, "';"]),
				  lists:foreach(
				    fun(Group) ->
					    ejabberd_odbc:sql_query_t(
					      ["insert into rostergroups("
					       "              username, jid, grp) "
					       " values ('", Username, "',"
					       "'", SJID, "',"
					       "'", ejabberd_odbc:escape(Group), "');"])
				    end,
				    NewGroups2),
				  {ok, Name, Subscription, NewGroups2};
			      _ ->
				  not_in_roster
			  end
		  end);
	    none ->
		%% Apollo change: force roster push anyway with success
		{atomic, {ok, Nick, Subs, NewGroups}}
	end,
    case {Result, Push} of
	{{atomic, {ok, Name, Subscription, NewGroups3}}, true} ->
	    roster_push(User, Server, JID,
			Name, atom_to_list(Subscription), NewGroups3),
	    ok;
	{{atomic, {ok, _Name, _Subscription, _NewGroups3}}, false} -> ok;
	{{atomic, not_in_roster}, _} -> not_in_roster;
	Error -> {error, Error}
    end.

%%%
%%% PubSub
%%%

update_status(JidString, NodeString, Itemid, PayloadString) ->
    Publisher = jlib:string_to_jid(JidString),
    Host = jlib:jid_tolower(jlib:jid_remove_resource(Publisher)),
    ServerHost = Publisher#jid.lserver,
    Node = mod_pubsub_on:string_to_node(NodeString),
    Payload = [xml_stream:parse_element(PayloadString)],
    ?DEBUG("PayloadString: ~n~p~nPayload elements: ~n~p", [PayloadString, Payload]),
    case mod_pubsub_on:publish_item_nothook(Host, ServerHost, Node, Publisher, Itemid, Payload) of
	{result, _} ->
	    "OK";
	{error, {xmlelement, _, _, _} = XmlEl} ->
	    "ERROR: " ++ xml:element_to_string(XmlEl);
	{error, ErrorAtom} when is_atom(ErrorAtom) ->
	    "ERROR: " ++ atom_to_list(ErrorAtom);
	{error, ErrorString} when is_list(ErrorString) ->
	    "ERROR: " ++ ErrorString
    end.

delete_status(JidString, NodeString, Itemid) ->
    Publisher = jlib:string_to_jid(JidString),
    Host = jlib:jid_tolower(jlib:jid_remove_resource(Publisher)),
    Node = mod_pubsub_on:string_to_node(NodeString),
    case mod_pubsub_on:delete_item_nothook(Host, Node, Publisher, Itemid, true) of
	{result, _} ->
	    "OK";
	{error, {xmlelement, _, _, _} = XmlEl} ->
	    "ERROR: " ++ xml:element_to_string(XmlEl);
	{error, ErrorAtom} when is_atom(ErrorAtom) ->
	    "ERROR: " ++ atom_to_list(ErrorAtom);
	{error, ErrorString} when is_list(ErrorString) ->
	    "ERROR: " ++ ErrorString
    end.

transport_register(Host, TransportString, JIDString, Username, Password) ->
    TransportAtom = list_to_atom(TransportString),
    case {lists:member(Host, ?MYHOSTS), jlib:string_to_jid(JIDString)} of
	{true, JID} when is_record(JID, jid) ->
	    case catch gen_transport:register(Host, TransportAtom, JIDString,
					      Username, Password) of
		ok ->
		    "OK";
		{error, Reason} ->
		    "ERROR: " ++ atom_to_list(Reason);
		{'EXIT', {timeout,_}} ->
		    "ERROR: timed_out";
		{'EXIT', _} ->
		    "ERROR: unexpected_error"
	    end;
	{false, _} ->
	    "ERROR: unknown_host";
	_ ->
	    "ERROR: bad_jid"
    end.

%%%
%%% Stanza
%%%

send_chat(FromJID, ToJID, Msg) ->
    From = jlib:string_to_jid(FromJID),
    To = jlib:string_to_jid(ToJID),
    Stanza = {xmlelement, "message", [{"type", "chat"}],
	      [{xmlelement, "body", [], [{xmlcdata, Msg}]}]},
    ejabberd_router:route(From, To, Stanza),
    0.

send_message(FromJID, ToJID, Sub, Msg) ->
    From = jlib:string_to_jid(FromJID),
    To = jlib:string_to_jid(ToJID),
    Stanza = {xmlelement, "message", [{"type", "normal"}],
	      [{xmlelement, "subject", [], [{xmlcdata, Sub}]},
	       {xmlelement, "body", [], [{xmlcdata, Msg}]}]},
    ejabberd_router:route(From, To, Stanza),
    0.

send_stanza(FromJID, ToJID, StanzaStr) ->
    case xml_stream:parse_element(StanzaStr) of
	{error, _} ->
	    1;
	Stanza ->
	    {xmlelement, _, Attrs, _} = Stanza,
	    From = jlib:string_to_jid(proplists:get_value("from", Attrs, FromJID)),
	    To = jlib:string_to_jid(proplists:get_value("to", Attrs, ToJID)),
	    ejabberd_router:route(From, To, Stanza),
	    0
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Internal functions

%% -----------------------------
%% Internal roster handling
%% -----------------------------

get_roster2(User, Server) ->
    LUser = jlib:nodeprep(User),
    LServer = jlib:nameprep(Server),
    case roster_backend(LServer) of
	mnesia -> mod_roster:get_user_roster([], {LUser, LServer});
	odbc -> mod_roster_odbc:get_user_roster([], {LUser, LServer})
    end.

add_rosteritem2(User, Server, JID, Nick, Group, Subscription, Push) ->
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
                                                                  ejabberd_odbc:sql_query_t(
                                                                       odbc_queries:update_roster_sql(
                                                                           Username, SJID, ItemVals, ItemGroups));
							       _ ->
								   already_added
							   end
						   end) of
		    {atomic, already_added} -> {atomic, already_added};
		    {atomic, _} -> {atomic, ok};
		    Error -> Error
		end
	end,
    case {Result, Push} of
	{{atomic, already_added}, _} -> ok;  %% No need for roster push
	{{atomic, ok}, true} -> roster_push(User, Server, JID, Nick, Subscription, Groups);
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
		     end
	     end,
    case {Result, Push} of
	{{atomic, ok}, true} -> roster_push(User, Server, JID, "", "remove", []);
	{{atomic, ok}, false} -> ok;
	_ -> error
    end,
    Result.

link_contacts2(JID1, Nick1, Group1, JID2, Nick2, Group2, Push) ->
    {U1, S1, _} = jlib:jid_tolower(jlib:string_to_jid(JID1)),
    {U2, S2, _} = jlib:jid_tolower(jlib:string_to_jid(JID2)),
    case add_rosteritem2(U1, S1, JID2, Nick2, Group1, "both", Push) of
	{atomic, ok} -> add_rosteritem2(U2, S2, JID1, Nick1, Group2, "both", Push);
	Error -> Error
    end.

unlink_contacts2(JID1, JID2, Push) ->
    {U1, S1, _} = jlib:jid_tolower(jlib:string_to_jid(JID1)),
    {U2, S2, _} = jlib:jid_tolower(jlib:string_to_jid(JID2)),
    case del_rosteritem(U1, S1, JID2, Push) of
	{atomic, ok} -> del_rosteritem(U2, S2, JID1, Push);
	Error -> Error
    end.

roster_push(User, Server, JID, Nick, Subscription, Groups) ->
    LJID = jlib:make_jid(User, Server, ""),
    TJID = jlib:string_to_jid(JID),
    {TU, TS, _} = jlib:jid_tolower(TJID),
    Presence =
        {xmlelement, "presence",
         [{"type",
           case Subscription of
               "remove" -> "unsubscribed";
               "none" -> "unsubscribe";
               "both" -> "subscribed";
               _ -> "subscribe"
           end}], []},
    ItemAttrs =
        case Nick of
            "" -> [{"jid", JID}, {"subscription", Subscription}];
            _ -> [{"jid", JID}, {"name", Nick}, {"subscription", Subscription}]
        end,
    ItemGroups =
        lists:map(fun(G) ->
                          {xmlelement, "group", [], [{xmlcdata, G}]}
                  end, Groups),
    Result =
        jlib:iq_to_xml(
          #iq{type = set, xmlns = ?NS_ROSTER, id = "push",
              lang = "langxmlrpc-en",
              sub_el = [{xmlelement, "query", [{"xmlns", ?NS_ROSTER}],
                       [{xmlelement, "item", ItemAttrs, ItemGroups}]}]}),
   %% ejabberd_router:route(TJID, LJID, Presence),
   %% ejabberd_router:route(LJID, LJID, Result),
   lists:foreach(
     fun(Resource) ->
            UJID = jlib:make_jid(User, Server, Resource),
            ejabberd_router:route(TJID, UJID, Presence),
            ejabberd_router:route(UJID, UJID, Result),
            case Subscription of
                "remove" -> none;
                _ ->
                    lists:foreach(
                      fun(TR) ->
                              ejabberd_router:route(
                                jlib:make_jid(TU, TS, TR), UJID,
                                {xmlelement, "presence", [], []})
                      end, get_resources(TU, TS))
            end
     end, [R || R <- get_resources(User, Server)]).

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
    Struct = {lists:flatten([User,"@",Server]),
	      Group,
	      Nick,
	      atom_to_list(Subs),
	      atom_to_list(Ask)
	     },
    format_roster(Items, [Struct|Structs]).

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
		   both -> get_presence2(User, Server);
		   from -> get_presence2(User, Server);
		   _Other -> {"", "unavailable", ""}
	       end,
    {Resource, Show, Status} =
	case Presence of
	    {_R, "invisible", _S} -> {"", "unavailable", ""};
	    _Status -> Presence
	end,
    Struct = {lists:flatten([User,"@",Server]),
	      Resource,
	      extract_group(Group),
	      Nick,
	      atom_to_list(Subs),
	      atom_to_list(Ask),
	      Show,
	      Status
	     },
    format_roster_with_presence(Items, [Struct|Structs]).

extract_group([]) -> [];
%extract_group([Group|_Groups]) -> Group.
extract_group(Groups) -> string:join(Groups, ";").

extract_groups([]) -> [];
%extract_groups([Group|_Groups]) -> Group.
extract_groups(Groups) -> {list, Groups}.

%% -----------------------------
%% Internal session handling
%% -----------------------------

%% This is inspired from ejabberd_sm.erl
get_presence2(User, Server) ->
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

get_resources2(User, Server) ->
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
		    0;
		_ ->
		    1
	    end;
	false ->
	    404
    end.
