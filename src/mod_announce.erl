%%%----------------------------------------------------------------------
%%% File    : mod_announce.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Manage announce messages
%%% Created : 11 Aug 2003 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2012   ProcessOne
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

%%% Implements a small subset of XEP-0133: Service Administration
%%% Version 1.1 (2005-08-19)

-module(mod_announce).
-author('alexey@process-one.net').

-behaviour(gen_mod).

-export([start/2,
	 init/0,
	 stop/1,
	 announce/3,
	 send_motd/1,
	 disco_identity/5,
	 disco_features/5,
	 disco_items/5,
	 send_announcement_to_all/3,
	 announce_commands/4,
	 announce_items/4]).

-include("ejabberd.hrl").
-include("jlib.hrl").
-include("adhoc.hrl").

-record(motd, {server, packet}).
-record(motd_users, {us, dummy = []}).

-define(PROCNAME, ejabberd_announce).

-define(NS_ADMINL(Sub), ["http:","jabber.org","protocol","admin", Sub]).
tokenize(Node) -> string:tokens(Node, "/#").

start(Host, Opts) ->
    case gen_mod:db_type(Opts) of
        mnesia ->
            mnesia:create_table(motd,
                                [{disc_copies, [node()]},
                                 {attributes,
                                  record_info(fields, motd)}]),
            mnesia:create_table(motd_users,
                                [{disc_copies, [node()]},
                                 {attributes,
                                  record_info(fields, motd_users)}]),
            update_tables();
        _ ->
            ok
    end,
    ejabberd_hooks:add(local_send_to_resource_hook, Host,
		       ?MODULE, announce, 50),
    ejabberd_hooks:add(disco_local_identity, Host, ?MODULE, disco_identity, 50),
    ejabberd_hooks:add(disco_local_features, Host, ?MODULE, disco_features, 50),
    ejabberd_hooks:add(disco_local_items, Host, ?MODULE, disco_items, 50),
    ejabberd_hooks:add(adhoc_local_items, Host, ?MODULE, announce_items, 50),
    ejabberd_hooks:add(adhoc_local_commands, Host, ?MODULE, announce_commands, 50),
    ejabberd_hooks:add(user_available_hook, Host,
		       ?MODULE, send_motd, 50),
    register(gen_mod:get_module_proc(Host, ?PROCNAME),
	     proc_lib:spawn(?MODULE, init, [])).

init() ->
    loop().

loop() ->
    receive
	{announce_all, From, To, Packet} ->
	    announce_all(From, To, Packet),
	    loop();
	{announce_all_hosts_all, From, To, Packet} ->
	    announce_all_hosts_all(From, To, Packet),
	    loop();
	{announce_online, From, To, Packet} ->
	    announce_online(From, To, Packet),
	    loop();
	{announce_all_hosts_online, From, To, Packet} ->
	    announce_all_hosts_online(From, To, Packet),
	    loop();
	{announce_motd, From, To, Packet} ->
	    announce_motd(From, To, Packet),
	    loop();
	{announce_all_hosts_motd, From, To, Packet} ->
	    announce_all_hosts_motd(From, To, Packet),
	    loop();
	{announce_motd_update, From, To, Packet} ->
	    announce_motd_update(From, To, Packet),
	    loop();
	{announce_all_hosts_motd_update, From, To, Packet} ->
	    announce_all_hosts_motd_update(From, To, Packet),
	    loop();
	{announce_motd_delete, From, To, Packet} ->
	    announce_motd_delete(From, To, Packet),
	    loop();
	{announce_all_hosts_motd_delete, From, To, Packet} ->
	    announce_all_hosts_motd_delete(From, To, Packet),
	    loop();
	_ ->
	    loop()
    end.

stop(Host) ->
    ejabberd_hooks:delete(adhoc_local_commands, Host, ?MODULE, announce_commands, 50),
    ejabberd_hooks:delete(adhoc_local_items, Host, ?MODULE, announce_items, 50),
    ejabberd_hooks:delete(disco_local_identity, Host, ?MODULE, disco_identity, 50),
    ejabberd_hooks:delete(disco_local_features, Host, ?MODULE, disco_features, 50),
    ejabberd_hooks:delete(disco_local_items, Host, ?MODULE, disco_items, 50),
    ejabberd_hooks:delete(local_send_to_resource_hook, Host,
			  ?MODULE, announce, 50),
    ejabberd_hooks:delete(user_available_hook, Host,
			  ?MODULE, send_motd, 50),
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    exit(whereis(Proc), stop),
    {wait, Proc}.

%% Announcing via messages to a custom resource
announce(From, To, Packet) ->
    case To of
	#jid{luser = "", lresource = Res} ->
	    {xmlelement, Name, _Attrs, _Els} = Packet,
	    Proc = gen_mod:get_module_proc(To#jid.lserver, ?PROCNAME),
	    case {Res, Name} of
		{"announce/all", "message"} ->
		    Proc ! {announce_all, From, To, Packet},
		    stop;
		{"announce/all-hosts/all", "message"} ->
		    Proc ! {announce_all_hosts_all, From, To, Packet},
		    stop;
		{"announce/online", "message"} ->
		    Proc ! {announce_online, From, To, Packet},
		    stop;
		{"announce/all-hosts/online", "message"} ->
		    Proc ! {announce_all_hosts_online, From, To, Packet},
		    stop;
		{"announce/motd", "message"} ->
		    Proc ! {announce_motd, From, To, Packet},
		    stop;
		{"announce/all-hosts/motd", "message"} ->
		    Proc ! {announce_all_hosts_motd, From, To, Packet},
		    stop;
		{"announce/motd/update", "message"} ->
		    Proc ! {announce_motd_update, From, To, Packet},
		    stop;
		{"announce/all-hosts/motd/update", "message"} ->
		    Proc ! {announce_all_hosts_motd_update, From, To, Packet},
		    stop;
		{"announce/motd/delete", "message"} ->
		    Proc ! {announce_motd_delete, From, To, Packet},
		    stop;
		{"announce/all-hosts/motd/delete", "message"} ->
		    Proc ! {announce_all_hosts_motd_delete, From, To, Packet},
		    stop;
		_ ->
		    ok
	    end;
	_ ->
	    ok
    end.

%%-------------------------------------------------------------------------
%% Announcing via ad-hoc commands
-define(INFO_COMMAND(Lang, Node),
        [{xmlelement, "identity",
	  [{"category", "automation"},
	   {"type", "command-node"},
	   {"name", get_title(Lang, Node)}], []}]).

disco_identity(Acc, _From, _To, Node, Lang) ->
    LNode = tokenize(Node),
    case LNode of
	?NS_ADMINL("announce") ->
	    ?INFO_COMMAND(Lang, Node);
	?NS_ADMINL("announce-allhosts") ->
	    ?INFO_COMMAND(Lang, Node);
	?NS_ADMINL("announce-all") ->
	    ?INFO_COMMAND(Lang, Node);
	?NS_ADMINL("announce-all-allhosts") ->
	    ?INFO_COMMAND(Lang, Node);
	?NS_ADMINL("set-motd") ->
	    ?INFO_COMMAND(Lang, Node);
	?NS_ADMINL("set-motd-allhosts") ->
	    ?INFO_COMMAND(Lang, Node);
	?NS_ADMINL("edit-motd") ->
	    ?INFO_COMMAND(Lang, Node);
	?NS_ADMINL("edit-motd-allhosts") ->
	    ?INFO_COMMAND(Lang, Node);
	?NS_ADMINL("delete-motd") ->
	    ?INFO_COMMAND(Lang, Node);
	?NS_ADMINL("delete-motd-allhosts") ->
	    ?INFO_COMMAND(Lang, Node);
	_ ->
	    Acc
    end.

%%-------------------------------------------------------------------------

-define(INFO_RESULT(Allow, Feats),
	case Allow of
	    deny ->
		{error, ?ERR_FORBIDDEN};
	    allow ->
		{result, Feats}
	end).

disco_features(Acc, From, #jid{lserver = LServer} = _To,
	       "announce", _Lang) ->
    case gen_mod:is_loaded(LServer, mod_adhoc) of
	false ->
	    Acc;
	_ ->
	    Access1 = gen_mod:get_module_opt(LServer, ?MODULE, access, none),
	    Access2 = gen_mod:get_module_opt(global, ?MODULE, access, none),
	    case {acl:match_rule(LServer, Access1, From),
		  acl:match_rule(global, Access2, From)} of
		{deny, deny} ->
		    {error, ?ERR_FORBIDDEN};
		_ ->
		    {result, []}
	    end
    end;

disco_features(Acc, From, #jid{lserver = LServer} = _To,
	       Node, _Lang) ->
    case gen_mod:is_loaded(LServer, mod_adhoc) of
	false ->
	    Acc;
	_ ->
	    Access = gen_mod:get_module_opt(LServer, ?MODULE, access, none),
	    Allow = acl:match_rule(LServer, Access, From),
	    AccessGlobal = gen_mod:get_module_opt(global, ?MODULE, access, none),
	    AllowGlobal = acl:match_rule(global, AccessGlobal, From),
	    case Node of
		?NS_ADMIN ++ "#announce" ->
		    ?INFO_RESULT(Allow, [?NS_COMMANDS]);
		?NS_ADMIN ++ "#announce-all" ->
		    ?INFO_RESULT(Allow, [?NS_COMMANDS]);
		?NS_ADMIN ++ "#set-motd" ->
		    ?INFO_RESULT(Allow, [?NS_COMMANDS]);
		?NS_ADMIN ++ "#edit-motd" ->
		    ?INFO_RESULT(Allow, [?NS_COMMANDS]);
		?NS_ADMIN ++ "#delete-motd" ->
		    ?INFO_RESULT(Allow, [?NS_COMMANDS]);
		?NS_ADMIN ++ "#announce-allhosts" ->
		    ?INFO_RESULT(AllowGlobal, [?NS_COMMANDS]);
		?NS_ADMIN ++ "#announce-all-allhosts" ->
		    ?INFO_RESULT(AllowGlobal, [?NS_COMMANDS]);
		?NS_ADMIN ++ "#set-motd-allhosts" ->
		    ?INFO_RESULT(AllowGlobal, [?NS_COMMANDS]);
		?NS_ADMIN ++ "#edit-motd-allhosts" ->
		    ?INFO_RESULT(AllowGlobal, [?NS_COMMANDS]);
		?NS_ADMIN ++ "#delete-motd-allhosts" ->
		    ?INFO_RESULT(AllowGlobal, [?NS_COMMANDS]);
		_ ->
		    Acc
	    end
    end.

%%-------------------------------------------------------------------------

-define(NODE_TO_ITEM(Lang, Server, Node),
	{xmlelement, "item",
	 [{"jid", Server},
	  {"node", Node},
	  {"name", get_title(Lang, Node)}],
	 []}).

-define(ITEMS_RESULT(Allow, Items),
	case Allow of
	    deny ->
		{error, ?ERR_FORBIDDEN};
	    allow ->
		{result, Items}
	end).

disco_items(Acc, From, #jid{lserver = LServer, server = Server} = _To,
	    "", Lang) ->
    case gen_mod:is_loaded(LServer, mod_adhoc) of
	false ->
	    Acc;
	_ ->
	    Access1 = gen_mod:get_module_opt(LServer, ?MODULE, access, none),
	    Access2 = gen_mod:get_module_opt(global, ?MODULE, access, none),
	    case {acl:match_rule(LServer, Access1, From),
		  acl:match_rule(global, Access2, From)} of
		{deny, deny} ->
		    Acc;
		_ ->
		    Items = case Acc of
				{result, I} -> I;
				_ -> []
			    end,
		    Nodes = [?NODE_TO_ITEM(Lang, Server, "announce")],
		    {result, Items ++ Nodes}
	    end
    end;

disco_items(Acc, From, #jid{lserver = LServer} = To, "announce", Lang) ->
    case gen_mod:is_loaded(LServer, mod_adhoc) of
	false ->
	    Acc;
	_ ->
	    announce_items(Acc, From, To, Lang)
    end;

disco_items(Acc, From, #jid{lserver = LServer} = _To, Node, _Lang) ->
    case gen_mod:is_loaded(LServer, mod_adhoc) of
	false ->
	    Acc;
	_ ->
	    Access = gen_mod:get_module_opt(LServer, ?MODULE, access, none),
	    Allow = acl:match_rule(LServer, Access, From),
	    AccessGlobal = gen_mod:get_module_opt(global, ?MODULE, access, none),
	    AllowGlobal = acl:match_rule(global, AccessGlobal, From),
	    case Node of
		?NS_ADMIN ++ "#announce" ->
		    ?ITEMS_RESULT(Allow, []);
		?NS_ADMIN ++ "#announce-all" ->
		    ?ITEMS_RESULT(Allow, []);
		?NS_ADMIN ++ "#set-motd" ->
		    ?ITEMS_RESULT(Allow, []);
		?NS_ADMIN ++ "#edit-motd" ->
		    ?ITEMS_RESULT(Allow, []);
		?NS_ADMIN ++ "#delete-motd" ->
		    ?ITEMS_RESULT(Allow, []);
		?NS_ADMIN ++ "#announce-allhosts" ->
		    ?ITEMS_RESULT(AllowGlobal, []);
		?NS_ADMIN ++ "#announce-all-allhosts" ->
		    ?ITEMS_RESULT(AllowGlobal, []);
		?NS_ADMIN ++ "#set-motd-allhosts" ->
		    ?ITEMS_RESULT(AllowGlobal, []);
		?NS_ADMIN ++ "#edit-motd-allhosts" ->
		    ?ITEMS_RESULT(AllowGlobal, []);
		?NS_ADMIN ++ "#delete-motd-allhosts" ->
		    ?ITEMS_RESULT(AllowGlobal, []);
		_ ->
		    Acc
	    end
    end.

%%-------------------------------------------------------------------------

announce_items(Acc, From, #jid{lserver = LServer, server = Server} = _To, Lang) ->
    Access1 = gen_mod:get_module_opt(LServer, ?MODULE, access, none),
    Nodes1 = case acl:match_rule(LServer, Access1, From) of
		 allow ->
		     [?NODE_TO_ITEM(Lang, Server, ?NS_ADMIN ++ "#announce"),
		      ?NODE_TO_ITEM(Lang, Server, ?NS_ADMIN ++ "#announce-all"),
		      ?NODE_TO_ITEM(Lang, Server, ?NS_ADMIN ++ "#set-motd"),
		      ?NODE_TO_ITEM(Lang, Server, ?NS_ADMIN ++ "#edit-motd"),
		      ?NODE_TO_ITEM(Lang, Server, ?NS_ADMIN ++ "#delete-motd")];
		 deny ->
		     []
	     end,
    Access2 = gen_mod:get_module_opt(global, ?MODULE, access, none),
    Nodes2 = case acl:match_rule(global, Access2, From) of
		 allow ->
		     [?NODE_TO_ITEM(Lang, Server, ?NS_ADMIN ++ "#announce-allhosts"),
		      ?NODE_TO_ITEM(Lang, Server, ?NS_ADMIN ++ "#announce-all-allhosts"),
		      ?NODE_TO_ITEM(Lang, Server, ?NS_ADMIN ++ "#set-motd-allhosts"),
		      ?NODE_TO_ITEM(Lang, Server, ?NS_ADMIN ++ "#edit-motd-allhosts"),
		      ?NODE_TO_ITEM(Lang, Server, ?NS_ADMIN ++ "#delete-motd-allhosts")];
		 deny ->
		     []
	     end,
    case {Nodes1, Nodes2} of
	{[], []} ->
	    Acc;
	_ ->
	    Items = case Acc of
			{result, I} -> I;
			_ -> []
		    end,
	    {result, Items ++ Nodes1 ++ Nodes2}
    end.

%%-------------------------------------------------------------------------

commands_result(Allow, From, To, Request) ->
    case Allow of
	deny ->
	    {error, ?ERR_FORBIDDEN};
	allow ->
	    announce_commands(From, To, Request)
    end.


announce_commands(Acc, From, #jid{lserver = LServer} = To,
		  #adhoc_request{ node = Node} = Request) ->
    LNode = tokenize(Node),
    F = fun() ->
		Access = gen_mod:get_module_opt(global, ?MODULE, access, none),
		Allow = acl:match_rule(global, Access, From),
		commands_result(Allow, From, To, Request)
	end,
    R = case LNode of
	    ?NS_ADMINL("announce-allhosts") -> F();
	    ?NS_ADMINL("announce-all-allhosts") -> F();
	    ?NS_ADMINL("set-motd-allhosts") -> F();
	    ?NS_ADMINL("edit-motd-allhosts") -> F();
	    ?NS_ADMINL("delete-motd-allhosts") -> F();
	    _ ->
		Access = gen_mod:get_module_opt(LServer, ?MODULE, access, none),
		Allow = acl:match_rule(LServer, Access, From),
		case LNode of
		    ?NS_ADMINL("announce") ->
			commands_result(Allow, From, To, Request);
		    ?NS_ADMINL("announce-all") ->
			commands_result(Allow, From, To, Request);
		    ?NS_ADMINL("set-motd") ->
			commands_result(Allow, From, To, Request);
		    ?NS_ADMINL("edit-motd") ->
			commands_result(Allow, From, To, Request);
		    ?NS_ADMINL("delete-motd") ->
			commands_result(Allow, From, To, Request);
		    _ ->
			unknown
		end
	end,
    case R of
	unknown -> Acc;
	_ -> {stop, R}
    end.

%%-------------------------------------------------------------------------

announce_commands(From, To,
		  #adhoc_request{lang = Lang,
				 node = Node,
				 action = Action,
				 xdata = XData} = Request) ->
    %% If the "action" attribute is not present, it is
    %% understood as "execute".  If there was no <actions/>
    %% element in the first response (which there isn't in our
    %% case), "execute" and "complete" are equivalent.
    ActionIsExecute = lists:member(Action,
				   ["", "execute", "complete"]),
    if Action == "cancel" ->
	    %% User cancels request
	    adhoc:produce_response(Request, 
				   #adhoc_response{status = canceled});
       XData == false, ActionIsExecute ->
	    %% User requests form
	    Elements = generate_adhoc_form(Lang, Node, To#jid.lserver),
	    adhoc:produce_response(
	      Request,
	      #adhoc_response{status = executing,
			      elements = [Elements]});
       XData /= false, ActionIsExecute ->
	    %% User returns form.
	    case jlib:parse_xdata_submit(XData) of
		invalid ->
		    {error, ?ERR_BAD_REQUEST};
		Fields ->
		    handle_adhoc_form(From, To, Request, Fields)
	    end;
       true ->
	    {error, ?ERR_BAD_REQUEST}
    end.

-define(VVALUE(Val),
	{xmlelement, "value", [], [{xmlcdata, Val}]}).
-define(TVFIELD(Type, Var, Val),
	{xmlelement, "field", [{"type", Type},
			       {"var", Var}],
	 vvaluel(Val)}).
-define(HFIELD(), ?TVFIELD("hidden", "FORM_TYPE", ?NS_ADMIN)).

vvaluel(Val) ->
    case Val of
        "" -> [];
        _ -> [?VVALUE(Val)]
    end.

generate_adhoc_form(Lang, Node, ServerHost) ->
    LNode = tokenize(Node),
    {OldSubject, OldBody} = if (LNode == ?NS_ADMINL("edit-motd")) 
			       or (LNode == ?NS_ADMINL("edit-motd-allhosts")) ->
				    get_stored_motd(ServerHost);
			       true -> 
				    {[], []}
			    end,
    {xmlelement, "x",
     [{"xmlns", ?NS_XDATA},
      {"type", "form"}],
     [?HFIELD(),
      {xmlelement, "title", [], [{xmlcdata, get_title(Lang, Node)}]}]
     ++
     if (LNode == ?NS_ADMINL("delete-motd"))
	or (LNode == ?NS_ADMINL("delete-motd-allhosts")) ->
	     [{xmlelement, "field",
	       [{"var", "confirm"},
		{"type", "boolean"},
		{"label", translate:translate(Lang, "Really delete message of the day?")}],
	       [{xmlelement, "value",
		 [],
		 [{xmlcdata, "true"}]}]}];
	true ->
	     [{xmlelement, "field", 
	       [{"var", "subject"},
		{"type", "text-single"},
		{"label", translate:translate(Lang, "Subject")}],
	       vvaluel(OldSubject)},
	      {xmlelement, "field",
	       [{"var", "body"},
		{"type", "text-multi"},
		{"label", translate:translate(Lang, "Message body")}],
	       vvaluel(OldBody)}]
     end}.

join_lines([]) ->
    [];
join_lines(Lines) ->
    join_lines(Lines, []).
join_lines([Line|Lines], Acc) ->
    join_lines(Lines, ["\n",Line|Acc]);
join_lines([], Acc) ->
    %% Remove last newline
    lists:flatten(lists:reverse(tl(Acc))).

handle_adhoc_form(From, #jid{lserver = LServer} = To,
		  #adhoc_request{lang = Lang,
				 node = Node,
				 sessionid = SessionID},
		  Fields) ->
    Confirm = case lists:keysearch("confirm", 1, Fields) of
		  {value, {"confirm", ["true"]}} ->
		      true;
		  {value, {"confirm", ["1"]}} ->
		      true;
		  _ ->
		      false
	      end,
    Subject = case lists:keysearch("subject", 1, Fields) of
		  {value, {"subject", SubjectLines}} ->
		      %% There really shouldn't be more than one
		      %% subject line, but can we stop them?
		      join_lines(SubjectLines);
		  _ ->
		      []
	      end,
    Body = case lists:keysearch("body", 1, Fields) of
	       {value, {"body", BodyLines}} ->
		   join_lines(BodyLines);
	       _ ->
		   []
	   end,
    Response = #adhoc_response{lang = Lang,
			       node = Node,
			       sessionid = SessionID,
			       status = completed},
    Packet = {xmlelement, "message", [{"type", "headline"}],
	      if Subject /= [] ->
		      [{xmlelement, "subject", [], 
			[{xmlcdata, Subject}]}];
		 true ->
		      []
	      end ++
	      if Body /= [] ->
		      [{xmlelement, "body", [],
			[{xmlcdata, Body}]}];
		 true ->
		      []
	      end},

    Proc = gen_mod:get_module_proc(LServer, ?PROCNAME),
    case {Node, Body} of
	{?NS_ADMIN ++ "#delete-motd", _} ->
	    if	Confirm ->
		    Proc ! {announce_motd_delete, From, To, Packet},
		    adhoc:produce_response(Response);
		true ->
		    adhoc:produce_response(Response)
	    end;
	{?NS_ADMIN ++ "#delete-motd-allhosts", _} ->
	    if	Confirm ->
		    Proc ! {announce_all_hosts_motd_delete, From, To, Packet},
		    adhoc:produce_response(Response);
		true ->
		    adhoc:produce_response(Response)
	    end;
	{_, []} ->
	    %% An announce message with no body is definitely an operator error.
	    %% Throw an error and give him/her a chance to send message again.
	    {error, ?ERRT_NOT_ACCEPTABLE(
		       Lang,
		       "No body provided for announce message")};
	%% Now send the packet to ?PROCNAME.
	%% We don't use direct announce_* functions because it
	%% leads to large delay in response and <iq/> queries processing
	{?NS_ADMIN ++ "#announce", _} ->
	    Proc ! {announce_online, From, To, Packet},
	    adhoc:produce_response(Response);
	{?NS_ADMIN ++ "#announce-allhosts", _} ->	    
	    Proc ! {announce_all_hosts_online, From, To, Packet},
	    adhoc:produce_response(Response);
	{?NS_ADMIN ++ "#announce-all", _} ->
	    Proc ! {announce_all, From, To, Packet},
	    adhoc:produce_response(Response);
	{?NS_ADMIN ++ "#announce-all-allhosts", _} ->	    
	    Proc ! {announce_all_hosts_all, From, To, Packet},
	    adhoc:produce_response(Response);
	{?NS_ADMIN ++ "#set-motd", _} ->
	    Proc ! {announce_motd, From, To, Packet},
	    adhoc:produce_response(Response);
	{?NS_ADMIN ++ "#set-motd-allhosts", _} ->	    
	    Proc ! {announce_all_hosts_motd, From, To, Packet},
	    adhoc:produce_response(Response);
	{?NS_ADMIN ++ "#edit-motd", _} ->
	    Proc ! {announce_motd_update, From, To, Packet},
	    adhoc:produce_response(Response);
	{?NS_ADMIN ++ "#edit-motd-allhosts", _} ->	    
	    Proc ! {announce_all_hosts_motd_update, From, To, Packet},
	    adhoc:produce_response(Response);
	_ ->
	    %% This can't happen, as we haven't registered any other
	    %% command nodes.
	    {error, ?ERR_INTERNAL_SERVER_ERROR}
    end.

get_title(Lang, "announce") ->
    translate:translate(Lang, "Announcements");
get_title(Lang, ?NS_ADMIN ++ "#announce-all") ->
    translate:translate(Lang, "Send announcement to all users");
get_title(Lang, ?NS_ADMIN ++ "#announce-all-allhosts") ->
    translate:translate(Lang, "Send announcement to all users on all hosts");
get_title(Lang, ?NS_ADMIN ++ "#announce") ->
    translate:translate(Lang, "Send announcement to all online users");
get_title(Lang, ?NS_ADMIN ++ "#announce-allhosts") ->
    translate:translate(Lang, "Send announcement to all online users on all hosts");
get_title(Lang, ?NS_ADMIN ++ "#set-motd") ->
    translate:translate(Lang, "Set message of the day and send to online users");
get_title(Lang, ?NS_ADMIN ++ "#set-motd-allhosts") ->
    translate:translate(Lang, "Set message of the day on all hosts and send to online users");
get_title(Lang, ?NS_ADMIN ++ "#edit-motd") ->
    translate:translate(Lang, "Update message of the day (don't send)");
get_title(Lang, ?NS_ADMIN ++ "#edit-motd-allhosts") ->
    translate:translate(Lang, "Update message of the day on all hosts (don't send)");
get_title(Lang, ?NS_ADMIN ++ "#delete-motd") ->
    translate:translate(Lang, "Delete message of the day");
get_title(Lang, ?NS_ADMIN ++ "#delete-motd-allhosts") ->
    translate:translate(Lang, "Delete message of the day on all hosts").

%%-------------------------------------------------------------------------

announce_all(From, To, Packet) ->
    Host = To#jid.lserver,
    Access = gen_mod:get_module_opt(Host, ?MODULE, access, none),
    case acl:match_rule(Host, Access, From) of
	deny ->
	    Err = jlib:make_error_reply(Packet, ?ERR_FORBIDDEN),
	    ejabberd_router:route(To, From, Err);
	allow ->
	    Local = jlib:make_jid("", To#jid.server, ""),
	    lists:foreach(
	      fun({User, Server}) ->
		      Dest = jlib:make_jid(User, Server, ""),
		      ejabberd_router:route(Local, Dest, Packet)
	      end, ejabberd_auth:get_vh_registered_users(Host))
    end.

announce_all_hosts_all(From, To, Packet) ->
    Access = gen_mod:get_module_opt(global, ?MODULE, access, none),
    case acl:match_rule(global, Access, From) of
	deny ->
	    Err = jlib:make_error_reply(Packet, ?ERR_FORBIDDEN),
	    ejabberd_router:route(To, From, Err);
	allow ->
	    Local = jlib:make_jid("", To#jid.server, ""),
	    lists:foreach(
	      fun({User, Server}) ->
		      Dest = jlib:make_jid(User, Server, ""),
		      ejabberd_router:route(Local, Dest, Packet)
	      end, ejabberd_auth:dirty_get_registered_users())
    end.

announce_online(From, To, Packet) ->
    Host = To#jid.lserver,
    Access = gen_mod:get_module_opt(Host, ?MODULE, access, none),
    case acl:match_rule(Host, Access, From) of
	deny ->
	    Err = jlib:make_error_reply(Packet, ?ERR_FORBIDDEN),
	    ejabberd_router:route(To, From, Err);
	allow ->
	    announce_online1(ejabberd_sm:get_vh_session_list(Host),
			     To#jid.server,
			     Packet)
    end.

announce_all_hosts_online(From, To, Packet) ->
    Access = gen_mod:get_module_opt(global, ?MODULE, access, none),
    case acl:match_rule(global, Access, From) of
	deny ->
	    Err = jlib:make_error_reply(Packet, ?ERR_FORBIDDEN),
	    ejabberd_router:route(To, From, Err);
	allow ->
	    announce_online1(ejabberd_sm:dirty_get_sessions_list(),
			     To#jid.server,
			     Packet)
    end.

announce_online1(Sessions, Server, Packet) ->
    Local = jlib:make_jid("", Server, ""),
    lists:foreach(
      fun({U, S, R}) ->
	      Dest = jlib:make_jid(U, S, R),
	      ejabberd_router:route(Local, Dest, Packet)
      end, Sessions).

announce_motd(From, To, Packet) ->
    Host = To#jid.lserver,
    Access = gen_mod:get_module_opt(Host, ?MODULE, access, none),
    case acl:match_rule(Host, Access, From) of
	deny ->
	    Err = jlib:make_error_reply(Packet, ?ERR_FORBIDDEN),
	    ejabberd_router:route(To, From, Err);
	allow ->
	    announce_motd(Host, Packet)
    end.

announce_all_hosts_motd(From, To, Packet) ->
    Access = gen_mod:get_module_opt(global, ?MODULE, access, none),
    case acl:match_rule(global, Access, From) of
	deny ->
	    Err = jlib:make_error_reply(Packet, ?ERR_FORBIDDEN),
	    ejabberd_router:route(To, From, Err);
	allow ->
	    Hosts = ?MYHOSTS,
	    [announce_motd(Host, Packet) || Host <- Hosts]
    end.

announce_motd(Host, Packet) ->
    LServer = jlib:nameprep(Host),
    announce_motd_update(LServer, Packet),
    Sessions = ejabberd_sm:get_vh_session_list(LServer),
    announce_online1(Sessions, LServer, Packet),
    case gen_mod:db_type(LServer, ?MODULE) of
        mnesia ->
            F = fun() ->
                        lists:foreach(
                          fun({U, S, _R}) ->
                                  mnesia:write(#motd_users{us = {U, S}})
                          end, Sessions)
                end,
            mnesia:transaction(F);
        odbc ->
            F = fun() ->
                        lists:foreach(
                          fun({U, _S, _R}) ->
                                  Username = ejabberd_odbc:escape(U),
                                  odbc_queries:update_t(
                                    "motd",
                                    ["username", "xml"],
                                    [Username, ""],
                                    ["username='", Username, "'"])
                          end, Sessions)
                end,
            ejabberd_odbc:sql_transaction(LServer, F)
    end.

announce_motd_update(From, To, Packet) ->
    Host = To#jid.lserver,
    Access = gen_mod:get_module_opt(Host, ?MODULE, access, none),
    case acl:match_rule(Host, Access, From) of
	deny ->
	    Err = jlib:make_error_reply(Packet, ?ERR_FORBIDDEN),
	    ejabberd_router:route(To, From, Err);
	allow ->
	    announce_motd_update(Host, Packet)
    end.

announce_all_hosts_motd_update(From, To, Packet) ->
    Access = gen_mod:get_module_opt(global, ?MODULE, access, none),
    case acl:match_rule(global, Access, From) of
	deny ->
	    Err = jlib:make_error_reply(Packet, ?ERR_FORBIDDEN),
	    ejabberd_router:route(To, From, Err);
	allow ->
	    Hosts = ?MYHOSTS,
	    [announce_motd_update(Host, Packet) || Host <- Hosts]
    end.

announce_motd_update(LServer, Packet) ->
    announce_motd_delete(LServer),
    case gen_mod:db_type(LServer, ?MODULE) of
        mnesia ->
            F = fun() ->
                        mnesia:write(#motd{server = LServer, packet = Packet})
                end,
            mnesia:transaction(F);
        odbc ->
            XML = ejabberd_odbc:escape(xml:element_to_binary(Packet)),
            F = fun() ->
                        odbc_queries:update_t(
                          "motd",
                          ["username", "xml"],
                          ["", XML],
                          ["username=''"])
                end,
            ejabberd_odbc:sql_transaction(LServer, F)
    end.

announce_motd_delete(From, To, Packet) ->
    Host = To#jid.lserver,
    Access = gen_mod:get_module_opt(Host, ?MODULE, access, none),
    case acl:match_rule(Host, Access, From) of
	deny ->
	    Err = jlib:make_error_reply(Packet, ?ERR_FORBIDDEN),
	    ejabberd_router:route(To, From, Err);
	allow ->
	    announce_motd_delete(Host)
    end.

announce_all_hosts_motd_delete(From, To, Packet) ->
    Access = gen_mod:get_module_opt(global, ?MODULE, access, none),
    case acl:match_rule(global, Access, From) of
	deny ->
	    Err = jlib:make_error_reply(Packet, ?ERR_FORBIDDEN),
	    ejabberd_router:route(To, From, Err);
	allow ->
	    Hosts = ?MYHOSTS,
	    [announce_motd_delete(Host) || Host <- Hosts]
    end.

announce_motd_delete(LServer) ->
    case gen_mod:db_type(LServer, ?MODULE) of
        mnesia ->
            F = fun() ->
                        mnesia:delete({motd, LServer}),
                        mnesia:write_lock_table(motd_users),
                        Users = mnesia:select(
                                  motd_users,
                                  [{#motd_users{us = '$1', _ = '_'},
                                    [{'==', {element, 2, '$1'}, LServer}],
                                    ['$1']}]),
                        lists:foreach(fun(US) ->
                                              mnesia:delete({motd_users, US})
                                      end, Users)
                end,
            mnesia:transaction(F);
        odbc ->
            F = fun() ->
                        ejabberd_odbc:sql_query_t(["delete from motd;"])
                end,
            ejabberd_odbc:sql_transaction(LServer, F)
    end.

send_motd(JID) ->
    send_motd(JID, gen_mod:db_type(JID#jid.lserver, ?MODULE)).

send_motd(#jid{luser = LUser, lserver = LServer} = JID, mnesia) ->
    case catch mnesia:dirty_read({motd, LServer}) of
	[#motd{packet = Packet}] ->
	    US = {LUser, LServer},
	    case catch mnesia:dirty_read({motd_users, US}) of
		[#motd_users{}] ->
		    ok;
		_ ->
		    Local = jlib:make_jid("", LServer, ""),
		    ejabberd_router:route(Local, JID, Packet),
		    F = fun() ->
				mnesia:write(#motd_users{us = US})
			end,
		    mnesia:transaction(F)
	    end;
	_ ->
	    ok
    end;
send_motd(#jid{luser = LUser, lserver = LServer} = JID, odbc) when LUser /= "" ->
    case catch ejabberd_odbc:sql_query(
                 LServer, ["select xml from motd where username='';"]) of
        {selected, ["xml"], [{XML}]} ->
            case xml_stream:parse_element(XML) of
                {error, _} ->
                    ok;
                Packet ->
                    Username = ejabberd_odbc:escape(LUser),
                    case catch ejabberd_odbc:sql_query(
                                 LServer,
                                 ["select username from motd "
                                  "where username='", Username, "';"]) of
                        {selected, ["username"], []} ->
                            Local = jlib:make_jid("", LServer, ""),
                            ejabberd_router:route(Local, JID, Packet),
                            F = fun() ->
                                        odbc_queries:update_t(
                                          "motd",
                                          ["username", "xml"],
                                          [Username, ""],
                                          ["username='", Username, "'"])
                                end,
                            ejabberd_odbc:sql_transaction(LServer, F);
                        _ ->
                            ok
                    end
            end;
        _ ->
            ok
    end;
send_motd(_, odbc) ->
    ok.

get_stored_motd(LServer) ->
    case get_stored_motd_packet(LServer, gen_mod:db_type(LServer, ?MODULE)) of
        {ok, Packet} ->
            {xml:get_subtag_cdata(Packet, "subject"),
	     xml:get_subtag_cdata(Packet, "body")};
        error ->
            {"", ""}
    end.

get_stored_motd_packet(LServer, mnesia) ->
    case catch mnesia:dirty_read({motd, LServer}) of
	[#motd{packet = Packet}] ->
            {ok, Packet};
	_ ->
	    error
    end;
get_stored_motd_packet(LServer, odbc) ->
    case catch ejabberd_odbc:sql_query(
                 LServer, ["select xml from motd where username='';"]) of
        {selected, ["xml"], [{XML}]} ->
            case xml_stream:parse_element(XML) of
                {error, _} ->
                    error;
                Packet ->
                    {ok, Packet}
            end;
        _ ->
            error
    end.

%% This function is similar to others, but doesn't perform any ACL verification
send_announcement_to_all(Host, SubjectS, BodyS) ->
    SubjectEls = if SubjectS /= [] ->
		      [{xmlelement, "subject", [], [{xmlcdata, SubjectS}]}];
		 true ->
		      []
	      end,
    BodyEls = if BodyS /= [] ->
		      [{xmlelement, "body", [], [{xmlcdata, BodyS}]}];
		 true ->
		      []
	      end,
    Packet = {xmlelement, "message", [{"type", "headline"}], SubjectEls ++ BodyEls},
    Sessions = ejabberd_sm:dirty_get_sessions_list(),
    Local = jlib:make_jid("", Host, ""),
    lists:foreach(
      fun({U, S, R}) ->
	      Dest = jlib:make_jid(U, S, R),
	      ejabberd_router:route(Local, Dest, Packet)
      end, Sessions).

%%-------------------------------------------------------------------------

update_tables() ->
    update_motd_table(),
    update_motd_users_table().

update_motd_table() ->
    Fields = record_info(fields, motd),
    case mnesia:table_info(motd, attributes) of
	Fields ->
	    ok;
	[id, packet] ->
	    ?INFO_MSG("Converting motd table from "
		      "{id, packet} format", []),
	    Host = ?MYNAME,
	    {atomic, ok} = mnesia:create_table(
			     mod_announce_tmp_table,
			     [{disc_only_copies, [node()]},
			      {type, bag},
			      {local_content, true},
			      {record_name, motd},
			      {attributes, record_info(fields, motd)}]),
	    mnesia:transform_table(motd, ignore, Fields),
	    F1 = fun() ->
			 mnesia:write_lock_table(mod_announce_tmp_table),
			 mnesia:foldl(
			   fun(#motd{server = _} = R, _) ->
				   mnesia:dirty_write(
				     mod_announce_tmp_table,
				     R#motd{server = Host})
			   end, ok, motd)
		 end,
	    mnesia:transaction(F1),
	    mnesia:clear_table(motd),
	    F2 = fun() ->
			 mnesia:write_lock_table(motd),
			 mnesia:foldl(
			   fun(R, _) ->
				   mnesia:dirty_write(R)
			   end, ok, mod_announce_tmp_table)
		 end,
	    mnesia:transaction(F2),
	    mnesia:delete_table(mod_announce_tmp_table);
	_ ->
	    ?INFO_MSG("Recreating motd table", []),
	    mnesia:transform_table(motd, ignore, Fields)
    end.


update_motd_users_table() ->
    Fields = record_info(fields, motd_users),
    case mnesia:table_info(motd_users, attributes) of
	Fields ->
	    ok;
	[luser, dummy] ->
	    ?INFO_MSG("Converting motd_users table from "
		      "{luser, dummy} format", []),
	    Host = ?MYNAME,
	    {atomic, ok} = mnesia:create_table(
			     mod_announce_tmp_table,
			     [{disc_only_copies, [node()]},
			      {type, bag},
			      {local_content, true},
			      {record_name, motd_users},
			      {attributes, record_info(fields, motd_users)}]),
	    mnesia:transform_table(motd_users, ignore, Fields),
	    F1 = fun() ->
			 mnesia:write_lock_table(mod_announce_tmp_table),
			 mnesia:foldl(
			   fun(#motd_users{us = U} = R, _) ->
				   mnesia:dirty_write(
				     mod_announce_tmp_table,
				     R#motd_users{us = {U, Host}})
			   end, ok, motd_users)
		 end,
	    mnesia:transaction(F1),
	    mnesia:clear_table(motd_users),
	    F2 = fun() ->
			 mnesia:write_lock_table(motd_users),
			 mnesia:foldl(
			   fun(R, _) ->
				   mnesia:dirty_write(R)
			   end, ok, mod_announce_tmp_table)
		 end,
	    mnesia:transaction(F2),
	    mnesia:delete_table(mod_announce_tmp_table);
	_ ->
	    ?INFO_MSG("Recreating motd_users table", []),
	    mnesia:transform_table(motd_users, ignore, Fields)
    end.
