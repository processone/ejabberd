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

-include_lib("exmpp/include/exmpp.hrl").

-include("ejabberd.hrl").
-include("adhoc.hrl").

-record(motd, {server, packet}).
-record(motd_users, {us, dummy = []}).

-define(PROCNAME, ejabberd_announce).

-define(NS_ADMINL(Sub), ["http:","jabber.org","protocol","admin", Sub]).
tokenize(Node) -> string:tokens(Node, "/#").

start(Host, Opts) when is_list(Host) ->
    start(list_to_binary(Host), Opts);
start(HostB, _Opts) ->
    mnesia:create_table(motd, [{disc_copies, [node()]},
			       {attributes, record_info(fields, motd)}]),
    mnesia:create_table(motd_users, [{disc_copies, [node()]},
				     {attributes, record_info(fields, motd_users)}]),
    update_tables(),
    ejabberd_hooks:add(local_send_to_resource_hook, HostB,
		       ?MODULE, announce, 50),
    ejabberd_hooks:add(disco_local_identity, HostB, ?MODULE, disco_identity, 50),
    ejabberd_hooks:add(disco_local_features, HostB, ?MODULE, disco_features, 50),
    ejabberd_hooks:add(disco_local_items, HostB, ?MODULE, disco_items, 50),
    ejabberd_hooks:add(adhoc_local_items, HostB, ?MODULE, announce_items, 50),
    ejabberd_hooks:add(adhoc_local_commands, HostB, ?MODULE, announce_commands, 50),
    ejabberd_hooks:add(user_available_hook, HostB,
		       ?MODULE, send_motd, 50),
    register(gen_mod:get_module_proc(HostB, ?PROCNAME),
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
    HostB = list_to_binary(Host),
    ejabberd_hooks:delete(adhoc_local_commands, HostB, ?MODULE, announce_commands, 50),
    ejabberd_hooks:delete(adhoc_local_items, HostB, ?MODULE, announce_items, 50),
    ejabberd_hooks:delete(disco_local_identity, HostB, ?MODULE, disco_identity, 50),
    ejabberd_hooks:delete(disco_local_features, HostB, ?MODULE, disco_features, 50),
    ejabberd_hooks:delete(disco_local_items, HostB, ?MODULE, disco_items, 50),
    ejabberd_hooks:delete(local_send_to_resource_hook, HostB,
			  ?MODULE, announce, 50),
    ejabberd_hooks:delete(user_available_hook, HostB,
			  ?MODULE, send_motd, 50),
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    exit(whereis(Proc), stop),
    {wait, Proc}.

%% Announcing via messages to a custom resource
announce(From, To, Packet) ->
    case {exmpp_jid:prep_node(To), exmpp_jid:prep_resource(To)} of
	    {undefined, Res} ->
	    Name = Packet#xmlel.name,
	    Proc = gen_mod:get_module_proc_existing(exmpp_jid:prep_domain_as_list(To), ?PROCNAME),
	    case {Res, Name} of
		{<<"announce/all">>, 'message'} ->
		    Proc ! {announce_all, From, To, Packet},
		    stop;
		{<<"announce/all-hosts/all">>, 'message'} ->
		    Proc ! {announce_all_hosts_all, From, To, Packet},
		    stop;
		{<<"announce/online">>, 'message'} ->
		    Proc ! {announce_online, From, To, Packet},
		    stop;
		{<<"announce/all-hosts/online">>, 'message'} ->
		    Proc ! {announce_all_hosts_online, From, To, Packet},
		    stop;
		{<<"announce/motd">>, 'message'} ->
		    Proc ! {announce_motd, From, To, Packet},
		    stop;
		{<<"announce/all-hosts/motd">>, 'message'} ->
		    Proc ! {announce_all_hosts_motd, From, To, Packet},
		    stop;
		{<<"announce/motd/update">>, 'message'} ->
		    Proc ! {announce_motd_update, From, To, Packet},
		    stop;
		{<<"announce/all-hosts/motd/update">>, 'message'} ->
		    Proc ! {announce_all_hosts_motd_update, From, To, Packet},
		    stop;
		{<<"announce/motd/delete">>, 'message'} ->
		    Proc ! {announce_motd_delete, From, To, Packet},
		    stop;
		{<<"announce/all-hosts/motd/delete">>, 'message'} ->
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
        [#xmlel{ns = ?NS_DISCO_INFO, name = 'identity', attrs =
	  [?XMLATTR(<<"category">>, <<"automation">>),
	   ?XMLATTR(<<"type">>, <<"command-node">>),
	   ?XMLATTR(<<"name">>, get_title(Lang, Node))]}]).

disco_identity(Acc, _From, _To, Node, Lang) ->
    LNode = tokenize(binary_to_list(Node)),
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
		{error, 'forbidden'};
	    allow ->
		{result, Feats}
	end).

disco_features(Acc, From, To, <<"announce">>, _Lang) ->
    LServer = exmpp_jid:prep_domain_as_list(To),
    case gen_mod:is_loaded(LServer, mod_adhoc) of
	false ->
	    Acc;
	_ ->
	    Access1 = gen_mod:get_module_opt(LServer, ?MODULE, access, none),
	    Access2 = gen_mod:get_module_opt(global, ?MODULE, access, none),
	    case {acl:match_rule(LServer, Access1, From),
		  acl:match_rule(global, Access2, From)} of
		{deny, deny} ->
		    {error, 'forbidden'};
		_ ->
		    {result, []}
	    end
    end;

disco_features(Acc, From, To, Node, _Lang) ->
    LServer = exmpp_jid:prep_domain_as_list(To),
    case gen_mod:is_loaded(LServer, mod_adhoc) of
	false ->
	    Acc;
	_ ->
	    Access = gen_mod:get_module_opt(LServer, ?MODULE, access, none),
	    Allow = acl:match_rule(LServer, Access, From),
	    AccessGlobal = gen_mod:get_module_opt(global, ?MODULE, access, none),
	    AllowGlobal = acl:match_rule(global, AccessGlobal, From),
	    case Node of
		<<?NS_ADMIN_s,  "#announce">> ->
		    ?INFO_RESULT(Allow, [?NS_ADHOC_s]);
		<<?NS_ADMIN_s,  "#announce-all">> ->
		    ?INFO_RESULT(Allow, [?NS_ADHOC_s]);
		<<?NS_ADMIN_s,  "#set-motd">> ->
		    ?INFO_RESULT(Allow, [?NS_ADHOC_s]);
		<<?NS_ADMIN_s,  "#edit-motd">> ->
		    ?INFO_RESULT(Allow, [?NS_ADHOC_s]);
		<<?NS_ADMIN_s,  "#delete-motd">> ->
		    ?INFO_RESULT(Allow, [?NS_ADHOC_s]);
		<<?NS_ADMIN_s,  "#announce-allhosts">> ->
		    ?INFO_RESULT(AllowGlobal, [?NS_ADHOC_s]);
		<<?NS_ADMIN_s,  "#announce-all-allhosts">> ->
		    ?INFO_RESULT(AllowGlobal, [?NS_ADHOC_s]);
		<<?NS_ADMIN_s,  "#set-motd-allhosts">> ->
		    ?INFO_RESULT(AllowGlobal, [?NS_ADHOC_s]);
		<<?NS_ADMIN_s,  "#edit-motd-allhosts">> ->
		    ?INFO_RESULT(AllowGlobal, [?NS_ADHOC_s]);
		<<?NS_ADMIN_s,  "#delete-motd-allhosts">> ->
		    ?INFO_RESULT(AllowGlobal, [?NS_ADHOC_s]);
		_ ->
		    Acc
	    end
    end.

%%-------------------------------------------------------------------------

-define(NODE_TO_ITEM(Lang, Server, Node),
	#xmlel{ns = ?NS_DISCO_ITEMS, name = 'item', attrs =
	 [?XMLATTR(<<"jid">>,  Server),
	  ?XMLATTR(<<"node">>, Node),
	  ?XMLATTR(<<"name">>, get_title(Lang, Node))]}).

-define(ITEMS_RESULT(Allow, Items),
	case Allow of
	    deny ->
		{error, 'forbidden'};
	    allow ->
		{result, Items}
	end).

disco_items(Acc, From, To, <<>>, Lang) ->
    LServer = exmpp_jid:prep_domain_as_list(To),
    Server = exmpp_jid:domain(To),

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
		    Nodes = [?NODE_TO_ITEM(Lang, Server, <<"announce">>)],
		    {result, Items ++ Nodes}
	    end
    end;

disco_items(Acc, From, To, <<"announce">>, Lang) ->
    LServer = exmpp_jid:prep_domain_as_list(To),
    case gen_mod:is_loaded(LServer, mod_adhoc) of
	false ->
	    Acc;
	_ ->
	    announce_items(Acc, From, To, Lang)
    end;

disco_items(Acc, From, To, Node, _Lang) ->
    LServer = exmpp_jid:prep_domain_as_list(To),
    case gen_mod:is_loaded(LServer, mod_adhoc) of
	false ->
	    Acc;
	_ ->
	    Access = gen_mod:get_module_opt(LServer, ?MODULE, access, none),
	    Allow = acl:match_rule(LServer, Access, From),
	    AccessGlobal = gen_mod:get_module_opt(global, ?MODULE, access, none),
	    AllowGlobal = acl:match_rule(global, AccessGlobal, From),
	    case Node of
		<<?NS_ADMIN_s, "#announce">> ->
		    ?ITEMS_RESULT(Allow, []);
		<<?NS_ADMIN_s, "#announce-all">> ->
		    ?ITEMS_RESULT(Allow, []);
		<<?NS_ADMIN_s, "#set-motd">> ->
		    ?ITEMS_RESULT(Allow, []);
		<<?NS_ADMIN_s, "#edit-motd">> ->
		    ?ITEMS_RESULT(Allow, []);
		<<?NS_ADMIN_s, "#delete-motd">> ->
		    ?ITEMS_RESULT(Allow, []);
		<<?NS_ADMIN_s, "#announce-allhosts">> ->
		    ?ITEMS_RESULT(AllowGlobal, []);
		<<?NS_ADMIN_s, "#announce-all-allhosts">> ->
		    ?ITEMS_RESULT(AllowGlobal, []);
		<<?NS_ADMIN_s, "#set-motd-allhosts">> ->
		    ?ITEMS_RESULT(AllowGlobal, []);
		<<?NS_ADMIN_s, "#edit-motd-allhosts">> ->
		    ?ITEMS_RESULT(AllowGlobal, []);
		<<?NS_ADMIN_s, "#delete-motd-allhosts">> ->
		    ?ITEMS_RESULT(AllowGlobal, []);
		_ ->
		    Acc
	    end
    end.

%%-------------------------------------------------------------------------

announce_items(Acc, From, To, Lang) ->
    LServer = exmpp_jid:prep_domain_as_list(To),
    Server = exmpp_jid:domain(To),
    Access1 = gen_mod:get_module_opt(LServer, ?MODULE, access, none),
    Nodes1 = case acl:match_rule(LServer, Access1, From) of
		 allow ->
		     [?NODE_TO_ITEM(Lang, Server, <<?NS_ADMIN_s, "#announce">>),
		      ?NODE_TO_ITEM(Lang, Server, <<?NS_ADMIN_s, "#announce-all">>),
		      ?NODE_TO_ITEM(Lang, Server, <<?NS_ADMIN_s, "#set-motd">>),
		      ?NODE_TO_ITEM(Lang, Server, <<?NS_ADMIN_s, "#edit-motd">>),
		      ?NODE_TO_ITEM(Lang, Server, <<?NS_ADMIN_s, "#delete-motd">>)];
		 deny ->
		     []
	     end,
    Access2 = gen_mod:get_module_opt(global, ?MODULE, access, none),
    Nodes2 = case acl:match_rule(global, Access2, From) of
		 allow ->
		     [?NODE_TO_ITEM(Lang, Server, <<?NS_ADMIN_s, "#announce-allhosts">>),
		      ?NODE_TO_ITEM(Lang, Server, <<?NS_ADMIN_s, "#announce-all-allhosts">>),
		      ?NODE_TO_ITEM(Lang, Server, <<?NS_ADMIN_s, "#set-motd-allhosts">>),
		      ?NODE_TO_ITEM(Lang, Server, <<?NS_ADMIN_s, "#edit-motd-allhosts">>),
		      ?NODE_TO_ITEM(Lang, Server, <<?NS_ADMIN_s, "#delete-motd-allhosts">>)];
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
	    {error, 'forbidden'};
	allow ->
	    announce_commands(From, To, Request)
    end.


announce_commands(Acc, From, To, #adhoc_request{ node = Node} = Request) ->
    LServer = exmpp_jid:prep_domain_as_list(To),
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
	    Elements = generate_adhoc_form(Lang, Node, exmpp_jid:prep_domain_as_list(To)),
	    adhoc:produce_response(
	      Request,
	      #adhoc_response{status = executing,
			      elements = [Elements]});
       XData /= false, ActionIsExecute ->
	    %% User returns form.
	    case jlib:parse_xdata_submit(XData) of
		invalid ->
		    {error, 'bad-request'};
		Fields ->
		    handle_adhoc_form(From, To, Request, Fields)
	    end;
       true ->
	    {error, 'bad-request'}
    end.

-define(VVALUE(Val),
	#xmlel{ns = ?NS_DATA_FORMS, name = 'value', children = [#xmlcdata{cdata = Val}]}).
-define(VVALUEL(Val),
	case Val of
	    <<>> -> [];
	    _ -> [?VVALUE(Val)]
	end).
-define(TVFIELD(Type, Var, Val),
	#xmlel{ns = ?NS_DATA_FORMS, name = 'field', attrs = [?XMLATTR(<<"type">>, Type),
			       ?XMLATTR(<<"var">>, Var)], children =
	 ?VVALUEL(Val)}).
-define(HFIELD(), ?TVFIELD(<<"hidden">>, <<"FORM_TYPE">>, list_to_binary(?NS_ADMIN_s))).

generate_adhoc_form(Lang, Node, ServerHost) ->
    LNode = tokenize(Node),
    {OldSubject, OldBody} = if (LNode == ?NS_ADMINL("edit-motd")) 
			       or (LNode == ?NS_ADMINL("edit-motd-allhosts")) ->
				    get_stored_motd(ServerHost);
			       true -> 
				    {[], []}
			    end,
    #xmlel{ns = ?NS_DATA_FORMS, name = 'x', attrs =
     [?XMLATTR(<<"type">>, <<"form">>)], children =
     [?HFIELD(),
      #xmlel{ns = ?NS_DATA_FORMS, name = 'title', children = [#xmlcdata{cdata = list_to_binary(get_title(Lang, Node))}]}]
     ++
     if (LNode == ?NS_ADMINL("delete-motd"))
	or (LNode == ?NS_ADMINL("delete-motd-allhosts")) ->
	     [#xmlel{ns = ?NS_DATA_FORMS, name = 'field', attrs =
	       [?XMLATTR(<<"var">>, <<"confirm">>),
		?XMLATTR(<<"type">>, <<"boolean">>),
		?XMLATTR(<<"label">>, translate:translate(Lang, "Really delete message of the day?"))], children =
	       [#xmlel{ns = ?NS_DATA_FORMS, name = 'value', children =
		 [#xmlcdata{cdata = <<"true">>}]}]}];
	true ->
	     [#xmlel{ns = ?NS_DATA_FORMS, name = 'field', attrs =
	       [?XMLATTR(<<"var">>, <<"subject">>),
		?XMLATTR(<<"type">>, <<"text-single">>),
		?XMLATTR(<<"label">>, translate:translate(Lang, "Subject"))], children =
	       ?VVALUEL(list_to_binary(OldSubject))},
	      #xmlel{ns = ?NS_DATA_FORMS, name = 'field', attrs =
	       [?XMLATTR(<<"var">>, <<"body">>),
		?XMLATTR(<<"type">>, <<"text-multi">>),
		?XMLATTR(<<"label">>, translate:translate(Lang, "Message body"))], children =
	       ?VVALUEL(list_to_binary(OldBody))}]
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

handle_adhoc_form(From, To,
		  #adhoc_request{lang = Lang,
				 node = Node,
				 sessionid = SessionID},
		  Fields) ->
    LServerB = exmpp_jid:prep_domain(To),
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
    Packet = #xmlel{ns = ?NS_JABBER_CLIENT, name = 'message', attrs = [?XMLATTR(<<"type">>, <<"normal">>)], children =
	      if Subject /= [] ->
		      [#xmlel{ns = ?NS_JABBER_CLIENT, name = 'subject', children =
			[#xmlcdata{cdata = list_to_binary(Subject)}]}];
		 true ->
		      []
	      end ++
	      if Body /= [] ->
		      [#xmlel{ns = ?NS_JABBER_CLIENT, name = 'body', children =
			[#xmlcdata{cdata = list_to_binary(Body)}]}];
		 true ->
		      []
	      end},

    Proc = gen_mod:get_module_proc_existing(LServerB, ?PROCNAME),
    case {Node, Body} of
	{?NS_ADMIN_s ++ "#delete-motd", _} ->
	    if	Confirm ->
		    Proc ! {announce_motd_delete, From, To, Packet},
		    adhoc:produce_response(Response);
		true ->
		    adhoc:produce_response(Response)
	    end;
	{?NS_ADMIN_s ++ "#delete-motd-allhosts", _} ->
	    if	Confirm ->
		    Proc ! {announce_all_hosts_motd_delete, From, To, Packet},
		    adhoc:produce_response(Response);
		true ->
		    adhoc:produce_response(Response)
	    end;
	{_, []} ->
	    %% An announce message with no body is definitely an operator error.
	    %% Throw an error and give him/her a chance to send message again.
	    {error, exmpp_stanza:error(?NS_JABBER_CLIENT, 'not-acceptable',
		{"en", "No body provided for announce message"})};
	%% Now send the packet to ?PROCNAME.
	%% We don't use direct announce_* functions because it
	%% leads to large delay in response and <iq/> queries processing
	{?NS_ADMIN_s ++ "#announce", _} ->
	    Proc ! {announce_online, From, To, Packet},
	    adhoc:produce_response(Response);
	{?NS_ADMIN_s ++ "#announce-allhosts", _} ->	    
	    Proc ! {announce_all_hosts_online, From, To, Packet},
	    adhoc:produce_response(Response);
	{?NS_ADMIN_s ++ "#announce-all", _} ->
	    Proc ! {announce_all, From, To, Packet},
	    adhoc:produce_response(Response);
	{?NS_ADMIN_s ++ "#announce-all-allhosts", _} ->	    
	    Proc ! {announce_all_hosts_all, From, To, Packet},
	    adhoc:produce_response(Response);
	{?NS_ADMIN_s ++ "#set-motd", _} ->
	    Proc ! {announce_motd, From, To, Packet},
	    adhoc:produce_response(Response);
	{?NS_ADMIN_s ++ "#set-motd-allhosts", _} ->	    
	    Proc ! {announce_all_hosts_motd, From, To, Packet},
	    adhoc:produce_response(Response);
	{?NS_ADMIN_s ++ "#edit-motd", _} ->
	    Proc ! {announce_motd_update, From, To, Packet},
	    adhoc:produce_response(Response);
	{?NS_ADMIN_s ++ "#edit-motd-allhosts", _} ->	    
	    Proc ! {announce_all_hosts_motd_update, From, To, Packet},
	    adhoc:produce_response(Response);
	_ ->
	    %% This can't happen, as we haven't registered any other
	    %% command nodes.
	    {error, 'internal-server-error'}
    end.
get_title(Lang, Node) when is_list(Node) ->
    get_title(Lang, list_to_binary(Node));
get_title(Lang, <<"announce">>) ->
    translate:translate(Lang, "Announcements");
get_title(Lang, <<?NS_ADMIN_s, "#announce-all">>) ->
    translate:translate(Lang, "Send announcement to all users");
get_title(Lang, <<?NS_ADMIN_s , "#announce-all-allhosts">>) ->
    translate:translate(Lang, "Send announcement to all users on all hosts");
get_title(Lang, <<?NS_ADMIN_s , "#announce">>) ->
    translate:translate(Lang, "Send announcement to all online users");
get_title(Lang, <<?NS_ADMIN_s , "#announce-allhosts">>) ->
    translate:translate(Lang, "Send announcement to all online users on all hosts");
get_title(Lang, <<?NS_ADMIN_s , "#set-motd">>) ->
    translate:translate(Lang, "Set message of the day and send to online users");
get_title(Lang, <<?NS_ADMIN_s , "#set-motd-allhosts">>) ->
    translate:translate(Lang, "Set message of the day on all hosts and send to online users");
get_title(Lang, <<?NS_ADMIN_s , "#edit-motd">>) ->
    translate:translate(Lang, "Update message of the day (don't send)");
get_title(Lang, <<?NS_ADMIN_s , "#edit-motd-allhosts">>) ->
    translate:translate(Lang, "Update message of the day on all hosts (don't send)");
get_title(Lang, <<?NS_ADMIN_s , "#delete-motd">>) ->
    translate:translate(Lang, "Delete message of the day");
get_title(Lang, <<?NS_ADMIN_s , "#delete-motd-allhosts">>) ->
    translate:translate(Lang, "Delete message of the day on all hosts").

%%-------------------------------------------------------------------------

announce_all(From, To, Packet) ->
    Host = exmpp_jid:prep_domain_as_list(To),
    Access = gen_mod:get_module_opt(Host, ?MODULE, access, none),
    case acl:match_rule(Host, Access, From) of
	deny ->
	    Err = exmpp_stanza:reply_with_error(Packet, 'forbidden'),
	    ejabberd_router:route(To, From, Err);
	allow ->
	    Local = exmpp_jid:make(exmpp_jid:domain(To)),
	    lists:foreach(
	      fun({User, Server}) ->
		      Dest = exmpp_jid:make(User, Server),
		      ejabberd_router:route(Local, Dest, Packet)
	      end, ejabberd_auth:get_vh_registered_users(Host))
    end.

announce_all_hosts_all(From, To, Packet) ->
    Access = gen_mod:get_module_opt(global, ?MODULE, access, none),
    case acl:match_rule(global, Access, From) of
	deny ->
	    Err = exmpp_stanza:reply_with_error(Packet, 'forbidden'),
	    ejabberd_router:route(To, From, Err);
	allow ->
	    Local = exmpp_jid:make(exmpp_jid:domain(To)),
	    lists:foreach(
	      fun({User, Server}) ->
		      Dest = exmpp_jid:make(User, Server),
		      ejabberd_router:route(Local, Dest, Packet)
	      end, ejabberd_auth:dirty_get_registered_users())
    end.

announce_online(From, To, Packet) ->
    Host = exmpp_jid:prep_domain_as_list(To),
    Access = gen_mod:get_module_opt(Host, ?MODULE, access, none),
    case acl:match_rule(Host, Access, From) of
	deny ->
	    Err = exmpp_stanza:reply_with_error(Packet, 'forbidden'),
	    ejabberd_router:route(To, From, Err);
	allow ->
	    announce_online1(ejabberd_sm:get_vh_session_list(exmpp_jid:prep_domain(To)),
			     exmpp_jid:domain_as_list(To),
			     Packet)
    end.

announce_all_hosts_online(From, To, Packet) ->
    Access = gen_mod:get_module_opt(global, ?MODULE, access, none),
    case acl:match_rule(global, Access, From) of
	deny ->
	    Err = exmpp_stanza:reply_with_error(Packet, 'forbidden'),
	    ejabberd_router:route(To, From, Err);
	allow ->
	    announce_online1(ejabberd_sm:dirty_get_sessions_list(),
			     exmpp_jid:domain_as_list(To),
			     Packet)
    end.

announce_online1(Sessions, Server, Packet) ->
    Local = exmpp_jid:make(Server),
    lists:foreach(
      fun({U, S, R}) ->
	      Dest = exmpp_jid:make(U, S, R),
	      ejabberd_router:route(Local, Dest, Packet)
      end, Sessions).

announce_motd(From, To, Packet) ->
    Host = exmpp_jid:prep_domain_as_list(To),
    Access = gen_mod:get_module_opt(Host, ?MODULE, access, none),
    case acl:match_rule(Host, Access, From) of
	deny ->
	    Err = exmpp_stanza:reply_with_error(Packet, 'forbidden'),
	    ejabberd_router:route(To, From, Err);
	allow ->
	    announce_motd(Host, Packet)
    end.

announce_all_hosts_motd(From, To, Packet) ->
    Access = gen_mod:get_module_opt(global, ?MODULE, access, none),
    case acl:match_rule(global, Access, From) of
	deny ->
	    Err = exmpp_stanza:reply_with_error(Packet, 'forbidden'),
	    ejabberd_router:route(To, From, Err);
	allow ->
	    Hosts = ?MYHOSTS,
	    [announce_motd(Host, Packet) || Host <- Hosts]
    end.

announce_motd(Host, Packet) ->
    announce_motd_update(Host, Packet),
    Sessions = ejabberd_sm:get_vh_session_list(list_to_binary(Host)),
    announce_online1(Sessions, Host, Packet),
    F = fun() ->
		lists:foreach(
		  fun({U, S, _R}) ->
			  mnesia:write(#motd_users{us = {U, S}})
		  end, Sessions)
	end,
    mnesia:transaction(F).

announce_motd_update(From, To, Packet) ->
    Host = exmpp_jid:prep_domain_as_list(To),
    Access = gen_mod:get_module_opt(Host, ?MODULE, access, none),
    case acl:match_rule(Host, Access, From) of
	deny ->
	    Err = exmpp_stanza:reply_with_error(Packet, 'forbidden'),
	    ejabberd_router:route(To, From, Err);
	allow ->
	    announce_motd_update(Host, Packet)
    end.

announce_all_hosts_motd_update(From, To, Packet) ->
    Access = gen_mod:get_module_opt(global, ?MODULE, access, none),
    case acl:match_rule(global, Access, From) of
	deny ->
	    Err = exmpp_stanza:reply_with_error(Packet, 'forbidden'),
	    ejabberd_router:route(To, From, Err);
	allow ->
	    Hosts = ?MYHOSTS,
	    [announce_motd_update(Host, Packet) || Host <- Hosts]
    end.

announce_motd_update(LServer, Packet) ->
    announce_motd_delete(LServer),
    F = fun() ->
		mnesia:write(#motd{server = LServer, packet = Packet})
	end,
    mnesia:transaction(F).

announce_motd_delete(From, To, Packet) ->
    Host = exmpp_jid:prep_domain_as_list(To),
    Access = gen_mod:get_module_opt(Host, ?MODULE, access, none),
    case acl:match_rule(Host, Access, From) of
	deny ->
	    Err = exmpp_stanza:reply_with_error(Packet, 'forbidden'),
	    ejabberd_router:route(To, From, Err);
	allow ->
	    announce_motd_delete(Host)
    end.

announce_all_hosts_motd_delete(From, To, Packet) ->
    Access = gen_mod:get_module_opt(global, ?MODULE, access, none),
    case acl:match_rule(global, Access, From) of
	deny ->
	    Err = exmpp_stanza:reply_with_error(Packet, 'forbidden'),
	    ejabberd_router:route(To, From, Err);
	allow ->
	    Hosts = ?MYHOSTS,
	    [announce_motd_delete(Host) || Host <- Hosts]
    end.

announce_motd_delete(LServer) ->
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
    mnesia:transaction(F).

send_motd(JID) ->
    LServer = exmpp_jid:prep_domain_as_list(JID), 
    LUser = exmpp_jid:prep_node_as_list(JID), 
    case catch mnesia:dirty_read({motd, LServer}) of
	[#motd{packet = Packet}] ->
	    US = {LUser, LServer},
	    case catch mnesia:dirty_read({motd_users, US}) of
		[#motd_users{}] ->
		    ok;
		_ ->
		    Local = exmpp_jid:make(exmpp_jid:prep_domain(JID)),
		    ejabberd_router:route(Local, JID, Packet),
		    F = fun() ->
				mnesia:write(#motd_users{us = US})
			end,
		    mnesia:transaction(F)
	    end;
	_ ->
	    ok
    end.

get_stored_motd(LServer) ->
    case catch mnesia:dirty_read({motd, LServer}) of
	[#motd{packet = Packet}] ->
	    {exmpp_xml:get_cdata_as_list(exmpp_xml:get_element(Packet, 'subject')),
	     exmpp_xml:get_cdata_as_list(exmpp_xml:get_element(Packet, 'body'))};
	_ ->
	    {"", ""}
    end.

%% This function is similar to others, but doesn't perform any ACL verification
send_announcement_to_all(Host, SubjectS, BodyS) ->
    SubjectEls = if SubjectS /= [] ->
		      [#xmlel{ns = ?NS_JABBER_CLIENT, name = 'subject', children =
			[#xmlcdata{cdata = list_to_binary(SubjectS)}]}];
		 true ->
		      []
	      end,
    BodyEls = if BodyS /= [] ->
		      [#xmlel{ns = ?NS_JABBER_CLIENT, name = 'body', children =
			[#xmlcdata{cdata = list_to_binary(BodyS)}]}];
		 true ->
		      []
	      end,
    Packet = #xmlel{ns = ?NS_JABBER_CLIENT, name = 'message', attrs = [?XMLATTR(<<"type">>, <<"normal">>)], children = SubjectEls ++ BodyEls},
    Sessions = ejabberd_sm:dirty_get_sessions_list(),
    Local = exmpp_jid:make(Host),
    lists:foreach(
      fun({U, S, R}) ->
	      Dest = exmpp_jid:make(U, S, R),
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
