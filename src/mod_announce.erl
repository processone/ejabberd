%%%----------------------------------------------------------------------
%%% File    : mod_announce.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Manage announce messages
%%% Created : 11 Aug 2003 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2015   ProcessOne
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
%%% You should have received a copy of the GNU General Public License along
%%% with this program; if not, write to the Free Software Foundation, Inc.,
%%% 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
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
	 export/1,
         import/1,
         import/3,
	 announce/3,
	 send_motd/1,
	 disco_identity/5,
	 disco_features/5,
	 disco_items/5,
	 send_announcement_to_all/3,
	 announce_commands/4,
	 announce_items/4]).

-include("ejabberd.hrl").
-include("logger.hrl").
-include("jlib.hrl").
-include("adhoc.hrl").

-record(motd, {server = <<"">> :: binary(),
               packet = #xmlel{} :: xmlel()}).
-record(motd_users, {us = {<<"">>, <<"">>} :: {binary(), binary()} | '$1',
                     dummy = [] :: [] | '_'}).

-define(PROCNAME, ejabberd_announce).

-define(NS_ADMINL(Sub), [<<"http:">>, <<"jabber.org">>, <<"protocol">>,
                         <<"admin">>, <<Sub>>]).

tokenize(Node) -> str:tokens(Node, <<"/#">>).

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
announce(From, #jid{luser = <<>>} = To, #xmlel{name = <<"message">>} = Packet) ->
    Proc = gen_mod:get_module_proc(To#jid.lserver, ?PROCNAME),
    case To#jid.lresource of
        <<"announce/all">> ->
            Proc ! {announce_all, From, To, Packet},
            stop;
        <<"announce/all-hosts/all">> ->
            Proc ! {announce_all_hosts_all, From, To, Packet},
            stop;
        <<"announce/online">> ->
            Proc ! {announce_online, From, To, Packet},
            stop;
        <<"announce/all-hosts/online">> ->
            Proc ! {announce_all_hosts_online, From, To, Packet},
            stop;
        <<"announce/motd">> ->
            Proc ! {announce_motd, From, To, Packet},
            stop;
        <<"announce/all-hosts/motd">> ->
            Proc ! {announce_all_hosts_motd, From, To, Packet},
            stop;
        <<"announce/motd/update">> ->
            Proc ! {announce_motd_update, From, To, Packet},
            stop;
        <<"announce/all-hosts/motd/update">> ->
            Proc ! {announce_all_hosts_motd_update, From, To, Packet},
            stop;
        <<"announce/motd/delete">> ->
            Proc ! {announce_motd_delete, From, To, Packet},
            stop;
        <<"announce/all-hosts/motd/delete">> ->
            Proc ! {announce_all_hosts_motd_delete, From, To, Packet},
            stop;
        _ ->
            ok
    end;
announce(_From, _To, _Packet) ->
    ok.

%%-------------------------------------------------------------------------
%% Announcing via ad-hoc commands
-define(INFO_COMMAND(Lang, Node),
        [#xmlel{name  = <<"identity">>,
                attrs = [{<<"category">>, <<"automation">>},
                         {<<"type">>, <<"command-node">>},
                         {<<"name">>, get_title(Lang, Node)}]}]).

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

disco_features(Acc, From, #jid{lserver = LServer} = _To, <<"announce">>, _Lang) ->
    case gen_mod:is_loaded(LServer, mod_adhoc) of
	false ->
	    Acc;
	_ ->
	    Access1 = get_access(LServer),
	    Access2 = get_access(global),
	    case {acl:match_rule(LServer, Access1, From),
		  acl:match_rule(global, Access2, From)} of
		{deny, deny} ->
		    {error, ?ERR_FORBIDDEN};
		_ ->
		    {result, []}
	    end
    end;

disco_features(Acc, From, #jid{lserver = LServer} = _To, Node, _Lang) ->
    case gen_mod:is_loaded(LServer, mod_adhoc) of
	false ->
	    Acc;
	_ ->
	    Access = get_access(LServer),
	    Allow = acl:match_rule(LServer, Access, From),
	    AccessGlobal = get_access(global),
	    AllowGlobal = acl:match_rule(global, AccessGlobal, From),
	    case Node of
		?NS_ADMIN_ANNOUNCE ->
		    ?INFO_RESULT(Allow, [?NS_COMMANDS]);
		?NS_ADMIN_ANNOUNCE_ALL ->
		    ?INFO_RESULT(Allow, [?NS_COMMANDS]);
		?NS_ADMIN_SET_MOTD ->
		    ?INFO_RESULT(Allow, [?NS_COMMANDS]);
		?NS_ADMIN_EDIT_MOTD ->
		    ?INFO_RESULT(Allow, [?NS_COMMANDS]);
		?NS_ADMIN_DELETE_MOTD ->
		    ?INFO_RESULT(Allow, [?NS_COMMANDS]);
		?NS_ADMIN_ANNOUNCE_ALLHOSTS ->
		    ?INFO_RESULT(AllowGlobal, [?NS_COMMANDS]);
		?NS_ADMIN_ANNOUNCE_ALL_ALLHOSTS ->
		    ?INFO_RESULT(AllowGlobal, [?NS_COMMANDS]);
		?NS_ADMIN_SET_MOTD_ALLHOSTS ->
		    ?INFO_RESULT(AllowGlobal, [?NS_COMMANDS]);
		?NS_ADMIN_EDIT_MOTD_ALLHOSTS ->
		    ?INFO_RESULT(AllowGlobal, [?NS_COMMANDS]);
		?NS_ADMIN_DELETE_MOTD_ALLHOSTS ->
		    ?INFO_RESULT(AllowGlobal, [?NS_COMMANDS]);
		_ ->
		    Acc
	    end
    end.

%%-------------------------------------------------------------------------
-define(NODE_TO_ITEM(Lang, Server, Node),
(
    #xmlel{
        name  = <<"item">>,
        attrs = [
            {<<"jid">>,  Server},
            {<<"node">>, Node},
            {<<"name">>, get_title(Lang, Node)}
        ]
    }
)).

-define(ITEMS_RESULT(Allow, Items),
	case Allow of
	    deny ->
		{error, ?ERR_FORBIDDEN};
	    allow ->
		{result, Items}
	end).

disco_items(Acc, From, #jid{lserver = LServer, server = Server} = _To, <<>>, Lang) ->
    case gen_mod:is_loaded(LServer, mod_adhoc) of
	false ->
	    Acc;
	_ ->
	    Access1 = get_access(LServer),
	    Access2 = get_access(global),
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

disco_items(Acc, From, #jid{lserver = LServer} = To, <<"announce">>, Lang) ->
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
	    Access = get_access(LServer),
	    Allow = acl:match_rule(LServer, Access, From),
	    AccessGlobal = get_access(global),
	    AllowGlobal = acl:match_rule(global, AccessGlobal, From),
	    case Node of
		?NS_ADMIN_ANNOUNCE ->
		    ?ITEMS_RESULT(Allow, []);
		?NS_ADMIN_ANNOUNCE_ALL ->
		    ?ITEMS_RESULT(Allow, []);
		?NS_ADMIN_SET_MOTD ->
		    ?ITEMS_RESULT(Allow, []);
		?NS_ADMIN_EDIT_MOTD ->
		    ?ITEMS_RESULT(Allow, []);
		?NS_ADMIN_DELETE_MOTD ->
		    ?ITEMS_RESULT(Allow, []);
		?NS_ADMIN_ANNOUNCE_ALLHOSTS ->
		    ?ITEMS_RESULT(AllowGlobal, []);
		?NS_ADMIN_ANNOUNCE_ALL_ALLHOSTS ->
		    ?ITEMS_RESULT(AllowGlobal, []);
		?NS_ADMIN_SET_MOTD_ALLHOSTS ->
		    ?ITEMS_RESULT(AllowGlobal, []);
		?NS_ADMIN_EDIT_MOTD_ALLHOSTS ->
		    ?ITEMS_RESULT(AllowGlobal, []);
		?NS_ADMIN_DELETE_MOTD_ALLHOSTS ->
		    ?ITEMS_RESULT(AllowGlobal, []);
		_ ->
		    Acc
	    end
    end.

%%-------------------------------------------------------------------------

announce_items(Acc, From, #jid{lserver = LServer, server = Server} = _To, Lang) ->
    Access1 = get_access(LServer),
    Nodes1 = case acl:match_rule(LServer, Access1, From) of
		 allow ->
		     [?NODE_TO_ITEM(Lang, Server, ?NS_ADMIN_ANNOUNCE),
		      ?NODE_TO_ITEM(Lang, Server, ?NS_ADMIN_ANNOUNCE_ALL),
		      ?NODE_TO_ITEM(Lang, Server, ?NS_ADMIN_SET_MOTD),
		      ?NODE_TO_ITEM(Lang, Server, ?NS_ADMIN_EDIT_MOTD),
		      ?NODE_TO_ITEM(Lang, Server, ?NS_ADMIN_DELETE_MOTD)];
		 deny ->
		     []
	     end,
    Access2 = get_access(global),
    Nodes2 = case acl:match_rule(global, Access2, From) of
		 allow ->
		     [?NODE_TO_ITEM(Lang, Server, ?NS_ADMIN_ANNOUNCE_ALLHOSTS),
		      ?NODE_TO_ITEM(Lang, Server, ?NS_ADMIN_ANNOUNCE_ALL_ALLHOSTS),
		      ?NODE_TO_ITEM(Lang, Server, ?NS_ADMIN_SET_MOTD_ALLHOSTS),
		      ?NODE_TO_ITEM(Lang, Server, ?NS_ADMIN_EDIT_MOTD_ALLHOSTS),
		      ?NODE_TO_ITEM(Lang, Server, ?NS_ADMIN_DELETE_MOTD_ALLHOSTS)];
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
		Access = get_access(global),
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
		Access = get_access(LServer),
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
    ActionIsExecute = lists:member(Action, [<<>>, <<"execute">>, <<"complete">>]),
    if Action == <<"cancel">> ->
	    %% User cancels request
	    adhoc:produce_response(Request, #adhoc_response{status = canceled});
       XData == false, ActionIsExecute ->
	    %% User requests form
	    Elements = generate_adhoc_form(Lang, Node, To#jid.lserver),
	    adhoc:produce_response(Request,
	      #adhoc_response{status = executing,elements = [Elements]});
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
(
    #xmlel{
        name     = <<"value">>,
        children = [{xmlcdata, Val}]
    }
)).

-define(TVFIELD(Type, Var, Val),
(
    #xmlel{
        name     = <<"field">>,
        attrs    = [{<<"type">>, Type}, {<<"var">>, Var}],
        children = vvaluel(Val)
    }
)).

-define(HFIELD(), ?TVFIELD(<<"hidden">>, <<"FORM_TYPE">>, ?NS_ADMIN)).

vvaluel(Val) ->
    case Val of
        <<>> -> [];
        _ -> [?VVALUE(Val)]
    end.

generate_adhoc_form(Lang, Node, ServerHost) ->
    LNode = tokenize(Node),
    {OldSubject, OldBody} = if (LNode == ?NS_ADMINL("edit-motd")) 
			       or (LNode == ?NS_ADMINL("edit-motd-allhosts")) ->
				    get_stored_motd(ServerHost);
			       true -> 
				    {<<>>, <<>>}
			    end,
    #xmlel{
        name     = <<"x">>,
        attrs    = [{<<"xmlns">>, ?NS_XDATA}, {<<"type">>, <<"form">>}],
        children = [
            ?HFIELD(),
            #xmlel{name = <<"title">>, children = [{xmlcdata, get_title(Lang, Node)}]}
        ]
        ++
        if (LNode == ?NS_ADMINL("delete-motd"))
        or (LNode == ?NS_ADMINL("delete-motd-allhosts")) ->
            [#xmlel{
                 name     = <<"field">>,
                 attrs    = [
                    {<<"var">>, <<"confirm">>},
                    {<<"type">>, <<"boolean">>},
                    {<<"label">>,
                     translate:translate(Lang, <<"Really delete message of the day?">>)}
                 ],
                 children = [
                    #xmlel{name = <<"value">>, children = [{xmlcdata, <<"true">>}]}
                 ]
             }
            ];
        true ->
            [#xmlel{
                 name     = <<"field">>,
                 attrs    = [
                    {<<"var">>, <<"subject">>},
                    {<<"type">>, <<"text-single">>},
                    {<<"label">>, translate:translate(Lang, <<"Subject">>)}],
                 children = vvaluel(OldSubject)
             },
             #xmlel{
                 name     = <<"field">>,
                 attrs    = [
                     {<<"var">>, <<"body">>},
                     {<<"type">>, <<"text-multi">>},
                     {<<"label">>, translate:translate(Lang, <<"Message body">>)}],
                 children = vvaluel(OldBody)
             }
            ]

    end}.

join_lines([]) ->
    <<>>;
join_lines(Lines) ->
    join_lines(Lines, []).
join_lines([Line|Lines], Acc) ->
    join_lines(Lines, [<<"\n">>,Line|Acc]);
join_lines([], Acc) ->
    %% Remove last newline
    iolist_to_binary(lists:reverse(tl(Acc))).

handle_adhoc_form(From, #jid{lserver = LServer} = To,
		  #adhoc_request{lang = Lang,
				 node = Node,
				 sessionid = SessionID},
		  Fields) ->
    Confirm = case lists:keysearch(<<"confirm">>, 1, Fields) of
		  {value, {<<"confirm">>, [<<"true">>]}} ->
		      true;
		  {value, {<<"confirm">>, [<<"1">>]}} ->
		      true;
		  _ ->
		      false
	      end,
    Subject = case lists:keysearch(<<"subject">>, 1, Fields) of
		  {value, {<<"subject">>, SubjectLines}} ->
		      %% There really shouldn't be more than one
		      %% subject line, but can we stop them?
		      join_lines(SubjectLines);
		  _ ->
		      <<>>
	      end,
    Body = case lists:keysearch(<<"body">>, 1, Fields) of
	       {value, {<<"body">>, BodyLines}} ->
		   join_lines(BodyLines);
	       _ ->
		   <<>>
	   end,
    Response = #adhoc_response{lang = Lang,
			       node = Node,
			       sessionid = SessionID,
			       status = completed},
    Packet = #xmlel{
        name     = <<"message">>,
        attrs    = [{<<"type">>, <<"headline">>}],
        children = if Subject /= <<>> ->
            [#xmlel{name = <<"subject">>, children = [{xmlcdata, Subject}]}];
        true ->
            []
        end
        ++
        if Body /= <<>> ->
            [#xmlel{name = <<"body">>, children = [{xmlcdata, Body}]}];
        true ->
            []
        end
    },
    Proc = gen_mod:get_module_proc(LServer, ?PROCNAME),
    case {Node, Body} of
	{?NS_ADMIN_DELETE_MOTD, _} ->
	    if	Confirm ->
		    Proc ! {announce_motd_delete, From, To, Packet},
		    adhoc:produce_response(Response);
		true ->
		    adhoc:produce_response(Response)
	    end;
	{?NS_ADMIN_DELETE_MOTD_ALLHOSTS, _} ->
	    if	Confirm ->
		    Proc ! {announce_all_hosts_motd_delete, From, To, Packet},
		    adhoc:produce_response(Response);
		true ->
		    adhoc:produce_response(Response)
	    end;
	{_, <<>>} ->
	    %% An announce message with no body is definitely an operator error.
	    %% Throw an error and give him/her a chance to send message again.
	    {error, ?ERRT_NOT_ACCEPTABLE(Lang,
		       <<"No body provided for announce message">>)};
	%% Now send the packet to ?PROCNAME.
	%% We don't use direct announce_* functions because it
	%% leads to large delay in response and <iq/> queries processing
	{?NS_ADMIN_ANNOUNCE, _} ->
	    Proc ! {announce_online, From, To, Packet},
	    adhoc:produce_response(Response);
	{?NS_ADMIN_ANNOUNCE_ALLHOSTS, _} ->	    
	    Proc ! {announce_all_hosts_online, From, To, Packet},
	    adhoc:produce_response(Response);
	{?NS_ADMIN_ANNOUNCE_ALL, _} ->
	    Proc ! {announce_all, From, To, Packet},
	    adhoc:produce_response(Response);
	{?NS_ADMIN_ANNOUNCE_ALL_ALLHOSTS, _} ->	    
	    Proc ! {announce_all_hosts_all, From, To, Packet},
	    adhoc:produce_response(Response);
	{?NS_ADMIN_SET_MOTD, _} ->
	    Proc ! {announce_motd, From, To, Packet},
	    adhoc:produce_response(Response);
	{?NS_ADMIN_SET_MOTD_ALLHOSTS, _} ->	    
	    Proc ! {announce_all_hosts_motd, From, To, Packet},
	    adhoc:produce_response(Response);
	{?NS_ADMIN_EDIT_MOTD, _} ->
	    Proc ! {announce_motd_update, From, To, Packet},
	    adhoc:produce_response(Response);
	{?NS_ADMIN_EDIT_MOTD_ALLHOSTS, _} ->	    
	    Proc ! {announce_all_hosts_motd_update, From, To, Packet},
	    adhoc:produce_response(Response);
	_ ->
	    %% This can't happen, as we haven't registered any other
	    %% command nodes.
	    {error, ?ERR_INTERNAL_SERVER_ERROR}
    end.

get_title(Lang, <<"announce">>) ->
    translate:translate(Lang, <<"Announcements">>);
get_title(Lang, ?NS_ADMIN_ANNOUNCE_ALL) ->
    translate:translate(Lang, <<"Send announcement to all users">>);
get_title(Lang, ?NS_ADMIN_ANNOUNCE_ALL_ALLHOSTS) ->
    translate:translate(Lang, <<"Send announcement to all users on all hosts">>);
get_title(Lang, ?NS_ADMIN_ANNOUNCE) ->
    translate:translate(Lang, <<"Send announcement to all online users">>);
get_title(Lang, ?NS_ADMIN_ANNOUNCE_ALLHOSTS) ->
    translate:translate(Lang, <<"Send announcement to all online users on all hosts">>);
get_title(Lang, ?NS_ADMIN_SET_MOTD) ->
    translate:translate(Lang, <<"Set message of the day and send to online users">>);
get_title(Lang, ?NS_ADMIN_SET_MOTD_ALLHOSTS) ->
    translate:translate(Lang, <<"Set message of the day on all hosts and send to online users">>);
get_title(Lang, ?NS_ADMIN_EDIT_MOTD) ->
    translate:translate(Lang, <<"Update message of the day (don't send)">>);
get_title(Lang, ?NS_ADMIN_EDIT_MOTD_ALLHOSTS) ->
    translate:translate(Lang, <<"Update message of the day on all hosts (don't send)">>);
get_title(Lang, ?NS_ADMIN_DELETE_MOTD) ->
    translate:translate(Lang, <<"Delete message of the day">>);
get_title(Lang, ?NS_ADMIN_DELETE_MOTD_ALLHOSTS) ->
    translate:translate(Lang, <<"Delete message of the day on all hosts">>).

%%-------------------------------------------------------------------------

announce_all(From, To, Packet) ->
    Host = To#jid.lserver,
    Access = get_access(Host),
    case acl:match_rule(Host, Access, From) of
	deny ->
	    Err = jlib:make_error_reply(Packet, ?ERR_FORBIDDEN),
	    ejabberd_router:route(To, From, Err);
	allow ->
	    Local = jlib:make_jid(<<>>, To#jid.server, <<>>),
	    lists:foreach(
	      fun({User, Server}) ->
		      Dest = jlib:make_jid(User, Server, <<>>),
		      ejabberd_router:route(Local, Dest, Packet)
	      end, ejabberd_auth:get_vh_registered_users(Host))
    end.

announce_all_hosts_all(From, To, Packet) ->
    Access = get_access(global),
    case acl:match_rule(global, Access, From) of
	deny ->
	    Err = jlib:make_error_reply(Packet, ?ERR_FORBIDDEN),
	    ejabberd_router:route(To, From, Err);
	allow ->
	    Local = jlib:make_jid(<<>>, To#jid.server, <<>>),
	    lists:foreach(
	      fun({User, Server}) ->
		      Dest = jlib:make_jid(User, Server, <<>>),
		      ejabberd_router:route(Local, Dest, Packet)
	      end, ejabberd_auth:dirty_get_registered_users())
    end.

announce_online(From, To, Packet) ->
    Host = To#jid.lserver,
    Access = get_access(Host),
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
    Access = get_access(global),
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
    Local = jlib:make_jid(<<>>, Server, <<>>),
    lists:foreach(
      fun({U, S, R}) ->
	      Dest = jlib:make_jid(U, S, R),
	      ejabberd_router:route(Local, Dest, Packet)
      end, Sessions).

announce_motd(From, To, Packet) ->
    Host = To#jid.lserver,
    Access = get_access(Host),
    case acl:match_rule(Host, Access, From) of
	deny ->
	    Err = jlib:make_error_reply(Packet, ?ERR_FORBIDDEN),
	    ejabberd_router:route(To, From, Err);
	allow ->
	    announce_motd(Host, Packet)
    end.

announce_all_hosts_motd(From, To, Packet) ->
    Access = get_access(global),
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
        riak ->
            try
                lists:foreach(
                  fun({U, S, _R}) ->
                          ok = ejabberd_riak:put(#motd_users{us = {U, S}},
						 motd_users_schema(),
                                                 [{'2i', [{<<"server">>, S}]}])
                  end, Sessions),
                {atomic, ok}
            catch _:{badmatch, Err} ->
                    {atomic, Err}
            end;
        odbc ->
            F = fun() ->
                        lists:foreach(
                          fun({U, _S, _R}) ->
                                  Username = ejabberd_odbc:escape(U),
                                  odbc_queries:update_t(
                                    <<"motd">>,
                                    [<<"username">>, <<"xml">>],
                                    [Username, <<"">>],
                                    [<<"username='">>, Username, <<"'">>])
                          end, Sessions)
                end,
            ejabberd_odbc:sql_transaction(LServer, F)
    end.

announce_motd_update(From, To, Packet) ->
    Host = To#jid.lserver,
    Access = get_access(Host),
    case acl:match_rule(Host, Access, From) of
	deny ->
	    Err = jlib:make_error_reply(Packet, ?ERR_FORBIDDEN),
	    ejabberd_router:route(To, From, Err);
	allow ->
	    announce_motd_update(Host, Packet)
    end.

announce_all_hosts_motd_update(From, To, Packet) ->
    Access = get_access(global),
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
        riak ->
            {atomic, ejabberd_riak:put(#motd{server = LServer,
                                             packet = Packet},
				       motd_schema())};
        odbc ->
            XML = ejabberd_odbc:escape(xml:element_to_binary(Packet)),
            F = fun() ->
                        odbc_queries:update_t(
                          <<"motd">>,
                          [<<"username">>, <<"xml">>],
                          [<<"">>, XML],
                          [<<"username=''">>])
                end,
            ejabberd_odbc:sql_transaction(LServer, F)
    end.

announce_motd_delete(From, To, Packet) ->
    Host = To#jid.lserver,
    Access = get_access(Host),
    case acl:match_rule(Host, Access, From) of
	deny ->
	    Err = jlib:make_error_reply(Packet, ?ERR_FORBIDDEN),
	    ejabberd_router:route(To, From, Err);
	allow ->
	    announce_motd_delete(Host)
    end.

announce_all_hosts_motd_delete(From, To, Packet) ->
    Access = get_access(global),
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
        riak ->
            try
                ok = ejabberd_riak:delete(motd, LServer),
                ok = ejabberd_riak:delete_by_index(motd_users,
                                                   <<"server">>,
                                                   LServer),
                {atomic, ok}
            catch _:{badmatch, Err} ->
                    {atomic, Err}
            end;
        odbc ->
            F = fun() ->
                        ejabberd_odbc:sql_query_t([<<"delete from motd;">>])
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
		    Local = jlib:make_jid(<<>>, LServer, <<>>),
		    ejabberd_router:route(Local, JID, Packet),
		    F = fun() ->
				mnesia:write(#motd_users{us = US})
			end,
		    mnesia:transaction(F)
	    end;
	_ ->
	    ok
    end;
send_motd(#jid{luser = LUser, lserver = LServer} = JID, riak) ->
    case catch ejabberd_riak:get(motd, motd_schema(), LServer) of
        {ok, #motd{packet = Packet}} ->
            US = {LUser, LServer},
            case ejabberd_riak:get(motd_users, motd_users_schema(), US) of
                {ok, #motd_users{}} ->
                    ok;
                _ ->
                    Local = jlib:make_jid(<<>>, LServer, <<>>),
		    ejabberd_router:route(Local, JID, Packet),
                    {atomic, ejabberd_riak:put(
                               #motd_users{us = US}, motd_users_schema(),
                               [{'2i', [{<<"server">>, LServer}]}])}
            end;
        _ ->
            ok
    end;
send_motd(#jid{luser = LUser, lserver = LServer} = JID, odbc) when LUser /= <<>> ->
    case catch ejabberd_odbc:sql_query(
                 LServer, [<<"select xml from motd where username='';">>]) of
        {selected, [<<"xml">>], [[XML]]} ->
            case xml_stream:parse_element(XML) of
                {error, _} ->
                    ok;
                Packet ->
                    Username = ejabberd_odbc:escape(LUser),
                    case catch ejabberd_odbc:sql_query(
                                 LServer,
                                 [<<"select username from motd "
                                    "where username='">>, Username, <<"';">>]) of
                        {selected, [<<"username">>], []} ->
                            Local = jlib:make_jid(<<"">>, LServer, <<"">>),
                            ejabberd_router:route(Local, JID, Packet),
                            F = fun() ->
                                        odbc_queries:update_t(
                                          <<"motd">>,
                                          [<<"username">>, <<"xml">>],
                                          [Username, <<"">>],
                                          [<<"username='">>, Username, <<"'">>])
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
            {xml:get_subtag_cdata(Packet, <<"subject">>),
             xml:get_subtag_cdata(Packet, <<"body">>)};
        error ->
            {<<>>, <<>>}
    end.

get_stored_motd_packet(LServer, mnesia) ->
    case catch mnesia:dirty_read({motd, LServer}) of
	[#motd{packet = Packet}] ->
            {ok, Packet};
	_ ->
	    error
    end;
get_stored_motd_packet(LServer, riak) ->
    case ejabberd_riak:get(motd, motd_schema(), LServer) of
        {ok, #motd{packet = Packet}} ->
            {ok, Packet};
	_ ->
	    error
    end;
get_stored_motd_packet(LServer, odbc) ->
    case catch ejabberd_odbc:sql_query(
                 LServer, [<<"select xml from motd where username='';">>]) of
        {selected, [<<"xml">>], [[XML]]} ->
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
    SubjectEls = if SubjectS /= <<>> ->
        [#xmlel{name = <<"subject">>, children = [{xmlcdata, SubjectS}]}];
    true ->
        []
    end,
    BodyEls = if BodyS /= <<>> ->
        [#xmlel{name = <<"body">>, children = [{xmlcdata, BodyS}]}];
    true ->
        []
    end,
    Packet = #xmlel{
        name     = <<"message">>,
        attrs    = [{<<"type">>, <<"headline">>}],
        children = SubjectEls ++ BodyEls
    },
    Sessions = ejabberd_sm:dirty_get_sessions_list(),
    Local = jlib:make_jid(<<>>, Host, <<>>),
    lists:foreach(
      fun({U, S, R}) ->
	      Dest = jlib:make_jid(U, S, R),
	      ejabberd_router:route(Local, Dest, Packet)
      end, Sessions).

-spec get_access(global | binary()) -> atom().

get_access(Host) ->
    gen_mod:get_module_opt(Host, ?MODULE, access,
                           fun(A) when is_atom(A) -> A end,
                           none).

%%-------------------------------------------------------------------------

update_tables() ->
    update_motd_table(),
    update_motd_users_table().

update_motd_table() ->
    Fields = record_info(fields, motd),
    case mnesia:table_info(motd, attributes) of
	Fields ->
            ejabberd_config:convert_table_to_binary(
              motd, Fields, set,
              fun(#motd{server = S}) -> S end,
              fun(#motd{server = S, packet = P} = R) ->
                      NewS = iolist_to_binary(S),
                      NewP = xml:to_xmlel(P),
                      R#motd{server = NewS, packet = NewP}
              end);
	_ ->
	    ?INFO_MSG("Recreating motd table", []),
	    mnesia:transform_table(motd, ignore, Fields)
    end.


update_motd_users_table() ->
    Fields = record_info(fields, motd_users),
    case mnesia:table_info(motd_users, attributes) of
	Fields ->
	    ejabberd_config:convert_table_to_binary(
              motd_users, Fields, set,
              fun(#motd_users{us = {U, _}}) -> U end,
              fun(#motd_users{us = {U, S}} = R) ->
                      NewUS = {iolist_to_binary(U),
                               iolist_to_binary(S)},
                      R#motd_users{us = NewUS}
              end);
	_ ->
	    ?INFO_MSG("Recreating motd_users table", []),
	    mnesia:transform_table(motd_users, ignore, Fields)
    end.

motd_schema() ->
    {record_info(fields, motd), #motd{}}.

motd_users_schema() ->
    {record_info(fields, motd_users), #motd_users{}}.

export(_Server) ->
    [{motd,
      fun(Host, #motd{server = LServer, packet = El})
            when LServer == Host ->
              [[<<"delete from motd where username='';">>],
               [<<"insert into motd(username, xml) values ('', '">>,
                ejabberd_odbc:escape(xml:element_to_binary(El)),
                <<"');">>]];
         (_Host, _R) ->
              []
      end},
     {motd_users,
      fun(Host, #motd_users{us = {LUser, LServer}})
            when LServer == Host, LUser /= <<"">> ->
              Username = ejabberd_odbc:escape(LUser),
              [[<<"delete from motd where username='">>, Username, <<"';">>],
               [<<"insert into motd(username, xml) values ('">>,
                Username, <<"', '');">>]];
         (_Host, _R) ->
              []
      end}].

import(LServer) ->
    [{<<"select xml from motd where username='';">>,
      fun([XML]) ->
              El = xml_stream:parse_element(XML),
              #motd{server = LServer, packet = El}
      end},
     {<<"select username from motd where xml='';">>,
      fun([LUser]) ->
              #motd_users{us = {LUser, LServer}}
      end}].

import(_LServer, mnesia, #motd{} = Motd) ->
    mnesia:dirty_write(Motd);
import(_LServer, mnesia, #motd_users{} = Users) ->
    mnesia:dirty_write(Users);
import(_LServer, riak, #motd{} = Motd) ->
    ejabberd_riak:put(Motd, motd_schema());
import(_LServer, riak, #motd_users{us = {_, S}} = Users) ->
    ejabberd_riak:put(Users, motd_users_schema(),
		      [{'2i', [{<<"server">>, S}]}]);
import(_, _, _) ->
    pass.
