%%%----------------------------------------------------------------------
%%% File    : mod_announce.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Manage announce messages
%%% Created : 11 Aug 2003 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2017   ProcessOne
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

-behaviour(gen_server).
-behaviour(gen_mod).

-export([start/2, stop/1, reload/3, export/1, import_info/0,
	 import_start/2, import/5, announce/1, send_motd/1, disco_identity/5,
	 disco_features/5, disco_items/5, depends/2,
	 send_announcement_to_all/3, announce_commands/4,
	 announce_items/4, mod_opt_type/1]).
-export([init/1, handle_call/3, handle_cast/2,
	 handle_info/2, terminate/2, code_change/3]).
-export([announce_all/1,
	 announce_all_hosts_all/1,
	 announce_online/1,
	 announce_all_hosts_online/1,
	 announce_motd/1,
	 announce_all_hosts_motd/1,
	 announce_motd_update/1,
	 announce_all_hosts_motd_update/1,
	 announce_motd_delete/1,
	 announce_all_hosts_motd_delete/1]).

-include("ejabberd.hrl").
-include("logger.hrl").
-include("xmpp.hrl").
-include("mod_announce.hrl").

-callback init(binary(), gen_mod:opts()) -> any().
-callback import(binary(), binary(), [binary()]) -> ok.
-callback set_motd_users(binary(), [{binary(), binary(), binary()}]) -> {atomic, any()}.
-callback set_motd(binary(), xmlel()) -> {atomic, any()}.
-callback delete_motd(binary()) -> {atomic, any()}.
-callback get_motd(binary()) -> {ok, xmlel()} | error.
-callback is_motd_user(binary(), binary()) -> boolean().
-callback set_motd_user(binary(), binary()) -> {atomic, any()}.

-record(state, {host :: binary()}).

-define(NS_ADMINL(Sub), [<<"http:">>, <<"jabber.org">>, <<"protocol">>,
                         <<"admin">>, <<Sub>>]).

tokenize(Node) -> str:tokens(Node, <<"/#">>).

%%====================================================================
%% gen_mod callbacks
%%====================================================================
start(Host, Opts) ->
    gen_mod:start_child(?MODULE, Host, Opts).

stop(Host) ->
    gen_mod:stop_child(?MODULE, Host).

reload(Host, NewOpts, OldOpts) ->
    NewMod = gen_mod:db_mod(Host, NewOpts, ?MODULE),
    OldMod = gen_mod:db_mod(Host, OldOpts, ?MODULE),
    if NewMod /= OldMod ->
	    NewMod:init(Host, NewOpts);
       true ->
	    ok
    end,
    ok.

depends(_Host, _Opts) ->
    [{mod_adhoc, hard}].

%%====================================================================
%% gen_server callbacks
%%====================================================================
init([Host, Opts]) ->
    process_flag(trap_exit, true),
    Mod = gen_mod:db_mod(Host, Opts, ?MODULE),
    Mod:init(Host, Opts),
    ejabberd_hooks:add(local_send_to_resource_hook, Host,
		       ?MODULE, announce, 50),
    ejabberd_hooks:add(disco_local_identity, Host, ?MODULE, disco_identity, 50),
    ejabberd_hooks:add(disco_local_features, Host, ?MODULE, disco_features, 50),
    ejabberd_hooks:add(disco_local_items, Host, ?MODULE, disco_items, 50),
    ejabberd_hooks:add(adhoc_local_items, Host, ?MODULE, announce_items, 50),
    ejabberd_hooks:add(adhoc_local_commands, Host, ?MODULE, announce_commands, 50),
    ejabberd_hooks:add(c2s_self_presence, Host,
		       ?MODULE, send_motd, 50),
    {ok, #state{host = Host}}.

handle_call(_Call, _From, State) ->
    {noreply, State}.

handle_cast({F, #message{from = From, to = To} = Pkt}, State) when is_atom(F) ->
    LServer = To#jid.lserver,
    Host = case F of
	       announce_all -> LServer;
	       announce_all_hosts_all -> global;
	       announce_online -> LServer;
	       announce_all_hosts_online -> global;
	       announce_motd -> LServer;
	       announce_all_hosts_motd -> global;
	       announce_motd_update -> LServer;
	       announce_all_hosts_motd_update -> global;
	       announce_motd_delete -> LServer;
	       announce_all_hosts_motd_delete -> global
	   end,
    Access = get_access(Host),
    case acl:match_rule(Host, Access, From) of
	deny ->
	    route_forbidden_error(Pkt);
	allow ->
	    ?MODULE:F(Pkt)
    end,
    {noreply, State};
handle_cast(Msg, State) ->
    ?WARNING_MSG("unexpected cast: ~p", [Msg]),
    {noreply, State}.


handle_info(Info, State) ->
    ?WARNING_MSG("unexpected info: ~p", [Info]),
    {noreply, State}.

terminate(_Reason, #state{host = Host}) ->
    ejabberd_hooks:delete(adhoc_local_commands, Host, ?MODULE, announce_commands, 50),
    ejabberd_hooks:delete(adhoc_local_items, Host, ?MODULE, announce_items, 50),
    ejabberd_hooks:delete(disco_local_identity, Host, ?MODULE, disco_identity, 50),
    ejabberd_hooks:delete(disco_local_features, Host, ?MODULE, disco_features, 50),
    ejabberd_hooks:delete(disco_local_items, Host, ?MODULE, disco_items, 50),
    ejabberd_hooks:delete(local_send_to_resource_hook, Host, ?MODULE, announce, 50),
    ejabberd_hooks:delete(c2s_self_presence, Host, ?MODULE, send_motd, 50).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Announcing via messages to a custom resource
-spec announce(stanza()) -> ok | stop.
announce(#message{to = #jid{luser = <<>>} = To} = Packet) ->
    Proc = gen_mod:get_module_proc(To#jid.lserver, ?MODULE),
    Res = case To#jid.lresource of
	      <<"announce/all">> ->
		  gen_server:cast(Proc, {announce_all, Packet});
	      <<"announce/all-hosts/all">> ->
		  gen_server:cast(Proc, {announce_all_hosts_all, Packet});
	      <<"announce/online">> ->
		  gen_server:cast(Proc, {announce_online, Packet});
	      <<"announce/all-hosts/online">> ->
		  gen_server:cast(Proc, {announce_all_hosts_online, Packet});
	      <<"announce/motd">> ->
		  gen_server:cast(Proc, {announce_motd, Packet});
	      <<"announce/all-hosts/motd">> ->
		  gen_server:cast(Proc, {announce_all_hosts_motd, Packet});
	      <<"announce/motd/update">> ->
		  gen_server:cast(Proc, {announce_motd_update, Packet});
	      <<"announce/all-hosts/motd/update">> ->
		  gen_server:cast(Proc, {announce_all_hosts_motd_update, Packet});
	      <<"announce/motd/delete">> ->
		  gen_server:cast(Proc, {announce_motd_delete, Packet});
	      <<"announce/all-hosts/motd/delete">> ->
		  gen_server:cast(Proc, {announce_all_hosts_motd_delete, Packet});
	      _ ->
		  undefined
	  end,
    case Res of
	ok -> stop;
	_ -> ok
    end;
announce(_Packet) ->
    ok.

%%-------------------------------------------------------------------------
%% Announcing via ad-hoc commands
-define(INFO_COMMAND(Lang, Node),
	[#identity{category = <<"automation">>,
		   type = <<"command-node">>,
		   name = get_title(Lang, Node)}]).

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

-define(INFO_RESULT(Allow, Feats, Lang),
	case Allow of
	    deny ->
		{error, xmpp:err_forbidden(<<"Denied by ACL">>, Lang)};
	    allow ->
		{result, Feats}
	end).

disco_features(Acc, From, #jid{lserver = LServer} = _To, <<"announce">>, Lang) ->
    case gen_mod:is_loaded(LServer, mod_adhoc) of
	false ->
	    Acc;
	_ ->
	    Access1 = get_access(LServer),
	    Access2 = get_access(global),
	    case {acl:match_rule(LServer, Access1, From),
		  acl:match_rule(global, Access2, From)} of
		{deny, deny} ->
		    Txt = <<"Denied by ACL">>,
		    {error, xmpp:err_forbidden(Txt, Lang)};
		_ ->
		    {result, []}
	    end
    end;

disco_features(Acc, From, #jid{lserver = LServer} = _To, Node, Lang) ->
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
		    ?INFO_RESULT(Allow, [?NS_COMMANDS], Lang);
		?NS_ADMIN_ANNOUNCE_ALL ->
		    ?INFO_RESULT(Allow, [?NS_COMMANDS], Lang);
		?NS_ADMIN_SET_MOTD ->
		    ?INFO_RESULT(Allow, [?NS_COMMANDS], Lang);
		?NS_ADMIN_EDIT_MOTD ->
		    ?INFO_RESULT(Allow, [?NS_COMMANDS], Lang);
		?NS_ADMIN_DELETE_MOTD ->
		    ?INFO_RESULT(Allow, [?NS_COMMANDS], Lang);
		?NS_ADMIN_ANNOUNCE_ALLHOSTS ->
		    ?INFO_RESULT(AllowGlobal, [?NS_COMMANDS], Lang);
		?NS_ADMIN_ANNOUNCE_ALL_ALLHOSTS ->
		    ?INFO_RESULT(AllowGlobal, [?NS_COMMANDS], Lang);
		?NS_ADMIN_SET_MOTD_ALLHOSTS ->
		    ?INFO_RESULT(AllowGlobal, [?NS_COMMANDS], Lang);
		?NS_ADMIN_EDIT_MOTD_ALLHOSTS ->
		    ?INFO_RESULT(AllowGlobal, [?NS_COMMANDS], Lang);
		?NS_ADMIN_DELETE_MOTD_ALLHOSTS ->
		    ?INFO_RESULT(AllowGlobal, [?NS_COMMANDS], Lang);
		_ ->
		    Acc
	    end
    end.

%%-------------------------------------------------------------------------
-define(NODE_TO_ITEM(Lang, Server, Node),
	#disco_item{jid = jid:make(Server),
		    node = Node,
		    name = get_title(Lang, Node)}).

-define(ITEMS_RESULT(Allow, Items, Lang),
	case Allow of
	    deny ->
		{error, xmpp:err_forbidden(<<"Denied by ACL">>, Lang)};
	    allow ->
		{result, Items}
	end).

disco_items(Acc, From, #jid{lserver = LServer, server = Server} = _To, <<"">>, Lang) ->
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

disco_items(Acc, From, #jid{lserver = LServer} = _To, Node, Lang) ->
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
		    ?ITEMS_RESULT(Allow, [], Lang);
		?NS_ADMIN_ANNOUNCE_ALL ->
		    ?ITEMS_RESULT(Allow, [], Lang);
		?NS_ADMIN_SET_MOTD ->
		    ?ITEMS_RESULT(Allow, [], Lang);
		?NS_ADMIN_EDIT_MOTD ->
		    ?ITEMS_RESULT(Allow, [], Lang);
		?NS_ADMIN_DELETE_MOTD ->
		    ?ITEMS_RESULT(Allow, [], Lang);
		?NS_ADMIN_ANNOUNCE_ALLHOSTS ->
		    ?ITEMS_RESULT(AllowGlobal, [], Lang);
		?NS_ADMIN_ANNOUNCE_ALL_ALLHOSTS ->
		    ?ITEMS_RESULT(AllowGlobal, [], Lang);
		?NS_ADMIN_SET_MOTD_ALLHOSTS ->
		    ?ITEMS_RESULT(AllowGlobal, [], Lang);
		?NS_ADMIN_EDIT_MOTD_ALLHOSTS ->
		    ?ITEMS_RESULT(AllowGlobal, [], Lang);
		?NS_ADMIN_DELETE_MOTD_ALLHOSTS ->
		    ?ITEMS_RESULT(AllowGlobal, [], Lang);
		_ ->
		    Acc
	    end
    end.

%%-------------------------------------------------------------------------
-spec announce_items(empty | {error, stanza_error()} | {result, [disco_item()]},
			jid(), jid(), binary()) -> {error, stanza_error()} |
						   {result, [disco_item()]} |
						   empty.
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
	    Lang = Request#adhoc_command.lang,
	    {error, xmpp:err_forbidden(<<"Denied by ACL">>, Lang)};
	allow ->
	    announce_commands(From, To, Request)
    end.

-spec announce_commands(empty | adhoc_command(), jid(), jid(), adhoc_command()) ->
			       adhoc_command() | {error, stanza_error()}.
announce_commands(Acc, From, #jid{lserver = LServer} = To,
		  #adhoc_command{node = Node} = Request) ->
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
		  #adhoc_command{lang = Lang,
				 node = Node,
				 sid = SID,
				 xdata = XData,
				 action = Action} = Request) ->
    ActionIsExecute = Action == execute orelse Action == complete,
    if Action == cancel ->
	    %% User cancels request
	    #adhoc_command{status = canceled, lang = Lang, node = Node,
			   sid = SID};
       XData == undefined, ActionIsExecute ->
	    %% User requests form
	    Form = generate_adhoc_form(Lang, Node, To#jid.lserver),
	    #adhoc_command{status = executing, lang = Lang, node = Node,
			   sid = SID, xdata = Form};
       XData /= undefined, ActionIsExecute ->
	    handle_adhoc_form(From, To, Request);
       true ->
	    Txt = <<"Unexpected action">>,
	    {error, xmpp:err_bad_request(Txt, Lang)}
    end.

-define(TVFIELD(Type, Var, Val),
	#xdata_field{type = Type, var = Var, values = vvaluel(Val)}).

vvaluel(Val) ->
    case Val of
        <<>> -> [];
        _ -> [Val]
    end.

generate_adhoc_form(Lang, Node, ServerHost) ->
    LNode = tokenize(Node),
    {OldSubject, OldBody} = if (LNode == ?NS_ADMINL("edit-motd")) 
			       or (LNode == ?NS_ADMINL("edit-motd-allhosts")) ->
				    get_stored_motd(ServerHost);
			       true -> 
				    {<<>>, <<>>}
			    end,
    Fs = if (LNode == ?NS_ADMINL("delete-motd"))
	    or (LNode == ?NS_ADMINL("delete-motd-allhosts")) ->
		 [#xdata_field{type = boolean,
			       var = <<"confirm">>,
			       label = translate:translate(
					 Lang, <<"Really delete message of the day?">>),
			       values = [<<"true">>]}];
	    true ->
		 [#xdata_field{type = 'text-single',
			       var = <<"subject">>,
			       label = translate:translate(Lang, <<"Subject">>),
			       values = vvaluel(OldSubject)},
		  #xdata_field{type = 'text-multi',
			       var = <<"body">>,
			       label = translate:translate(Lang, <<"Message body">>),
			       values = vvaluel(OldBody)}]
	 end,
    #xdata{type = form,
	   title = get_title(Lang, Node),
	   fields = [#xdata_field{type = hidden, var = <<"FORM_TYPE">>,
				  values = [?NS_ADMIN]}|Fs]}.

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
		  #adhoc_command{lang = Lang, node = Node,
				 sid = SessionID, xdata = XData}) ->
    Confirm = case xmpp_util:get_xdata_values(<<"confirm">>, XData) of
		  [<<"true">>] -> true;
		  [<<"1">>] -> true;
		  _ -> false
	      end,
    Subject = join_lines(xmpp_util:get_xdata_values(<<"subject">>, XData)),
    Body = join_lines(xmpp_util:get_xdata_values(<<"body">>, XData)),
    Response = #adhoc_command{lang = Lang, node = Node, sid = SessionID,
			      status = completed},
    Packet = #message{from = From,
		      to = To,
		      type = headline,
		      body = xmpp:mk_text(Body),
		      subject = xmpp:mk_text(Subject)},
    Proc = gen_mod:get_module_proc(LServer, ?MODULE),
    case {Node, Body} of
	{?NS_ADMIN_DELETE_MOTD, _} ->
	    if	Confirm ->
		    gen_server:cast(Proc, {announce_motd_delete, Packet}),
		    Response;
		true ->
		    Response
	    end;
	{?NS_ADMIN_DELETE_MOTD_ALLHOSTS, _} ->
	    if	Confirm ->
		    gen_server:cast(Proc, {announce_all_hosts_motd_delete, Packet}),
		    Response;
		true ->
		    Response
	    end;
	{_, <<>>} ->
	    %% An announce message with no body is definitely an operator error.
	    %% Throw an error and give him/her a chance to send message again.
	    {error, xmpp:err_not_acceptable(
		      <<"No body provided for announce message">>, Lang)};
	%% Now send the packet to ?MODULE.
	%% We don't use direct announce_* functions because it
	%% leads to large delay in response and <iq/> queries processing
	{?NS_ADMIN_ANNOUNCE, _} ->
	    gen_server:cast(Proc, {announce_online, Packet}),
	    Response;
	{?NS_ADMIN_ANNOUNCE_ALLHOSTS, _} ->	    
	    gen_server:cast(Proc, {announce_all_hosts_online, Packet}),
	    Response;
	{?NS_ADMIN_ANNOUNCE_ALL, _} ->
	    gen_server:cast(Proc, {announce_all, Packet}),
	    Response;
	{?NS_ADMIN_ANNOUNCE_ALL_ALLHOSTS, _} ->	    
	    gen_server:cast(Proc, {announce_all_hosts_all, Packet}),
	    Response;
	{?NS_ADMIN_SET_MOTD, _} ->
	    gen_server:cast(Proc, {announce_motd, Packet}),
	    Response;
	{?NS_ADMIN_SET_MOTD_ALLHOSTS, _} ->	    
	    gen_server:cast(Proc, {announce_all_hosts_motd, Packet}),
	    Response;
	{?NS_ADMIN_EDIT_MOTD, _} ->
	    gen_server:cast(Proc, {announce_motd_update, Packet}),
	    Response;
	{?NS_ADMIN_EDIT_MOTD_ALLHOSTS, _} ->	    
	    gen_server:cast(Proc, {announce_all_hosts_motd_update, Packet}),
	    Response;
	Junk ->
	    %% This can't happen, as we haven't registered any other
	    %% command nodes.
	    ?ERROR_MSG("got unexpected node/body = ~p", [Junk]),
	    {error, xmpp:err_internal_server_error()}
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

announce_all(#message{to = To} = Packet) ->
    Local = jid:make(To#jid.server),
    lists:foreach(
      fun({User, Server}) ->
	      Dest = jid:make(User, Server),
	      ejabberd_router:route(
		xmpp:set_from_to(add_store_hint(Packet), Local, Dest))
      end, ejabberd_auth:get_vh_registered_users(To#jid.lserver)).

announce_all_hosts_all(#message{to = To} = Packet) ->
    Local = jid:make(To#jid.server),
    lists:foreach(
      fun({User, Server}) ->
	      Dest = jid:make(User, Server),
	      ejabberd_router:route(
		xmpp:set_from_to(add_store_hint(Packet), Local, Dest))
      end, ejabberd_auth:dirty_get_registered_users()).

announce_online(#message{to = To} = Packet) ->
    announce_online1(ejabberd_sm:get_vh_session_list(To#jid.lserver),
		     To#jid.server, Packet).

announce_all_hosts_online(#message{to = To} = Packet) ->
    announce_online1(ejabberd_sm:dirty_get_sessions_list(),
		     To#jid.server, Packet).

announce_online1(Sessions, Server, Packet) ->
    Local = jid:make(Server),
    lists:foreach(
      fun({U, S, R}) ->
	      Dest = jid:make(U, S, R),
	      ejabberd_router:route(xmpp:set_from_to(Packet, Local, Dest))
      end, Sessions).

announce_motd(#message{to = To} = Packet) ->
    announce_motd(To#jid.lserver, Packet).

announce_all_hosts_motd(Packet) ->
    Hosts = ?MYHOSTS,
    [announce_motd(Host, Packet) || Host <- Hosts].

announce_motd(Host, Packet) ->
    LServer = jid:nameprep(Host),
    announce_motd_update(LServer, Packet),
    Sessions = ejabberd_sm:get_vh_session_list(LServer),
    announce_online1(Sessions, LServer, Packet),
    Mod = gen_mod:db_mod(LServer, ?MODULE),
    Mod:set_motd_users(LServer, Sessions).

announce_motd_update(#message{to = To} = Packet) ->
    announce_motd_update(To#jid.lserver, Packet).

announce_all_hosts_motd_update(Packet) ->
    Hosts = ?MYHOSTS,
    [announce_motd_update(Host, Packet) || Host <- Hosts].

announce_motd_update(LServer, Packet) ->
    Mod = gen_mod:db_mod(LServer, ?MODULE),
    Mod:delete_motd(LServer),
    Mod:set_motd(LServer, xmpp:encode(Packet)).

announce_motd_delete(#message{to = To}) ->
    LServer = To#jid.lserver,
    Mod = gen_mod:db_mod(LServer, ?MODULE),
    Mod:delete_motd(LServer).

announce_all_hosts_motd_delete(_Packet) ->
    lists:foreach(
      fun(Host) ->
	      Mod = gen_mod:db_mod(Host, ?MODULE),
	      Mod:delete_motd(Host)
      end, ?MYHOSTS).

-spec send_motd({presence(), ejabberd_c2s:state()}) -> {presence(), ejabberd_c2s:state()}.
send_motd({_, #{pres_last := _}} = Acc) ->
    %% This is just a presence update, nothing to do
    Acc;
send_motd({#presence{type = available},
	   #{jid := #jid{luser = LUser, lserver = LServer} = JID}} = Acc)
  when LUser /= <<>> ->
    Mod = gen_mod:db_mod(LServer, ?MODULE),
    case Mod:get_motd(LServer) of
	{ok, Packet} ->
	    try xmpp:decode(Packet, ?NS_CLIENT, [ignore_els]) of
		Msg ->
		    case Mod:is_motd_user(LUser, LServer) of
			false ->
			    Local = jid:make(LServer),
			    ejabberd_router:route(
			      xmpp:set_from_to(Msg, Local, JID)),
			    Mod:set_motd_user(LUser, LServer);
			true ->
			    ok
		    end
	    catch _:{xmpp_codec, Why} ->
		    ?ERROR_MSG("failed to decode motd packet ~p: ~s",
			       [Packet, xmpp:format_error(Why)])
	    end;
	error ->
	    ok
    end,
    Acc;
send_motd(Acc) ->
    Acc.

get_stored_motd(LServer) ->
    Mod = gen_mod:db_mod(LServer, ?MODULE),
    case Mod:get_motd(LServer) of
        {ok, Packet} ->
	    try xmpp:decode(Packet, ?NS_CLIENT, [ignore_els]) of
		#message{body = Body, subject = Subject} ->
		    {xmpp:get_text(Subject), xmpp:get_text(Body)}
	    catch _:{xmpp_codec, Why} ->
		    ?ERROR_MSG("failed to decode motd packet ~p: ~s",
			       [Packet, xmpp:format_error(Why)])
	    end;
        error ->
            {<<>>, <<>>}
    end.

%% This function is similar to others, but doesn't perform any ACL verification
send_announcement_to_all(Host, SubjectS, BodyS) ->
    Packet = #message{type = headline,
		      body = xmpp:mk_text(BodyS),
		      subject = xmpp:mk_text(SubjectS)},
    Sessions = ejabberd_sm:dirty_get_sessions_list(),
    Local = jid:make(Host),
    lists:foreach(
      fun({U, S, R}) ->
	      Dest = jid:make(U, S, R),
	      ejabberd_router:route(
		xmpp:set_from_to(add_store_hint(Packet), Local, Dest))
      end, Sessions).

-spec get_access(global | binary()) -> atom().

get_access(Host) ->
    gen_mod:get_module_opt(Host, ?MODULE, access,
                           fun(A) -> A end,
                           none).

-spec add_store_hint(stanza()) -> stanza().
add_store_hint(El) ->
    xmpp:set_subtag(El, #hint{type = store}).

-spec route_forbidden_error(stanza()) -> ok.
route_forbidden_error(Packet) ->
    Lang = xmpp:get_lang(Packet),
    Err = xmpp:err_forbidden(<<"Denied by ACL">>, Lang),
    ejabberd_router:route_error(Packet, Err).

%%-------------------------------------------------------------------------
export(LServer) ->
    Mod = gen_mod:db_mod(LServer, ?MODULE),
    Mod:export(LServer).

import_info() ->
    [{<<"motd">>, 3}].

import_start(LServer, DBType) ->
    Mod = gen_mod:db_mod(DBType, ?MODULE),
    Mod:init(LServer, []).

import(LServer, {sql, _}, DBType, Tab, List) ->
    Mod = gen_mod:db_mod(DBType, ?MODULE),
    Mod:import(LServer, Tab, List).

mod_opt_type(access) -> fun acl:access_rules_validator/1;
mod_opt_type(db_type) -> fun(T) -> ejabberd_config:v_db(?MODULE, T) end;
mod_opt_type(_) -> [access, db_type].
