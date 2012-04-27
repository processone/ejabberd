%%%----------------------------------------------------------------------
%%% File    : mod_irc_odbc.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : IRC transport
%%% Created : 15 Feb 2003 by Alexey Shchepin <alexey@process-one.net>
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

-module(mod_irc_odbc).
-author('alexey@process-one.net').

-behaviour(gen_server).
-behaviour(gen_mod).

%% API
-export([start_link/2,
	 start/2,
	 stop/1,
	 closed_connection/3,
	 get_connection_params/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("ejabberd.hrl").
-include("jlib.hrl").
-include("adhoc.hrl").

-define(DEFAULT_IRC_ENCODING, "iso8859-1").
-define(DEFAULT_IRC_PORT, 6667).
-define(POSSIBLE_ENCODINGS, ["koi8-r", "iso8859-1", "iso8859-2", "utf-8", "utf-8+latin-1"]).

-record(irc_connection, {jid_server_host, pid}).

-record(state, {host, server_host, access}).

-define(PROCNAME, ejabberd_mod_irc).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(Host, Opts) ->
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    gen_server:start_link({local, Proc}, ?MODULE, [Host, Opts], []).

start(Host, Opts) ->
    start_supervisor(Host),
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    ChildSpec =
	{Proc,
	 {?MODULE, start_link, [Host, Opts]},
	 temporary,
	 1000,
	 worker,
	 [?MODULE]},
    supervisor:start_child(ejabberd_sup, ChildSpec).

stop(Host) ->
    stop_supervisor(Host),
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    gen_server:call(Proc, stop),
    supervisor:delete_child(ejabberd_sup, Proc).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([Host, Opts]) ->
    iconv:start(),
    MyHost = gen_mod:get_opt_host(Host, Opts, "irc.@HOST@"),
    Access = gen_mod:get_opt(access, Opts, all),
    catch ets:new(irc_connection, [named_table,
				   public,
				   {keypos, #irc_connection.jid_server_host}]),
    ejabberd_router:register_route(MyHost),
    {ok, #state{host = MyHost,
		server_host = Host,
		access = Access}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(stop, _From, State) ->
    {stop, normal, ok, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info({route, From, To, Packet},
	    #state{host = Host,
		   server_host = ServerHost,
		   access = Access} = State) ->
    case catch do_route(Host, ServerHost, Access, From, To, Packet) of
	{'EXIT', Reason} ->
	    ?ERROR_MSG("~p", [Reason]);
	_ ->
	    ok
    end,
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, State) ->
    ejabberd_router:unregister_route(State#state.host),
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
start_supervisor(Host) ->
    Proc = gen_mod:get_module_proc(Host, ejabberd_mod_irc_sup),
    ChildSpec =
	{Proc,
	 {ejabberd_tmp_sup, start_link,
	  [Proc, mod_irc_connection]},
	 permanent,
	 infinity,
	 supervisor,
	 [ejabberd_tmp_sup]},
    supervisor:start_child(ejabberd_sup, ChildSpec).

stop_supervisor(Host) ->
    Proc = gen_mod:get_module_proc(Host, ejabberd_mod_irc_sup),
    supervisor:terminate_child(ejabberd_sup, Proc),
    supervisor:delete_child(ejabberd_sup, Proc).

do_route(Host, ServerHost, Access, From, To, Packet) ->
    case acl:match_rule(ServerHost, Access, From) of
	allow ->
	    do_route1(Host, ServerHost, From, To, Packet);
	_ ->
	    {xmlelement, _Name, Attrs, _Els} = Packet,
	    Lang = xml:get_attr_s("xml:lang", Attrs),
	    ErrText = "Access denied by service policy",
	    Err = jlib:make_error_reply(Packet,
					?ERRT_FORBIDDEN(Lang, ErrText)),
	    ejabberd_router:route(To, From, Err)
    end.

do_route1(Host, ServerHost, From, To, Packet) ->
    #jid{user = ChanServ, resource = Resource} = To,
    {xmlelement, _Name, _Attrs, _Els} = Packet,
    case ChanServ of
	"" ->
	    case Resource of
		"" ->
		    case jlib:iq_query_info(Packet) of
			#iq{type = get, xmlns = ?NS_DISCO_INFO = XMLNS,
			    sub_el = SubEl, lang = Lang} = IQ ->
			    Node = xml:get_tag_attr_s("node", SubEl),
			    Info = ejabberd_hooks:run_fold(
				     disco_info, ServerHost, [],
				     [ServerHost, ?MODULE, "", ""]),
			    case iq_disco(ServerHost, Node, Lang) of
				[] ->
				    Res = IQ#iq{type = result,
						sub_el = [{xmlelement, "query",
							   [{"xmlns", XMLNS}],
							   []}]},
				    ejabberd_router:route(To,
							  From,
							  jlib:iq_to_xml(Res));
				DiscoInfo ->
				    Res = IQ#iq{type = result,
						sub_el = [{xmlelement, "query",
							   [{"xmlns", XMLNS}],
							   DiscoInfo ++ Info}]},
				    ejabberd_router:route(To,
							  From,
							  jlib:iq_to_xml(Res))
			    end;
			#iq{type = get, xmlns = ?NS_DISCO_ITEMS = XMLNS,
			    sub_el = SubEl, lang = Lang} = IQ ->
			    Node = xml:get_tag_attr_s("node", SubEl),
			    case Node of
				[] ->
				    ResIQ = IQ#iq{type = result,
						sub_el = [{xmlelement, "query",
							   [{"xmlns", XMLNS}],
							   []}]},
				    Res = jlib:iq_to_xml(ResIQ);
				"join" ->
				    ResIQ = IQ#iq{type = result,
						sub_el = [{xmlelement, "query",
							   [{"xmlns", XMLNS}],
							   []}]},
				    Res = jlib:iq_to_xml(ResIQ);
				"register" ->
				    ResIQ = IQ#iq{type = result,
						sub_el = [{xmlelement, "query",
							   [{"xmlns", XMLNS}],
							   []}]},
				    Res = jlib:iq_to_xml(ResIQ);
				?NS_COMMANDS ->
				    ResIQ = IQ#iq{type = result,
						sub_el = [{xmlelement, "query",
							   [{"xmlns", XMLNS},
							    {"node", Node}],
							   command_items(ServerHost,
                                                                         Host, Lang)}]},
				    Res = jlib:iq_to_xml(ResIQ);
				_ ->
				    Res = jlib:make_error_reply(
					    Packet, ?ERR_ITEM_NOT_FOUND)
			    end,
			    ejabberd_router:route(To,
						  From,
						  Res);
			#iq{xmlns = ?NS_REGISTER} = IQ ->
			    process_register(ServerHost, Host, From, To, IQ);
			#iq{type = get, xmlns = ?NS_VCARD = XMLNS,
			    lang = Lang} = IQ ->
			    Res = IQ#iq{type = result,
					sub_el =
                                            [{xmlelement, "vCard",
                                              [{"xmlns", XMLNS}],
                                              iq_get_vcard(Lang)}]},
                            ejabberd_router:route(To,
                                                  From,
                                                  jlib:iq_to_xml(Res));
			#iq{type = set, xmlns = ?NS_COMMANDS,
			    lang = _Lang, sub_el = SubEl} = IQ ->
			    Request = adhoc:parse_request(IQ),
			    case lists:keysearch(Request#adhoc_request.node,
                                                 1, commands(ServerHost)) of
				{value, {_, _, Function}} ->
				    case catch Function(From, To, Request) of
					{'EXIT', Reason} ->
					    ?ERROR_MSG("~p~nfor ad-hoc handler of ~p",
						       [Reason, {From, To, IQ}]),
					    Res = IQ#iq{type = error, sub_el = [SubEl,
										?ERR_INTERNAL_SERVER_ERROR]};
					ignore ->
					    Res = ignore;
					{error, Error} ->
					    Res = IQ#iq{type = error, sub_el = [SubEl, Error]};
					Command ->
					    Res = IQ#iq{type = result, sub_el = [Command]}
				    end,
				    if Res /= ignore ->
					    ejabberd_router:route(To, From, jlib:iq_to_xml(Res));
				       true ->
					    ok
				    end;
				_ ->
				    Err = jlib:make_error_reply(
					    Packet, ?ERR_ITEM_NOT_FOUND),
				    ejabberd_router:route(To, From, Err)
			    end;
			#iq{} = _IQ ->
			    Err = jlib:make_error_reply(
				    Packet, ?ERR_FEATURE_NOT_IMPLEMENTED),
			    ejabberd_router:route(To, From, Err);
			_ ->
			    ok
		    end;
		_ ->
		    Err = jlib:make_error_reply(Packet, ?ERR_BAD_REQUEST),
		    ejabberd_router:route(To, From, Err)
	    end;
	_ ->
	    case string:tokens(ChanServ, "%") of
		[[_ | _] = Channel, [_ | _] = Server] ->
		    case ets:lookup(irc_connection, {From, Server, Host}) of
			[] ->
			    ?DEBUG("open new connection~n", []),
			    {Username, Encoding, Port, Password} = get_connection_params(
						     Host, ServerHost, From, Server),
			    ConnectionUsername =
				case Packet of
				    %% If the user tries to join a
				    %% chatroom, the packet for sure
				    %% contains the desired username.
				    {xmlelement, "presence", _, _} ->
					Resource;
				    %% Otherwise, there is no firm
				    %% conclusion from the packet.
				    %% Better to use the configured
				    %% username (which defaults to the
				    %% username part of the JID).
				    _ ->
					Username
				end,
			    {ok, Pid} = mod_irc_connection:start(
					  From, Host, ServerHost, Server,
					  ConnectionUsername, Encoding, Port,
                                          Password, ?MODULE),
			    ets:insert(
			      irc_connection,
			      #irc_connection{jid_server_host = {From, Server, Host},
					      pid = Pid}),
			    mod_irc_connection:route_chan(
			      Pid, Channel, Resource, Packet),
			    ok;
			[R] ->
			    Pid = R#irc_connection.pid,
			    ?DEBUG("send to process ~p~n",
				      [Pid]),
			    mod_irc_connection:route_chan(
			      Pid, Channel, Resource, Packet),
			    ok
		    end;
		_ ->
		    case string:tokens(ChanServ, "!") of
			[[_ | _] = Nick, [_ | _] = Server] ->
			    case ets:lookup(irc_connection, {From, Server, Host}) of
				[] ->
				    Err = jlib:make_error_reply(
					    Packet, ?ERR_SERVICE_UNAVAILABLE),
				    ejabberd_router:route(To, From, Err);
				[R] ->
				    Pid = R#irc_connection.pid,
				    ?DEBUG("send to process ~p~n",
					      [Pid]),
				    mod_irc_connection:route_nick(
				      Pid, Nick, Packet),
				    ok
			    end;
			_ ->
			    Err = jlib:make_error_reply(
				    Packet, ?ERR_BAD_REQUEST),
			    ejabberd_router:route(To, From, Err)
		    end
	    end
    end.


closed_connection(Host, From, Server) ->
    ets:delete(irc_connection, {From, Server, Host}).


iq_disco(_ServerHost, [], Lang) ->
    [{xmlelement, "identity",
      [{"category", "conference"},
       {"type", "irc"},
       {"name", translate:translate(Lang, "IRC Transport")}], []},
     {xmlelement, "feature", [{"var", ?NS_DISCO_INFO}], []},
     {xmlelement, "feature", [{"var", ?NS_MUC}], []},
     {xmlelement, "feature", [{"var", ?NS_REGISTER}], []},
     {xmlelement, "feature", [{"var", ?NS_VCARD}], []},
     {xmlelement, "feature", [{"var", ?NS_COMMANDS}], []}];
iq_disco(ServerHost, Node, Lang) ->
    case lists:keysearch(Node, 1, commands(ServerHost)) of
	{value, {_, Name, _}} ->
	    [{xmlelement, "identity",
	      [{"category", "automation"},
	       {"type", "command-node"},
	       {"name", translate:translate(Lang, Name)}], []},
	     {xmlelement, "feature",
	      [{"var", ?NS_COMMANDS}], []},
	     {xmlelement, "feature",
	      [{"var", ?NS_XDATA}], []}];
	_ ->
	    []
    end.

iq_get_vcard(Lang) ->
    [{xmlelement, "FN", [],
      [{xmlcdata, "ejabberd/mod_irc"}]},                  
     {xmlelement, "URL", [],
      [{xmlcdata, ?EJABBERD_URI}]},
     {xmlelement, "DESC", [],
      [{xmlcdata, translate:translate(Lang, "ejabberd IRC module") ++ 
        "\nCopyright (c) 2003-2012 ProcessOne"}]}].

command_items(ServerHost, Host, Lang) ->
    lists:map(fun({Node, Name, _Function})
		 -> {xmlelement, "item",
		     [{"jid", Host},
		      {"node", Node},
		      {"name", translate:translate(Lang, Name)}], []}
	      end, commands(ServerHost)).

commands(ServerHost) ->
    [{"join", "Join channel", fun adhoc_join/3},
     {"register", "Configure username, encoding, port and password",
      fun(From, To, Request) ->
              adhoc_register(ServerHost, From, To, Request)
      end}].

process_register(ServerHost, Host, From, To, #iq{} = IQ) ->
    case catch process_irc_register(ServerHost, Host, From, To, IQ) of
	{'EXIT', Reason} ->
	    ?ERROR_MSG("~p", [Reason]);
	ResIQ ->
	    if
		ResIQ /= ignore ->
		    ejabberd_router:route(To, From,
					  jlib:iq_to_xml(ResIQ));
		true ->
		    ok
	    end
    end.

find_xdata_el({xmlelement, _Name, _Attrs, SubEls}) ->
    find_xdata_el1(SubEls).

find_xdata_el1([]) ->
    false;

find_xdata_el1([{xmlelement, Name, Attrs, SubEls} | Els]) ->
    case xml:get_attr_s("xmlns", Attrs) of
	?NS_XDATA ->
	    {xmlelement, Name, Attrs, SubEls};
	_ ->
	    find_xdata_el1(Els)
    end;

find_xdata_el1([_ | Els]) ->
    find_xdata_el1(Els).

process_irc_register(ServerHost, Host, From, _To,
		     #iq{type = Type, xmlns = XMLNS,
			 lang = Lang, sub_el = SubEl} = IQ) ->
    case Type of
	set ->
	    XDataEl = find_xdata_el(SubEl),
	    case XDataEl of
		false ->
		    IQ#iq{type = error, sub_el = [SubEl, ?ERR_NOT_ACCEPTABLE]};
		{xmlelement, _Name, Attrs, _SubEls} ->
		    case xml:get_attr_s("type", Attrs) of
			"cancel" ->
			    IQ#iq{type = result,
				sub_el = [{xmlelement, "query",
					   [{"xmlns", XMLNS}], []}]};
			"submit" ->
			    XData = jlib:parse_xdata_submit(XDataEl),
			    case XData of
				invalid ->
				    IQ#iq{type = error,
					  sub_el = [SubEl, ?ERR_BAD_REQUEST]};
				_ ->
				    Node = string:tokens(
					     xml:get_tag_attr_s("node", SubEl),
					     "/"),
				    case set_form(
					   ServerHost, Host, From,
                                           Node, Lang, XData) of
					{result, Res} ->
					    IQ#iq{type = result,
						  sub_el = [{xmlelement, "query",
							     [{"xmlns", XMLNS}],
							     Res
							    }]};
					{error, Error} ->
					    IQ#iq{type = error,
						  sub_el = [SubEl, Error]}
				    end
			    end;
			_ ->
			    IQ#iq{type = error,
				  sub_el = [SubEl, ?ERR_BAD_REQUEST]}
		    end
	    end;
	get ->
	    Node =
		string:tokens(xml:get_tag_attr_s("node", SubEl), "/"),
	    case get_form(ServerHost, Host, From, Node, Lang) of
		{result, Res} ->
		    IQ#iq{type = result,
			  sub_el = [{xmlelement, "query",
				     [{"xmlns", XMLNS}],
				     Res
				    }]};
		{error, Error} ->
		    IQ#iq{type = error,
			  sub_el = [SubEl, Error]}
	    end
    end.



get_form(ServerHost, Host, From, [], Lang) ->
    #jid{user = User, server = Server} = From,
    LServer = jlib:nameprep(ServerHost),
    SJID = ejabberd_odbc:escape(
             jlib:jid_to_string(
               jlib:jid_tolower(
                 jlib:jid_remove_resource(From)))),
    SHost = ejabberd_odbc:escape(Host),
    DefaultEncoding = get_default_encoding(Host),
    Customs =
        case catch ejabberd_odbc:sql_query(
                     LServer,
                     ["select data from irc_custom where "
                      "jid='", SJID, "' and host='", SHost, "';"]) of
            {selected, ["data"], [{SData}]} ->
                Data = ejabberd_odbc:decode_term(SData),
                {xml:get_attr_s(username, Data),
	 	 xml:get_attr_s(connections_params, Data)};
            {'EXIT', _} ->
                {error, ?ERR_INTERNAL_SERVER_ERROR};
            {selected, _, _} ->
                {User, []}
        end,
    case Customs of
	{error, _Error} ->
	    Customs;
	{Username, ConnectionsParams} ->
	    {result,
	     [{xmlelement, "instructions", [],
	       [{xmlcdata,
	         translate:translate(
		   Lang,
		   "You need an x:data capable client "
		   "to configure mod_irc settings")}]},
	      {xmlelement, "x", [{"xmlns", ?NS_XDATA}],
	       [{xmlelement, "title", [],
	         [{xmlcdata,
		   translate:translate(
		     Lang,
		     "Registration in mod_irc for ") ++ User ++ "@" ++ Server}]},
	              {xmlelement, "instructions", [],
	               [{xmlcdata,
	                 translate:translate(
	                   Lang,
			   "Enter username, encodings, ports and passwords you wish to use for "
			   "connecting to IRC servers")}]},
	        {xmlelement, "field", [{"type", "text-single"},
				       {"label",
				        translate:translate(
					  Lang, "IRC Username")},
				       {"var", "username"}],
	         [{xmlelement, "value", [], [{xmlcdata, Username}]}]},
	        {xmlelement, "field", [{"type", "fixed"}],
	         [{xmlelement, "value", [],
		   [{xmlcdata,
		     lists:flatten(
		       io_lib:format(
		         translate:translate(
			   Lang,
			   "If you want to specify different ports, "
			   "passwords, encodings for IRC servers, fill "
			   "this list with values in format "
			   "'{\"irc server\", \"encoding\", port, \"password\"}'.  "
			   "By default this service use \"~s\" encoding, port ~p, "
			   "empty password."),
		         [DefaultEncoding, ?DEFAULT_IRC_PORT]))}]}]},
	        {xmlelement, "field", [{"type", "fixed"}],
	         [{xmlelement, "value", [],
		   [{xmlcdata,
		     translate:translate(
		       Lang,
		       "Example: [{\"irc.lucky.net\", \"koi8-r\", 6667, \"secret\"}, "
		       "{\"vendetta.fef.net\", \"iso8859-1\", 7000}, {\"irc.sometestserver.net\", \"utf-8\"}]."
		    )}]}]},
	        {xmlelement, "field", [{"type", "text-multi"},
				       {"label",
				        translate:translate(Lang, "Connections parameters")},
				       {"var", "connections_params"}],
		         lists:map(
			   fun(S) ->
				   {xmlelement, "value", [], [{xmlcdata, S}]}
			   end,
			   string:tokens(
			     lists:flatten(
			       io_lib:format("~p.", [ConnectionsParams])),
			     "\n"))
	        }
	       ]}]}
    end;

get_form(_ServerHost, _Host, _, _, _Lang) ->
    {error, ?ERR_SERVICE_UNAVAILABLE}.




set_form(ServerHost, Host, From, [], _Lang, XData) ->
    LServer = jlib:nameprep(ServerHost),
    SJID = ejabberd_odbc:escape(
             jlib:jid_to_string(
               jlib:jid_tolower(
                 jlib:jid_remove_resource(From)))),
    SHost = ejabberd_odbc:escape(Host),
    case {lists:keysearch("username", 1, XData),
	  lists:keysearch("connections_params", 1, XData)} of
	{{value, {_, [Username]}}, {value, {_, Strings}}} ->
	    EncString = lists:foldl(fun(S, Res) ->
					    Res ++ S ++ "\n"
				    end, "", Strings),
	    case erl_scan:string(EncString) of
		{ok, Tokens, _} ->
		    case erl_parse:parse_term(Tokens) of
			{ok, ConnectionsParams} ->
                            SData = ejabberd_odbc:encode_term(
                                      [{username,
                                        Username},
                                       {connections_params,
                                        ConnectionsParams}]),
                            case ejabberd_odbc:sql_transaction(
                                   LServer,
                                   fun() ->
                                           odbc_queries:update_t(
                                             "irc_custom",
                                             ["jid", "host", "data"],
                                             [SJID, SHost, SData],
                                             ["jid='", SJID,
                                              "' and host='",
                                              SHost, "'"]),
                                           ok
                                   end) of
				{atomic, _} ->
				    {result, []};
				_ ->
				    {error, ?ERR_NOT_ACCEPTABLE}
			    end;
			_ ->
			    {error, ?ERR_NOT_ACCEPTABLE}
		    end;
		_ ->
		    {error, ?ERR_NOT_ACCEPTABLE}
	    end;
	_ ->
	    {error, ?ERR_NOT_ACCEPTABLE}
    end;


set_form(_ServerHost, _Host, _, _, _Lang, _XData) ->
    {error, ?ERR_SERVICE_UNAVAILABLE}.


%% Host = "irc.example.com"
%% ServerHost = "example.com"
get_connection_params(Host, From, IRCServer) ->
    [_ | HostTail] = string:tokens(Host, "."),
    ServerHost = string:join(HostTail, "."),
    get_connection_params(Host, ServerHost, From, IRCServer).

get_default_encoding(ServerHost) ->
    Result = gen_mod:get_module_opt(
		     ServerHost, ?MODULE, default_encoding,
		     ?DEFAULT_IRC_ENCODING),
    ?INFO_MSG("The default_encoding configured for host ~p is: ~p~n", [ServerHost, Result]),
    Result.

get_connection_params(Host, ServerHost, From, IRCServer) ->
    #jid{user = User, server = _Server} = From,
    LServer = jlib:nameprep(ServerHost),
    SJID = ejabberd_odbc:escape(
             jlib:jid_to_string(
               jlib:jid_tolower(
                 jlib:jid_remove_resource(From)))),
    SHost = ejabberd_odbc:escape(Host),
    DefaultEncoding = get_default_encoding(ServerHost),
    case catch ejabberd_odbc:sql_query(
                 LServer,
                 ["select data from irc_custom where "
                  "jid='", SJID, "' and host='", SHost, "';"]) of
        {'EXIT', _Reason} ->
	    {User, DefaultEncoding, ?DEFAULT_IRC_PORT, ""};
	{selected, ["data"], []} ->
	    {User, DefaultEncoding, ?DEFAULT_IRC_PORT, ""};
        {selected, ["data"], [{SData}]} ->
            Data = ejabberd_odbc:decode_term(SData),
	    Username = xml:get_attr_s(username, Data),
	    {NewUsername, NewEncoding, NewPort, NewPassword} = 
    		case lists:keysearch(
                       IRCServer, 1,
                       xml:get_attr_s(connections_params, Data)) of
	    	    {value, {_, Encoding, Port, Password}} ->
		        {Username, Encoding, Port, Password};
		    {value, {_, Encoding, Port}} ->
			{Username, Encoding, Port, ""};
		    {value, {_, Encoding}} ->
			{Username, Encoding, ?DEFAULT_IRC_PORT, ""};
		    _ ->
			{Username, DefaultEncoding, ?DEFAULT_IRC_PORT, ""}
    		end,
	    {NewUsername, 
	     NewEncoding, 
	     if 
                 NewPort >= 0 andalso NewPort =< 65535 -> 
                     NewPort; 
                 true -> 
                     ?DEFAULT_IRC_PORT
	     end,
	     NewPassword}
    end.

adhoc_join(_From, _To, #adhoc_request{action = "cancel"} = Request) ->
    adhoc:produce_response(Request,
			   #adhoc_response{status = canceled});
adhoc_join(From, To, #adhoc_request{lang = Lang,
				    node = _Node,
				    action = _Action,
				    xdata = XData} = Request) ->
    %% Access control has already been taken care of in do_route.
    if XData == false ->
	    Form =
		{xmlelement, "x",
		 [{"xmlns", ?NS_XDATA},
		  {"type", "form"}],
		 [{xmlelement, "title", [], [{xmlcdata, translate:translate(Lang, "Join IRC channel")}]},
		  {xmlelement, "field",
		   [{"var", "channel"},
		    {"type", "text-single"},
		    {"label", translate:translate(Lang, "IRC channel (don't put the first #)")}],
		   [{xmlelement, "required", [], []}]},
		  {xmlelement, "field",
		   [{"var", "server"},
		    {"type", "text-single"},
		    {"label", translate:translate(Lang, "IRC server")}],
		   [{xmlelement, "required", [], []}]}]},
	    adhoc:produce_response(Request,
				   #adhoc_response{status = executing,
						   elements = [Form]});
       true ->
	    case jlib:parse_xdata_submit(XData) of
		invalid ->
		    {error, ?ERR_BAD_REQUEST};
		Fields ->
		    Channel = case lists:keysearch("channel", 1, Fields) of
				  {value, {"channel", C}} ->
				      C;
				  _ ->
				      false
			      end,
		    Server = case lists:keysearch("server", 1, Fields) of
				 {value, {"server", S}} ->
				     S;
				 _ ->
				     false
			     end,
		    if Channel /= false,
		       Server /= false ->
			    RoomJID = Channel ++ "%" ++ Server ++ "@" ++ To#jid.server,
			    Invite = {xmlelement, "message", [],
				      [{xmlelement, "x",
					[{"xmlns", ?NS_MUC_USER}],
					[{xmlelement, "invite", 
					  [{"from", jlib:jid_to_string(From)}],
					  [{xmlelement, "reason", [],
					    [{xmlcdata, 
					      translate:translate(Lang,
								  "Join the IRC channel here.")}]}]}]},
				       {xmlelement, "x",
					[{"xmlns", ?NS_XCONFERENCE}],
					[{xmlcdata, translate:translate(Lang,
								  "Join the IRC channel here.")}]},
				       {xmlelement, "body", [],
					[{xmlcdata, io_lib:format(
						      translate:translate(Lang,
									  "Join the IRC channel in this Jabber ID: ~s"),
						      [RoomJID])}]}]},
			    ejabberd_router:route(jlib:string_to_jid(RoomJID), From, Invite),
			    adhoc:produce_response(Request, #adhoc_response{status = completed});
		       true ->
			    {error, ?ERR_BAD_REQUEST}
		    end
	    end
    end.

adhoc_register(_ServerHost, _From, _To, #adhoc_request{action = "cancel"} = Request) ->
    adhoc:produce_response(Request,
			   #adhoc_response{status = canceled});
adhoc_register(ServerHost, From, To, #adhoc_request{lang = Lang,
                                                    node = _Node,
                                                    xdata = XData,
                                                    action = Action} = Request) ->
    #jid{user = User} = From,
    #jid{lserver = Host} = To,
    LServer = jlib:nameprep(ServerHost),
    SHost = ejabberd_odbc:escape(Host),
    SJID = ejabberd_odbc:escape(
             jlib:jid_to_string(
               jlib:jid_tolower(
                 jlib:jid_remove_resource(From)))),
    %% Generate form for setting username and encodings.  If the user
    %% hasn't begun to fill out the form, generate an initial form
    %% based on current values.
    if XData == false ->
            case catch ejabberd_odbc:sql_query(
                         LServer,
                         ["select data from irc_custom where "
                          "jid='", SJID, "' and host='", SHost, "';"]) of
		{'EXIT', _Reason} ->
		    Username = User,
		    ConnectionsParams = [];
		{selected, ["data"], []} ->
		    Username = User,
		    ConnectionsParams = [];
		{selected, ["data"], [{Data1}]} ->
                    Data = ejabberd_odbc:decode_term(Data1),
		    Username = xml:get_attr_s(username, Data),
		    ConnectionsParams = xml:get_attr_s(connections_params, Data)
	    end,
	    Error = false;
       true ->
	    case jlib:parse_xdata_submit(XData) of
		invalid ->
		    Error = {error, ?ERR_BAD_REQUEST},
		    Username = false,
		    ConnectionsParams = false;
		Fields ->
		    Username = case lists:keysearch("username", 1, Fields) of
				   {value, {"username", U}} ->
				       U;
				   _ ->
				       User
			       end,
		    ConnectionsParams = parse_connections_params(Fields),
		    Error = false
	    end
    end,
    
    if Error /= false ->
	    Error;
       Action == "complete" ->
            SData = ejabberd_odbc:encode_term(
                      [{username, Username},
                       {connections_params, ConnectionsParams}]),
            case catch ejabberd_odbc:sql_transaction(
                         LServer,
                         fun() ->
                                 odbc_queries:update_t(
                                   "irc_custom",
                                   ["jid", "host", "data"],
                                   [SJID, SHost, SData],
                                   ["jid='", SJID,
                                    "' and host='", SHost, "'"]),
                                 ok
                         end) of
		{atomic, ok} ->
		    adhoc:produce_response(Request, #adhoc_response{status = completed});
		_ ->
		    {error, ?ERR_INTERNAL_SERVER_ERROR}
	    end;
       true ->
	    Form = generate_adhoc_register_form(Lang, Username, ConnectionsParams),
	    adhoc:produce_response(Request,
				   #adhoc_response{status = executing,
						   elements = [Form],
						   actions = ["next", "complete"]})
    end.

generate_adhoc_register_form(Lang, Username, ConnectionsParams) ->
    {xmlelement, "x",
     [{"xmlns", ?NS_XDATA},
      {"type", "form"}],
     [{xmlelement, "title", [], [{xmlcdata, translate:translate(Lang, "IRC settings")}]},
      {xmlelement, "instructions", [],
       [{xmlcdata,
	 translate:translate(
	   Lang,
	   "Enter username and encodings you wish to use for "
	   "connecting to IRC servers.  Press 'Next' to get more fields "
	   "to fill in.  Press 'Complete' to save settings.")}]},
      {xmlelement, "field",
       [{"var", "username"},
	{"type", "text-single"},
	{"label", translate:translate(Lang, "IRC username")}], 
       [{xmlelement, "required", [], []},
	{xmlelement, "value", [], [{xmlcdata, Username}]}]}] ++
    generate_connection_params_fields(Lang, ConnectionsParams, 1, [])}.

generate_connection_params_fields(Lang, [], Number, Acc) ->
    Field = generate_connection_params_field(Lang, "", "", -1, "", Number),
    lists:reverse(Field ++ Acc);
    
generate_connection_params_fields(Lang, [ConnectionParams | ConnectionsParams], Number, Acc) ->
    case ConnectionParams of
	{Server, Encoding, Port, Password} ->
	    Field = generate_connection_params_field(Lang, Server, Encoding, Port, Password, Number),
	    generate_connection_params_fields(Lang, ConnectionsParams, Number + 1, Field ++ Acc);	
	{Server, Encoding, Port} ->	    
	    Field = generate_connection_params_field(Lang, Server, Encoding, Port, [], Number),
	    generate_connection_params_fields(Lang, ConnectionsParams, Number + 1, Field ++ Acc);
	{Server, Encoding} ->	
	    Field = generate_connection_params_field(Lang, Server, Encoding, [], [], Number),
	    generate_connection_params_fields(Lang, ConnectionsParams, Number + 1, Field ++ Acc);
	_ -> 
	    []
    end.

generate_connection_params_field(Lang, Server, Encoding, Port, Password, Number) ->
    EncodingUsed = case Encoding of
		       [] ->
			   get_default_encoding(Server);
		       _ ->
			   Encoding
		   end,
    PortUsedInt = if
		    Port >= 0 andalso Port =< 65535 ->
			Port;
		    true ->
			?DEFAULT_IRC_PORT
	       end,	
    PortUsed = integer_to_list(PortUsedInt),
    PasswordUsed = case Password of
		    [] -> 
			"";
		    _ -> 
			Password
		   end,	      		
    NumberString = integer_to_list(Number),
    %% Fields are in reverse order, as they will be reversed again later.
    [{xmlelement, "field",
      [{"var", "password" ++ NumberString},
       {"type", "text-single"},
       {"label", io_lib:format(translate:translate(Lang, "Password ~b"), [Number])}],
      [{xmlelement, "value", [], [{xmlcdata, PasswordUsed}]}]},
     {xmlelement, "field",
      [{"var", "port" ++ NumberString},
       {"type", "text-single"},
       {"label", io_lib:format(translate:translate(Lang, "Port ~b"), [Number])}],
      [{xmlelement, "value", [], [{xmlcdata, PortUsed}]}]},    
     {xmlelement, "field",
      [{"var", "encoding" ++ NumberString},
       {"type", "list-single"},
       {"label", io_lib:format(translate:translate(Lang, "Encoding for server ~b"), [Number])}],
      [{xmlelement, "value", [], [{xmlcdata, EncodingUsed}]} |
       lists:map(fun(E) ->
			 {xmlelement, "option", [{"label", E}],
			  [{xmlelement, "value", [], [{xmlcdata, E}]}]}
		 end, ?POSSIBLE_ENCODINGS)]},
     {xmlelement, "field",
      [{"var", "server" ++ NumberString},
       {"type", "text-single"},
       {"label", io_lib:format(translate:translate(Lang, "Server ~b"), [Number])}],
      [{xmlelement, "value", [], [{xmlcdata, Server}]}]}].

parse_connections_params(Fields) ->
    %% Find all fields staring with serverN, encodingN, portN and passwordN for any values
    %% of N, and generate lists of {"N", Value}.
    Servers = lists:sort(
		[{lists:nthtail(6, Var), lists:flatten(Value)} || {Var, Value} <- Fields,
								  lists:prefix("server", Var)]),
    Encodings = lists:sort(
		  [{lists:nthtail(8, Var), lists:flatten(Value)} || {Var, Value} <- Fields,
								    lists:prefix("encoding", Var)]),
								    
    Ports = lists:sort(
	      [{lists:nthtail(4, Var), lists:flatten(Value)} || {Var, Value} <- Fields,
								lists:prefix("port", Var)]),
								  
    Passwords = lists:sort(
		  [{lists:nthtail(8, Var), lists:flatten(Value)} || {Var, Value} <- Fields,
								    lists:prefix("password", Var)]),
    
    %% Now sort the lists, and find the corresponding pairs.
    parse_connections_params(Servers, Encodings, Ports, Passwords).
    
retrieve_connections_params(ConnectionParams, ServerN) ->
    case ConnectionParams of
        [{ConnectionParamN, ConnectionParam} | ConnectionParamsTail] ->	    
    	    if 
		ServerN == ConnectionParamN ->
	    	    {ConnectionParam, ConnectionParamsTail};		
	        ServerN < ConnectionParamN ->
		    {[], [{ConnectionParamN, ConnectionParam} | ConnectionParamsTail]};
		ServerN > ConnectionParamN ->    
		    {[], ConnectionParamsTail}
	    end;
	    _ ->
		{[], []}
	end.
	
parse_connections_params([], _, _, _) ->
    [];
parse_connections_params(_, [], [], []) ->
    [];	

parse_connections_params([{ServerN, Server} | Servers], Encodings, Ports, Passwords) ->
    %% Try to match matches of servers, ports, passwords and encodings, no matter what fields
    %% the client might have left out.
    {NewEncoding, NewEncodings} = retrieve_connections_params(Encodings, ServerN),
    {NewPort, NewPorts} = retrieve_connections_params(Ports, ServerN),
    {NewPassword, NewPasswords} = retrieve_connections_params(Passwords, ServerN),
    [{Server, NewEncoding, NewPort, NewPassword} | parse_connections_params(Servers, NewEncodings, NewPorts, NewPasswords)].
