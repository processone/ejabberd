%%%----------------------------------------------------------------------
%%% File    : mod_irc.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : IRC transport
%%% Created : 15 Feb 2003 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2008   ProcessOne
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

-module(mod_irc).
-author('alexey@process-one.net').

-behaviour(gen_server).
-behaviour(gen_mod).

%% API
-export([start_link/2,
	 start/2,
	 stop/1,
	 closed_connection/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("ejabberd.hrl").
-include("jlib.hrl").

-record(irc_connection, {jid_server_host, pid}).
-record(irc_custom, {us_host, data}).

-record(state, {host, server_host, default_encoding, access}).

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
    mnesia:create_table(irc_custom,
			[{disc_copies, [node()]},
			 {attributes, record_info(fields, irc_custom)}]),
    MyHost = gen_mod:get_opt_host(Host, Opts, "irc.@HOST@"),
    update_table(MyHost),
    Access = gen_mod:get_opt(access, Opts, all),
	DefaultEncoding = gen_mod:get_opt(default_encoding, Opts, "koi8-r"),
    catch ets:new(irc_connection, [named_table,
				   public,
				   {keypos, #irc_connection.jid_server_host}]),
    ejabberd_router:register_route(MyHost),
    {ok, #state{host = MyHost,
		server_host = Host,
		default_encoding = DefaultEncoding,
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
		   default_encoding = DefEnc,
		   access = Access} = State) ->
    case catch do_route(Host, ServerHost, Access, From, To, Packet, DefEnc) of
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

do_route(Host, ServerHost, Access, From, To, Packet, DefEnc) ->
    case acl:match_rule(ServerHost, Access, From) of
	allow ->
	    do_route1(Host, ServerHost, From, To, Packet, DefEnc);
	_ ->
	    {xmlelement, _Name, Attrs, _Els} = Packet,
	    Lang = xml:get_attr_s("xml:lang", Attrs),
	    ErrText = "Access denied by service policy",
	    Err = jlib:make_error_reply(Packet,
					?ERRT_FORBIDDEN(Lang, ErrText)),
	    ejabberd_router:route(To, From, Err)
    end.

do_route1(Host, ServerHost, From, To, Packet, DefEnc) ->
    #jid{user = ChanServ, resource = Resource} = To,
    {xmlelement, _Name, _Attrs, _Els} = Packet,
    case ChanServ of
	"" ->
	    case Resource of
		"" ->
		    case jlib:iq_query_info(Packet) of
			#iq{type = get, xmlns = ?NS_DISCO_INFO = XMLNS,
			    sub_el = _SubEl, lang = Lang} = IQ ->
			    Res = IQ#iq{type = result,
					sub_el = [{xmlelement, "query",
						   [{"xmlns", XMLNS}],
						   iq_disco(Lang)}]},
			    ejabberd_router:route(To,
						  From,
						  jlib:iq_to_xml(Res));
			#iq{type = get, xmlns = ?NS_DISCO_ITEMS = XMLNS} = IQ ->
			    Res = IQ#iq{type = result,
					sub_el = [{xmlelement, "query",
						   [{"xmlns", XMLNS}],
						   []}]},
			    ejabberd_router:route(To,
						  From,
						  jlib:iq_to_xml(Res));
			#iq{xmlns = ?NS_REGISTER} = IQ ->
			    process_register(Host, From, To, DefEnc, IQ);
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
			    {Username, Encoding} = get_user_and_encoding(
						     Host, From, Server, DefEnc),
			    {ok, Pid} = mod_irc_connection:start(
					  From, Host, ServerHost, Server,
					  Username, Encoding),
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


iq_disco(Lang) ->
    [{xmlelement, "identity",
      [{"category", "conference"},
       {"type", "irc"},
       {"name", translate:translate(Lang, "IRC Transport")}], []},
     {xmlelement, "feature",
      [{"var", ?NS_MUC}], []},
     {xmlelement, "feature",
      [{"var", ?NS_REGISTER}], []},
     {xmlelement, "feature",
      [{"var", ?NS_VCARD}], []}].

iq_get_vcard(Lang) ->
    [{xmlelement, "FN", [],
      [{xmlcdata, "ejabberd/mod_irc"}]},                  
     {xmlelement, "URL", [],
      [{xmlcdata, ?EJABBERD_URI}]},
     {xmlelement, "DESC", [],
      [{xmlcdata, translate:translate(Lang, "ejabberd IRC module") ++
        "\nCopyright (c) 2003-2008 Alexey Shchepin"}]}].

process_register(Host, From, To, DefEnc, #iq{} = IQ) ->
    case catch process_irc_register(Host, From, To, DefEnc, IQ) of
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

process_irc_register(Host, From, _To, DefEnc,
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
					   Host, From, Node, Lang, XData) of
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
	    case get_form(Host, From, Node, Lang ,DefEnc) of
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



get_form(Host, From, [], Lang, DefEnc) ->
    #jid{user = User, server = Server,
	 luser = LUser, lserver = LServer} = From,
    US = {LUser, LServer},
    Customs =
	case catch mnesia:dirty_read({irc_custom, {US, Host}}) of
	    {'EXIT', _Reason} ->
		{error, ?ERR_INTERNAL_SERVER_ERROR};
	    [] ->
		{User, []};
	    [#irc_custom{data = Data}] ->
		{xml:get_attr_s(username, Data),
		 xml:get_attr_s(encodings, Data)}
	end,
    case Customs of
	{error, _Error} ->
	    Customs;
	{Username, Encodings} ->
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
			   "Enter username and encodings you wish to use for "
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
			   "If you want to specify different encodings "
			   "for IRC servers, fill this list with values "
			   "in format '{\"irc server\", \"encoding\"}'.  "
			   "By default this service use \"~s\" encoding."),
		         [DefEnc]))}]}]},
	        {xmlelement, "field", [{"type", "fixed"}],
	         [{xmlelement, "value", [],
		   [{xmlcdata,
		     translate:translate(
		       Lang,
		       "Example: [{\"irc.lucky.net\", \"koi8-r\"}, "
		       "{\"vendetta.fef.net\", \"iso8859-1\"}]."
		    )}]}]},
	        {xmlelement, "field", [{"type", "text-multi"},
				       {"label",
				        translate:translate(Lang, "Encodings")},
				       {"var", "encodings"}],
		         lists:map(
			   fun(S) ->
				   {xmlelement, "value", [], [{xmlcdata, S}]}
			   end,
			   string:tokens(
			     lists:flatten(
			       io_lib:format("~p.", [Encodings])),
			     "\n"))
	        }
	       ]}]}
    end;

get_form(_Host, _, _, _Lang, _) ->
    {error, ?ERR_SERVICE_UNAVAILABLE}.




set_form(Host, From, [], _Lang, XData) ->
    {LUser, LServer, _} = jlib:jid_tolower(From),
    US = {LUser, LServer},
    case {lists:keysearch("username", 1, XData),
	  lists:keysearch("encodings", 1, XData)} of
	{{value, {_, [Username]}}, {value, {_, Strings}}} ->
	    EncString = lists:foldl(fun(S, Res) ->
					    Res ++ S ++ "\n"
				    end, "", Strings),
	    case erl_scan:string(EncString) of
		{ok, Tokens, _} ->
		    case erl_parse:parse_term(Tokens) of
			{ok, Encodings} ->
			    case mnesia:transaction(
				   fun() ->
					   mnesia:write(
					     #irc_custom{us_host =
							 {US, Host},
							 data =
							 [{username,
							   Username},
							  {encodings,
							   Encodings}]})
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


set_form(_Host, _, _, _Lang, _XData) ->
    {error, ?ERR_SERVICE_UNAVAILABLE}.


get_user_and_encoding(Host, From, IRCServer, DefEnc) ->
    #jid{user = User, server = _Server,
	 luser = LUser, lserver = LServer} = From,
    US = {LUser, LServer},
    case catch mnesia:dirty_read({irc_custom, {US, Host}}) of
	{'EXIT', _Reason} ->
	    {User, DefEnc};
	[] ->
	    {User, DefEnc};
	[#irc_custom{data = Data}] ->
	    {xml:get_attr_s(username, Data),
	     case xml:get_attr_s(IRCServer, xml:get_attr_s(encodings, Data)) of
		"" -> DefEnc;
		E -> E
	     end}
    end.


update_table(Host) ->
    Fields = record_info(fields, irc_custom),
    case mnesia:table_info(irc_custom, attributes) of
	Fields ->
	    ok;
	[userserver, data] ->
	    ?INFO_MSG("Converting irc_custom table from "
		      "{userserver, data} format", []),
	    {atomic, ok} = mnesia:create_table(
			     mod_irc_tmp_table,
			     [{disc_only_copies, [node()]},
			      {type, bag},
			      {local_content, true},
			      {record_name, irc_custom},
			      {attributes, record_info(fields, irc_custom)}]),
	    mnesia:transform_table(irc_custom, ignore, Fields),
	    F1 = fun() ->
			 mnesia:write_lock_table(mod_irc_tmp_table),
			 mnesia:foldl(
			   fun(#irc_custom{us_host = US} = R, _) ->
				   mnesia:dirty_write(
				     mod_irc_tmp_table,
				     R#irc_custom{us_host = {US, Host}})
			   end, ok, irc_custom)
		 end,
	    mnesia:transaction(F1),
	    mnesia:clear_table(irc_custom),
	    F2 = fun() ->
			 mnesia:write_lock_table(irc_custom),
			 mnesia:foldl(
			   fun(R, _) ->
				   mnesia:dirty_write(R)
			   end, ok, mod_irc_tmp_table)
		 end,
	    mnesia:transaction(F2),
	    mnesia:delete_table(mod_irc_tmp_table);
	_ ->
	    ?INFO_MSG("Recreating irc_custom table", []),
	    mnesia:transform_table(irc_custom, ignore, Fields)
    end.
