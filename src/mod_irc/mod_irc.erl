%%%----------------------------------------------------------------------
%%% File    : mod_irc.erl
%%% Author  : Alexey Shchepin <alexey@sevcom.net>
%%% Purpose : IRC transport
%%% Created : 15 Feb 2003 by Alexey Shchepin <alexey@sevcom.net>
%%% Id      : $Id$
%%%----------------------------------------------------------------------

-module(mod_irc).
-author('alexey@sevcom.net').
-vsn('$Revision$ ').

-behaviour(gen_mod).

-export([start/1, init/1, stop/0, closed_conection/2,
	 get_user_and_encoding/2]).

-include("ejabberd.hrl").
-include("jlib.hrl").

-define(DEFAULT_IRC_ENCODING, "koi8-r").

-record(irc_connection, {userserver, pid}).
-record(irc_custom, {userserver, data}).

start(Opts) ->
    iconv:start(),
    mnesia:create_table(irc_custom,
			[{disc_copies, [node()]},
			 {attributes, record_info(fields, irc_custom)}]),
    Host = gen_mod:get_opt(host, Opts, "irc." ++ ?MYNAME),
    register(ejabberd_mod_irc, spawn(?MODULE, init, [Host])).

init(Host) ->
    catch ets:new(irc_connection, [named_table,
				   public,
				   {keypos, #irc_connection.userserver}]),
    ejabberd_router:register_route(Host),
    loop(Host).

loop(Host) ->
    receive
	{route, From, To, Packet} ->
	    case catch do_route(Host, From, To, Packet) of
		{'EXIT', Reason} ->
		    ?ERROR_MSG("~p", [Reason]);
		_ ->
		    ok
	    end,
	    loop(Host);
	stop ->
	    ejabberd_router:unregister_global_route(Host),
	    ok;
	_ ->
	    loop(Host)
    end.


do_route(Host, From, To, Packet) ->
    #jid{user = ChanServ, resource = Resource} = To,
    case ChanServ of
	"" ->
	    case Resource of
		"" ->
		    case jlib:iq_query_info(Packet) of
			#iq{type = get, xmlns = ?NS_DISCO_INFO = XMLNS,
			    sub_el = SubEl} = IQ ->
			    Res = IQ#iq{type = result,
					sub_el = [{xmlelement, "query",
						   [{"xmlns", XMLNS}],
						   iq_disco()}]},
			    ejabberd_router:route(To,
						  From,
						  jlib:iq_to_xml(Res));
			#iq{type = Type, xmlns = ?NS_IQDATA,
			    sub_el = SubEl} = IQ ->
			    iq_data(From, To, IQ);
			#iq{} = IQ ->
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
		    case ets:lookup(irc_connection, {From, Server}) of
			[] ->
			    io:format("open new connection~n"),
			    {Username, Encoding} = get_user_and_encoding(
						     From, Server),
			    {ok, Pid} = mod_irc_connection:start(
					  From, Host, Server,
					  Username, Encoding),
			    ets:insert(
			      irc_connection,
			      #irc_connection{userserver = {From, Server},
					      pid = Pid}),
			    mod_irc_connection:route_chan(
			      Pid, Channel, Resource, Packet),
			    ok;
			[R] ->
			    Pid = R#irc_connection.pid,
			    io:format("send to process ~p~n",
				      [Pid]),
			    mod_irc_connection:route_chan(
			      Pid, Channel, Resource, Packet),
			    ok
		    end;
		_ ->
		    case string:tokens(ChanServ, "!") of
			[[_ | _] = Nick, [_ | _] = Server] ->
			    case ets:lookup(irc_connection, {From, Server}) of
				[] ->
				    Err = jlib:make_error_reply(
					    Packet, ?ERR_SERVICE_UNAVAILABLE),
				    ejabberd_router:route(To, From, Err);
				[R] ->
				    Pid = R#irc_connection.pid,
				    io:format("send to process ~p~n",
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




stop() ->
    ejabberd_mod_irc ! stop,
    ok.

closed_conection(From, Server) ->
    ets:delete(irc_connection, {From, Server}).


iq_disco() ->
    [{xmlelement, "identity",
      [{"category", "conference"},
       {"type", "irc"},
       {"name", "ejabberd/mod_irc"}], []},
     {xmlelement, "feature",
      [{"var", ?NS_MUC}], []},
     {xmlelement, "feature",
      [{"var", ?NS_IQDATA}], []}].



iq_data(From, To, #iq{type = Type} = IQ) ->
    case catch process_iq_data(From, To, IQ) of
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


process_iq_data(From, To,
		#iq{type = Type, xmlns = XMLNS, sub_el = SubEl} = IQ) ->
    Lang = xml:get_tag_attr_s("xml:lang", SubEl),
    case Type of
	set ->
	    case xml:get_tag_attr_s("type", SubEl) of
		"cancel" ->
		    IQ#iq{type = result,
			  sub_el = [{xmlelement, "query",
				     [{"xmlns", XMLNS}], []}]};
		"submit" ->
		    XData = jlib:parse_xdata_submit(SubEl),
		    case XData of
			invalid ->
			    IQ#iq{type = error,
				  sub_el = [SubEl, ?ERR_BAD_REQUEST]};
			_ ->
			    Node =
				string:tokens(
				  xml:get_tag_attr_s("node", SubEl),
				  "/"),
			    case set_form(From, Node, Lang, XData) of
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
		    IQ#iq{type = error, sub_el = [SubEl, ?ERR_NOT_ALLOWED]}
	    end;
	get ->
	    Node =
		string:tokens(xml:get_tag_attr_s("node", SubEl), "/"),
	    case get_form(From, Node, Lang) of
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



get_form(From, [], Lang) ->
    #jid{user = User, server = Server,
	 luser = LUser, lserver = LServer} = From,
    Customs =
	case catch mnesia:dirty_read({irc_custom, {LUser, LServer}}) of
	    {'EXIT', Reason} ->
		{error, ?ERR_INTERNAL_SERVER_ERROR};
	    [] ->
		{User, []};
	    [#irc_custom{data = Data}] ->
		{xml:get_attr_s(username, Data),
		 xml:get_attr_s(encodings, Data)}
	end,
    case Customs of
	{error, _, _} ->
	    Customs;
	{Username, Encodings} ->
	    {result,
	     [{xmlelement, "title", [],
	       [{xmlcdata,
		 User ++ "@" ++ Server ++ " " ++
		 translate:translate(Lang, "Configuration")}]},
	              %{xmlelement, "instructions", [],
	              % [{xmlcdata,
	              %   translate:translate(
	              %     Lang, "")}]},
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
		       [?DEFAULT_IRC_ENCODING]))}]}]},
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
	     ]}
    end;


get_form(_, _, Lang) ->
    {error, ?ERR_SERVICE_UNAVAILABLE}.




set_form(From, [], Lang, XData) ->
    {LUser, LServer, _} = jlib:jid_tolower(From),
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
					     #irc_custom{userserver =
							 {LUser, LServer},
							 data =
							 [{username,
							   Username},
							  {encodings,
							   Encodings}]})
				   end) of
				{atomic, _} ->
				    {result, []};
				_ ->
				    {error, "406", "Not Acceptable"}
			    end;
			_ ->
			    {error, "406", "Not Acceptable"}
		    end;
		_ ->
		    {error, "406", "Not Acceptable"}
	    end;
	_ ->
	    {error, "406", "Not Acceptable"}
    end;


set_form(_, _, Lang, XData) ->
    {error, ?ERR_SERVICE_UNAVAILABLE}.


get_user_and_encoding(From, IRCServer) ->
    #jid{user = User, server = Server,
	 luser = LUser, lserver = LServer} = From,
    case catch mnesia:dirty_read({irc_custom, {LUser, LServer}}) of
	{'EXIT', Reason} ->
	    {User, ?DEFAULT_IRC_ENCODING};
	[] ->
	    {User, ?DEFAULT_IRC_ENCODING};
	[#irc_custom{data = Data}] ->
	    {xml:get_attr_s(username, Data),
	     case xml:get_attr_s(IRCServer, xml:get_attr_s(encodings, Data)) of
		"" -> ?DEFAULT_IRC_ENCODING;
		E -> E
	     end}
    end.

