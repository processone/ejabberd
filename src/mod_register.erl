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

-export([start/0, init/0, process_iq/3]).

-include("ejabberd.hrl").

start() ->
    ejabberd_local:register_iq_handler("jabber:iq:register",
				       ?MODULE, process_iq),
    ok.

init() ->
    ok.

process_iq(From, To, {iq, ID, Type, XMLNS, SubEl}) ->
    case Type of
	set ->
	    UTag = xml:get_subtag(SubEl, "username"),
	    PTag = xml:get_subtag(SubEl, "password"),
	    RTag = xml:get_subtag(SubEl, "remove"),
	    if
		(UTag /= false) and (RTag /= false) ->
		    {iq, ID, error, XMLNS,
		     [SubEl, {xmlelement,
			      "error",
			      [{"code", "501"}],
			      [{xmlcdata, "Not Implemented"}]}]};
		(UTag /= false) and (PTag /= false) ->
		    User = xml:get_tag_cdata(UTag),
		    Password = xml:get_tag_cdata(PTag),
		    Server = ?MYNAME,
		    case From of
			{User, Server, _} ->
			    ejabberd_auth:set_password(User, Password),
			    {iq, ID, result, XMLNS, [SubEl]};
			_ ->
			    case try_register(User, Password) of
				ok ->
				    {iq, ID, result, XMLNS, [SubEl]};
				{error, Code, Reason} ->
				    {iq, ID, error, XMLNS,
				     [SubEl, {xmlelement,
					      "error",
					      [{"code", Code}],
					      [{xmlcdata, Reason}]}]}
			    end
		    end
	    end;
	get ->
	    {iq, ID, result, XMLNS, [{xmlelement,
				      "query",
				      [{"xmlns", "jabber:iq:register"}],
				      [{xmlelement, "instructions", [],
					[{xmlcdata,
					  "Choose a username and password "
					  "to register with this server."}]},
				       {xmlelement, "username", [], []},
				       {xmlelement, "password", [], []}]}]}
    end.


try_register(User, Password) ->
    case ejabberd_auth:try_register(User, Password) of
	{atomic, ok} ->
	    ok;
	{atomic, exists} ->
	    {error, "400", "Bad Request"};
	{error, Reason} ->
	    {error, "500", "Internal Server Error"}
    end.



