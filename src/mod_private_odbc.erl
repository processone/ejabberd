%%%----------------------------------------------------------------------
%%% File    : mod_private_odbc.erl
%%% Author  : Alexey Shchepin <alexey@sevcom.net>
%%% Purpose : Private storage support
%%% Created :  5 Oct 2006 by Alexey Shchepin <alexey@sevcom.net>
%%% Id      : $Id$
%%%----------------------------------------------------------------------

-module(mod_private_odbc).
-author('alexey@sevcom.net').
-vsn('$Revision$ ').

-behaviour(gen_mod).

-export([start/2,
	 stop/1,
	 process_sm_iq/3,
	 remove_user/2]).

-include("ejabberd.hrl").
-include("jlib.hrl").

start(Host, Opts) ->
    IQDisc = gen_mod:get_opt(iqdisc, Opts, one_queue),
    ejabberd_hooks:add(remove_user, Host,
		       ?MODULE, remove_user, 50),
    gen_iq_handler:add_iq_handler(ejabberd_sm, Host, ?NS_PRIVATE,
				  ?MODULE, process_sm_iq, IQDisc).

stop(Host) ->
    ejabberd_hooks:delete(remove_user, Host,
			  ?MODULE, remove_user, 50),
    gen_iq_handler:remove_iq_handler(ejabberd_sm, Host, ?NS_PRIVATE).


process_sm_iq(From, _To, #iq{type = Type, sub_el = SubEl} = IQ) ->
    #jid{luser = LUser, lserver = LServer} = From,
    case lists:member(LServer, ?MYHOSTS) of
	true ->
	    {xmlelement, Name, Attrs, Els} = SubEl,
	    case Type of
		set ->
		    F = fun() ->
				lists:foreach(
				  fun(El) ->
					  set_data(LUser, LServer, El)
				  end, Els)
			end,
		    ejabberd_odbc:sql_transaction(LServer, F),
		    IQ#iq{type = result,
			  sub_el = [{xmlelement, Name, Attrs, []}]};
		get ->
		    case catch get_data(LUser, LServer, Els) of
			{'EXIT', _Reason} ->
			    IQ#iq{type = error,
				  sub_el = [SubEl,
					    ?ERR_INTERNAL_SERVER_ERROR]};
			Res ->
			    IQ#iq{type = result,
				  sub_el = [{xmlelement, Name, Attrs, Res}]}
		    end
	    end;
	false ->
	    IQ#iq{type = error, sub_el = [SubEl, ?ERR_NOT_ALLOWED]}
    end.

set_data(LUser, _LServer, El) ->
    case El of
	{xmlelement, _Name, Attrs, _Els} ->
	    XMLNS = xml:get_attr_s("xmlns", Attrs),
	    case XMLNS of
		"" ->
		    ignore;
		_ ->
		    Username = ejabberd_odbc:escape(LUser),
		    LXMLNS = ejabberd_odbc:escape(XMLNS),
		    SData = ejabberd_odbc:escape(
			       lists:flatten(xml:element_to_string(El))),
		    ejabberd_odbc:sql_query_t(
		      ["delete from private_storage "
		       "where username='", Username, "' and "
		       "namespace='", LXMLNS, "';"]),
		    ejabberd_odbc:sql_query_t(
		      ["insert into private_storage(username, namespace, data) "
		       "values ('", Username, "', '", LXMLNS, "', "
		       "'", SData, "');"])
	    end;
	_ ->
	    ignore
    end.

get_data(LUser, LServer, Els) ->
    get_data(LUser, LServer, Els, []).

get_data(_LUser, _LServer, [], Res) ->
    lists:reverse(Res);
get_data(LUser, LServer, [El | Els], Res) ->
    case El of
	{xmlelement, _Name, Attrs, _} ->
	    XMLNS = xml:get_attr_s("xmlns", Attrs),
	    Username = ejabberd_odbc:escape(LUser),
	    LXMLNS = ejabberd_odbc:escape(XMLNS),
	    case catch ejabberd_odbc:sql_query(
			 LServer,
			 ["select data from private_storage "
			  "where username='", Username, "' and "
			  "namespace='", LXMLNS, "';"]) of
		{selected, ["data"], [{SData}]} ->
		    case xml_stream:parse_element(SData) of
			Data when element(1, Data) == xmlelement ->
			    get_data(LUser, LServer, Els,
				     [Data | Res])
		    end;
		{selected, ["vcard"], []} ->
		    get_data(LUser, LServer, Els,
			     [El | Res])
	    end;
	_ ->
	    get_data(LUser, LServer, Els, Res)
    end.


remove_user(User, Server) ->
    LUser = jlib:nodeprep(User),
    LServer = jlib:nameprep(Server),
    Username = ejabberd_odbc:escape(LUser),
    ejabberd_odbc:sql_transaction(
      LServer,
      ["delete from private_storage where username='", Username, "';"]).


