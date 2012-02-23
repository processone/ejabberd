%%%----------------------------------------------------------------------
%%% File    : mod_configure2.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Support for online configuration of ejabberd
%%% Created : 26 Oct 2003 by Alexey Shchepin <alexey@process-one.net>
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

-module(mod_configure2).
-author('alexey@process-one.net').

-behaviour(gen_mod).

-export([start/2,
	 stop/1,
	 process_local_iq/3]).

-include("ejabberd.hrl").
-include("jlib.hrl").

-define(NS_ECONFIGURE, "http://ejabberd.jabberstudio.org/protocol/configure").

start(Host, Opts) ->
    IQDisc = gen_mod:get_opt(iqdisc, Opts, one_queue),
    gen_iq_handler:add_iq_handler(ejabberd_local, Host, ?NS_ECONFIGURE,
				  ?MODULE, process_local_iq, IQDisc),
    ok.

stop(Host) ->
    gen_iq_handler:remove_iq_handler(ejabberd_local, Host, ?NS_ECONFIGURE).


process_local_iq(From, To, #iq{type = Type, lang = _Lang, sub_el = SubEl} = IQ) ->
    case acl:match_rule(To#jid.lserver, configure, From) of
	deny ->
	    IQ#iq{type = error, sub_el = [SubEl, ?ERR_NOT_ALLOWED]};
	allow ->
	    case Type of
		set ->
		    IQ#iq{type = error,
			  sub_el = [SubEl, ?ERR_FEATURE_NOT_IMPLEMENTED]};
		    %%case xml:get_tag_attr_s("type", SubEl) of
		    %%    "cancel" ->
		    %%        IQ#iq{type = result,
		    %%		   sub_el = [{xmlelement, "query",
		    %%			      [{"xmlns", XMLNS}], []}]};
		    %%    "submit" ->
		    %%        XData = jlib:parse_xdata_submit(SubEl),
		    %%        case XData of
		    %%    	invalid ->
		    %%    	    IQ#iq{type = error,
		    %%			  sub_el = [SubEl, ?ERR_BAD_REQUEST]};
		    %%    	_ ->
		    %%    	    Node =
		    %%    		string:tokens(
		    %%    		  xml:get_tag_attr_s("node", SubEl),
		    %%    		  "/"),
		    %%    	    case set_form(Node, Lang, XData) of
		    %%    		{result, Res} ->
		    %%    		    IQ#iq{type = result,
		    %%				  sub_el = [{xmlelement, "query",
		    %%					     [{"xmlns", XMLNS}],
		    %%					     Res
		    %%					    }]};
		    %%    		{error, Error} ->
		    %%    		    IQ#iq{type = error,
		    %%				  sub_el = [SubEl, Error]}
		    %%    	    end
		    %%        end;
		    %%    _ ->
		    %%        IQ#iq{type = error,
		    %%		   sub_el = [SubEl, ?ERR_NOT_ALLOWED]}
		    %%end;
		get ->
		    case process_get(SubEl) of
			{result, Res} ->
			    IQ#iq{type = result, sub_el = [Res]};
			{error, Error} ->
			    IQ#iq{type = error, sub_el = [SubEl, Error]}
		    end
	    end
    end.


process_get({xmlelement, "info", _Attrs, _SubEls}) ->
    S2SConns = ejabberd_s2s:dirty_get_connections(),
    TConns = lists:usort([element(2, C) || C <- S2SConns]),
    Attrs = [{"registered-users",
	      integer_to_list(mnesia:table_info(passwd, size))},
	     {"online-users",
	      integer_to_list(mnesia:table_info(presence, size))},
	     {"running-nodes",
	      integer_to_list(length(mnesia:system_info(running_db_nodes)))},
	     {"stopped-nodes",
	      integer_to_list(
		length(lists:usort(mnesia:system_info(db_nodes) ++
				   mnesia:system_info(extra_db_nodes)) --
		       mnesia:system_info(running_db_nodes)))},
	     {"outgoing-s2s-servers", integer_to_list(length(TConns))}],
    {result, {xmlelement, "info",
	      [{"xmlns", ?NS_ECONFIGURE} | Attrs], []}};
process_get({xmlelement, "welcome-message", Attrs, _SubEls}) ->
    {Subj, Body} = case ejabberd_config:get_local_option(welcome_message) of
		       {_Subj, _Body} = SB -> SB;
		       _ -> {"", ""}
		   end,
    {result, {xmlelement, "welcome-message", Attrs,
	      [{xmlelement, "subject", [], [{xmlcdata, Subj}]},
	       {xmlelement, "body", [], [{xmlcdata, Body}]}]}};
process_get({xmlelement, "registration-watchers", Attrs, _SubEls}) ->
    SubEls =
	case ejabberd_config:get_local_option(registration_watchers) of
	    JIDs when is_list(JIDs) ->
		lists:map(fun(JID) ->
				  {xmlelement, "jid", [], [{xmlcdata, JID}]}
			  end, JIDs);
	    _ ->
		[]
	end,
    {result, {xmlelement, "registration_watchers", Attrs, SubEls}};
process_get({xmlelement, "acls", Attrs, _SubEls}) ->
    Str = lists:flatten(io_lib:format("~p.", [ets:tab2list(acl)])),
    {result, {xmlelement, "acls", Attrs, [{xmlcdata, Str}]}};
process_get({xmlelement, "access", Attrs, _SubEls}) ->
    Str =
	lists:flatten(
	  io_lib:format(
	    "~p.",
	    [ets:select(config,
			[{{config, {access, '$1'}, '$2'},
			  [],
			  [{{access, '$1', '$2'}}]}])
	    ])),
    {result, {xmlelement, "access", Attrs, [{xmlcdata, Str}]}};
process_get({xmlelement, "last", Attrs, _SubEls}) ->
    case catch mnesia:dirty_select(
		 last_activity, [{{last_activity, '_', '$1', '_'}, [], ['$1']}]) of
	{'EXIT', _Reason} ->
	    {error, ?ERR_INTERNAL_SERVER_ERROR};
	Vals ->
	    {MegaSecs, Secs, _MicroSecs} = now(),
	    TimeStamp = MegaSecs * 1000000 + Secs,
	    Str = lists:flatten(
		    lists:append(
		      [[integer_to_list(TimeStamp - V), " "] || V <- Vals])),
	    {result, {xmlelement, "last", Attrs, [{xmlcdata, Str}]}}
    end;
%%process_get({xmlelement, Name, Attrs, SubEls}) ->
%%    {result, };
process_get(_) ->
    {error, ?ERR_BAD_REQUEST}.

