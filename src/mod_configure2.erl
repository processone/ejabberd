%%%----------------------------------------------------------------------
%%% File    : mod_configure2.erl
%%% Author  : Alexey Shchepin <alexey@sevcom.net>
%%% Purpose : Support for online configuration of ejabberd
%%% Created : 26 Oct 2003 by Alexey Shchepin <alexey@sevcom.net>
%%% Id      : $Id$
%%%----------------------------------------------------------------------

-module(mod_configure2).
-author('alexey@sevcom.net').
-vsn('$Revision$ ').

-behaviour(gen_mod).

-export([start/1,
	 stop/0,
	 process_local_iq/3]).

-include("ejabberd.hrl").
-include("jlib.hrl").

-define(NS_ECONFIGURE, "http://ejabberd.jabberstudio.org/protocol/configure").

start(Opts) ->
    IQDisc = gen_mod:get_opt(iqdisc, Opts, one_queue),
    gen_iq_handler:add_iq_handler(ejabberd_local, ?NS_ECONFIGURE,
				  ?MODULE, process_local_iq, IQDisc),
    ok.

stop() ->
    gen_iq_handler:remove_iq_handler(ejabberd_local, ?NS_IQDATA),
    gen_iq_handler:remove_iq_handler(ejabberd_sm, ?NS_IQDATA).


process_local_iq(From, _To, {iq, ID, Type, XMLNS, SubEl}) ->
    case acl:match_rule(configure, From) of
	deny ->
	    {iq, ID, error, XMLNS, [SubEl, ?ERR_NOT_ALLOWED]};
	allow ->
	    %Lang = xml:get_tag_attr_s("xml:lang", SubEl),
	    case Type of
		set ->
		    {iq, ID, error, XMLNS,
		     [SubEl, ?ERR_FEATURE_NOT_IMPLEMENTED]};
		    %case xml:get_tag_attr_s("type", SubEl) of
		    %    "cancel" ->
		    %        {iq, ID, result, XMLNS,
		    %         [{xmlelement, "query", [{"xmlns", XMLNS}], []}]};
		    %    "submit" ->
		    %        XData = jlib:parse_xdata_submit(SubEl),
		    %        case XData of
		    %    	invalid ->
		    %    	    {iq, ID, error, XMLNS,
		    %    	     [SubEl, ?ERR_BAD_REQUEST]};
		    %    	_ ->
		    %    	    Node =
		    %    		string:tokens(
		    %    		  xml:get_tag_attr_s("node", SubEl),
		    %    		  "/"),
		    %    	    case set_form(Node, Lang, XData) of
		    %    		{result, Res} ->
		    %    		    {iq, ID, result, XMLNS,
		    %    		     [{xmlelement, "query",
		    %    		       [{"xmlns", XMLNS}],
		    %    		       Res
		    %    		      }]};
		    %    		{error, Error} ->
		    %    		    {iq, ID, error, XMLNS,
		    %    		     [SubEl, Error]}
		    %    	    end
		    %        end;
		    %    _ ->
		    %        {iq, ID, error, XMLNS,
		    %         [SubEl, ?ERR_NOT_ALLOWED]}
		    %end;
		get ->
		    case process_get(SubEl) of
			{result, Res} ->
			    {iq, ID, result, XMLNS, [Res]};
			{error, Error} ->
			    {iq, ID, error, XMLNS, [SubEl, Error]}
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
		 last_activity, [{{last_activity, '_', '$1'}, [], ['$1']}]) of
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
%process_get({xmlelement, Name, Attrs, SubEls}) ->
%    {result, };
process_get(_) ->
    {error, ?ERR_BAD_REQUEST}.

