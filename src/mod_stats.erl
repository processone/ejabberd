%%%----------------------------------------------------------------------
%%% File    : mod_stats.erl
%%% Author  : Alexey Shchepin <alexey@sevcom.net>
%%% Purpose : 
%%% Created : 11 Jan 2003 by Alexey Shchepin <alexey@sevcom.net>
%%% Id      : $Id$
%%%----------------------------------------------------------------------

-module(mod_stats).
-author('alexey@sevcom.net').
-vsn('$Revision$ ').

-export([start/0,
	 process_local_iq/3]).

-include("namespaces.hrl").

start() ->
    ejabberd_local:register_iq_handler(?NS_STATS, ?MODULE, process_local_iq).


process_local_iq(From, To, {iq, ID, Type, XMLNS, SubEl}) ->
    Lang = xml:get_tag_attr_s("xml:lang", SubEl),
    case Type of
	set ->
	    {iq, ID, error, XMLNS, [SubEl, {xmlelement, "error",
					    [{"code", "405"}],
					    [{xmlcdata, "Not Allowed"}]}]};
	get ->
	    {xmlelement, _, Attrs, Els} = SubEl,
	    Node = string:tokens(xml:get_tag_attr_s("node", SubEl), "/"),
	    Names = get_names(Els, []),
	    
	    {T, Res} = get_local_stats(Node, Names),
	    case T of
		result ->
		    {iq, ID, result, ?NS_STATS, Res};
		error ->
		    {iq, ID, error, ?NS_STATS, [SubEl ++ Res]}
	    end

	    %case Node of
		%[] ->
		%    {iq, ID, result, XMLNS,
		%     [{xmlelement,
		%       "query",
		%       [{"xmlns", ?NS_STATS}],
		%       [{xmlelement, "stat", [{"name", "time/uptime"}], []},
		%	{xmlelement, "stat", [{"name", "time/cputime"}], []}
		%       ]}]};
		%["online users"] ->
		%    {iq, ID, result, XMLNS,
		%     [{xmlelement, "query", [{"xmlns", ?NS_DISCO_ITEMS}],
		%       get_online_users()
		%      }]};
		%["all users"] ->
		%    {iq, ID, result, XMLNS,
		%     [{xmlelement, "query", [{"xmlns", ?NS_DISCO_ITEMS}],
		%       get_all_users()
		%      }]};
		%["outgoing s2s"] ->
		%    {iq, ID, result, XMLNS,
		%     [{xmlelement, "query", [{"xmlns", ?NS_DISCO_ITEMS}],
		%       get_outgoing_s2s(Lang)
		%      }]};
		%["outgoing s2s", Host] ->
		%    {iq, ID, result, XMLNS,
		%     [{xmlelement, "query", [{"xmlns", ?NS_DISCO_ITEMS}],
		%       get_outgoing_s2s(Lang, Host)
		%      }]};
		%_ ->
		%    {iq, ID, error, XMLNS,
		%     [SubEl, {xmlelement, "error",
		%	      [{"code", "501"}],
		%	      [{xmlcdata, "Not Implemented"}]}]}
	    %end
    end.


get_names([], Res) ->
    Res;
get_names([{xmlelement, "stat", Attrs, _} | Els], Res) ->
    Name = xml:get_attr_s("name", Attrs),
    case Name of
	"" ->
	    get_names(Els, Res);
	_ ->
	    get_names(Els, [Name | Res])
    end;
get_names([_ | Els], Res) ->
    get_names(Els, Res).


get_local_stats([], []) ->
    {result,
     [{xmlelement,
       "query",
       [{"xmlns", ?NS_STATS}],
       [{xmlelement, "stat", [{"name", "time/uptime"}], []},
	{xmlelement, "stat", [{"name", "time/cputime"}], []},
	{xmlelement, "stat", [{"name", "users/online"}], []},
	{xmlelement, "stat", [{"name", "users/total"}], []}
       ]}]};
get_local_stats([], Names) ->
    {result,
     [{xmlelement,
       "query",
       [{"xmlns", ?NS_STATS}],
       lists:map(fun(Name) -> get_local_stat([], Name) end, Names)
      }]};
get_local_stats(_, _) ->
    {error,
     [{xmlelement, "error",
       [{"code", "501"}],
       [{xmlcdata, "Not Implemented"}]}]}.


-define(STAT(Val, Unit),
	{xmlelement, "stat",
	 [{"name", Name},
	  {"units", Unit},
	  {"value", Val}
	 ], []}).

-define(STATERR(Code, Desc),
	{xmlelement, "stat",
	 [{"name", Name}],
	 [{xmlelement, "error",
	   [{"code", Code}],
	   [{xmlcdata, Desc}]}]}).


get_local_stat([], Name) when Name == "time/uptime" ->
    ?STAT(io_lib:format("~.3f", [element(1, statistics(wall_clock))/1000]),
	  "seconds");
get_local_stat([], Name) when Name == "time/cputime" ->
    ?STAT(io_lib:format("~.3f", [element(1, statistics(runtime))/1000]),
	  "seconds");
get_local_stat([], Name) when Name == "users/online" ->
    case catch ejabberd_sm:dirty_get_sessions_list() of
	{'EXIT', Reason} ->
	    ?STATERR("500", "Internal Server Error");
	Users ->
	    ?STAT(integer_to_list(length(Users)), "users")
    end;
get_local_stat([], Name) when Name == "users/total" ->
    case catch ejabberd_auth:dirty_get_registered_users() of
	{'EXIT', Reason} ->
	    ?STATERR("500", "Internal Server Error");
	Users ->
	    ?STAT(integer_to_list(length(Users)), "users")
    end;
get_local_stat(_, Name) ->
    ?STATERR("404", "Not Found").
