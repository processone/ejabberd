%%%-------------------------------------------------------------------
%%% @author Evgeny Khramtsov <ekhramtsov@process-one.net>
%%% @copyright (C) 2017, Evgeny Khramtsov
%%% @doc
%%%
%%% @end
%%% Created : 11 Jan 2017 by Evgeny Khramtsov <ekhramtsov@process-one.net>
%%%-------------------------------------------------------------------
-module(ejabberd_router_mnesia).
-behaviour(ejabberd_router).

%% API
-export([init/0, register_route/4, unregister_route/2, find_routes/1,
	 host_of_route/1, is_my_route/1, is_my_host/1, get_all_routes/0,
	 handle_event/1]).

-include("ejabberd.hrl").
-include("ejabberd_router.hrl").
-include("logger.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

%%%===================================================================
%%% API
%%%===================================================================
init() ->
    update_tables(),
    ejabberd_mnesia:create(?MODULE, route,
			   [{ram_copies, [node()]},
			    {type, bag},
			    {attributes, record_info(fields, route)}]),
    mnesia:add_table_copy(route, node(), ram_copies),
    mnesia:subscribe({table, route, simple}),
    lists:foreach(
      fun (Pid) -> erlang:monitor(process, Pid) end,
      mnesia:dirty_select(route,
			  [{{route, '_', '$1', '_'}, [], ['$1']}])).

register_route(Domain, ServerHost, LocalHint, undefined) ->
    F = fun () ->
		mnesia:write(#route{domain = Domain,
				    pid = self(),
				    server_host = ServerHost,
				    local_hint = LocalHint})
	end,
    transaction(F);
register_route(Domain, ServerHost, _LocalHint, N) ->
    Pid = self(),
    F = fun () ->
		case mnesia:wread({route, Domain}) of
		    [] ->
			mnesia:write(#route{domain = Domain,
					    server_host = ServerHost,
					    pid = Pid,
					    local_hint = 1}),
			lists:foreach(
			  fun (I) ->
				  mnesia:write(
				    #route{domain = Domain,
					   pid = undefined,
					   server_host = ServerHost,
					   local_hint = I})
			  end,
			  lists:seq(2, N));
		    Rs ->
			lists:any(
			  fun (#route{pid = undefined,
				      local_hint = I} = R) ->
				  mnesia:write(
				    #route{domain = Domain,
					   pid = Pid,
					   server_host = ServerHost,
					   local_hint = I}),
				  mnesia:delete_object(R),
				  true;
			      (_) -> false
			  end,
			  Rs)
		end
	end,
    transaction(F).

unregister_route(Domain, undefined) ->
    F = fun () ->
		case mnesia:match_object(
		       #route{domain = Domain, pid = self(), _ = '_'}) of
		    [R] -> mnesia:delete_object(R);
		    _ -> ok
		end
	end,
    transaction(F);
unregister_route(Domain, _) ->
    F = fun () ->
		case mnesia:match_object(
		       #route{domain = Domain, pid = self(), _ = '_'}) of
		    [R] ->
			I = R#route.local_hint,
			ServerHost = R#route.server_host,
			mnesia:write(#route{domain = Domain,
					    server_host = ServerHost,
					    pid = undefined,
					    local_hint = I}),
			mnesia:delete_object(R);
		    _ -> ok
		end
	end,
    transaction(F).

find_routes(Domain) ->
    mnesia:dirty_read(route, Domain).

host_of_route(Domain) ->
    case mnesia:dirty_read(route, Domain) of
	[#route{server_host = ServerHost}|_] ->
	    {ok, ServerHost};
	[] ->
	    error
    end.

is_my_route(Domain) ->
    mnesia:dirty_read(route, Domain) /= [].

is_my_host(Domain) ->
    case mnesia:dirty_read(route, Domain) of
	[#route{server_host = Host}|_] ->
	    Host == Domain;
	[] ->
	    false
    end.

get_all_routes() ->
    mnesia:dirty_select(
      route,
      ets:fun2ms(
	fun(#route{domain = Domain, server_host = ServerHost})
	      when Domain /= ServerHost -> Domain
	end)).

handle_event({mnesia_table_event,
	      {write, #route{pid = Pid}, _ActivityId}}) ->
    erlang:monitor(process, Pid);
handle_event({'DOWN', _Ref, _Type, Pid, _Info}) ->
    F = fun () ->
		Es = mnesia:select(route,
				   [{#route{pid = Pid, _ = '_'}, [], ['$_']}]),
		lists:foreach(
		  fun(E) ->
			  if is_integer(E#route.local_hint) ->
				  LDomain = E#route.domain,
				  I = E#route.local_hint,
				  ServerHost = E#route.server_host,
				  mnesia:write(#route{domain = LDomain,
						      server_host = ServerHost,
						      pid = undefined,
						      local_hint = I}),
				  mnesia:delete_object(E);
			     true ->
				  mnesia:delete_object(E)
			  end
		  end, Es)
	end,
    transaction(F).

%%%===================================================================
%%% Internal functions
%%%===================================================================
transaction(F) ->
    case mnesia:transaction(F) of
	{atomic, _} ->
	    ok;
	{aborted, Reason} ->
	    ?ERROR_MSG("Mnesia transaction failed: ~p", [Reason]),
	    {error, Reason}
    end.

-spec update_tables() -> ok.
update_tables() ->
    try
	mnesia:transform_table(route, ignore, record_info(fields, route))
    catch exit:{aborted, {no_exists, _}} ->
	    ok
    end,
    case lists:member(local_route, mnesia:system_info(tables)) of
	true -> mnesia:delete_table(local_route);
	false -> ok
    end.
