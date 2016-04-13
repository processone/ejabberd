%%%-------------------------------------------------------------------
%%% @author Evgeny Khramtsov <ekhramtsov@process-one.net>
%%% @copyright (C) 2016, Evgeny Khramtsov
%%% @doc
%%%
%%% @end
%%% Created : 13 Apr 2016 by Evgeny Khramtsov <ekhramtsov@process-one.net>
%%%-------------------------------------------------------------------
-module(mod_muc_sql).

-behaviour(mod_muc).

%% API
-export([init/2, store_room/4, restore_room/3, forget_room/3,
	 can_use_nick/4, get_rooms/2, get_nick/3, set_nick/4,
	 import/1, import/2, export/1]).

-include("jlib.hrl").
-include("mod_muc.hrl").
-include("logger.hrl").

%%%===================================================================
%%% API
%%%===================================================================
init(_Host, _Opts) ->
    ok.

store_room(LServer, Host, Name, Opts) ->
    SName = ejabberd_odbc:escape(Name),
    SHost = ejabberd_odbc:escape(Host),
    SOpts = ejabberd_odbc:encode_term(Opts),
    F = fun () ->
		odbc_queries:update_t(<<"muc_room">>,
				      [<<"name">>, <<"host">>, <<"opts">>],
				      [SName, SHost, SOpts],
				      [<<"name='">>, SName, <<"' and host='">>,
				       SHost, <<"'">>])
	end,
    ejabberd_odbc:sql_transaction(LServer, F).

restore_room(LServer, Host, Name) ->
    SName = ejabberd_odbc:escape(Name),
    SHost = ejabberd_odbc:escape(Host),
    case catch ejabberd_odbc:sql_query(LServer,
				       [<<"select opts from muc_room where name='">>,
					SName, <<"' and host='">>, SHost,
					<<"';">>]) of
	{selected, [<<"opts">>], [[Opts]]} ->
	    mod_muc:opts_to_binary(ejabberd_odbc:decode_term(Opts));
	_ ->
	    error
    end.

forget_room(LServer, Host, Name) ->
    SName = ejabberd_odbc:escape(Name),
    SHost = ejabberd_odbc:escape(Host),
    F = fun () ->
		ejabberd_odbc:sql_query_t([<<"delete from muc_room where name='">>,
					   SName, <<"' and host='">>, SHost,
					   <<"';">>])
	end,
    ejabberd_odbc:sql_transaction(LServer, F).

can_use_nick(LServer, Host, JID, Nick) ->
    SJID = jid:to_string(jid:tolower(jid:remove_resource(JID))),
    SNick = ejabberd_odbc:escape(Nick),
    SHost = ejabberd_odbc:escape(Host),
    case catch ejabberd_odbc:sql_query(LServer,
				       [<<"select jid from muc_registered ">>,
					<<"where nick='">>, SNick,
					<<"' and host='">>, SHost, <<"';">>]) of
	{selected, [<<"jid">>], [[SJID1]]} -> SJID == SJID1;
	_ -> true
    end.

get_rooms(LServer, Host) ->
    SHost = ejabberd_odbc:escape(Host),
    case catch ejabberd_odbc:sql_query(LServer,
				       [<<"select name, opts from muc_room ">>,
					<<"where host='">>, SHost, <<"';">>]) of
	{selected, [<<"name">>, <<"opts">>], RoomOpts} ->
	    lists:map(
	      fun([Room, Opts]) ->
		      #muc_room{name_host = {Room, Host},
				opts = mod_muc:opts_to_binary(
					 ejabberd_odbc:decode_term(Opts))}
	      end, RoomOpts);
	Err ->
	    ?ERROR_MSG("failed to get rooms: ~p", [Err]),
	    []
    end.

get_nick(LServer, Host, From) ->
    SJID = ejabberd_odbc:escape(jid:to_string(jid:tolower(jid:remove_resource(From)))),
    SHost = ejabberd_odbc:escape(Host),
    case catch ejabberd_odbc:sql_query(LServer,
				       [<<"select nick from muc_registered where "
					  "jid='">>,
					SJID, <<"' and host='">>, SHost,
					<<"';">>]) of
	{selected, [<<"nick">>], [[Nick]]} -> Nick;
	_ -> error
    end.

set_nick(LServer, Host, From, Nick) ->
    JID = jid:to_string(jid:tolower(jid:remove_resource(From))),
    SJID = ejabberd_odbc:escape(JID),
    SNick = ejabberd_odbc:escape(Nick),
    SHost = ejabberd_odbc:escape(Host),
    F = fun () ->
		case Nick of
		    <<"">> ->
			ejabberd_odbc:sql_query_t(
			  [<<"delete from muc_registered where ">>,
			   <<"jid='">>, SJID,
			   <<"' and host='">>, Host,
			   <<"';">>]),
			ok;
		    _ ->
			Allow = case ejabberd_odbc:sql_query_t(
				       [<<"select jid from muc_registered ">>,
					<<"where nick='">>,
					SNick,
					<<"' and host='">>,
					SHost, <<"';">>]) of
				    {selected, [<<"jid">>], [[J]]} -> J == JID;
				    _ -> true
				end,
			if Allow ->
				odbc_queries:update_t(<<"muc_registered">>,
						      [<<"jid">>, <<"host">>,
						       <<"nick">>],
						      [SJID, SHost, SNick],
						      [<<"jid='">>, SJID,
						       <<"' and host='">>, SHost,
						       <<"'">>]),
				ok;
			   true ->
				false
			end
		end
	end,
    ejabberd_odbc:sql_transaction(LServer, F).

export(_Server) ->
    [{muc_room,
      fun(Host, #muc_room{name_host = {Name, RoomHost}, opts = Opts}) ->
              case str:suffix(Host, RoomHost) of
                  true ->
                      SName = ejabberd_odbc:escape(Name),
                      SRoomHost = ejabberd_odbc:escape(RoomHost),
                      SOpts = ejabberd_odbc:encode_term(Opts),
                      [[<<"delete from muc_room where name='">>, SName,
                        <<"' and host='">>, SRoomHost, <<"';">>],
                       [<<"insert into muc_room(name, host, opts) ",
                          "values (">>,
                        <<"'">>, SName, <<"', '">>, SRoomHost,
                        <<"', '">>, SOpts, <<"');">>]];
                  false ->
                      []
              end
      end},
     {muc_registered,
      fun(Host, #muc_registered{us_host = {{U, S}, RoomHost},
                                nick = Nick}) ->
              case str:suffix(Host, RoomHost) of
                  true ->
                      SJID = ejabberd_odbc:escape(
                               jid:to_string(
                                 jid:make(U, S, <<"">>))),
                      SNick = ejabberd_odbc:escape(Nick),
                      SRoomHost = ejabberd_odbc:escape(RoomHost),
                      [[<<"delete from muc_registered where jid='">>,
                        SJID, <<"' and host='">>, SRoomHost, <<"';">>],
                       [<<"insert into muc_registered(jid, host, "
                          "nick) values ('">>,
                        SJID, <<"', '">>, SRoomHost, <<"', '">>, SNick,
                        <<"');">>]];
                  false ->
                      []
              end
      end}].

import(_LServer) ->
    [{<<"select name, host, opts from muc_room;">>,
      fun([Name, RoomHost, SOpts]) ->
              Opts = mod_muc:opts_to_binary(ejabberd_odbc:decode_term(SOpts)),
              #muc_room{name_host = {Name, RoomHost}, opts = Opts}
      end},
     {<<"select jid, host, nick from muc_registered;">>,
      fun([J, RoomHost, Nick]) ->
              #jid{user = U, server = S} = jid:from_string(J),
              #muc_registered{us_host = {{U, S}, RoomHost},
                              nick = Nick}
      end}].

import(_, _) ->
    pass.

%%%===================================================================
%%% Internal functions
%%%===================================================================
