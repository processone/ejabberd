%%%-------------------------------------------------------------------
%%% @author Evgeny Khramtsov <ekhramtsov@process-one.net>
%%% @copyright (C) 2016, Evgeny Khramtsov
%%% @doc
%%%
%%% @end
%%% Created : 14 Apr 2016 by Evgeny Khramtsov <ekhramtsov@process-one.net>
%%%-------------------------------------------------------------------
-module(mod_blocking_riak).

-behaviour(mod_blocking).

%% API
-export([process_blocklist_block/3, unblock_by_filter/3,
	 process_blocklist_get/2]).

-include("mod_privacy.hrl").

%%%===================================================================
%%% API
%%%===================================================================
process_blocklist_block(LUser, LServer, Filter) ->
    {atomic,
     begin
         case ejabberd_riak:get(privacy, mod_privacy_riak:privacy_schema(),
				{LUser, LServer}) of
             {ok, #privacy{default = Default, lists = Lists} = P} ->
                 case lists:keysearch(Default, 1, Lists) of
                     {value, {_, List}} ->
                         NewDefault = Default,
                         NewLists1 = lists:keydelete(Default, 1, Lists);
                     false ->
                         NewDefault = <<"Blocked contacts">>,
                         NewLists1 = Lists,
                         List = []
                 end;
             {error, _} ->
                 P = #privacy{us = {LUser, LServer}},
                 NewDefault = <<"Blocked contacts">>,
                 NewLists1 = [],
                 List = []
         end,
         NewList = Filter(List),
         NewLists = [{NewDefault, NewList} | NewLists1],
         case ejabberd_riak:put(P#privacy{default = NewDefault,
                                          lists = NewLists},
				mod_privacy_riak:privacy_schema()) of
             ok ->
                 {ok, NewDefault, NewList};
             Err ->
                 Err
         end
     end}.

unblock_by_filter(LUser, LServer, Filter) ->
    {atomic,
     case ejabberd_riak:get(privacy, mod_privacy_riak:privacy_schema(),
			    {LUser, LServer}) of
         {error, _} ->
             %% No lists, nothing to unblock
             ok;
         {ok, #privacy{default = Default, lists = Lists} = P} ->
             case lists:keysearch(Default, 1, Lists) of
                 {value, {_, List}} ->
                     NewList = Filter(List),
                     NewLists1 = lists:keydelete(Default, 1, Lists),
                     NewLists = [{Default, NewList} | NewLists1],
                     case ejabberd_riak:put(P#privacy{lists = NewLists},
					    mod_privacy_riak:privacy_schema()) of
                         ok ->
                             {ok, Default, NewList};
                         Err ->
                             Err
                     end;
                 false ->
                     %% No default list, nothing to unblock
                     ok
             end
     end}.

process_blocklist_get(LUser, LServer) ->
    case ejabberd_riak:get(privacy, mod_privacy_riak:privacy_schema(),
			   {LUser, LServer}) of
        {ok, #privacy{default = Default, lists = Lists}} ->
            case lists:keysearch(Default, 1, Lists) of
                {value, {_, List}} -> List;
                _ -> []
            end;
        {error, notfound} ->
            [];
        {error, _} ->
            error
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
