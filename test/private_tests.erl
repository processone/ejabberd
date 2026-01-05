%%%-------------------------------------------------------------------
%%% Author  : Evgeny Khramtsov <ekhramtsov@process-one.net>
%%% Created : 23 Nov 2018 by Evgeny Khramtsov <ekhramtsov@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2026   ProcessOne
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
%%% You should have received a copy of the GNU General Public License along
%%% with this program; if not, write to the Free Software Foundation, Inc.,
%%% 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
%%%
%%%-------------------------------------------------------------------
-module(private_tests).

%% API
-compile(export_all).
-import(suite, [my_jid/1, server_jid/1, is_feature_advertised/3,
		send_recv/2, disconnect/1]).

-include("suite.hrl").

%%%===================================================================
%%% API
%%%===================================================================
%%%===================================================================
%%% Single user tests
%%%===================================================================
single_cases() ->
    {private_single, [sequence],
     [single_test(test_features),
      single_test(test_no_namespace),
      single_test(test_set_get),
      single_test(test_published)]}.

test_features(Config) ->
    Server = jid:encode(server_jid(Config)),
    MyJID = my_jid(Config),
    case gen_mod:is_loaded(Server, mod_pubsub) of
	true ->
	    true = is_feature_advertised(Config, ?NS_BOOKMARKS_CONVERSION_0,
					 jid:remove_resource(MyJID));
	false ->
	    ok
    end,
    disconnect(Config).

test_no_namespace(Config) ->
    WrongEl = #xmlel{name = <<"wrong">>},
    #iq{type = error} =
        send_recv(Config, #iq{type = get,
			      sub_els = [#private{sub_els = [WrongEl]}]}),
    disconnect(Config).

test_set_get(Config) ->
    Storage = bookmark_storage(),
    StorageXMLOut = xmpp:encode(Storage),
    #iq{type = result, sub_els = []} =
        send_recv(
          Config, #iq{type = set,
                      sub_els = [#private{sub_els = [StorageXMLOut]}]}),
    #iq{type = result,
        sub_els = [#private{sub_els = [StorageXMLIn]}]} =
        send_recv(
          Config,
          #iq{type = get,
              sub_els = [#private{sub_els = [xmpp:encode(
                                               #bookmark_storage{})]}]}),
    Storage = xmpp:decode(StorageXMLIn),
    disconnect(Config).

test_published(Config) ->
    Server = jid:encode(server_jid(Config)),
    case gen_mod:is_loaded(Server, mod_pubsub) of
	true ->
	    Storage = bookmark_storage(),
	    Node = xmpp:get_ns(Storage),
	    #iq{type = result,
		sub_els = [#pubsub{items = #ps_items{node = Node, items = Items}}]} =
		send_recv(
		  Config,
		  #iq{type = get,
		      sub_els = [#pubsub{items = #ps_items{node = Node}}]}),
	    [#ps_item{sub_els = [StorageXMLIn]}] = Items,
	    Storage = xmpp:decode(StorageXMLIn),
	    #iq{type = result, sub_els = []} =
		send_recv(Config,
			  #iq{type = set,
			      sub_els = [#pubsub_owner{delete = {Node, <<>>}}]}),
	    #iq{type = result, sub_els = []} =
		send_recv(Config,
			  #iq{type = set,
			      sub_els = [#pubsub_owner{delete = {?NS_PEP_BOOKMARKS, <<>>}}]});
	false ->
	    ok
    end,
    disconnect(Config).

%%%===================================================================
%%% Internal functions
%%%===================================================================
single_test(T) ->
    list_to_atom("private_" ++ atom_to_list(T)).

conference_bookmark() ->
    #bookmark_conference{
       name = <<"Some name">>,
       autojoin = true,
       jid = jid:make(<<"some">>, <<"some.conference.org">>)}.

bookmark_storage() ->
    #bookmark_storage{conference = [conference_bookmark()]}.
