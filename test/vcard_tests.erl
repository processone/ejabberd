%%%-------------------------------------------------------------------
%%% Author  : Evgeny Khramtsov <ekhramtsov@process-one.net>
%%% Created : 16 Nov 2016 by Evgeny Khramtsov <ekhramtsov@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2017   ProcessOne
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
%%%----------------------------------------------------------------------

-module(vcard_tests).

%% API
-compile(export_all).
-import(suite, [send_recv/2, disconnect/1, is_feature_advertised/2,
		is_feature_advertised/3, server_jid/1,
		my_jid/1, wait_for_slave/1, wait_for_master/1,
		recv_presence/1, recv/1]).

-include("suite.hrl").

%%%===================================================================
%%% API
%%%===================================================================
%%%===================================================================
%%% Single user tests
%%%===================================================================
single_cases() ->
    {vcard_single, [sequence],
     [single_test(feature_enabled),
      single_test(get_set),
      single_test(service_vcard)]}.

feature_enabled(Config) ->
    BareMyJID = jid:remove_resource(my_jid(Config)),
    true = is_feature_advertised(Config, ?NS_VCARD),
    true = is_feature_advertised(Config, ?NS_VCARD, BareMyJID),
    disconnect(Config).

get_set(Config) ->
    VCard =
        #vcard_temp{fn = <<"Peter Saint-Andre">>,
		    n = #vcard_name{family = <<"Saint-Andre">>,
				    given = <<"Peter">>},
		    nickname = <<"stpeter">>,
		    bday = <<"1966-08-06">>,
		    adr = [#vcard_adr{work = true,
				      extadd = <<"Suite 600">>,
				      street = <<"1899 Wynkoop Street">>,
				      locality = <<"Denver">>,
				      region = <<"CO">>,
				      pcode = <<"80202">>,
				      ctry = <<"USA">>},
			   #vcard_adr{home = true,
				      locality = <<"Denver">>,
				      region = <<"CO">>,
				      pcode = <<"80209">>,
				      ctry = <<"USA">>}],
		    tel = [#vcard_tel{work = true,voice = true,
				      number = <<"303-308-3282">>},
			   #vcard_tel{home = true,voice = true,
				      number = <<"303-555-1212">>}],
		    email = [#vcard_email{internet = true,pref = true,
					  userid = <<"stpeter@jabber.org">>}],
		    jabberid = <<"stpeter@jabber.org">>,
		    title = <<"Executive Director">>,role = <<"Patron Saint">>,
		    org = #vcard_org{name = <<"XMPP Standards Foundation">>},
		    url = <<"http://www.xmpp.org/xsf/people/stpeter.shtml">>,
		    desc = <<"More information about me is located on my "
			     "personal website: http://www.saint-andre.com/">>},
    #iq{type = result, sub_els = []} =
        send_recv(Config, #iq{type = set, sub_els = [VCard]}),
    %% TODO: check if VCard == VCard1.
    #iq{type = result, sub_els = [_VCard1]} =
        send_recv(Config, #iq{type = get, sub_els = [#vcard_temp{}]}),
    disconnect(Config).

service_vcard(Config) ->
    JID = server_jid(Config),
    ct:comment("Retreiving vCard from ~s", [jid:encode(JID)]),
    #iq{type = result, sub_els = [#vcard_temp{}]} =
	send_recv(Config, #iq{type = get, to = JID, sub_els = [#vcard_temp{}]}),
    disconnect(Config).

%%%===================================================================
%%% Master-slave tests
%%%===================================================================
master_slave_cases() ->
    {vcard_master_slave, [sequence], []}.
   %%[master_slave_test(xupdate)]}.

xupdate_master(Config) ->
    Img = <<137, "PNG\r\n", 26, $\n>>,
    ImgHash = p1_sha:sha(Img),
    MyJID = my_jid(Config),
    Peer = ?config(slave, Config),
    wait_for_slave(Config),
    #presence{from = MyJID, type = available} = send_recv(Config, #presence{}),
    #presence{from = Peer, type = available} = recv_presence(Config),
    VCard = #vcard_temp{photo = #vcard_photo{type = <<"image/png">>, binval = Img}},
    #iq{type = result, sub_els = []} =
	send_recv(Config, #iq{type = set, sub_els = [VCard]}),
    #presence{from = MyJID, type = available,
	      sub_els = [#vcard_xupdate{hash = ImgHash}]} = recv_presence(Config),
    #iq{type = result, sub_els = []} =
	send_recv(Config, #iq{type = set, sub_els = [#vcard_temp{}]}),
    ?recv2(#presence{from = MyJID, type = available,
		     sub_els = [#vcard_xupdate{hash = undefined}]},
	   #presence{from = Peer, type = unavailable}),
    disconnect(Config).

xupdate_slave(Config) ->
    Img = <<137, "PNG\r\n", 26, $\n>>,
    ImgHash = p1_sha:sha(Img),
    MyJID = my_jid(Config),
    Peer = ?config(master, Config),
    #presence{from = MyJID, type = available} = send_recv(Config, #presence{}),
    wait_for_master(Config),
    #presence{from = Peer, type = available} = recv_presence(Config),
    #presence{from = Peer, type = available,
	      sub_els = [#vcard_xupdate{hash = ImgHash}]} = recv_presence(Config),
    #presence{from = Peer, type = available,
	      sub_els = [#vcard_xupdate{hash = undefined}]} = recv_presence(Config),
    disconnect(Config).

%%%===================================================================
%%% Internal functions
%%%===================================================================
single_test(T) ->
    list_to_atom("vcard_" ++ atom_to_list(T)).

master_slave_test(T) ->
    {list_to_atom("vcard_" ++ atom_to_list(T)), [parallel],
     [list_to_atom("vcard_" ++ atom_to_list(T) ++ "_master"),
      list_to_atom("vcard_" ++ atom_to_list(T) ++ "_slave")]}.
