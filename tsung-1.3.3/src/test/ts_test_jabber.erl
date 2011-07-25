%%%
%%%  Copyright © Nicolas Niclausse 2007
%%%
%%%	 Author : Nicolas Niclausse <Nicolas.Niclausse@niclux.org>
%%%  Created: 17 Mar 2007 by Nicolas Niclausse <Nicolas.Niclausse@niclux.org>
%%%
%%%  This program is free software; you can redistribute it and/or modify
%%%  it under the terms of the GNU General Public License as published by
%%%  the Free Software Foundation; either version 2 of the License, or
%%%  (at your option) any later version.
%%%
%%%  This program is distributed in the hope that it will be useful,
%%%  but WITHOUT ANY WARRANTY; without even the implied warranty of
%%%  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%%  GNU General Public License for more details.
%%%
%%%  You should have received a copy of the GNU General Public License
%%%  along with this program; if not, write to the Free Software
%%%  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307, USA.
%%% 

-module(ts_test_jabber).
-vc('$Id$ ').
-author('Nicolas.Niclausse@niclux.org').

-compile(export_all).

-include("ts_profile.hrl").
-include("ts_jabber.hrl").
-include_lib("eunit/include/eunit.hrl").

test()->ok.

bidi_subscribeok_test()->
    myset_env(),
    Req=list_to_binary("<presence type='subscribe' to='toto@im.apinc.org' from='tintin@jabber.org'>
  <status>Hi dude.</status>
</presence>"),
    Resp=list_to_binary("<presence to='tintin@jabber.org' type='subscribed'/>"),
    State=#state_rcv{},
    ?assertMatch({Resp,State}, ts_jabber:parse_bidi(Req,State)).

bidi_multisubscribeok_test()->
    myset_env(),
    Req=list_to_binary("<presence type='subscribe' to='toto@im.apinc.org' from='tintin@jabber.org'>  <status>Hi dude.</status></presence><presence type='subscribe' to='toto@im.apinc.org' from='glop@jabber.org'>  <status>Copaing?.</status></presence>"),
    Resp=list_to_binary("<presence to='tintin@jabber.org' type='subscribed'/><presence to='glop@jabber.org' type='subscribed'/>"),
    State=#state_rcv{},
    ?assertMatch({Resp,State}, ts_jabber:parse_bidi(Req,State)).

bidi_multisubscribe_nok_test()->
    myset_env(),
    Req=list_to_binary("<presence type='subscribe' to='toto@im.apinc.org' from='tintin@jabber.org'>  <status>Hi dude.</status></presence><presence type='subscribed'  from='glop@jabber.org'>  <status>Copaing?.</status></presence>"),
    Resp=list_to_binary("<presence to='tintin@jabber.org' type='subscribed'/>"),
    State=#state_rcv{},
    ?assertMatch({Resp,State}, ts_jabber:parse_bidi(Req,State)).

bidi_subscribe_nok_test()->
    myset_env(),
    Req=list_to_binary("<presence type='subscribed' from='tintin@jabber.org'>
  <status>Hi dude.</status>
</presence>"),
    State=#state_rcv{},
    ?assertMatch({nodata,State}, ts_jabber:parse_bidi(Req,State)).

bidi_nok_test()->
    myset_env(),
    Req=list_to_binary("<presence from='tintin@jabber.org'><status>Alive.</status></presence>"),
    State=#state_rcv{},
    ?assertMatch({nodata,State}, ts_jabber:parse_bidi(Req,State)).

myset_env()->
    application:set_env(stdlib,debug_level,0).
