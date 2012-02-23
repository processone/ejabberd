%%%----------------------------------------------------------------------
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

-record(roster, {usj,
		 us,
		 jid,
		 name = <<>>,
		 subscription = none,
		 ask = none,
		 groups = [],
		 askmessage = <<>>,
		 xs = []}).

-record(roster_version, {us,
			version}).

%% @type rosteritem() = {roster, USJ, US, Contact_JID, Name, Subscription, Ask, Groups, Askmessage, Xs}
%%     USJ = {LUser, LServer, Prepd_Contact_JID}
%%         LUser = binary()
%%         LServer = binary()
%%         Prepd_Contact_JID = jlib:shortjid()
%%     US = {LUser, LServer}
%%     Contact_JID = jlib:shortjid()
%%     Name = binary()
%%     Subscription = none | to | from | both
%%     Ask = none | out | in | both
%%     Groups = [binary()]
%%     Askmessage = binary()
%%     Xs = [exmpp_xml:xmlel()]

%% TODO: keep a non-prepped jid like in mnesia mod_roster?
-record(rosteritem, {user_host_jid,
		     name = <<"">>,
		     subscription = none,
		     ask = none,
		     askmessage = <<"">>}).
-record(rostergroup, {user_host_jid,
		      grp}).
