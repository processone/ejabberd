%%%----------------------------------------------------------------------
%%% File    : mod_roster.hrl
%%% Author  : Alexey Shchepin <alexey@sevcom.net>
%%% Purpose : Roster management
%%% Created :  5 Mar 2005 by Alexey Shchepin <alexey@sevcom.net>
%%% Id      : $Id: mod_roster.hrl 11 2005-03-06 22:36:15Z alexey $
%%%----------------------------------------------------------------------

-record(roster, {usj,
		 us,
		 jid,
		 name = "",
		 subscription = none,
		 ask = none,
		 groups = [],
		 askmessage = [],
		 xs = []}).

