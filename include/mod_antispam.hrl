%%%----------------------------------------------------------------------
%%%
%%% ejabberd, Copyright (C) 2002-2025   ProcessOne
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

-define(MODULE_ANTISPAM, mod_antispam).

-type url() :: binary().
-type filename() :: binary() | none | false.
-type jid_set() :: sets:set(ljid()).
-type url_set() :: sets:set(url()).

-define(DEFAULT_RTBL_DOMAINS_NODE, <<"spam_source_domains">>).

-record(rtbl_service,
        {host = none                       :: binary() | none,
         node = ?DEFAULT_RTBL_DOMAINS_NODE :: binary(),
         subscribed = false                :: boolean(),
         retry_timer = undefined           :: reference() | undefined}).

-type rtbl_service() :: #rtbl_service{}.
