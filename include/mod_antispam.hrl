%%%----------------------------------------------------------------------
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
%%%----------------------------------------------------------------------

-define(MODULE_ANTISPAM, mod_antispam).

-type url() :: binary().
-type filename() :: binary() | none | false.
-type jid_set() :: sets:set(jid:ljid()).
-type url_set() :: sets:set(url()).

-define(DEFAULT_SPAM_SOURCE_DOMAINS_NODE, <<"spam_source_domains">>).
-define(DEFAULT_MUC_BANS_NODE, <<"muc_bans_sha256">>).

-record(rtbl_service_node,
        {type = spam_source_domains               :: rtbl_service_node_type(),
         subscribed = false                       :: boolean(),
         retry_timer = undefined                  :: reference() | undefined}).

-type rtbl_service_node_type() :: spam_source_domains | muc_bans.
-type rtbl_service_node() :: #rtbl_service_node{}.
-type rtbl_services() :: #{Host :: binary() => #{Node :: binary() => rtbl_service_node()}}.
