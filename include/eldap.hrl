%%%----------------------------------------------------------------------
%%%
%%% ejabberd, Copyright (C) 2002-2023   ProcessOne
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

-define(LDAP_PORT, 389).

-define(LDAPS_PORT, 636).

-type scope() :: baseObject | singleLevel | wholeSubtree.

-record(eldap_search,
	{scope = wholeSubtree              :: scope(),
         base = <<"">>                     :: binary(),
         filter                            :: eldap:filter() | undefined,
         limit = 0                         :: non_neg_integer(),
	 attributes = []                   :: [binary()],
         types_only = false                :: boolean(),
	 deref_aliases = neverDerefAliases :: neverDerefAliases |
                                              derefInSearching |
                                              derefFindingBaseObj |
                                              derefAlways,
         timeout = 0                       :: non_neg_integer()}).

-record(eldap_search_result, {entries = []   :: [eldap_entry()],
                              referrals = [] :: list()}).

-record(eldap_entry, {object_name = <<>> :: binary(),
                      attributes = []    :: [{binary(), [binary()]}]}).

-type tlsopts() :: [{encrypt, tls | starttls | none} |
		    {tls_certfile, binary() | undefined} |
                    {tls_cacertfile, binary() | undefined} |
                    {tls_depth, non_neg_integer() | undefined} |
                    {tls_verify, hard | soft | false}].

-record(eldap_config, {servers = [] :: [binary()],
                       backups = [] :: [binary()],
                       tls_options = [] :: tlsopts(),
                       port = ?LDAP_PORT :: inet:port_number(),
                       dn = <<"">> :: binary(),
                       password = <<"">> :: binary(),
                       base = <<"">> :: binary(),
                       deref_aliases = never :: never | searching |
                                                finding | always}).

-type eldap_config() :: #eldap_config{}.
-type eldap_search() :: #eldap_search{}.
-type eldap_entry() :: #eldap_entry{}.

-define(eldap_config(M, H),
	#eldap_config{
	   servers = M:ldap_servers(H),
	   backups = M:ldap_backups(H),
	   tls_options = [{encrypt, M:ldap_encrypt(H)},
			  {tls_verify, M:ldap_tls_verify(H)},
			  {tls_certfile, M:ldap_tls_certfile(H)},
			  {tls_cacertfile, M:ldap_tls_cacertfile(H)},
			  {tls_depth, M:ldap_tls_depth(H)}],
	   port = M:ldap_port(H),
	   dn = M:ldap_rootdn(H),
	   password = M:ldap_password(H),
	   base = M:ldap_base(H),
	   deref_aliases = M:ldap_deref_aliases(H)}).
