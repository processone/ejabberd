%%%
%%%  Copyright © Nicolas Niclausse 2005
%%%
%%%	 Author : Nicolas Niclausse <nicolas.niclausse@niclux.org>
%%%  Created: 6 Nov 2005 by Nicolas Niclausse <nicolas.niclausse@niclux.org>
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
%%%  In addition, as a special exception, you have the permission to
%%%  link the code of this program with any library released under
%%%  the EPL license and distribute linked combinations including
%%%  the two.


-vc('$Id$ ').
-author('nicolas.niclausse@niclux.org').

%% use by the client to create the request
-record(pgsql_request, {
          type,
          username,
          passwd,
          salt,
          auth_method,
          database,
          sql
         }).

%%
-record(pgsql_dyndata,
        {
          auth_method,
          username, % FIXME: add this at config time in #pgsql_request ?
          salt
         }
       ).

%% unused
-record(pgsql,
        {
          fixme
         }
       ).

%%% Version 3.0 of the protocol.
%%% Supported in postgres from version 7.4
-define(PROTOCOL_MAJOR, 3).
-define(PROTOCOL_MINOR, 0).

-define(PG_PASSWORD_MSG, $p).

-define(PG_AUTH_OK, 0).
-define(PG_AUTH_KRB4, 1).
-define(PG_AUTH_KRB5, 2).
-define(PG_AUTH_PASSWD, 3).
-define(PG_AUTH_CRYPT, 4).
-define(PG_AUTH_MD5, 5).
