%%%  Created :  July 2008 by Grégoire Reboul <gregoire.reboul@laposte.net>
%%%  From    :  ts_pgsql.hrl by Nicolas Niclausse <nicolas.niclausse@niclux.org>
%%%  Note    :  Based on erlang-mysql by Magnus Ahltorp & Fredrik Thulin <ft@it.su.se>
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

%%% In addition, as a special exception, you have the permission to
%%% link the code of this program with any library released under
%%% the EPL license and distribute linked combinations including
%%% the two.

-author('gregoire.reboul@laposte.net').

%% use by the client to create the request
-record(mysql_request, {
          type,
          username,
          passwd,
          salt,
          database,
          sql
         }).

%%
-record(mysql_dyndata,
        {
          salt
         }
       ).

%% unused
-record(mysql,
        {
          fixme
         }
       ).

%% Support for MySQL 4.1.x et 5.0.x
-define(MYSQL_4_0, 40).
-define(MYSQL_4_1, 41).
-define(LONG_PASSWORD, 1).
-define(LONG_FLAG, 4).
-define(PROTOCOL_41, 512).
-define(TRANSACTIONS, 8192).
-define(SECURE_CONNECTION, 32768).
-define(CONNECT_WITH_DB, 8).
-define(MAX_PACKET_SIZE, 1000000).
-define(MYSQL_QUERY_OP, 3).
-define(MYSQL_CLOSE_OP, 1).
