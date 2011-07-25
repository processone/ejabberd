%%%
%%%  Copyright © Nicolas Niclausse. 2008
%%%
%%%  Author : Nicolas Niclausse <nicolas@niclux.org>
%%%  Created: 12 mar 2008 by Nicolas Niclausse <nicolas@niclux.org>
%%%
%%%  This program is free software; you can redistribute it and/or modify
%%%  it under the terms of the GNU General Public License as published by
%%%  the Free Software Foundation; either version 2 of the License
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

-module(ts_webdav).
-vc('$Id: ts_webdav.erl,v 0.0 2008/03/12 12:47:07 nniclaus Exp $ ').
-author('nicolas.niclausse@niclux.org').

-include("ts_profile.hrl").
-include("ts_http.hrl").

-export([init_dynparams/0,
         add_dynparams/4,
         get_message/1,
         session_defaults/0,
         parse/2,
         parse_config/2,
         new_session/0]).

session_defaults() -> {ok, true}.
new_session() -> #http{}.


%% we should implement methods defined in rfc4918

get_message(Req=#http_request{method=Method}) when Method == propfind;
                                                   Method == proppatch;
                                                   Method == copy;
                                                   Method == move;
                                                   Method == lock;
                                                   Method == mkactivity;
                                                   Method == unlock;
                                                   Method == report;
                                                   Method == 'version-control'
                                                   ->
    M = string:to_upper(atom_to_list(Method)),
    ts_http_common:http_body(M, Req);
get_message(Req=#http_request{method=Method}) when Method == mkcol->
    ts_http_common:http_no_body("MKCOL", Req);
get_message(Req) ->
    ts_http:get_message(Req).

parse(Data, State) ->
    ts_http_common:parse(Data, State).
parse_config(Element, Conf) ->
    ts_config_http:parse_config(Element, Conf).

add_dynparams(Subst, DynData, Param, HostData) ->
    ts_http:add_dynparams(Subst, DynData, Param, HostData).

init_dynparams() -> ts_http:init_dynparams().

%%% methode PROPFIND; entetes: Depth (optionel); body: XML
%%% methode COPY; entete Destination: URL, If (optionel), Overwrite (Optionel), Depth; Body: XML (Optionel)
%%% methode MOVE; entete Destination: URL, If (optionel), Overwrite (Optionel), Depth; Body: XML (Optionel)
%%% methode PROPPATCH body: XML
%%% methode MKCOL
%%% methode LOCK; entete: Timeout (optionel ?), If (Optionel),Depth (Optionel); Body: XML (optionel ?)
%%% methode UNLOCK; entete: Lock-Token; Body: XML (optionel ?)
