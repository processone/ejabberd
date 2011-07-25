%%%
%%%  Created: Apr 2006 by Jason Tucker <jasonwtucker@gmail.com>
%%%
%%%  Modified by Nicolas Niclausse
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

-module(ts_digest).
-author('jasonwtucker@gmail.com').

-export([ digest/2, sip_digest/4,
          md5hex/1, shahex/1, tohex/1
         ]).

%%%----------------------------------------------------------------------
%%% Func: sip_digest/4
%%%----------------------------------------------------------------------
sip_digest(Nonce, Jid, Realm, Passwd) ->
    HA1 = md5hex(Jid ++ ":" ++ Realm ++ ":" ++ Passwd),
    HA2 = md5hex("REGISTER:" ++ Jid),
    INTEGRITY = md5hex(Nonce ++ ":" ++ HA2),
    HA3 = md5hex(HA1 ++ ":" ++ INTEGRITY),
    {HA3,INTEGRITY}.

%%%----------------------------------------------------------------------
%%% Func: digest/2
%%%   Computes XMPP digest password described in JEP-0078
%%%----------------------------------------------------------------------
digest(Sid, Passwd) ->
    HA1 = shahex(Sid ++ Passwd),
    {HA1}.

%%%----------------------------------------------------------------------
%%% Func: md5hex/1
%%%----------------------------------------------------------------------
md5hex(Clear) ->
    tohex(binary_to_list(erlang:md5(Clear))).

%%%----------------------------------------------------------------------
%%% Func: shahex/1
%%%----------------------------------------------------------------------
shahex(Clear) ->
    ShaVal= case catch crypto:sha(Clear) of 
                {'EXIT',_} ->
                    crypto:start(),
                    crypto:sha(Clear);
                Sha -> Sha
            end,
    tohex(binary_to_list(ShaVal)).

%%%----------------------------------------------------------------------
%%% Func: tohex/1
%%% Purpose: convert list of integers to hexadecimal string
%%%----------------------------------------------------------------------
tohex(A)->
    Fun = fun(X)->
                  ts_utils:to_lower(padhex(httpd_util:integer_to_hexlist(X)))
          end,
    lists:flatten( lists:map(Fun, A) ).

%%%----------------------------------------------------------------------
%%% Func: padhex/1
%%% Purpose: needed because httpd_util:integer_to_hexlist returns hex
%%%    values <10 as only 1 character, ie. "0F" is simply returned as
%%%    "F". For our digest, we need these leading zeros to be present.
%%% ----------------------------------------------------------------------
padhex(S=[_Char]) -> "0" ++ S;
padhex(String) -> String.
