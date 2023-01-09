%%%-------------------------------------------------------------------
%%% Author  : Holger Weiss <holger@zedat.fu-berlin.de>
%%% Created : 22 Apr 2020 by Holger Weiss <holger@zedat.fu-berlin.de>
%%%
%%%
%%% ejabberd, Copyright (C) 2020-2023   ProcessOne
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

-module(stundisco_tests).

%% API
-compile(export_all).
-import(suite, [send_recv/2, disconnect/1, is_feature_advertised/2,
		server_jid/1]).

-include("suite.hrl").

%%%===================================================================
%%% API
%%%===================================================================
%%%===================================================================
%%% Single user tests
%%%===================================================================
single_cases() ->
    {stundisco_single, [sequence],
     [single_test(feature_enabled),
      single_test(stun_service),
      single_test(turn_service),
      single_test(turns_service),
      single_test(turn_credentials),
      single_test(turns_credentials)]}.

feature_enabled(Config) ->
    true = is_feature_advertised(Config, ?NS_EXTDISCO_2),
    disconnect(Config).

stun_service(Config) ->
    ServerJID = server_jid(Config),
    Host = {203, 0, 113, 3},
    Port = ct:get_config(stun_port, 3478),
    Type = stun,
    Transport = udp,
    Request = #services{type = Type},
    #iq{type = result,
	sub_els = [#services{
		      type = undefined,
		      list = [#service{host = Host,
				       port = Port,
				       type = Type,
				       transport = Transport,
				       restricted = false,
				       username = <<>>,
				       password = <<>>,
				       expires = undefined,
				       action = undefined,
				       xdata = undefined}]}]} =
	send_recv(Config, #iq{type = get, to = ServerJID, sub_els = [Request]}),
    disconnect(Config).

turn_service(Config) ->
    ServerJID = server_jid(Config),
    Host = {203, 0, 113, 3},
    Port = ct:get_config(stun_port, 3478),
    Type = turn,
    Transport = udp,
    Request = #services{type = Type},
    #iq{type = result,
	sub_els = [#services{
		      type = undefined,
		      list = [#service{host = Host,
				       port = Port,
				       type = Type,
				       transport = Transport,
				       restricted = true,
				       username = Username,
				       password = Password,
				       expires = Expires,
				       action = undefined,
				       xdata = undefined}]}]} =
	send_recv(Config, #iq{type = get, to = ServerJID, sub_els = [Request]}),
    true = check_password(Username, Password),
    true = check_expires(Expires),
    disconnect(Config).

turns_service(Config) ->
    ServerJID = server_jid(Config),
    Host = <<"example.com">>,
    Port = 5349,
    Type = turns,
    Transport = tcp,
    Request = #services{type = Type},
    #iq{type = result,
	sub_els = [#services{
		      type = undefined,
		      list = [#service{host = Host,
				       port = Port,
				       type = Type,
				       transport = Transport,
				       restricted = true,
				       username = Username,
				       password = Password,
				       expires = Expires,
				       action = undefined,
				       xdata = undefined}]}]} =
	send_recv(Config, #iq{type = get, to = ServerJID, sub_els = [Request]}),
    true = check_password(Username, Password),
    true = check_expires(Expires),
    disconnect(Config).

turn_credentials(Config) ->
    ServerJID = server_jid(Config),
    Host = {203, 0, 113, 3},
    Port = ct:get_config(stun_port, 3478),
    Type = turn,
    Transport = udp,
    Request = #credentials{services = [#service{host = Host,
						port = Port,
						type = Type}]},
    #iq{type = result,
	sub_els = [#services{
		      type = undefined,
		      list = [#service{host = Host,
				       port = Port,
				       type = Type,
				       transport = Transport,
				       restricted = true,
				       username = Username,
				       password = Password,
				       expires = Expires,
				       action = undefined,
				       xdata = undefined}]}]} =
	send_recv(Config, #iq{type = get, to = ServerJID, sub_els = [Request]}),
    true = check_password(Username, Password),
    true = check_expires(Expires),
    disconnect(Config).

turns_credentials(Config) ->
    ServerJID = server_jid(Config),
    Host = <<"example.com">>,
    Port = 5349,
    Type = turns,
    Transport = tcp,
    Request = #credentials{services = [#service{host = Host,
						port = Port,
						type = Type}]},
    #iq{type = result,
	sub_els = [#services{
		      type = undefined,
		      list = [#service{host = Host,
				       port = Port,
				       type = Type,
				       transport = Transport,
				       restricted = true,
				       username = Username,
				       password = Password,
				       expires = Expires,
				       action = undefined,
				       xdata = undefined}]}]} =
	send_recv(Config, #iq{type = get, to = ServerJID, sub_els = [Request]}),
    true = check_password(Username, Password),
    true = check_expires(Expires),
    disconnect(Config).

%%%===================================================================
%%% Internal functions
%%%===================================================================
single_test(T) ->
    list_to_atom("stundisco_" ++ atom_to_list(T)).

check_password(Username, Password) ->
    Secret = <<"cryptic">>,
    Password == base64:encode(misc:crypto_hmac(sha, Secret, Username)).

check_expires({_, _, _} = Expires) ->
    Now = {MegaSecs, Secs, MicroSecs} = erlang:timestamp(),
    Later = {MegaSecs + 1, Secs, MicroSecs},
    (Expires > Now) and (Expires < Later).
