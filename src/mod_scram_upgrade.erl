%%%-------------------------------------------------------------------
%%% Created : 20 Oct 2024 by Pawel Chmielowski <pawel@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2024   ProcessOne
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
%%%-------------------------------------------------------------------
-module(mod_scram_upgrade).
-behaviour(gen_mod).
-protocol({xep, 480, '0.1', '24.10', "complete", ""}).

%% gen_mod API
-export([start/2, stop/1, reload/3, depends/2, mod_options/1, mod_opt_type/1]).
-export([mod_doc/0]).
%% Hooks
-export([c2s_inline_features/2, c2s_handle_sasl2_inline/1,
	 c2s_handle_sasl2_task_next/4, c2s_handle_sasl2_task_data/3]).

-include_lib("xmpp/include/xmpp.hrl").
-include_lib("xmpp/include/scram.hrl").
-include("logger.hrl").
-include("translate.hrl").

%%%===================================================================
%%% API
%%%===================================================================
start(_Host, _Opts) ->
    {ok, [{hook, c2s_inline_features, c2s_inline_features, 50},
	  {hook, c2s_handle_sasl2_inline, c2s_handle_sasl2_inline, 10},
	  {hook, c2s_handle_sasl2_task_next, c2s_handle_sasl2_task_next, 10},
	  {hook, c2s_handle_sasl2_task_data, c2s_handle_sasl2_task_data, 10}]}.

stop(_Host) ->
    ok.

reload(_Host, _NewOpts, _OldOpts) ->
    ok.

depends(_Host, _Opts) ->
    [].

mod_opt_type(offered_upgrades) ->
    econf:list(econf:enum([sha256, sha512])).

mod_options(_Host) ->
    [{offered_upgrades, [sha256, sha512]}].

mod_doc() ->
    #{desc =>
      [?T("The module adds support for "
	  "https://xmpp.org/extensions/xep-0480.html"
	  "[XEP-0480: SASL Upgrade Tasks] that allows users to upgrade "
	  "passwords to more secure representation.")],
      note => "added in 24.10",
      opts => [{offered_upgrades,
		#{value => "list(sha256, sha512)",
		  desc => ?T("List with upgrade types that should be offered")}}],
      example =>
      ["modules:",
       "  mod_scram_upgrade:",
       "    offered_upgrades:",
       "      - sha256",
       "      - sha512"]}.

c2s_inline_features({Sasl, Bind, Extra}, Host) ->
    Methods = lists:map(
	fun(sha256) -> #sasl_upgrade{cdata = <<"UPGR-SCRAM-SHA-256">>};
	   (sha512) -> #sasl_upgrade{cdata = <<"UPGR-SCRAM-SHA-512">>}
	end, mod_scram_upgrade_opt:offered_upgrades(Host)),
    {Sasl, Bind, Methods ++ Extra}.

c2s_handle_sasl2_inline({State, Els, _Results} = Acc) ->
    case lists:keyfind(sasl_upgrade, 1, Els) of
	false ->
	    Acc;
	#sasl_upgrade{cdata = Type} ->
	    {stop, {State, {continue, [Type]}, []}}
    end.

c2s_handle_sasl2_task_next({_, State}, Task, _Els, _InlineEls) ->
    Algo = case Task of
	       <<"UPGR-SCRAM-SHA-256">> -> sha256;
	       <<"UPGR-SCRAM-SHA-512">> -> sha512
	   end,
    Salt = p1_rand:bytes(16),
    {task_data, [#scram_upgrade_salt{cdata = Salt, iterations = 4096}],
     State#{scram_upgrade => {Algo, Salt, 4096}}}.

c2s_handle_sasl2_task_data({_, #{user := User, server := Server,
				 scram_upgrade := {Algo, Salt, Iter}} = State},
			   Els, InlineEls) ->
    case xmpp:get_subtag(#sasl2_task_data{sub_els = Els}, #scram_upgrade_hash{}) of
	#scram_upgrade_hash{data = SaltedPassword} ->
	    StoredKey = scram:stored_key(Algo, scram:client_key(Algo, SaltedPassword)),
	    ServerKey = scram:server_key(Algo, SaltedPassword),
	    ejabberd_auth:set_password(User, Server,
				       #scram{hash = Algo, iterationcount = Iter, salt = Salt,
					      serverkey = ServerKey, storedkey = StoredKey}),
	    State2 = maps:remove(scram_upgrade, State),
	    InlineEls2 = lists:keydelete(sasl_upgrade, 1, InlineEls),
	    case ejabberd_c2s:handle_sasl2_inline(InlineEls2, State2) of
		{State3, NewEls, Results} ->
		    {success, NewEls, Results, State3}
	    end;
	_ ->
	    {abort, State}
    end.
