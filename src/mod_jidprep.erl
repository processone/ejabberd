%%%----------------------------------------------------------------------
%%% File    : mod_jidprep.erl
%%% Author  : Holger Weiss <holger@zedat.fu-berlin.de>
%%% Purpose : JID Prep (XEP-0328)
%%% Created : 11 Sep 2019 by Holger Weiss <holger@zedat.fu-berlin.de>
%%%
%%%
%%% ejabberd, Copyright (C) 2019-2024 ProcessOne
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

-module(mod_jidprep).
-author('holger@zedat.fu-berlin.de').
-protocol({xep, 328, '0.1', '19.09', "", ""}).

-behaviour(gen_mod).

%% gen_mod callbacks.
-export([start/2, stop/1, reload/3, mod_opt_type/1, mod_options/1, depends/2]).
-export([mod_doc/0]).

%% ejabberd_hooks callbacks.
-export([disco_local_features/5]).

%% gen_iq_handler callback.
-export([process_iq/1]).

-include("logger.hrl").
-include("translate.hrl").
-include_lib("xmpp/include/xmpp.hrl").

%%--------------------------------------------------------------------
%% gen_mod callbacks.
%%--------------------------------------------------------------------
-spec start(binary(), gen_mod:opts()) -> {ok, [gen_mod:registration()]}.
start(_Host, _Opts) ->
    {ok, [{iq_handler, ejabberd_local, ?NS_JIDPREP_0, process_iq},
          {hook, disco_local_features, disco_local_features, 50}]}.

-spec stop(binary()) -> ok.
stop(_Host) ->
    ok.

-spec reload(binary(), gen_mod:opts(), gen_mod:opts()) -> ok.
reload(_Host, _NewOpts, _OldOpts) ->
    ok.

-spec depends(binary(), gen_mod:opts()) -> [{module(), hard | soft}].
depends(_Host, _Opts) ->
    [].

-spec mod_opt_type(atom()) -> econf:validator().
mod_opt_type(access) ->
    econf:acl().

-spec mod_options(binary()) -> [{atom(), any()}].
mod_options(_Host) ->
    [{access, local}].

mod_doc() ->
    #{desc =>
          ?T("This module allows XMPP clients to ask the "
             "server to normalize a JID as per the rules specified "
             "in https://tools.ietf.org/html/rfc6122"
             "[RFC 6122: XMPP Address Format]. This might be useful "
             "for clients in certain constrained environments, "
             "or for testing purposes."),
      opts =>
          [{access,
            #{value => ?T("AccessName"),
              desc =>
                  ?T("This option defines which access rule will "
                     "be used to control who is allowed to use this "
                     "service. The default value is 'local'.")}}]}.

%%--------------------------------------------------------------------
%% Service discovery.
%%--------------------------------------------------------------------
-spec disco_local_features(mod_disco:features_acc(), jid(), jid(), binary(),
			   binary()) -> mod_disco:features_acc().
disco_local_features(empty, From, To, Node, Lang) ->
    disco_local_features({result, []}, From, To, Node, Lang);
disco_local_features({result, OtherFeatures} = Acc, From,
		     #jid{lserver = LServer}, <<"">>, _Lang) ->
    Access = mod_jidprep_opt:access(LServer),
    case acl:match_rule(LServer, Access, From) of
	allow ->
	    {result, [?NS_JIDPREP_0 | OtherFeatures]};
	deny ->
	    Acc
    end;
disco_local_features(Acc, _From, _To, _Node, _Lang) ->
    Acc.

%%--------------------------------------------------------------------
%% IQ handlers.
%%--------------------------------------------------------------------
-spec process_iq(iq()) -> iq().
process_iq(#iq{type = set, lang = Lang} = IQ) ->
    Txt = ?T("Value 'set' of 'type' attribute is not allowed"),
    xmpp:make_error(IQ, xmpp:err_not_allowed(Txt, Lang));
process_iq(#iq{from = From, to = #jid{lserver = LServer}, lang = Lang,
	       sub_els = [#jidprep{jid = #jid{luser = U,
					      lserver = S,
					      lresource = R} = JID}]} = IQ) ->
    Access = mod_jidprep_opt:access(LServer),
    case acl:match_rule(LServer, Access, From) of
	allow ->
	    case jid:make(U, S, R) of
		#jid{} = Normalized ->
		    ?DEBUG("Normalized JID for ~ts: ~ts",
			   [jid:encode(From), jid:encode(JID)]),
		    xmpp:make_iq_result(IQ, #jidprep{jid = Normalized});
		error -> % Cannot happen.
		    ?DEBUG("Normalizing JID failed for ~ts: ~ts",
			   [jid:encode(From), jid:encode(JID)]),
		    Txt = ?T("JID normalization failed"),
		    xmpp:make_error(IQ, xmpp:err_jid_malformed(Txt, Lang))
	    end;
	deny ->
	    ?DEBUG("Won't return normalized JID to ~ts: ~ts",
		   [jid:encode(From), jid:encode(JID)]),
	    Txt = ?T("JID normalization denied by service policy"),
            xmpp:make_error(IQ, xmpp:err_forbidden(Txt, Lang))
    end;
process_iq(#iq{lang = Lang} = IQ) ->
    Txt = ?T("No module is handling this query"),
    xmpp:make_error(IQ, xmpp:err_service_unavailable(Txt, Lang)).
