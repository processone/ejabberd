%%%----------------------------------------------------------------------
%%% File    : mod_adhoc.erl
%%% Author  : Magnus Henoch <henoch@dtek.chalmers.se>
%%% Purpose : Handle incoming ad-doc requests (XEP-0050)
%%% Created : 15 Nov 2005 by Magnus Henoch <henoch@dtek.chalmers.se>
%%%
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

-module(mod_adhoc).

-author('henoch@dtek.chalmers.se').

-protocol({xep, 50, '1.3.0', '1.1.0', "complete", ""}).

-behaviour(gen_mod).

-export([start/2, stop/1, reload/3, process_local_iq/1,
	 process_sm_iq/1, get_local_commands/5,
	 get_local_identity/5, get_local_features/5,
	 get_sm_commands/5, get_sm_identity/5, get_sm_features/5,
	 ping_item/4, ping_command/4, mod_opt_type/1, depends/2,
	 mod_options/1, mod_doc/0]).

-include("logger.hrl").
-include_lib("xmpp/include/xmpp.hrl").
-include("translate.hrl").

start(_Host, _Opts) ->
    {ok, [{iq_handler, ejabberd_local, ?NS_COMMANDS, process_local_iq},
          {iq_handler, ejabberd_sm, ?NS_COMMANDS, process_sm_iq},
          {hook, disco_local_identity, get_local_identity, 99},
          {hook, disco_local_features, get_local_features, 99},
          {hook, disco_local_items, get_local_commands, 99},
          {hook, disco_sm_identity, get_sm_identity, 99},
          {hook, disco_sm_features, get_sm_features, 99},
          {hook, disco_sm_items, get_sm_commands, 99},
          {hook, adhoc_local_items, ping_item, 100},
          {hook, adhoc_local_commands, ping_command, 100}]}.

stop(_Host) ->
    ok.

reload(_Host, _NewOpts, _OldOpts) ->
    ok.

%-------------------------------------------------------------------------
-spec get_local_commands(mod_disco:items_acc(), jid(), jid(), binary(), binary()) -> mod_disco:items_acc().
get_local_commands(Acc, _From,
		   #jid{server = Server, lserver = LServer} = _To, <<"">>,
		   Lang) ->
    Display = mod_adhoc_opt:report_commands_node(LServer),
    case Display of
      false -> Acc;
      _ ->
	  Items = case Acc of
		    {result, I} -> I;
		    _ -> []
		  end,
	  Nodes = [#disco_item{jid = jid:make(Server),
			       node = ?NS_COMMANDS,
			       name = translate:translate(Lang, ?T("Commands"))}],
	  {result, Items ++ Nodes}
    end;
get_local_commands(_Acc, From,
		   #jid{lserver = LServer} = To, ?NS_COMMANDS, Lang) ->
    ejabberd_hooks:run_fold(adhoc_local_items, LServer,
			    {result, []}, [From, To, Lang]);
get_local_commands(_Acc, _From, _To, <<"ping">>,
		   _Lang) ->
    {result, []};
get_local_commands(Acc, _From, _To, _Node, _Lang) ->
    Acc.

%-------------------------------------------------------------------------
-spec get_sm_commands(mod_disco:items_acc(), jid(), jid(), binary(), binary()) -> mod_disco:items_acc().
get_sm_commands(Acc, _From,
		#jid{lserver = LServer} = To, <<"">>, Lang) ->
    Display = mod_adhoc_opt:report_commands_node(LServer),
    case Display of
      false -> Acc;
      _ ->
	  Items = case Acc of
		    {result, I} -> I;
		    _ -> []
		  end,
	  Nodes = [#disco_item{jid = To,
			       node = ?NS_COMMANDS,
			       name = translate:translate(Lang, ?T("Commands"))}],
	  {result, Items ++ Nodes}
    end;
get_sm_commands(_Acc, From,
		#jid{lserver = LServer} = To, ?NS_COMMANDS, Lang) ->
    ejabberd_hooks:run_fold(adhoc_sm_items, LServer,
			    {result, []}, [From, To, Lang]);
get_sm_commands(Acc, _From, _To, _Node, _Lang) -> Acc.

%-------------------------------------------------------------------------
-spec get_local_identity([identity()], jid(), jid(), binary(), binary()) -> [identity()].
%% On disco info request to the ad-hoc node, return automation/command-list.
get_local_identity(Acc, _From, _To, ?NS_COMMANDS,
		   Lang) ->
    [#identity{category = <<"automation">>,
	       type = <<"command-list">>,
	       name = translate:translate(Lang, ?T("Commands"))}
     | Acc];
get_local_identity(Acc, _From, _To, <<"ping">>, Lang) ->
    [#identity{category = <<"automation">>,
	       type = <<"command-node">>,
	       name = translate:translate(Lang, ?T("Ping"))}
     | Acc];
get_local_identity(Acc, _From, _To, _Node, _Lang) ->
    Acc.

%-------------------------------------------------------------------------
-spec get_sm_identity([identity()], jid(), jid(), binary(), binary()) -> [identity()].
%% On disco info request to the ad-hoc node, return automation/command-list.
get_sm_identity(Acc, _From, _To, ?NS_COMMANDS, Lang) ->
    [#identity{category = <<"automation">>,
	       type = <<"command-list">>,
	       name = translate:translate(Lang, ?T("Commands"))}
     | Acc];
get_sm_identity(Acc, _From, _To, _Node, _Lang) -> Acc.

%-------------------------------------------------------------------------
-spec get_local_features(mod_disco:features_acc(), jid(), jid(), binary(), binary()) -> mod_disco:features_acc().
get_local_features(Acc, _From, _To, <<"">>, _Lang) ->
    Feats = case Acc of
	      {result, I} -> I;
	      _ -> []
	    end,
    {result, Feats ++ [?NS_COMMANDS]};
get_local_features(_Acc, _From, _To, ?NS_COMMANDS,
		   _Lang) ->
    {result, []};
get_local_features(_Acc, _From, _To, <<"ping">>,
		   _Lang) ->
    {result, [?NS_COMMANDS]};
get_local_features(Acc, _From, _To, _Node, _Lang) ->
    Acc.

%-------------------------------------------------------------------------
-spec get_sm_features(mod_disco:features_acc(), jid(), jid(), binary(), binary()) -> mod_disco:features_acc().
get_sm_features(Acc, _From, _To, <<"">>, _Lang) ->
    Feats = case Acc of
	      {result, I} -> I;
	      _ -> []
	    end,
    {result, Feats ++ [?NS_COMMANDS]};
get_sm_features(_Acc, _From, _To, ?NS_COMMANDS,
		_Lang) ->
    {result, []};
get_sm_features(Acc, _From, _To, _Node, _Lang) -> Acc.

%-------------------------------------------------------------------------
-spec process_local_iq(iq()) -> iq() | ignore.
process_local_iq(IQ) ->
    process_adhoc_request(IQ, local).

-spec process_sm_iq(iq()) -> iq() | ignore.
process_sm_iq(IQ) ->
    process_adhoc_request(IQ, sm).

-spec process_adhoc_request(iq(), sm | local) -> iq() | ignore.
process_adhoc_request(#iq{from = From, to = To,
			  type = set, lang = Lang,
			  sub_els = [#adhoc_command{} = SubEl]} = IQ, Type) ->
    Host = To#jid.lserver,
    Res = case Type of
	      local ->
		  ejabberd_hooks:run_fold(adhoc_local_commands, Host, empty,
					  [From, To, fix_lang(Lang, SubEl)]);
	      sm ->
		  ejabberd_hooks:run_fold(adhoc_sm_commands, Host, empty,
					  [From, To, fix_lang(Lang, SubEl)])
	  end,
    case Res of
	ignore ->
	    ignore;
	empty ->
	    Txt = ?T("No hook has processed this command"),
	    xmpp:make_error(IQ, xmpp:err_item_not_found(Txt, Lang));
	{error, Error} ->
	    xmpp:make_error(IQ, Error);
	Command ->
	    xmpp:make_iq_result(IQ, Command)
    end;
process_adhoc_request(#iq{} = IQ, _Hooks) ->
    xmpp:make_error(IQ, xmpp:err_bad_request()).

-spec ping_item(mod_disco:items_acc(), jid(), jid(), binary()) -> {result, [disco_item()]}.
ping_item(Acc, _From, #jid{server = Server} = _To,
	  Lang) ->
    Items = case Acc of
	      {result, I} -> I;
	      _ -> []
	    end,
    Nodes = [#disco_item{jid = jid:make(Server),
			 node = <<"ping">>,
			 name = translate:translate(Lang, ?T("Ping"))}],
    {result, Items ++ Nodes}.

-spec ping_command(adhoc_command(), jid(), jid(), adhoc_command()) ->
			  adhoc_command() | {error, stanza_error()}.
ping_command(_Acc, _From, _To,
	     #adhoc_command{lang = Lang, node = <<"ping">>,
			    action = Action} = Request) ->
    if Action == execute ->
	    xmpp_util:make_adhoc_response(
	      Request,
	      #adhoc_command{
		 status = completed,
		 notes = [#adhoc_note{
			     type = info,
			     data = translate:translate(Lang, ?T("Pong"))}]});
       true ->
	    Txt = ?T("Incorrect value of 'action' attribute"),
	    {error, xmpp:err_bad_request(Txt, Lang)}
    end;
ping_command(Acc, _From, _To, _Request) -> Acc.

-spec fix_lang(binary(), adhoc_command()) -> adhoc_command().
fix_lang(Lang, #adhoc_command{lang = <<>>} = Cmd) ->
    Cmd#adhoc_command{lang = Lang};
fix_lang(_, Cmd) ->
    Cmd.

depends(_Host, _Opts) ->
    [].

mod_opt_type(report_commands_node) ->
    econf:bool().

mod_options(_Host) ->
    [{report_commands_node, false}].

mod_doc() ->
    #{desc =>
          [?T("def:ad-hoc command"), "",
           ?T(": Command that can be executed by an XMPP client using XEP-0050."), "",
           ?T("This module implements https://xmpp.org/extensions/xep-0050.html"
             "[XEP-0050: Ad-Hoc Commands]. It's an auxiliary module and is "
             "only needed by some of the other modules.")],
      opts =>
          [{report_commands_node,
            #{value => "true | false",
              desc =>
                  ?T("Provide the Commands item in the Service Discovery. "
		     "Default value: 'false'.")}}]}.
