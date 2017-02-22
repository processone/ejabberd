%%%----------------------------------------------------------------------
%%% File    : mod_adhoc.erl
%%% Author  : Magnus Henoch <henoch@dtek.chalmers.se>
%%% Purpose : Handle incoming ad-doc requests (XEP-0050)
%%% Created : 15 Nov 2005 by Magnus Henoch <henoch@dtek.chalmers.se>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2017   ProcessOne
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

-protocol({xep, 50, '1.2'}).

-behaviour(gen_mod).

-export([start/2, stop/1, reload/3, process_local_iq/1,
	 process_sm_iq/1, get_local_commands/5,
	 get_local_identity/5, get_local_features/5,
	 get_sm_commands/5, get_sm_identity/5, get_sm_features/5,
	 ping_item/4, ping_command/4, mod_opt_type/1, depends/2]).

-include("ejabberd.hrl").
-include("logger.hrl").
-include("xmpp.hrl").

start(Host, Opts) ->
    IQDisc = gen_mod:get_opt(iqdisc, Opts, fun gen_iq_handler:check_type/1,
                             one_queue),
    gen_iq_handler:add_iq_handler(ejabberd_local, Host,
				  ?NS_COMMANDS, ?MODULE, process_local_iq,
				  IQDisc),
    gen_iq_handler:add_iq_handler(ejabberd_sm, Host,
				  ?NS_COMMANDS, ?MODULE, process_sm_iq, IQDisc),
    ejabberd_hooks:add(disco_local_identity, Host, ?MODULE,
		       get_local_identity, 99),
    ejabberd_hooks:add(disco_local_features, Host, ?MODULE,
		       get_local_features, 99),
    ejabberd_hooks:add(disco_local_items, Host, ?MODULE,
		       get_local_commands, 99),
    ejabberd_hooks:add(disco_sm_identity, Host, ?MODULE,
		       get_sm_identity, 99),
    ejabberd_hooks:add(disco_sm_features, Host, ?MODULE,
		       get_sm_features, 99),
    ejabberd_hooks:add(disco_sm_items, Host, ?MODULE,
		       get_sm_commands, 99),
    ejabberd_hooks:add(adhoc_local_items, Host, ?MODULE,
		       ping_item, 100),
    ejabberd_hooks:add(adhoc_local_commands, Host, ?MODULE,
		       ping_command, 100).

stop(Host) ->
    ejabberd_hooks:delete(adhoc_local_commands, Host,
			  ?MODULE, ping_command, 100),
    ejabberd_hooks:delete(adhoc_local_items, Host, ?MODULE,
			  ping_item, 100),
    ejabberd_hooks:delete(disco_sm_items, Host, ?MODULE,
			  get_sm_commands, 99),
    ejabberd_hooks:delete(disco_sm_features, Host, ?MODULE,
			  get_sm_features, 99),
    ejabberd_hooks:delete(disco_sm_identity, Host, ?MODULE,
			  get_sm_identity, 99),
    ejabberd_hooks:delete(disco_local_items, Host, ?MODULE,
			  get_local_commands, 99),
    ejabberd_hooks:delete(disco_local_features, Host,
			  ?MODULE, get_local_features, 99),
    ejabberd_hooks:delete(disco_local_identity, Host,
			  ?MODULE, get_local_identity, 99),
    gen_iq_handler:remove_iq_handler(ejabberd_sm, Host,
				     ?NS_COMMANDS),
    gen_iq_handler:remove_iq_handler(ejabberd_local, Host,
				     ?NS_COMMANDS).

reload(Host, NewOpts, OldOpts) ->
    case gen_mod:is_equal_opt(iqdisc, NewOpts, OldOpts,
			      fun gen_iq_handler:check_type/1,
			      one_queue) of
	{false, IQDisc, _} ->
	    gen_iq_handler:add_iq_handler(ejabberd_local, Host, ?NS_COMMANDS,
					  ?MODULE, process_local_iq, IQDisc),
	    gen_iq_handler:add_iq_handler(ejabberd_sm, Host, ?NS_COMMANDS,
					  ?MODULE, process_sm_iq, IQDisc);
	true ->
	    ok
    end.

%-------------------------------------------------------------------------

get_local_commands(Acc, _From,
		   #jid{server = Server, lserver = LServer} = _To, <<"">>,
		   Lang) ->
    Display = gen_mod:get_module_opt(LServer, ?MODULE,
				     report_commands_node,
                                     fun(B) when is_boolean(B) -> B end,
                                     false),
    case Display of
      false -> Acc;
      _ ->
	  Items = case Acc of
		    {result, I} -> I;
		    _ -> []
		  end,
	  Nodes = [#disco_item{jid = jid:make(Server),
			       node = ?NS_COMMANDS,
			       name = translate:translate(Lang, <<"Commands">>)}],
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

get_sm_commands(Acc, _From,
		#jid{lserver = LServer} = To, <<"">>, Lang) ->
    Display = gen_mod:get_module_opt(LServer, ?MODULE,
				     report_commands_node,
                                     fun(B) when is_boolean(B) -> B end,
                                     false),
    case Display of
      false -> Acc;
      _ ->
	  Items = case Acc of
		    {result, I} -> I;
		    _ -> []
		  end,
	  Nodes = [#disco_item{jid = To,
			       node = ?NS_COMMANDS,
			       name = translate:translate(Lang, <<"Commands">>)}],
	  {result, Items ++ Nodes}
    end;
get_sm_commands(_Acc, From,
		#jid{lserver = LServer} = To, ?NS_COMMANDS, Lang) ->
    ejabberd_hooks:run_fold(adhoc_sm_items, LServer,
			    {result, []}, [From, To, Lang]);
get_sm_commands(Acc, _From, _To, _Node, _Lang) -> Acc.

%-------------------------------------------------------------------------

%% On disco info request to the ad-hoc node, return automation/command-list.
get_local_identity(Acc, _From, _To, ?NS_COMMANDS,
		   Lang) ->
    [#identity{category = <<"automation">>,
	       type = <<"command-list">>,
	       name = translate:translate(Lang, <<"Commands">>)}
     | Acc];
get_local_identity(Acc, _From, _To, <<"ping">>, Lang) ->
    [#identity{category = <<"automation">>,
	       type = <<"command-node">>,
	       name = translate:translate(Lang, <<"Ping">>)}
     | Acc];
get_local_identity(Acc, _From, _To, _Node, _Lang) ->
    Acc.

%-------------------------------------------------------------------------

%% On disco info request to the ad-hoc node, return automation/command-list.
get_sm_identity(Acc, _From, _To, ?NS_COMMANDS, Lang) ->
    [#identity{category = <<"automation">>,
	       type = <<"command-list">>,
	       name = translate:translate(Lang, <<"Commands">>)}
     | Acc];
get_sm_identity(Acc, _From, _To, _Node, _Lang) -> Acc.

%-------------------------------------------------------------------------
-spec get_local_features({error, stanza_error()} | {result, [binary()]} | empty,
			 jid(), jid(), binary(), binary()) ->
				{error, stanza_error()} | {result, [binary()]} | empty.
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

process_local_iq(IQ) ->
    process_adhoc_request(IQ, local).

process_sm_iq(IQ) ->
    process_adhoc_request(IQ, sm).

process_adhoc_request(#iq{from = From, to = To,
			  type = set, lang = Lang,
			  sub_els = [#adhoc_command{} = SubEl]} = IQ, Type) ->
    Host = To#jid.lserver,
    Res = case Type of
	      local ->
		  ejabberd_hooks:run_fold(adhoc_local_commands, Host, empty,
					  [From, To, SubEl]);
	      sm ->
		  ejabberd_hooks:run_fold(adhoc_sm_commands, Host, empty,
					  [From, To, SubEl])
	  end,
    case Res of
	ignore ->
	    ignore;
	empty ->
	    Txt = <<"No hook has processed this command">>,
	    xmpp:make_error(IQ, xmpp:err_item_not_found(Txt, Lang));
	{error, Error} ->
	    xmpp:make_error(IQ, Error);
	Command ->
	    xmpp:make_iq_result(IQ, Command)
    end;
process_adhoc_request(#iq{} = IQ, _Hooks) ->
    xmpp:make_error(IQ, xmpp:err_bad_request()).

-spec ping_item(empty | {error, stanza_error()} | {result, [disco_item()]},
		jid(), jid(), binary()) -> {result, [disco_item()]}.
ping_item(Acc, _From, #jid{server = Server} = _To,
	  Lang) ->
    Items = case Acc of
	      {result, I} -> I;
	      _ -> []
	    end,
    Nodes = [#disco_item{jid = jid:make(Server),
			 node = <<"ping">>,
			 name = translate:translate(Lang, <<"Ping">>)}],
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
			     data = translate:translate(Lang, <<"Pong">>)}]});
       true ->
	    Txt = <<"Incorrect value of 'action' attribute">>,
	    {error, xmpp:err_bad_request(Txt, Lang)}
    end;
ping_command(Acc, _From, _To, _Request) -> Acc.

depends(_Host, _Opts) ->
    [].

mod_opt_type(iqdisc) -> fun gen_iq_handler:check_type/1;
mod_opt_type(report_commands_node) ->
    fun (B) when is_boolean(B) -> B end;
mod_opt_type(_) -> [iqdisc, report_commands_node].
