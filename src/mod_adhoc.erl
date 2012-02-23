%%%----------------------------------------------------------------------
%%% File    : mod_adhoc.erl
%%% Author  : Magnus Henoch <henoch@dtek.chalmers.se>
%%% Purpose : Handle incoming ad-doc requests (XEP-0050)
%%% Created : 15 Nov 2005 by Magnus Henoch <henoch@dtek.chalmers.se>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2012   ProcessOne
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
%%% You should have received a copy of the GNU General Public License
%%% along with this program; if not, write to the Free Software
%%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
%%% 02111-1307 USA
%%%
%%%----------------------------------------------------------------------

-module(mod_adhoc).
-author('henoch@dtek.chalmers.se').

-behaviour(gen_mod).

-export([start/2,
	 stop/1,
	 process_local_iq/3,
	 process_sm_iq/3,
	 get_local_commands/5,
	 get_local_identity/5,
	 get_local_features/5,
	 get_sm_commands/5,
	 get_sm_identity/5,
	 get_sm_features/5,
	 ping_item/4,
	 ping_command/4]).

-include_lib("exmpp/include/exmpp.hrl").

-include("ejabberd.hrl").
-include("adhoc.hrl").

start(Host, Opts) when is_list(Host) ->
    start(list_to_binary(Host), Opts);
start(HostB, Opts) ->
    IQDisc = gen_mod:get_opt(iqdisc, Opts, one_queue),

    gen_iq_handler:add_iq_handler(ejabberd_local, HostB, ?NS_ADHOC,
				  ?MODULE, process_local_iq, IQDisc),
    gen_iq_handler:add_iq_handler(ejabberd_sm, HostB, ?NS_ADHOC,
				  ?MODULE, process_sm_iq, IQDisc),
    
    ejabberd_hooks:add(disco_local_identity, HostB, ?MODULE, get_local_identity, 99),
    ejabberd_hooks:add(disco_local_features, HostB, ?MODULE, get_local_features, 99),
    ejabberd_hooks:add(disco_local_items, HostB, ?MODULE, get_local_commands, 99),
    ejabberd_hooks:add(disco_sm_identity, HostB, ?MODULE, get_sm_identity, 99),
    ejabberd_hooks:add(disco_sm_features, HostB, ?MODULE, get_sm_features, 99),
    ejabberd_hooks:add(disco_sm_items, HostB, ?MODULE, get_sm_commands, 99),
    ejabberd_hooks:add(adhoc_local_items, HostB, ?MODULE, ping_item, 100),
    ejabberd_hooks:add(adhoc_local_commands, HostB, ?MODULE, ping_command, 100).

stop(Host) ->
    HostB = list_to_binary(Host),
    ejabberd_hooks:delete(adhoc_local_commands, HostB, ?MODULE, ping_command, 100),
    ejabberd_hooks:delete(adhoc_local_items, HostB, ?MODULE, ping_item, 100),
    ejabberd_hooks:delete(disco_sm_items, HostB, ?MODULE, get_sm_commands, 99),
    ejabberd_hooks:delete(disco_sm_features, HostB, ?MODULE, get_sm_features, 99),
    ejabberd_hooks:delete(disco_sm_identity, HostB, ?MODULE, get_sm_identity, 99),
    ejabberd_hooks:delete(disco_local_items, HostB, ?MODULE, get_local_commands, 99),
    ejabberd_hooks:delete(disco_local_features, HostB, ?MODULE, get_local_features, 99),
    ejabberd_hooks:delete(disco_local_identity, HostB, ?MODULE, get_local_identity, 99),

    gen_iq_handler:remove_iq_handler(ejabberd_sm, HostB, ?NS_ADHOC),
    gen_iq_handler:remove_iq_handler(ejabberd_local, HostB, ?NS_ADHOC).

%-------------------------------------------------------------------------

get_local_commands(Acc, _From, To, <<>>, Lang) ->
    Server = exmpp_jid:domain(To),
    LServer = exmpp_jid:prep_domain_as_list(To),
    Display = gen_mod:get_module_opt(LServer, ?MODULE, report_commands_node, false),
    case Display of
	false ->
	    Acc;
	_ ->
	    Items = case Acc of
			{result, I} -> I;
			_ -> []
		    end,
	    Nodes = [#xmlel{ns = ?NS_DISCO_ITEMS,
		      name = 'item', attrs =
		      [?XMLATTR(<<"jid">>, Server),
		       ?XMLATTR(<<"node">>, ?NS_ADHOC_s),
                       ?XMLATTR(<<"name">>, translate:translate(Lang, "Commands"))]
		      }],
	    {result, Items ++ Nodes}
    end;

get_local_commands(_Acc, From, To, ?NS_ADHOC_b, Lang) ->
    ejabberd_hooks:run_fold(adhoc_local_items, exmpp_jid:prep_domain(To), {result, []}, [From, To, Lang]);

get_local_commands(_Acc, _From, _To, <<"ping">>, _Lang) ->
    {result, []};

get_local_commands(Acc, _From, _To, _Node, _Lang) ->
    Acc.

%-------------------------------------------------------------------------

get_sm_commands(Acc, _From, To, <<>>, Lang) ->
    LServer = exmpp_jid:prep_domain_as_list(To),
    Display = gen_mod:get_module_opt(LServer, ?MODULE, report_commands_node, false),
    case Display of
	false ->
	    Acc;
	_ ->
	    Items = case Acc of
			{result, I} -> I;
			_ -> []
		    end,
	    Nodes = [#xmlel{ns = ?NS_DISCO_ITEMS,
		      name = 'item', attrs =
		      [?XMLATTR(<<"jid">>, exmpp_jid:to_binary(To)),
		       ?XMLATTR(<<"node">>, ?NS_ADHOC_s),
		       ?XMLATTR(<<"name">>, translate:translate(Lang, "Commands"))]
		      }],
	    {result, Items ++ Nodes}
    end;

get_sm_commands(_Acc, From, To, ?NS_ADHOC_b, Lang) ->
    ejabberd_hooks:run_fold(adhoc_sm_items, exmpp_jid:prep_domain(To), {result, []}, [From, To, Lang]);

get_sm_commands(Acc, _From, _To, _Node, _Lang) ->
    Acc.

%-------------------------------------------------------------------------

%% On disco info request to the ad-hoc node, return automation/command-list.
get_local_identity(Acc, _From, _To, ?NS_ADHOC_b, Lang) ->
    [#xmlel{ns = ?NS_DISCO_INFO, name = 'identity', attrs =
      [?XMLATTR(<<"category">>, <<"automation">>),
       ?XMLATTR(<<"type">>, <<"command-list">>),
       ?XMLATTR(<<"name">>, translate:translate(Lang, "Commands"))]} | Acc];

get_local_identity(Acc, _From, _To, <<"ping">>, Lang) ->
    [#xmlel{ns = ?NS_DISCO_INFO, name = 'identity', attrs =
      [?XMLATTR(<<"category">>, <<"automation">>),
       ?XMLATTR(<<"type">>, <<"command-node">>),
       ?XMLATTR(<<"name">>, translate:translate(Lang, "Ping"))]} | Acc];

get_local_identity(Acc, _From, _To, _Node, _Lang) ->
    Acc.

%-------------------------------------------------------------------------

%% On disco info request to the ad-hoc node, return automation/command-list.
get_sm_identity(Acc, _From, _To, ?NS_ADHOC_s, Lang) ->
    [#xmlel{ns = ?NS_DISCO_INFO, name = 'identity', attrs =
      [?XMLATTR(<<"category">>, <<"automation">>),
       ?XMLATTR(<<"type">>, <<"command-list">>),
       ?XMLATTR(<<"name">>, translate:translate(Lang, "Commands"))]} | Acc];

get_sm_identity(Acc, _From, _To, _Node, _Lang) ->
    Acc.

%-------------------------------------------------------------------------

get_local_features(Acc, _From, _To, <<>>, _Lang) ->
    Feats = case Acc of
		{result, I} -> I;
		_ -> []
	    end,
    {result, Feats ++ [?NS_ADHOC_s]};

get_local_features(_Acc, _From, _To, ?NS_ADHOC_b, _Lang) ->
    %% override all lesser features...
    {result, []};

get_local_features(_Acc, _From, _To, <<"ping">>, _Lang) ->
    %% override all lesser features...
    {result, [?NS_ADHOC_s]};

get_local_features(Acc, _From, _To, _Node, _Lang) ->
    Acc.

%-------------------------------------------------------------------------

get_sm_features(Acc, _From, _To, "", _Lang) ->
    Feats = case Acc of
		{result, I} -> I;
		_ -> []
	    end,
    {result, Feats ++ [?NS_ADHOC_s]};

get_sm_features(_Acc, _From, _To, ?NS_ADHOC_s, _Lang) ->
    %% override all lesser features...
    {result, []};

get_sm_features(Acc, _From, _To, _Node, _Lang) ->
    Acc.

%-------------------------------------------------------------------------

process_local_iq(From, To, IQ_Rec) ->
    process_adhoc_request(From, To, IQ_Rec, adhoc_local_commands).


process_sm_iq(From, To, IQ_Rec) ->
    process_adhoc_request(From, To, IQ_Rec, adhoc_sm_commands).


process_adhoc_request(From, To, IQ_Rec, Hook) ->
    ?DEBUG("About to parse ~p...", [IQ_Rec]),
    case adhoc:parse_request(IQ_Rec) of
	{error, Error} ->
            exmpp_iq:error(IQ_Rec, Error);
	#adhoc_request{} = AdhocRequest ->
	    case ejabberd_hooks:run_fold(Hook, exmpp_jid:prep_domain(To), empty,
					 [From, To, AdhocRequest]) of
		ignore ->
		    ignore;
		empty ->
                    exmpp_iq:error(IQ_Rec, 'item-not-found');
		{error, Error} ->
                    exmpp_iq:error(IQ_Rec, Error);
		Command ->
                    exmpp_iq:result(IQ_Rec, Command)
	    end
    end.


ping_item(Acc, _From, To, Lang) ->
    Server = exmpp_jid:domain(To),
    Items = case Acc of
		{result, I} ->
		    I;
		_ ->
		    []
	    end,
    Nodes = [#xmlel{ns = ?NS_DISCO_INFO, name = 'item', attrs =
	      [?XMLATTR(<<"jid">>, Server),
	       ?XMLATTR(<<"node">>, <<"ping">>),
	       ?XMLATTR(<<"name">>, translate:translate(Lang, "Ping"))]}],
    {result, Items ++ Nodes}.


ping_command(_Acc, _From, _To,
	     #adhoc_request{lang = Lang,
			    node = "ping",
			    sessionid = _Sessionid,
			    action = Action} = Request) ->
    if 
	Action == ""; Action == "execute" ->
	    adhoc:produce_response(
	      Request,
	      #adhoc_response{status = completed,
			      notes = [{"info", translate:translate(
						  Lang,
						  "Pong")}]});
	true ->
	    {error, 'bad-request'}
    end;

ping_command(Acc, _From, _To, _Request) ->
    Acc.

