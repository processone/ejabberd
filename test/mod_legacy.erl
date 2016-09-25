%%%-------------------------------------------------------------------
%%% @author Evgeny Khramtsov <ekhramtsov@process-one.net>
%%% @copyright (C) 2016, Evgeny Khramtsov
%%% @doc
%%%
%%% @end
%%% Created : 25 Sep 2016 by Evgeny Khramtsov <ekhramtsov@process-one.net>
%%%-------------------------------------------------------------------
-module(mod_legacy).
-behaviour(gen_mod).

%% API
-export([start/2, stop/1, process_iq/3]).
-include("jlib.hrl").

%%%===================================================================
%%% API
%%%===================================================================
start(Host, Opts) ->
    IQDisc = gen_mod:get_opt(iqdisc, Opts, fun gen_iq_handler:check_type/1,
                             one_queue),
    gen_iq_handler:add_iq_handler(ejabberd_local, Host, ?NS_EVENT,
				  ?MODULE, process_iq, IQDisc).

stop(Host) ->
    gen_iq_handler:remove_iq_handler(ejabberd_local, Host, ?MODULE).

%%%===================================================================
%%% Internal functions
%%%===================================================================
process_iq(_From, _To, IQ) ->
    IQ#iq{type = result, sub_el = []}.
