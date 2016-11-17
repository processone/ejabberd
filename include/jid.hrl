%%%-------------------------------------------------------------------
%%% @author Evgeny Khramtsov <ekhramtsov@process-one.net>
%%% @copyright (C) 2016, Evgeny Khramtsov
%%% @doc
%%%
%%% @end
%%% Created : 10 Jul 2016 by Evgeny Khramtsov <ekhramtsov@process-one.net>
%%%-------------------------------------------------------------------
-record(jid, {user = <<"">> :: binary(),
              server = <<"">> :: binary(),
              resource = <<"">> :: binary(),
              luser = <<"">> :: binary(),
              lserver = <<"">> :: binary(),
              lresource = <<"">> :: binary()}).

-type(jid() :: #jid{}).
-type(ljid() :: {binary(), binary(), binary()}).
