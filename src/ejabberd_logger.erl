%%%-------------------------------------------------------------------
%%% @author Evgeniy Khramtsov <ekhramtsov@process-one.net>
%%% @copyright (C) 2012, Evgeniy Khramtsov
%%% @doc This module is needed to shut up Dialyzer
%%% @end
%%% Created : 10 Jul 2012 by Evgeniy Khramtsov <ekhramtsov@process-one.net>
%%%-------------------------------------------------------------------
-module(ejabberd_logger).

-compile({no_auto_import, [{get, 0}]}).

%% API
-export([debug_msg/4, info_msg/4, warning_msg/4,
         error_msg/4, critical_msg/4, get/0]).

%%%===================================================================
%%% API
%%%===================================================================
-spec debug_msg(atom(), pos_integer(), string(), list()) -> ok.
-spec info_msg(atom(), pos_integer(), string(), list()) -> ok.
-spec warning_msg(atom(), pos_integer(), string(), list()) -> ok.
-spec error_msg(atom(), pos_integer(), string(), list()) -> ok.
-spec critical_msg(atom(), pos_integer(), string(), list()) -> ok.
-spec get() -> {non_neg_integer(), [{atom(), non_neg_integer()}]}.

debug_msg(_Mod, _Line, _Format, _Args) -> ok.
info_msg(_Mod, _Line, _Format, _Args) -> ok.
warning_msg(_Mod, _Line, _Format, _Args) -> ok.
error_msg(_Mod, _Line, _Format, _Args) -> ok.
critical_msg(_Mod, _Line, _Format, _Args) -> ok.
get() -> {0, [{foo, 0}]}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
