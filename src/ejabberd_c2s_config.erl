%%%----------------------------------------------------------------------
%%% File    : ejabberd_c2s_config.erl
%%% Author  : Mickael Remond <mremond@process-one.net>
%%% Purpose : Functions for c2s interactions from other client
%%%           connector modules
%%% Created :  2 Nov 2007 by Mickael Remond <mremond@process-one.net>
%%% Id      : $Id$
%%%----------------------------------------------------------------------

-module(ejabberd_c2s_config).
-author('mremond@process-one.net').
-svn('$Revision$ ').

-export([get_c2s_limits/0]).

%% Get first c2s configuration limitations to apply it to other c2s
%% connectors.
get_c2s_limits() ->
    case ejabberd_config:get_local_option(listen) of
	undefined ->
	    [];
	C2SFirstListen ->
	    case lists:keysearch(ejabberd_c2s, 2, C2SFirstListen) of
		false ->
		    [];
		{value, {_Port, ejabberd_c2s, Opts}} ->
		    select_opts_values(Opts)
	    end
    end.
%% Only get access, shaper and max_stanza_size values
select_opts_values(Opts) ->
    select_opts_values(Opts, []).
select_opts_values([], SelectedValues) ->
    SelectedValues;
select_opts_values([{access,Value}|Opts], SelectedValues) ->
    select_opts_values(Opts, [{access, Value}|SelectedValues]);
select_opts_values([{shaper,Value}|Opts], SelectedValues) ->
    select_opts_values(Opts, [{shaper, Value}|SelectedValues]);
select_opts_values([{max_stanza_size,Value}|Opts], SelectedValues) ->
    select_opts_values(Opts, [{max_stanza_size, Value}|SelectedValues]);
select_opts_values([_Opt|Opts], SelectedValues) ->
    select_opts_values(Opts, SelectedValues).

