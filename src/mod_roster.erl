%%%----------------------------------------------------------------------
%%% File    : mod_roster.erl
%%% Author  : Alexey Shchepin <alexey@sevcom.net>
%%% Purpose : 
%%% Created : 11 Dec 2002 by Alexey Shchepin <alexey@sevcom.net>
%%% Id      : $Id$
%%%----------------------------------------------------------------------

-module(mod_roster).
-author('alexey@sevcom.net').
-vsn('$Revision$ ').

-export([]).

-export([start/0]).

-record(roster, {user, jid, name, subscription, groups, xs}).


start() ->
    mnesia:create_table(roster,[{disc_copies, [node()]},
				{type, bag},
				{attributes, record_info(fields, roster)}]),
    ejabberd_local:register_iq_handler("jabber:iq:roster",
				       ?MODULE, process_iq).
    %spawn(mod_roster, init, []).

%init() ->
%    loop().
%
%loop() ->
%    receive
%	_ ->
%	    loop()
%    end.


% TODO
process_iq(From, To, {iq, ID, Type, XMLNS, SubEl}) ->
    case Type of
	set ->
	    {iq, ID, error, XMLNS, []};
	get ->
	    {iq, ID, error, XMLNS, []}
    end.


