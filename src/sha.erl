%%%----------------------------------------------------------------------
%%% File    : sha.erl
%%% Author  : Alexey Shchepin <alexey@sevcom.net>
%%% Purpose : 
%%% Created : 20 Dec 2002 by Alexey Shchepin <alexey@sevcom.net>
%%% Id      : $Id$
%%%----------------------------------------------------------------------

-module(sha).
-author('alexey@sevcom.net').
-vsn('$Revision$ ').

-export([start/0, sha/1]).

start() ->
    crypto:start().

digit_to_xchar(D) when (D >= 0) and (D < 10) ->
    D + 48;
digit_to_xchar(D) ->
    D + 87.

sha(Text) ->
    Bin = crypto:sha(Text),
    lists:reverse(ints_to_rxstr(binary_to_list(Bin), [])).

ints_to_rxstr([], Res) ->
    Res;
ints_to_rxstr([N | Ns], Res) ->
    ints_to_rxstr(Ns, [digit_to_xchar(N rem 16),
		       digit_to_xchar(N div 16) | Res]).

