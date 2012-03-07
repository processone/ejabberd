%%%----------------------------------------------------------------------
%%% File    : idna.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Support for IDNA (RFC3490)
%%% Created : 10 Apr 2004 by Alexey Shchepin <alexey@process-one.net>
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

-module(idna).
-author('alexey@process-one.net').

%%-compile(export_all).
-export([domain_utf8_to_ascii/1,
	 domain_ucs2_to_ascii/1]).


domain_utf8_to_ascii(Domain) ->
    domain_ucs2_to_ascii(utf8_to_ucs2(Domain)).

utf8_to_ucs2(S) ->
    utf8_to_ucs2(S, "").

utf8_to_ucs2([], R) ->
    lists:reverse(R);
utf8_to_ucs2([C | S], R) when C < 16#80 ->
    utf8_to_ucs2(S, [C | R]);
utf8_to_ucs2([C1, C2 | S], R) when C1 < 16#E0 ->
    utf8_to_ucs2(S, [((C1 band 16#1F) bsl 6) bor
		     (C2 band 16#3F) | R]);
utf8_to_ucs2([C1, C2, C3 | S], R) when C1 < 16#F0 ->
    utf8_to_ucs2(S, [((C1 band 16#0F) bsl 12) bor
		     ((C2 band 16#3F) bsl 6) bor
		     (C3 band 16#3F) | R]).


domain_ucs2_to_ascii(Domain) ->
    case catch domain_ucs2_to_ascii1(Domain) of
	{'EXIT', _Reason} ->
	    false;
	Res ->
	    Res
    end.

domain_ucs2_to_ascii1(Domain) ->
    Parts = string:tokens(Domain, [16#002E, 16#3002, 16#FF0E, 16#FF61]),
    ASCIIParts = lists:map(fun(P) ->
				   to_ascii(P)
			   end, Parts),
    string:strip(lists:flatmap(fun(P) -> [$. | P] end, ASCIIParts),
		 left, $.).

%% Domain names are already nameprep'ed in ejabberd, so we skiping this step
to_ascii(Name) ->
    false = lists:any(
	      fun(C) when
		 (    0 =< C) and (C =< 16#2C) or
		 (16#2E =< C) and (C =< 16#2F) or
		 (16#3A =< C) and (C =< 16#40) or
		 (16#5B =< C) and (C =< 16#60) or
		 (16#7B =< C) and (C =< 16#7F) ->
		      true;
		 (_) ->
		      false
	      end, Name),
    case Name of
	[H | _] when H /= $- ->
	    true = lists:last(Name) /= $-
    end,
    ASCIIName = case lists:any(fun(C) -> C > 16#7F end, Name) of
		    true ->
			true = case Name of
				   "xn--" ++ _ -> false;
				   _ -> true
			       end,
			"xn--" ++ punycode_encode(Name);
		    false ->
			Name
		end,
    L = length(ASCIIName),
    true = (1 =< L) and (L =< 63),
    ASCIIName.


%%% PUNYCODE (RFC3492)

-define(BASE,         36).
-define(TMIN,         1).
-define(TMAX,         26).
-define(SKEW,         38).
-define(DAMP,         700).
-define(INITIAL_BIAS, 72).
-define(INITIAL_N,    128).

punycode_encode(Input) ->
    N = ?INITIAL_N,
    Delta = 0,
    Bias = ?INITIAL_BIAS,
    Basic = lists:filter(fun(C) -> C =< 16#7f end, Input),
    NonBasic = lists:filter(fun(C) -> C > 16#7f end, Input),
    L = length(Input),
    B = length(Basic),
    SNonBasic = lists:usort(NonBasic),
    Output1 = if
		  B > 0 -> Basic ++ "-";
		  true -> ""
	      end,
    Output2 = punycode_encode1(Input, SNonBasic, B, B, L, N, Delta, Bias, ""),
    Output1 ++ Output2.


punycode_encode1(Input, [M | SNonBasic], B, H, L, N, Delta, Bias, Out)
  when H < L ->
    Delta1 = Delta + (M - N) * (H + 1),
						% let n = m
    {NewDelta, NewBias, NewH, NewOut} =
	lists:foldl(
	  fun(C, {ADelta, ABias, AH, AOut}) ->
		  if
		      C < M ->
			  {ADelta + 1, ABias, AH, AOut};
		      C == M ->
			  NewOut = punycode_encode_delta(ADelta, ABias, AOut),
			  NewBias = adapt(ADelta, H + 1, H == B),
			  {0, NewBias, AH + 1, NewOut};
		      true ->
			  {ADelta, ABias, AH, AOut}
		  end
	  end, {Delta1, Bias, H, Out}, Input),
    punycode_encode1(
      Input, SNonBasic, B, NewH, L, M + 1, NewDelta + 1, NewBias, NewOut);

punycode_encode1(_Input, _SNonBasic, _B, _H, _L, _N, _Delta, _Bias, Out) ->
    lists:reverse(Out).


punycode_encode_delta(Delta, Bias, Out) ->
    punycode_encode_delta(Delta, Bias, Out, ?BASE).

punycode_encode_delta(Delta, Bias, Out, K) ->
    T = if
	    K =< Bias         -> ?TMIN;
	    K >= Bias + ?TMAX -> ?TMAX;
	    true              -> K - Bias
	end,
    if
	Delta < T ->
	    [codepoint(Delta) | Out];
	true ->
	    C = T + ((Delta - T) rem (?BASE - T)),
	    punycode_encode_delta((Delta - T) div (?BASE - T), Bias,
				  [codepoint(C) | Out], K + ?BASE)
    end.


adapt(Delta, NumPoints, FirstTime) ->
    Delta1 = if
		 FirstTime -> Delta div ?DAMP;
		 true -> Delta div 2
	     end,
    Delta2 = Delta1 + (Delta1 div NumPoints),
    adapt1(Delta2, 0).

adapt1(Delta, K) ->
    if
	Delta > ((?BASE - ?TMIN) * ?TMAX) div 2 ->
	    adapt1(Delta div (?BASE - ?TMIN), K + ?BASE);
	true ->
	    K + (((?BASE - ?TMIN + 1) * Delta) div (Delta + ?SKEW))
    end.


codepoint(C) ->
    if
	(0 =< C) and (C =< 25) ->
	    C + 97;
	(26 =< C) and (C =< 35) ->
	    C + 22
    end.
