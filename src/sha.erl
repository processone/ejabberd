%%%----------------------------------------------------------------------
%%% File    : sha.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : 
%%% Created : 20 Dec 2002 by Alexey Shchepin <alexey@process-one.net>
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

-module(sha).
-author('alexey@process-one.net').

-export([start/0, sha/1, sha1/1, sha224/1, sha256/1, sha384/1,
	 sha512/1]).

-ifdef(HAVE_MD2).
-export([md2/1]).
-endif.

-include("ejabberd.hrl").

-define(DRIVER, sha_drv).

start() ->
    crypto:start(),
    Res = case erl_ddll:load_driver(ejabberd:get_so_path(), ?DRIVER) of
	      ok -> ok;
	      {error, already_loaded} -> ok;
	      Err -> Err
	  end,
    case Res of
	ok ->
	    Port = open_port({spawn, atom_to_list(?DRIVER)}, [binary]),
	    register(?DRIVER, Port);
	{error, Reason} ->
	    ?CRITICAL_MSG("unable to load driver '~s': ~s",
			  [driver_path(), erl_ddll:format_error(Reason)])
    end.

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

sha1(Text) ->
    crypto:sha(Text).

sha224(Text) ->
    erlang:port_control(?DRIVER, 224, Text).

sha256(Text) ->
    erlang:port_control(?DRIVER, 256, Text).

sha384(Text) ->
    erlang:port_control(?DRIVER, 384, Text).

sha512(Text) ->
    erlang:port_control(?DRIVER, 512, Text).

-ifdef(HAVE_MD2).
md2(Text) ->
    erlang:port_control(?DRIVER, 2, Text).
-endif.

driver_path() ->
    Suffix = case os:type() of
		 {win32, _} -> ".dll";
		 _ -> ".so"
	     end,
    filename:join(ejabberd:get_so_path(), atom_to_list(?DRIVER) ++ Suffix).
