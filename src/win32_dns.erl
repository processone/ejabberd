%%%----------------------------------------------------------------------
%%% File    : win32_dns.erl
%%% Author  : Geoff Cant
%%% Purpose : Get name servers in a Windows machine
%%% Created : 5 Mar 2009 by Geoff Cant
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

-module(win32_dns).
-export([get_nameservers/0]).

-include("ejabberd.hrl").
-include("logger.hrl").

-define(IF_KEY, "\\hklm\\system\\CurrentControlSet\\Services\\TcpIp\\Parameters\\Interfaces").
-define(TOP_KEY, "\\hklm\\system\\CurrentControlSet\\Services\\TcpIp\\Parameters").

get_nameservers() ->
    {_, Config} = pick_config(),
    IPTs = get_value(["NameServer"], Config),
    lists:filter(fun(IPTuple) -> is_good_ns(IPTuple) end, IPTs).

is_good_ns(Addr) ->
    element(1,
	    inet_res:nnslookup("a.root-servers.net", in, any, [{Addr,53}],
			       timer:seconds(5)
			      )
	   ) =:= ok.

reg() ->
    {ok, R} = win32reg:open([read]),
    R.

interfaces(R) ->
    ok = win32reg:change_key(R, ?IF_KEY),
    {ok, I} = win32reg:sub_keys(R),
    I.
config_keys(R, Key) ->
    ok = win32reg:change_key(R, Key),
    [ {K,
       case win32reg:value(R, K) of
           {ok, V} -> try_translate(K, V);
           _ -> undefined
       end
      } || K <- ["Domain", "DhcpDomain",
                 "NameServer", "DhcpNameServer", "SearchList"]].

try_translate(K, V) ->
    try translate(K, V) of
	Res ->
	    Res
    catch
	A:B ->
	    ?ERROR_MSG("Error '~p' translating Win32 registry~n"
		       "K: ~p~nV: ~p~nError: ~p", [A, K, V, B]),
	    undefined
    end.

translate(NS, V) when NS =:= "NameServer"; NS =:= "DhcpNameServer" ->
    %% The IPs may be separated by commas ',' or by spaces " "
    %% The parts of an IP are separated by dots '.'
    IPsStrings = [string:tokens(IP, ".") || IP <- string:tokens(V, " ,")],
    [ list_to_tuple([list_to_integer(String) || String <- IpStrings])
      || IpStrings <- IPsStrings];
translate(_, V) -> V.

interface_configs(R) ->
    [{If, config_keys(R, ?IF_KEY ++ "\\" ++ If)}
     || If <- interfaces(R)].

sort_configs(Configs) ->
    lists:sort(fun ({_, A}, {_, B}) ->
                       ANS = proplists:get_value("NameServer", A),
                       BNS = proplists:get_value("NameServer", B),
                       if ANS =/= undefined, BNS =:= undefined -> false;
                          true -> count_undef(A) < count_undef(B)
                       end
               end,
	       Configs).

count_undef(L) when is_list(L) ->
    lists:foldl(fun ({_K, undefined}, Acc) -> Acc +1;
                    ({_K, []}, Acc) -> Acc +1;
                    (_, Acc) -> Acc
                end, 0, L).

all_configs() ->
    R = reg(),
    TopConfig = config_keys(R, ?TOP_KEY),
    Configs = [{top, TopConfig}
               | interface_configs(R)],
    win32reg:close(R),
    {TopConfig, Configs}.

pick_config() ->
    {TopConfig, Configs} = all_configs(),
    NSConfigs = [{If, C} || {If, C} <- Configs,
			    get_value(["DhcpNameServer","NameServer"], C)
				=/= undefined],
    case get_value(["DhcpNameServer","NameServer"],
                   TopConfig) of
        %% No top level nameserver to pick interface with
        undefined ->
            hd(sort_configs(NSConfigs));
        %% Top level has a nameserver - use this to select an interface.
        NS ->
            Cs = [ {If, C}
                   || {If, C} <- Configs,
		      lists:member(NS,
				   [get_value(["NameServer"], C),
				    get_value(["DhcpNameServer"], C)])],
            hd(sort_configs(Cs))
    end.

get_value([], _Config) -> undefined;
get_value([K|Keys], Config) ->
    case proplists:get_value(K, Config) of
        undefined -> get_value(Keys, Config);
        V -> V
    end.
