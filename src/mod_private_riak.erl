%%%----------------------------------------------------------------------
%%% File    : mod_private_riak.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Private storage support
%%% Created :  6 Jan 2012 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2011   ProcessOne
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

-module(mod_private_riak).
-author('alexey@process-one.net').

-behaviour(gen_mod).

-export([start/2,
	 stop/1,
	 process_sm_iq/3,
	 remove_user/2]).

-include("ejabberd.hrl").
-include("jlib.hrl").

start(Host, Opts) ->
    IQDisc = gen_mod:get_opt(iqdisc, Opts, one_queue),
    ejabberd_hooks:add(remove_user, Host,
		       ?MODULE, remove_user, 50),
    gen_iq_handler:add_iq_handler(ejabberd_sm, Host, ?NS_PRIVATE,
				  ?MODULE, process_sm_iq, IQDisc).

stop(Host) ->
    ejabberd_hooks:delete(remove_user, Host,
			  ?MODULE, remove_user, 50),
    gen_iq_handler:remove_iq_handler(ejabberd_sm, Host, ?NS_PRIVATE).


process_sm_iq(From, _To, #iq{type = Type, sub_el = SubEl} = IQ) ->
    #jid{luser = LUser, lserver = LServer} = From,
    case lists:member(LServer, ?MYHOSTS) of
	true ->
	    {xmlelement, Name, Attrs, Els} = SubEl,
	    case Type of
		set ->
                    lists:foreach(
                      fun(El) ->
                              set_data(LUser, LServer, El)
                      end, Els),
		    IQ#iq{type = result,
			  sub_el = [{xmlelement, Name, Attrs, []}]};
		get ->
		    case catch get_data(LUser, LServer, Els) of
			{'EXIT', _Reason} ->
			    IQ#iq{type = error,
				  sub_el = [SubEl,
					    ?ERR_INTERNAL_SERVER_ERROR]};
			Res ->
			    IQ#iq{type = result,
				  sub_el = [{xmlelement, Name, Attrs, Res}]}
		    end
	    end;
	false ->
	    IQ#iq{type = error, sub_el = [SubEl, ?ERR_NOT_ALLOWED]}
    end.

set_data(LUser, LServer, El) ->
    case El of
	{xmlelement, _Name, Attrs, _Els} ->
	    XMLNS = xml:get_attr_s("xmlns", Attrs),
	    case XMLNS of
		"" ->
		    ignore;
		_ ->
                    Username = list_to_binary(LUser),
                    Key = list_to_binary([LUser, $@, LServer, $@, XMLNS]),
		    SData = xml:element_to_binary(El),
                    ejabberd_riak:put(
                      LServer, <<"private">>, Key, SData,
                      [{<<"user_bin">>, Username}]),
                    ok
	    end;
	_ ->
	    ignore
    end.

get_data(LUser, LServer, Els) ->
    get_data(LUser, LServer, Els, []).

get_data(_LUser, _LServer, [], Res) ->
    lists:reverse(Res);
get_data(LUser, LServer, [El | Els], Res) ->
    case El of
	{xmlelement, _Name, Attrs, _} ->
	    XMLNS = xml:get_attr_s("xmlns", Attrs),
            Key = list_to_binary([LUser, $@, LServer, $@, XMLNS]),
	    case ejabberd_riak:get(LServer, <<"private">>, Key) of
		{ok, SData} ->
		    case xml_stream:parse_element(SData) of
			Data when element(1, Data) == xmlelement ->
			    get_data(LUser, LServer, Els,
				     [Data | Res])
		    end;
                _ -> 
                    get_data(LUser, LServer, Els, [El | Res])
	    end;
	_ ->
	    get_data(LUser, LServer, Els, Res)
    end.


remove_user(User, Server) ->
    LUser = jlib:nodeprep(User),
    LServer = jlib:nameprep(Server),
    Username = list_to_binary(LUser),
    case ejabberd_riak:get_keys_by_index(
           LServer, <<"private">>, <<"user_bin">>, Username) of
        {ok, Keys} ->
            lists:foreach(
              fun(Key) ->
                      ejabberd_riak:delete(LServer, <<"private">>, Key)
              end, Keys);
        _ ->
            ok
    end.
