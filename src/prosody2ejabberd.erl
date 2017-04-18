%%%-------------------------------------------------------------------
%%% File    : prosody2ejabberd.erl
%%% Author  : Evgeny Khramtsov <ekhramtsov@process-one.net>
%%% Created : 20 Jan 2016 by Evgeny Khramtsov <ekhramtsov@process-one.net>
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

-module(prosody2ejabberd).

%% API
-export([from_dir/1]).

-include("ejabberd.hrl").
-include("xmpp.hrl").
-include("logger.hrl").
-include("mod_roster.hrl").
-include("mod_offline.hrl").
-include("mod_privacy.hrl").

%%%===================================================================
%%% API
%%%===================================================================
from_dir(ProsodyDir) ->
    case file:list_dir(ProsodyDir) of
	{ok, HostDirs} ->
	    lists:foreach(
	      fun(HostDir) ->
		      Host = list_to_binary(HostDir),
		      lists:foreach(
			fun(SubDir) ->
				Path = filename:join(
					 [ProsodyDir, HostDir, SubDir]),
				convert_dir(Path, Host, SubDir)
			end, ["vcard", "accounts", "roster",
			      "private", "config", "offline",
			      "privacy"])
	      end, HostDirs);
	{error, Why} = Err ->
	    ?ERROR_MSG("failed to list ~s: ~s",
		       [ProsodyDir, file:format_error(Why)]),
	    Err
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
convert_dir(Path, Host, Type) ->
    case file:list_dir(Path) of
	{ok, Files} ->
	    lists:foreach(
	      fun(File) ->
		      FilePath = filename:join(Path, File),
		      case eval_file(FilePath) of
			  {ok, Data} ->
			      Name = iolist_to_binary(filename:rootname(File)),
			      convert_data(Host, Type, Name, Data);
			  Err ->
			      Err
		      end
	      end, Files);
	{error, enoent} ->
	    ok;
	{error, Why} = Err ->
	    ?ERROR_MSG("failed to list ~s: ~s",
		       [Path, file:format_error(Why)]),
	    Err
    end.

eval_file(Path) ->
    case file:read_file(Path) of
	{ok, Data} ->
	    State0 = luerl:init(),
	    State1 = luerl:set_table([item],
				     fun([X], State) -> {[X], State} end,
				     State0),
	    NewData = case filename:extension(Path) of
			  ".list" ->
			      <<"return {", Data/binary, "};">>;
			  _ ->
			      Data
		      end,
	    case luerl:eval(NewData, State1) of
		{ok, _} = Res ->
		    Res;
		{error, Why} = Err ->
		    ?ERROR_MSG("failed to eval ~s: ~p", [Path, Why]),
		    Err
	    end;
	{error, Why} = Err ->
	    ?ERROR_MSG("failed to read file ~s: ~s",
		       [Path, file:format_error(Why)]),
	    Err
    end.

maybe_get_scram_auth(Data) ->
    case proplists:get_value(<<"iteration_count">>, Data, no_ic) of
	IC when is_float(IC) -> %% A float like 4096.0 is read
	    #scram{
		storedkey = misc:hex_to_base64(proplists:get_value(<<"stored_key">>, Data, <<"">>)),
		serverkey = misc:hex_to_base64(proplists:get_value(<<"server_key">>, Data, <<"">>)),
		salt = misc:hex_to_base64(proplists:get_value(<<"salt">>, Data, <<"">>)),
		iterationcount = round(IC)
	    };
	_ -> <<"">>
    end.

convert_data(Host, "accounts", User, [Data]) ->
    Password = case proplists:get_value(<<"password">>, Data, no_pass) of
	no_pass ->
	    maybe_get_scram_auth(Data);
	Pass when is_binary(Pass) ->
	    Pass
    end,
    case ejabberd_auth:try_register(User, Host, Password) of
	{atomic, ok} ->
	    ok;
	Err ->
	    ?ERROR_MSG("failed to register user ~s@~s: ~p",
		       [User, Host, Err]),
	    Err
    end;
convert_data(Host, "roster", User, [Data]) ->
    LUser = jid:nodeprep(User),
    LServer = jid:nameprep(Host),
    Rosters =
	lists:flatmap(
	  fun({<<"pending">>, L}) ->
		  convert_pending_item(LUser, LServer, L);
	     ({S, L}) when is_binary(S) ->
		  convert_roster_item(LUser, LServer, S, L);
	     (_) ->
		  []
	  end, Data),
    lists:foreach(fun mod_roster:set_roster/1, Rosters);
convert_data(Host, "private", User, [Data]) ->
    LUser = jid:nodeprep(User),
    LServer = jid:nameprep(Host),
    PrivData = lists:flatmap(
		 fun({_TagXMLNS, Raw}) ->
			 case deserialize(Raw) of
			     [El] ->
				 XMLNS = fxml:get_tag_attr_s(<<"xmlns">>, El),
				 [{XMLNS, El}];
			     _ ->
				 []
			 end
		 end, Data),
    mod_private:set_data(LUser, LServer, PrivData);
convert_data(Host, "vcard", User, [Data]) ->
    LServer = jid:nameprep(Host),
    case deserialize(Data) of
	[VCard] ->
	    mod_vcard:set_vcard(User, LServer, VCard);
	_ ->
	    ok
    end;
convert_data(_Host, "config", _User, [Data]) ->
    RoomJID = jid:decode(proplists:get_value(<<"jid">>, Data, <<"">>)),
    Config = proplists:get_value(<<"_data">>, Data, []),
    RoomCfg = convert_room_config(Data),
    case proplists:get_bool(<<"persistent">>, Config) of
	true when RoomJID /= error ->
	    mod_muc:store_room(?MYNAME, RoomJID#jid.lserver,
			       RoomJID#jid.luser, RoomCfg);
	_ ->
	    ok
    end;
convert_data(Host, "offline", User, [Data]) ->
    LUser = jid:nodeprep(User),
    LServer = jid:nameprep(Host),
    Msgs = lists:flatmap(
	     fun({_, RawXML}) ->
		     case deserialize(RawXML) of
			 [El] -> el_to_offline_msg(LUser, LServer, El);
			 _ -> []
		     end
	     end, Data),
    mod_offline:store_offline_msg(
      LServer, {LUser, LServer}, Msgs, length(Msgs), infinity);
convert_data(Host, "privacy", User, [Data]) ->
    LUser = jid:nodeprep(User),
    LServer = jid:nameprep(Host),
    Lists = proplists:get_value(<<"lists">>, Data, []),
    Priv = #privacy{
	      us = {LUser, LServer},
	      default = proplists:get_value(<<"default">>, Data, none),
	      lists = lists:flatmap(
			fun({Name, Vals}) ->
				Items = proplists:get_value(<<"items">>, Vals, []),
				case lists:map(fun convert_privacy_item/1,
					       Items) of
				    [] -> [];
				    ListItems -> [{Name, ListItems}]
				end
			end, Lists)},
    mod_privacy:set_privacy_list(Priv);
convert_data(_Host, _Type, _User, _Data) ->
    ok.

convert_pending_item(LUser, LServer, LuaList) ->
    lists:flatmap(
      fun({S, true}) ->
	      try jid:decode(S) of
		  J ->
		      LJID = jid:tolower(J),
		      [#roster{usj = {LUser, LServer, LJID},
			       us = {LUser, LServer},
			       jid = LJID,
			       ask = in}]
	      catch _:{bad_jid, _} ->
		      []
	      end;
	 (_) ->
	      []
      end, LuaList).

convert_roster_item(LUser, LServer, JIDstring, LuaList) ->
    try jid:decode(JIDstring) of
	JID ->
	    LJID = jid:tolower(JID),
	    InitR = #roster{usj = {LUser, LServer, LJID},
			    us = {LUser, LServer},
			    jid = LJID},
	    Roster =
		lists:foldl(
		  fun({<<"groups">>, Val}, R) ->
			  Gs = lists:flatmap(
				 fun({G, true}) -> [G];
				    (_) -> []
				 end, Val),
			  R#roster{groups = Gs};
		     ({<<"subscription">>, Sub}, R) ->
			  R#roster{subscription = misc:binary_to_atom(Sub)};
		     ({<<"ask">>, <<"subscribe">>}, R) ->
			  R#roster{ask = out};
		     ({<<"name">>, Name}, R) ->
			  R#roster{name = Name}
		  end, InitR, LuaList),
	    [Roster]
    catch _:{bad_jid, _} ->
	    []
    end.

convert_room_affiliations(Data) ->
    lists:flatmap(
      fun({J, Aff}) ->
	      try jid:decode(J) of
		  #jid{luser = U, lserver = S} ->
		      [{{U, S, <<>>}, misc:binary_to_atom(Aff)}]
	      catch _:{bad_jid, _} ->
		      []
	      end
      end, proplists:get_value(<<"_affiliations">>, Data, [])).

convert_room_config(Data) ->
    Config = proplists:get_value(<<"_data">>, Data, []),
    Pass = case proplists:get_value(<<"password">>, Config, <<"">>) of
	       <<"">> ->
		   [];
	       Password ->
		   [{password_protected, true},
		    {password, Password}]
	   end,
    Subj = try jid:decode(
		  proplists:get_value(
		    <<"subject_from">>, Config, <<"">>)) of
	       #jid{lresource = Nick} when Nick /= <<"">> ->
		   [{subject, proplists:get_value(<<"subject">>, Config, <<"">>)},
		    {subject_author, Nick}]
	   catch _:{bad_jid, _} ->
		   []
	   end,
    Anonymous = case proplists:get_value(<<"whois">>, Config, <<"moderators">>) of
		    <<"moderators">> -> true;
		    _ -> false
		end,
    [{affiliations, convert_room_affiliations(Data)},
     {allow_change_subj, proplists:get_bool(<<"changesubject">>, Config)},
     {description, proplists:get_value(<<"description">>, Config, <<"">>)},
     {members_only,	proplists:get_bool(<<"members_only">>, Config)},
     {moderated, proplists:get_bool(<<"moderated">>, Config)},
     {anonymous, Anonymous}] ++ Pass ++ Subj.

convert_privacy_item({_, Item}) ->
    Action = proplists:get_value(<<"action">>, Item, <<"allow">>),
    Order = proplists:get_value(<<"order">>, Item, 0),
    T = misc:binary_to_atom(proplists:get_value(<<"type">>, Item, <<"none">>)),
    V = proplists:get_value(<<"value">>, Item, <<"">>),
    MatchIQ = proplists:get_bool(<<"iq">>, Item),
    MatchMsg = proplists:get_bool(<<"message">>, Item),
    MatchPresIn = proplists:get_bool(<<"presence-in">>, Item),
    MatchPresOut = proplists:get_bool(<<"presence-out">>, Item),
    MatchAll = if (MatchIQ == false) and (MatchMsg == false) and
		  (MatchPresIn == false) and (MatchPresOut == false) ->
		       true;
		  true ->
		       false
	       end,
    {Type, Value} = try case T of
			    none -> {T, none};
			    group -> {T, V};
			    jid -> {T, jid:tolower(jid:decode(V))};
			    subscription -> {T, misc:binary_to_atom(V)}
			end
		    catch _:_ ->
			    {none, none}
		    end,
    #listitem{type = Type,
	      value = Value,
	      action = misc:binary_to_atom(Action),
	      order = erlang:trunc(Order),
	      match_all = MatchAll,
	      match_iq = MatchIQ,
	      match_message = MatchMsg,
	      match_presence_in = MatchPresIn,
	      match_presence_out = MatchPresOut}.

el_to_offline_msg(LUser, LServer, #xmlel{attrs = Attrs} = El) ->
    try
	TS = xmpp_util:decode_timestamp(
	       fxml:get_attr_s(<<"stamp">>, Attrs)),
	Attrs1 = lists:filter(
		   fun({<<"stamp">>, _}) -> false;
		      ({<<"stamp_legacy">>, _}) -> false;
		      (_) -> true
		   end, Attrs),
	El1 = El#xmlel{attrs = Attrs1},
	case xmpp:decode(El1, ?NS_CLIENT, [ignore_els]) of
	    #message{from = #jid{} = From, to = #jid{} = To} = Packet ->
		[#offline_msg{
		    us = {LUser, LServer},
		    timestamp = TS,
		    expire = never,
		    from = From,
		    to = To,
		    packet = Packet}];
	    _ ->
		[]
	end
    catch _:{bad_timestamp, _} ->
	    [];
	  _:{bad_jid, _} ->
	    [];
	  _:{xmpp_codec, _} ->
	    []
    end.

deserialize(L) ->
    deserialize(L, #xmlel{}, []).

deserialize([{<<"attr">>, Attrs}|T], El, Acc) ->
    deserialize(T, El#xmlel{attrs = Attrs ++ El#xmlel.attrs}, Acc);
deserialize([{<<"name">>, Name}|T], El, Acc) ->
    deserialize(T, El#xmlel{name = Name}, Acc);
deserialize([{_, S}|T], #xmlel{children = Els} = El, Acc) when is_binary(S) ->
    deserialize(T, El#xmlel{children = [{xmlcdata, S}|Els]}, Acc);
deserialize([{_, L}|T], #xmlel{children = Els} = El, Acc) when is_list(L) ->
    deserialize(T, El#xmlel{children = deserialize(L) ++ Els}, Acc);
deserialize([], El, Acc) ->
    [El|Acc].
