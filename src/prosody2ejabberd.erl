%%%-------------------------------------------------------------------
%%% @author Evgeny Khramtsov <ekhramtsov@process-one.net>
%%% @copyright (C) 2016, Evgeny Khramtsov
%%% @doc
%%%
%%% @end
%%% Created : 20 Jan 2016 by Evgeny Khramtsov <ekhramtsov@process-one.net>
%%%-------------------------------------------------------------------
-module(prosody2ejabberd).

%% API
-export([from_dir/1]).

-include("ejabberd.hrl").
-include("jlib.hrl").
-include("logger.hrl").
-include("mod_roster.hrl").
-include("mod_offline.hrl").

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
			      "private", "config", "offline"])
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

convert_data(Host, "accounts", User, [Data]) ->
    Password = proplists:get_value(<<"password">>, Data, <<>>),
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
				 XMLNS = xml:get_tag_attr_s(<<"xmlns">>, El),
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
    RoomJID = jid:from_string(proplists:get_value(<<"jid">>, Data, <<"">>)),
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
convert_data(_Host, _Type, _User, _Data) ->
    ok.

convert_pending_item(LUser, LServer, LuaList) ->
    lists:flatmap(
      fun({S, true}) ->
	      case jid:from_string(S) of
		  #jid{} = J ->
		      LJID = jid:tolower(J),
		      [#roster{usj = {LUser, LServer, LJID},
			       us = {LUser, LServer},
			       jid = LJID,
			       ask = in}];
		  error ->
		      []
	      end;
	 (_) ->
	      []
      end, LuaList).

convert_roster_item(LUser, LServer, JIDstring, LuaList) ->
    case jid:from_string(JIDstring) of
	#jid{} = JID ->
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
			  R#roster{subscription = jlib:binary_to_atom(Sub)};
		     ({<<"ask">>, <<"subscribe">>}, R) ->
			  R#roster{ask = out};
		     ({<<"name">>, Name}, R) ->
			  R#roster{name = Name}
		  end, InitR, LuaList),
	    [Roster];
	error ->
	    []
    end.

convert_room_affiliations(Data) ->
    lists:flatmap(
      fun({J, Aff}) ->
	      case jid:from_string(J) of
		  #jid{luser = U, lserver = S} ->
		      [{{U, S, <<>>}, jlib:binary_to_atom(Aff)}];
		  error ->
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
    Subj = case jid:from_string(
		  proplists:get_value(
		    <<"subject_from">>, Config, <<"">>)) of
	       #jid{lresource = Nick} when Nick /= <<"">> ->
		   [{subject, proplists:get_value(<<"subject">>, Config, <<"">>)},
		    {subject_author, Nick}];
	       _ ->
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

el_to_offline_msg(LUser, LServer, #xmlel{attrs = Attrs} = El) ->
    case jlib:datetime_string_to_timestamp(
	   xml:get_attr_s(<<"stamp">>, Attrs)) of
	{_, _, _} = TS ->
	    Attrs1 = lists:filter(
		       fun(<<"stamp">>) -> false;
			  (<<"stamp_legacy">>) -> false;
			  (_) -> true
		       end, Attrs),
	    Packet = El#xmlel{attrs = Attrs1},
	    case {jid:from_string(xml:get_attr_s(<<"from">>, Attrs)),
		  jid:from_string(xml:get_attr_s(<<"to">>, Attrs))} of
		{#jid{} = From, #jid{} = To} ->
		    [#offline_msg{
			us = {LUser, LServer},
			timestamp = TS,
			expire = never,
			from = From,
			to = To,
			packet = Packet}];
		_ ->
		    []
	    end;
	_ ->
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
