%%%----------------------------------------------------------------------
%%% File    : jd2ejd.erl
%%% Author  : Alexey Shchepin <alexey@sevcom.net>
%%% Purpose : Import of jabberd1.4 user spool file
%%% Created :  2 Feb 2003 by Alexey Shchepin <alexey@sevcom.net>
%%% Id      : $Id$
%%%----------------------------------------------------------------------

-module(jd2ejd).
-author('alexey@sevcom.net').
-vsn('$Revision$ ').

%% External exports
-export([import_file/1,
	 import_dir/1]).

-include("ejabberd.hrl").
-include("jlib.hrl").



%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------

import_file(File) ->
    User = filename:rootname(filename:basename(File)),
    Server = filename:basename(filename:dirname(File)),
    case (jlib:nodeprep(User) /= error) andalso
	(jlib:nameprep(Server) /= error) of
	true ->
	    case file:read_file(File) of
		{ok, Text} ->
		    case xml_stream:parse_element(Text) of
			El when element(1, El) == xmlelement ->
			    case catch process_xdb(User, Server, El) of
				{'EXIT', Reason} ->
				    ?ERROR_MSG(
				       "Error while processing file \"~s\": ~p~n",
				       [File, Reason]);
				_ ->
				    ok
			    end;
			{error, Reason} ->
			    ?ERROR_MSG("Can't parse file \"~s\": ~p~n",
				       [File, Reason])
		    end;
		{error, Reason} ->
		    ?ERROR_MSG("Can't read file \"~s\": ~p~n", [File, Reason])
	    end;
	false ->
	    ?ERROR_MSG("Incorrect user/server name in file \"~s\"~n", [File])
    end.


import_dir(Dir) ->
    {ok, Files} = file:list_dir(Dir),
    MsgFiles = lists:filter(
		 fun(FN) ->
			 case string:len(FN) > 4 of
			     true ->
				 string:substr(FN,
					       string:len(FN) - 3) == ".xml";
			     _ ->
				 false
			 end
		 end, Files),
    lists:foreach(
      fun(FN) ->
	      import_file(filename:join([Dir, FN]))
      end, MsgFiles),
    ok.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

process_xdb(User, Server, {xmlelement, Name, _Attrs, Els}) ->
    case Name of
	"xdb" ->
	    lists:foreach(
	      fun(El) ->
		      xdb_data(User, Server, El)
	      end, Els);
	_ ->
	    ok
    end.


xdb_data(User, Server, {xmlcdata, _CData}) ->
    ok;
xdb_data(User, Server, {xmlelement, _Name, Attrs, _Els} = El) ->
    From = jlib:make_jid(User, Server, ""),
    case xml:get_attr_s("xmlns", Attrs) of
	?NS_AUTH ->
	    Password = xml:get_tag_cdata(El),
	    ejabberd_auth:set_password(User, Server, Password),
	    ok;
	?NS_ROSTER ->
	    catch mod_roster:set_items(User, Server, El),
	    ok;
	?NS_VCARD ->
	    catch mod_vcard:process_sm_iq(
		    From,
		    jlib:make_jid("", ?MYNAME, ""),
		    #iq{type = set, xmlns = ?NS_VCARD, sub_el = El}),
	    ok;
	"jabber:x:offline" ->
	    process_offline(From, El),
	    ok;
	XMLNS ->
	    case xml:get_attr_s("j_private_flag", Attrs) of
		"1" ->
		    catch mod_private:process_local_iq(
			    From,
			    jlib:make_jid("", ?MYNAME, ""),
			    #iq{type = set, xmlns = ?NS_PRIVATE,
				sub_el = {xmlelement, "query", [],
					  [jlib:remove_attr(
					     "j_private_flag",
					     jlib:remove_attr("xdbns", El))]}});
		_ ->
		    ?DEBUG("jd2ejd: Unknown namespace \"~s\"~n", [XMLNS])
	    end,
	    ok
    end.


process_offline(To, {xmlelement, _, _, Els}) ->
    lists:foreach(fun({xmlelement, _, Attrs, _} = El) ->
			  FromS = xml:get_attr_s("from", Attrs),
			  From = case FromS of
				     "" ->
					 jlib:make_jid("", ?MYNAME, "");
				     _ ->
					 jlib:string_to_jid(FromS)
				 end,
			  case From of
			      error ->
				  ok;
			      _ ->
				  catch mod_offline:store_packet(From, To, El)
			  end
		  end, Els).

