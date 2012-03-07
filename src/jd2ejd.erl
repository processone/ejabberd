%%%----------------------------------------------------------------------
%%% File    : jd2ejd.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Import of jabberd14 user spool file
%%% Created :  2 Feb 2003 by Alexey Shchepin <alexey@process-one.net>
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

-module(jd2ejd).
-author('alexey@process-one.net').

%% External exports
-export([import_file/1,
	 import_dir/1]).

-include_lib("exmpp/include/exmpp.hrl").

-include("ejabberd.hrl").



%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------

import_file(File) ->
    User = filename:rootname(filename:basename(File)),
    Server = filename:basename(filename:dirname(File)),
    case exmpp_stringprep:is_node(User) andalso
	exmpp_stringprep:is_name(Server) of
	true ->
	    case file:read_file(File) of
		{ok, Text} ->
		    try
			[El] = exmpp_xml:parse_document(Text,
			  [names_as_atom]),
			case catch process_xdb(User, Server, El) of
			    {'EXIT', Reason} ->
				?ERROR_MSG(
				   "Error while processing file \"~s\": ~p~n",
				   [File, Reason]),
				{error, Reason};
			    _ ->
				ok
			end
		    catch
			_:Reason1 ->
			    ?ERROR_MSG("Can't parse file \"~s\": ~p~n",
				       [File, Reason1]),
			    {error, Reason1}
		    end;
		{error, Reason} ->
		    ?ERROR_MSG("Can't read file \"~s\": ~p~n", [File, Reason]),
		    {error, Reason}
	    end;
	false ->
	    ?ERROR_MSG("Illegal user/server name in file \"~s\"~n", [File]),
	    {error, "illegal user/server"}
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
    lists:foldl(
      fun(FN, A) ->
	      Res = import_file(filename:join([Dir, FN])),
	      case {A, Res} of
		  {ok, ok} -> ok;
		  {ok, _} -> {error, "see ejabberd log for details"};
		  _ -> A
	      end
      end, ok, MsgFiles).

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

process_xdb(User, Server, #xmlel{name = "xdb", children = Els}) ->
    lists:foreach(
      fun(El) ->
	      xdb_data(User, Server, El)
      end, Els);
process_xdb(_User, _Server, _El) ->
    ok.


xdb_data(_User, _Server, #xmlcdata{}) ->
    ok;
xdb_data(User, Server, #xmlel{ns = NS} = El) ->
    From = exmpp_jid:make(User, Server),
    UserB = list_to_binary(User),
    ServerB = list_to_binary(Server),
    case NS of
	?NS_LEGACY_AUTH ->
	    Password = exmpp_xml:get_cdata_as_list(El),
	    ejabberd_auth:set_password(UserB, ServerB, Password),
	    ok;
	?NS_ROSTER ->
	    catch mod_roster:set_items(UserB, ServerB, El),
	    ok;
	?NS_LAST_ACTIVITY ->
	    TimeStamp = exmpp_xml:get_attribute_as_list(El, <<"last">>, ""),
	    Status = exmpp_xml:get_cdata(El),
		    catch mod_last:store_last_info(
			    UserB,
			    ServerB,
			    list_to_integer(TimeStamp),
			    Status),
	    ok;
	?NS_VCARD ->
		    catch mod_vcard:process_sm_iq(
			    From,
			    exmpp_jid:make(Server),
			    #iq{kind = request, type = set, ns = ?NS_VCARD, payload = El, iq_ns = ?NS_JABBER_CLIENT}),
	    ok;
	"jabber:x:offline" ->
	    process_offline(Server, From, El),
	    ok;
	XMLNS ->
	    case exmpp_xml:get_attribute_as_list(El, <<"j_private_flag">>, "") of
		"1" ->
		    catch mod_private:process_sm_iq(
			    From,
			    exmpp_jid:make(Server),
			    #iq{kind = request, type = set, ns = ?NS_PRIVATE,
				iq_ns = ?NS_JABBER_CLIENT,
				payload = #xmlel{name = 'query', children =
					  [exmpp_xml:remove_attribute(
					     exmpp_xml:remove_attribute(El, <<"xdbns">>), <<"j_private_flag">>)]}});
		_ ->
		    ?DEBUG("jd2ejd: Unknown namespace \"~s\"~n", [XMLNS])
	    end,
	    ok
    end.


process_offline(Server, To, #xmlel{children = Els}) ->
    LServer = exmpp_stringprep:nameprep(Server),
    lists:foreach(fun(#xmlel{} = El) ->
			  FromS = exmpp_stanza:get_sender(El),
			  From = case FromS of
				     undefined ->
					 exmpp_jid:make(Server);
				     _ ->
					 try
					     exmpp_jid:parse(FromS)
					 catch
					     _ ->
						 error
					 end
				 end,
			  case From of
			      error ->
				  ok;
			      _ ->
				  ejabberd_hooks:run(offline_message_hook,
						     list_to_binary(LServer),
						     [From, To, El])
			  end
		  end, Els).

