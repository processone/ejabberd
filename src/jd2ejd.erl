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

-behaviour(gen_fsm).

%% External exports
-export([start/1,
	 start/2,
	 import_file/1,
	 import_dir/1]).

%% gen_fsm callbacks
-export([init/1,
	 wait_for_xdb/2,
	 xdb_data/2,
	 handle_event/3,
	 handle_sync_event/4,
	 code_change/4,
	 handle_info/3,
	 terminate/3]).

-include("ejabberd.hrl").
-include("namespaces.hrl").

-record(state, {socket, pid,
		user = "", server = ?MYNAME, resource = ""
	       }).

%-define(DBGFSM, true).

-ifdef(DBGFSM).
-define(FSMOPTS, [{debug, [trace]}]).
-else.
-define(FSMOPTS, []).
-endif.


%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start(File) ->
    User = filename:rootname(filename:basename(File)),
    start(File, User).

start(File, User) ->
    gen_fsm:start(?MODULE, [File, User, self()], ?FSMOPTS).

%%%----------------------------------------------------------------------
%%% Callback functions from gen_fsm
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, StateName, StateData}          |
%%          {ok, StateName, StateData, Timeout} |
%%          ignore                              |
%%          {stop, StopReason}                   
%%----------------------------------------------------------------------
init([File, User, Pid]) ->
    XMLStreamPid = xml_stream:start(self()),
    {ok, Text} = file:read_file(File),
    xml_stream:send_text(XMLStreamPid, Text),
    {ok, wait_for_xdb, #state{user = User, pid = Pid}}.

%%----------------------------------------------------------------------
%% Func: StateName/2
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}                         
%%----------------------------------------------------------------------

wait_for_xdb({xmlstreamstart, Name, Attrs}, StateData) ->
    case Name of
	"xdb" ->
	    {next_state, xdb_data, StateData};
	_ ->
	    {stop, normal, StateData}
    end;

wait_for_xdb(closed, StateData) ->
    {stop, normal, StateData}.


xdb_data({xmlstreamelement, El}, StateData) ->
    {xmlelement, Name, Attrs, Els} = El,
    Server = StateData#state.server,
    From = {StateData#state.user,
	    Server,
	    ""},
    NewState =
	case xml:get_attr_s("xmlns", Attrs) of
	    ?NS_AUTH ->
		Password = xml:get_tag_cdata(El),
		ejabberd_auth:set_password(StateData#state.user, Password),
		StateData;
	    ?NS_ROSTER ->
		%mod_roster:process_iq(From,
		%		      {"", ?MYNAME, ""},
		%		      {iq, "", set, ?NS_ROSTER, El}),
		mod_roster:set_items(StateData#state.user, El),
		StateData;
	    ?NS_VCARD ->
		Res = mod_vcard:process_local_iq(From,
						 {"", ?MYNAME, ""},
						 {iq, "", set, ?NS_VCARD, El}),
		StateData;
	    "jabber:x:offline" ->
		process_offline(From, El),
		StateData;
	    %?NS_REGISTER ->
	    %    mod_register:process_iq(
	    %      {"", "", ""}, {"", ?MYNAME, ""},
	    %      {iq, "", set, ?NS_REGISTER, El}),
	    %    User = xml:get_path_s(El, [{elem, "username"}, cdata]),
	    %    io:format("user ~s~n", [User]),
	    %    StateData;
	    XMLNS ->
		io:format("jd2ejd: Unknown namespace \"~s\"~n", [XMLNS]),
		StateData
	end,
    {next_state, xdb_data, NewState};

xdb_data({xmlstreamend, Name}, StateData) ->
    {stop, normal, StateData};

xdb_data(closed, StateData) ->
    % TODO
    {stop, normal, StateData}.



%%----------------------------------------------------------------------
%% Func: StateName/3
%% Returns: {next_state, NextStateName, NextStateData}            |
%%          {next_state, NextStateName, NextStateData, Timeout}   |
%%          {reply, Reply, NextStateName, NextStateData}          |
%%          {reply, Reply, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}                          |
%%          {stop, Reason, Reply, NewStateData}                    
%%----------------------------------------------------------------------
%state_name(Event, From, StateData) ->
%    Reply = ok,
%    {reply, Reply, state_name, StateData}.

%%----------------------------------------------------------------------
%% Func: handle_event/3
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}                         
%%----------------------------------------------------------------------
handle_event(Event, StateName, StateData) ->
    {next_state, StateName, StateData}.

%%----------------------------------------------------------------------
%% Func: handle_sync_event/4
%% Returns: {next_state, NextStateName, NextStateData}            |
%%          {next_state, NextStateName, NextStateData, Timeout}   |
%%          {reply, Reply, NextStateName, NextStateData}          |
%%          {reply, Reply, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}                          |
%%          {stop, Reason, Reply, NewStateData}                    
%%----------------------------------------------------------------------
handle_sync_event(Event, From, StateName, StateData) ->
    Reply = ok,
    {reply, Reply, StateName, StateData}.

code_change(OldVsn, StateName, StateData, Extra) ->
    {ok, StateName, StateData}.

%%----------------------------------------------------------------------
%% Func: handle_info/3
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}                         
%%----------------------------------------------------------------------
handle_info(_, StateName, StateData) ->
    {next_state, StateName, StateData}.

%%----------------------------------------------------------------------
%% Func: terminate/3
%% Purpose: Shutdown the fsm
%% Returns: any
%%----------------------------------------------------------------------
terminate(Reason, StateName, StateData) ->
    StateData#state.pid ! {jd2ejd, exited},
    ok.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

process_offline(To, {xmlelement, _, _, Els}) ->
    lists:foreach(fun({xmlelement, _, Attrs, _} = El) ->
			  FromS = xml:get_attr_s("from", Attrs),
			  From = case FromS of
				     "" ->
					 {"", ?MYNAME, ""};
				     _ ->
					 jlib:string_to_jid(FromS)
				 end,
			  case From of
			      error ->
				  ok;
			      _ ->
				  mod_offline:store_packet(From, To, El)
			  end
		  end, Els).


import_file(File) ->
    start(File),
    receive
	M -> M
	after 1000 -> ok
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



