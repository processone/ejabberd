%%%  This program is free software; you can redistribute it and/or modify
%%%  it under the terms of the GNU General Public License as published by
%%%  the Free Software Foundation; either version 2 of the License, or
%%%  (at your option) any later version.
%%%
%%%  This program is distributed in the hope that it will be useful,
%%%  but WITHOUT ANY WARRANTY; without even the implied warranty of
%%%  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%%  GNU General Public License for more details.
%%%
%%%  You should have received a copy of the GNU General Public License
%%%  along with this program; if not, write to the Free Software
%%%  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307, USA.
%%%

%%% In addition, as a special exception, you have the permission to
%%% link the code of this program with any library released under
%%% the EPL license and distribute linked combinations including
%%% the two.

%%% File    : ts_ldap.erl
%%% Author  : Pablo Polvorin <ppolv@yahoo.com.ar>
%%% Purpose : LDAP plugin


-module(ts_ldap).

-export([init_dynparams/0,
         add_dynparams/4,
         get_message/1,
         session_defaults/0,
         parse/2,
         parse_config/2,
         new_session/0
         ]).

-include("ts_profile.hrl").
-include("ts_ldap.hrl").
-include("ELDAPv3.hrl").

%%----------------------------------------------------------------
%%-----Configuration parsing
%%----------------------------------------------------------------
parse_config(Element, Conf) ->
    ts_config_ldap:parse_config(Element,Conf).

%%----------------------------------------------------------------------
%% Function: session_default/0
%% Purpose: default parameters for session
%% Returns: {ok, persistent = true|false}
%%----------------------------------------------------------------------
session_defaults() ->
    {ok, true}.

%%----------------------------------------------------------------------
%% Function: new_session/0
%% Purpose: initialize session information
%% Returns: record or []
%%----------------------------------------------------------------------
new_session() ->
    ts_ldap_common:empty_packet_state().
%%FIXME: this won't be necessary when the SSL module support the asn1
%%       packet type.  At this moment we are parsing the packet by
%%       ourselves, even over plain gen_tcp sockets, which can
%%       recognize asn1...


%%----------------------------------------------------------------------
%% Function: parse/2
%% Purpose: parse the response from the server and keep information
%%          about the response in State#state_rcv.session
%% Args:    Data (binary), State (#state_rcv)
%% Returns: {NewState, Options for socket (list), Close = true|false}
%%----------------------------------------------------------------------
parse(closed, State) ->
    {State#state_rcv{ack_done = true, datasize=0}, [], true};


%% Shortcut, when using ssl i'm getting lots <<>> data. Also, next
%% clause is an infinite loop if data is <<>>
parse(<<>>,State) ->
    {State,[],false};

%% new response, compute data size (for stats)
parse(Data, State=#state_rcv{acc = [], datasize= 0}) ->
    parse(Data, State#state_rcv{datasize= size(Data)});

parse(Data, State=#state_rcv{acc = [], dyndata=_DynData,session=Session,datasize=PrevSize}) ->
    St = ts_ldap_common:push(Data,Session),
    parse_packets(State#state_rcv{session=St,datasize =PrevSize + size(Data) },St).


%% Can read more than one entire asn1 packet from the network.  Read
%% packets until either there are no more packets available in the
%% buffer (ack_done=false), or the ack_done flag was set true by the
%% appropiate parse_ldap_response
parse_packets(State,Asn1St) ->
    case ts_ldap_common:get_packet(Asn1St) of
        {none,NewAsn1St} ->
            {State#state_rcv{ack_done=false,session=NewAsn1St},[],false};
        {packet,Packet,NewAsn1St} ->
            {ok,Resp} = asn1rt:decode('ELDAPv3', 'LDAPMessage', Packet),
            parse_packet(Resp,State#state_rcv{session = NewAsn1St})
    end.

parse_packet(Resp,State) ->
    R = parse_ldap_response(Resp,State),
    {St,_Opts,_Close} = R,
    if
        St#state_rcv.ack_done == true ->  R;
        St#state_rcv.ack_done == false -> parse_packets(St,St#state_rcv.session)
    end.


%%TODO: see if its useful to count how many response records we get for each search.
parse_ldap_response( #'LDAPMessage'{protocolOp = {bindResponse,Result}},State)->
    case Result#'BindResponse'.resultCode of
        success ->
            ?Debug("Bind successful~n"),
            ts_mon:add({ count, ldap_bind_ok}),
            {State#state_rcv{ack_done=true},[],false};
        _Error   ->
            ts_mon:add({ count, ldap_bind_error}), %FIXME: retry,fail,etc. should be configurable
            ?LOG("Bind fail~n",?INFO),
            {State#state_rcv{ack_done=true},[],true}
    end;

parse_ldap_response( #'LDAPMessage'{protocolOp = {'searchResDone',_R}},State)  ->
    ?DebugF("LDAP Search response Done ~p~n",[_R]),
    {State#state_rcv{ack_done=true},[],false}; %%Response done, mark as acknowledged
parse_ldap_response( #'LDAPMessage'{protocolOp = {'searchResEntry',R}},State)  ->
    NewState = acumulate_result(R,State),

    ?DebugF("LDAP search response Entry ~p~n",[R]),
    {NewState#state_rcv{ack_done=false},[],false};
parse_ldap_response(#'LDAPMessage'{protocolOp = {'searchResRef',_R}},State)  ->
    ?DebugF("LDAP search response Ref ~p~n",[_R]),
    {State#state_rcv{ack_done=false},[],false};


%% When get a possitive response to a startTLS command, inmediatly start ssl over that socket.
parse_ldap_response(#'LDAPMessage'{protocolOp = {'extendedResp',ExtResponse }},State)  ->
    case ExtResponse#'ExtendedResponse'.resultCode of
        success ->
            #ts_request{param = LDAPRequest} = State#state_rcv.request,
            %%Warnning: this won't work unless using a really recent OTP
            {ok,Ssl_socket} = ssl:connect(State#state_rcv.socket,[{cacertfile,LDAPRequest#ldap_request.cacertfile},
                                                                  {certfile,LDAPRequest#ldap_request.certfile},
                                                                  {keyfile,LDAPRequest#ldap_request.keyfile}
                                                                 ]),
            {State#state_rcv{socket=Ssl_socket,protocol=ssl,ack_done=true},[],false};
        _Error ->
            ts_mon:add({ count, ldap_starttls_error}),
            ?LOG("StartTLS fail",?INFO),
            {State#state_rcv{ack_done=true},[],false}
    end;


parse_ldap_response(#'LDAPMessage'{protocolOp = {'addResponse',Result}},State)  ->
    case Result#'LDAPResult'.resultCode of
        success ->
            {State#state_rcv{ack_done=true},[],false};
        _Error   ->
            ts_mon:add({ count, ldap_add_error}),
            ?LOG("Add fail",?INFO),
            {State#state_rcv{ack_done=true},[],true}
    end;

parse_ldap_response(#'LDAPMessage'{protocolOp = {'modifyResponse',Result}},State)  ->
    case Result#'LDAPResult'.resultCode of
        success ->
            {State#state_rcv{ack_done=true},[],false};
        _Error   ->
            ts_mon:add({ count, ldap_modify_error}),
            ?LOG("Modify fail",?INFO),
            {State#state_rcv{ack_done=true},[],true}
end;

parse_ldap_response(Resp,State) ->
    ?LOGF("Got unexpected response: ~p~n",[Resp],?INFO),
    ts_mon:add({ count, ldap_unexpected_msg_resp}),
    {State#state_rcv{ack_done=true},[],false}.

acumulate_result(R,State = #state_rcv{request =
                                        #ts_request{param=#ldap_request{result_var = ResultVar}},
                                      dyndata=DynData}) ->
    case ResultVar of
        none -> State;
        {ok,VarName} -> State#state_rcv{dyndata=accumulate_dyndata(R,VarName,DynData)}
    end.

accumulate_dyndata(R,VarName,DynData = #dyndata{dynvars=DynVars}) when is_list(DynVars)->
    Prev = proplists:get_value(VarName,DynVars,[]),
    NewDynVars = lists:keystore(VarName,1,DynVars,{VarName,[R|Prev]}),
    DynData#dyndata{dynvars=NewDynVars};

accumulate_dyndata(R,VarName,DynData) ->
    DynData#dyndata{dynvars=[{VarName,[R]}]}.


%%----------------------------------------------------------------------
%% Function: add_dynparams/4
%% Purpose: add dynamic parameters to build the message
%% Args: Subst (true|false), DynData = #dyndata, Param = #myproto_request
%%                                               Host  = String
%% Returns: #ldap_request
%%
%%----------------------------------------------------------------------
add_dynparams(false, _DynData, Param, _HostData) ->
    Param;

%% Bind message. Substitution on user and password.
add_dynparams(true, DynData, Param = #ldap_request{type=bind,user=User,password=Password}, _HostData)  ->
    Param#ldap_request{user=ts_search:subst(User,DynData#dyndata.dynvars),password=ts_search:subst(Password,DynData#dyndata.dynvars)};

%% Search message. Only perfom substitutions on the filter of the search requests.
%% The filter text was already parsed into a tree-like struct, substitution
%% is perfomed in the "leaf" of this tree.
add_dynparams(true, DynData, Param = #ldap_request{type=search, filter = Filter}, _HostData)  ->
    Param#ldap_request{filter = subs_filter(Filter,DynData#dyndata.dynvars)};


%% Add message. Substitution on DN and attrs values.
add_dynparams(true,DynData,Param = #ldap_request{type=add,dn=DN,attrs=Attrs},_HostData) ->
    Param#ldap_request{dn=ts_search:subst(DN,DynData#dyndata.dynvars), attrs=subs_attrs(Attrs,DynData#dyndata.dynvars)};

%% Modification message. Substitution on DN and attrs values.
add_dynparams(true,DynData,Param = #ldap_request{type=modify,dn=DN,modifications=Modifications},_HostData) ->
    SubsModifications = [{Operation,AttrType,[ts_search:subst(Value,DynData#dyndata.dynvars) || Value <- Values]} || {Operation,AttrType,Values}<- Modifications ],
    Param#ldap_request{dn=ts_search:subst(DN,DynData#dyndata.dynvars), attrs=SubsModifications}.

subs_filter({Rel,Filters},DynVars) when (Rel == 'and') or (Rel == 'or') ->
    {Rel,lists:map(fun(F)-> subs_filter(F,DynVars) end,Filters)};

subs_filter({'not',Filter},DynVars) ->
    {'not',subs_filter(Filter,DynVars)};

subs_filter({BinRel,Attr,Val},DynVars) when (BinRel == 'aprox') or (BinRel == 'get') or (BinRel == 'let') or (BinRel=='eq')->
    {BinRel,Attr,ts_search:subst(Val,DynVars)};

subs_filter({substring,Attr,Substrings},DynVars) ->
    {substring,Attr,lists:map(fun({Pos,Val}) -> {Pos,ts_search:subst(Val,DynVars)} end, Substrings)}.

subs_attrs(Attrs,DynVars) ->
    [{Attr,[ts_search:subst(Value,DynVars) || Value <- Values]} || {Attr,Values}<-Attrs ].

%%----------------------------------------------------------------------
%% Function: init_dynparams/0
%% Purpose:  initial dynamic parameters value
%% Returns:  #dyndata
%%----------------------------------------------------------------------
init_dynparams() ->
    #dyndata{proto=none}.


%%----------------------------------------------------------------
%%-----Messages
%%----------------------------------------------------------------

get_message(#ldap_request{type=bind,user=User,password=Password}) ->
    X = ts_ldap_common:bind_msg(ts_msg_server:get_id(),User,Password),
    iolist_to_binary(X);
%% TODO: we really need to consult the central msg_server to find a session-specific id?, any reason to prevent
%% the same id to be used in different sessions?

get_message(#ldap_request{type=search,base=Base,scope=Scope,filter=Filter,attributes=Attributes}) ->
    EncodedFilter = ts_ldap_common:encode_filter(Filter),
    X = ts_ldap_common:search_msg(ts_msg_server:get_id(),Base,Scope,EncodedFilter,Attributes),
    iolist_to_binary(X);

get_message(#ldap_request{type=start_tls}) ->
    X = ts_ldap_common:start_tls_msg(ts_msg_server:get_id()),
    iolist_to_binary(X);

get_message(#ldap_request{type=unbind}) ->
    iolist_to_binary(ts_ldap_common:unbind_msg(ts_msg_server:get_id()));

get_message(#ldap_request{type=add,dn=DN,attrs=Attrs}) ->
    iolist_to_binary(ts_ldap_common:add_msg(ts_msg_server:get_id(),DN,Attrs));


get_message(#ldap_request{type=modify,dn=DN,modifications=Modifications}) ->
    iolist_to_binary(ts_ldap_common:modify_msg(ts_msg_server:get_id(),DN,Modifications)).

