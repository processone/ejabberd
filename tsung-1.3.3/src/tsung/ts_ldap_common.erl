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

%%% File    : ts_ldap_common.erl
%%% Author  : Pablo Polvorin <ppolv@yahoo.com.ar>
%%% Purpose : LDAP plugin

-module(ts_ldap_common).

-export([encode_filter/1,
         bind_msg/3,
         unbind_msg/1,
         search_msg/5,
         start_tls_msg/1,
         add_msg/3,
         modify_msg/3 ]).

-export([push/2,get_packet/1,empty_packet_state/0]).

-define(LDAP_VERSION, 3).
-define(START_TLS_OID,"1.3.6.1.4.1.1466.20037").

-define(MAX_HEADER, 8).  %% mm...

-include("ELDAPv3.hrl").

encode_filter({'and',L}) -> {'and',lists:map(fun encode_filter/1,L)};
encode_filter({'or',L}) ->  {'or',lists:map(fun encode_filter/1,L)};
encode_filter({'not',I}) -> {'not',encode_filter(I)};
encode_filter(I = {'present',_}) -> I;
encode_filter({'substring',Attr,Subs}) -> eldap:substrings(Attr,Subs);
encode_filter({eq,Attr,Value}) -> eldap:equalityMatch(Attr,Value);
encode_filter({'let',Attr,Value}) -> eldap:lessOrEqual(Attr,Value);
encode_filter({get,Attr,Value}) -> eldap:greaterOrEqual(Attr,Value);
encode_filter({aproxq,Attr,Value}) -> eldap:approxMatch(Attr,Value).

bind_msg(Id,User,Password) ->
    Req = {bindRequest,#'BindRequest'{version=?LDAP_VERSION, name=User, authentication = {simple, Password}}},
    Message = #'LDAPMessage'{messageID  = Id,
                             protocolOp = Req},
    {ok,Bytes} = asn1rt:encode('ELDAPv3', 'LDAPMessage', Message),
    Bytes.


search_msg(Id,Base,Scope,Filter,Attributes) ->
        Req = #'SearchRequest'{baseObject = Base,
               scope = Scope,
               derefAliases = neverDerefAliases,
               sizeLimit = 0, % no size limit
               timeLimit = 0,
               typesOnly = false,
               filter = Filter,
               attributes = Attributes},
    Message = #'LDAPMessage'{messageID  = Id,
                 protocolOp = {searchRequest,Req}},
    {ok,Bytes} = asn1rt:encode('ELDAPv3', 'LDAPMessage', Message),
    Bytes.



start_tls_msg(Id) ->
    Req = #'ExtendedRequest'{requestName = ?START_TLS_OID},
    Message = #'LDAPMessage'{messageID  = Id,  protocolOp = {extendedReq,Req}},
    {ok,Bytes} = asn1rt:encode('ELDAPv3', 'LDAPMessage', Message),
    Bytes.


unbind_msg(Id) ->
    Message = #'LDAPMessage'{messageID  = Id,
                 protocolOp = {unbindRequest,[]}},
    {ok,Bytes} = asn1rt:encode('ELDAPv3', 'LDAPMessage', Message),
    Bytes.


add_msg(Id,DN,Attrs) ->
    Req = #'AddRequest'{entry = DN,
            attributes = [ {'AddRequest_attributes',Type, Values}  || {Type,Values} <- Attrs]},
    Message = #'LDAPMessage'{messageID  = Id,
                 protocolOp = {addRequest,Req}},
    {ok,Bytes} = asn1rt:encode('ELDAPv3', 'LDAPMessage', Message),
    Bytes.


modify_msg(Id,DN,Modifications) ->
    Mods = [ #'ModifyRequest_modification_SEQOF'{
                                   operation = Operation,
                                   modification = #'AttributeTypeAndValues'{type=Type,vals=Values}}
                                   || {Operation,Type,Values} <- Modifications],
    Req = #'ModifyRequest'{object = DN,    modification = Mods},
    Message = #'LDAPMessage'{messageID  = Id,  protocolOp = {modifyRequest,Req}},
    {ok,Bytes} = asn1rt:encode('ELDAPv3', 'LDAPMessage', Message),
    Bytes.



%% -------------------------------------------
%% asn1 packet buffering and delimiting
%%
%% Temporary fix until the new ssl module incorporate appropiate
%% support for asn1 packets.
%% -------------------------------------------

-record(asn1_packet_state,
            {
            length = undefined,
            buffer = <<>>
            }).


empty_packet_state() -> #asn1_packet_state{}.

push(<<>>,S) ->
    S;

push(Data,S =#asn1_packet_state{buffer = B}) ->
    S#asn1_packet_state{buffer = <<B/binary,Data/binary>>}.


get_packet(S = #asn1_packet_state{buffer= <<>>}) ->
    {none,S};

get_packet(S = #asn1_packet_state{length=undefined,buffer=Buffer}) ->
    case packet_length(Buffer) of
        {ok,Length} -> extract_packet(S#asn1_packet_state{length=Length});
        not_enough_data -> {none,S}
    end;

get_packet(S) -> extract_packet(S).


extract_packet(#asn1_packet_state{length=N,buffer=Buffer}) when (size(Buffer) >= N) ->
    <<Packet:N/binary,Rest/binary>> = Buffer,
    {packet,Packet,#asn1_packet_state{length=undefined,buffer=Rest}};

extract_packet(S) when is_record(S,asn1_packet_state) -> {none,S}.

packet_length(Buffer) ->
    try asn1rt_ber_bin:decode_tag_and_length(Buffer) of
        {_Tag, Len,_Rest,RemovedBytes} ->    {ok,Len+RemovedBytes}
    catch
        _Type:_Error ->
            case size(Buffer) > ?MAX_HEADER of
                true -> throw({invalid_packet,Buffer});
                false -> not_enough_data
            end
    end.
