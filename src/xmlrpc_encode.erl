%% Copyright (C) 2003 Joakim Grebenö <jocke@gleipnir.com>.
%% All rights reserved.
%%
%% Redistribution and use in source and binary forms, with or without
%% modification, are permitted provided that the following conditions
%% are met: 
%%
%% 1. Redistributions of source code must retain the above copyright
%%    notice, this list of conditions and the following disclaimer. 
%% 2. Redistributions in binary form must reproduce the above
%%    copyright notice, this list of conditions and the following
%%    disclaimer in the documentation and/or other materials provided
%%    with the distribution.
%%
%% THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS
%% OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
%% WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
%% ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
%% DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
%% DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
%% GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
%% INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
%% WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
%% NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
%% SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

-module(xmlrpc_encode).
-author('jocke@gleipnir.com').
-export([payload/1]).

%% Exported: payload/1

payload({call, Name, Params}) when atom(Name), list(Params) ->
    case encode_params(Params) of
	{error, Reason} -> {error, Reason};
	EncodedParams ->
	    EncodedPayload =
		["<?xml version=\"1.0\"?><methodCall><methodName>",
		 atom_to_list(Name), "</methodName>", EncodedParams,
		 "</methodCall>"],
	    {ok, EncodedPayload}
    end;
payload({response, {fault, Code, String}}) when integer(Code) ->
    case xmlrpc_util:is_string(String) of
	yes ->
	    EncodedPayload =
		["<?xml version=\"1.0\"?><methodResponse><fault>"
		 "<value><struct><member><name>faultCode</name><value><int>",
		 integer_to_list(Code), "</int></value></member><member><name>"
		 "faultString</name><value><string>", escape_string(String),
		 "</string></value></member></struct></value></fault>",
		 "</methodResponse>"],
	    {ok, EncodedPayload};
	no -> {error, {bad_string, String}}
    end;
payload({response, []} = Payload) ->
    {ok, ["<?xml version=\"1.0\"?><methodResponse></methodResponse>"]};
payload({response, [Param]} = Payload) ->
    case encode_params([Param]) of
	{error, Reason} -> {error, Reason};
	EncodedParam ->
	    {ok, ["<?xml version=\"1.0\"?><methodResponse>", EncodedParam,
		  "</methodResponse>"]}
    end;
payload(Payload) -> {error, {bad_payload, Payload}}.

encode_params(Params) -> encode_params(Params, []).

encode_params([], []) -> [];
encode_params([], Acc) -> ["<params>", Acc, "</params>"];
encode_params([Param|Rest], Acc) ->
    case encode(Param) of
	{error, Reason} -> {error, Reason};
	EncodedParam ->
	    NewAcc = Acc++["<param><value>", EncodedParam, "</value></param>"],
	    encode_params(Rest, NewAcc)
    end.

encode({struct, Struct}) ->
    case encode_members(Struct) of
	{error, Reason} -> {error, Reason};
	Members -> ["<struct>", Members, "</struct>"]
    end;
encode({array, Array}) when list(Array) ->
    case encode_values(Array)of
	{error, Reason} -> {error, Reason};
	Values -> ["<array><data>", Values, "</data></array>"]
    end;
encode(Integer) when integer(Integer) ->
    ["<int>", integer_to_list(Integer), "</int>"];
encode(true) -> "<boolean>1</boolean>"; % duh!
encode(false) -> "<boolean>0</boolean>"; % duh!
encode(Double) when float(Double) ->
    ["<double>", io_lib:format("~p", [Double]), "</double>"];
encode({date, Date}) ->
    case xmlrpc_util:is_iso8601_date(Date) of
	yes -> ["<dateTime.iso8601>", Date, "</dateTime.iso8601>"];
	no -> {error, {bad_date, Date}}
    end;
encode({base64, Base64}) ->
    case xmlrpc_util:is_base64(Base64) of
	yes -> ["<base64>", Base64, "</base64>"];
	no -> {error, {bad_base64, Base64}}
    end;
encode(Value) ->
    case xmlrpc_util:is_string(Value) of
	yes -> escape_string(Value);
	no -> {error, {bad_value, Value}}
    end.

escape_string([]) -> [];
escape_string([$<|Rest]) -> ["&lt;", escape_string(Rest)];
escape_string([$>|Rest]) -> ["&gt;", escape_string(Rest)];
escape_string([$&|Rest]) -> ["&amp;", escape_string(Rest)];
escape_string([C|Rest]) -> [C|escape_string(Rest)].

encode_members(Struct) -> encode_members(Struct, []).

encode_members([], Acc) -> Acc;
encode_members([{Name, Value}|Rest], Acc) when atom(Name) ->
    case encode(Value) of
	{error, Reason} -> {error, Reason};
	EncodedValue ->
	    NewAcc =
		Acc++["<member><name>", atom_to_list(Name), "</name><value>",
		      EncodedValue, "</value></member>"],
	    encode_members(Rest, NewAcc)
    end;
encode_members([{Name, Value}|Rest], Acc) -> {error, {invalid_name, Name}};
encode_members(UnknownMember, Acc) ->
    {error, {unknown_member, UnknownMember}}.

encode_values(Array) -> encode_values(Array, []).

encode_values([], Acc) -> Acc;
encode_values([Value|Rest], Acc) ->
    case encode(Value) of
	{error, Reason} -> {error, Reason};
	EncodedValue ->
	    NewAcc = Acc++["<value>", EncodedValue, "</value>"],
	    encode_values(Rest, NewAcc)
    end;
encode_values([{Name, Value}|Rest], Acc) -> {error, {invalid_name, Name}};
encode_values(UnknownMember, Acc) ->
    {error, {unknown_member, UnknownMember}}.
