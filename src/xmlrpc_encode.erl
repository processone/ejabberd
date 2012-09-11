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

-type xmlrpc() :: number() | boolean() | binary() |
                  {base64, binary()} | {date, binary()} |
                  {array, [xmlrpc()]} | {struct, [{atom(), xmlrpc()}]}.

-spec payload({call, atom(), [xmlrpc()]} |
              {response, {fault, integer(), binary()} | [xmlrpc()]}) ->
                     binary().

payload({call, Name, Params}) ->
    <<"<?xml version=\"1.0\"?><methodCall><methodName>",
      (jlib:atom_to_binary(Name))/binary,
      "</methodName>",
      (encode_params(Params))/binary,
      "</methodCall>">>;
payload({response, {fault, Code, String}}) ->
    <<"<?xml version=\"1.0\"?><methodResponse><fault"
      "><value><struct><member><name>faultCode</name"
      "><value><int>",
      (jlib:integer_to_binary(Code))/binary,
      "</int></value></member><member><name>faultStr"
      "ing</name><value><string>",
      (escape_string(String))/binary,
      "</string></value></member></struct></value></"
      "fault></methodResponse>">>;
payload({response, []}) ->
    <<"<?xml version=\"1.0\"?><methodResponse></methodResponse>">>;
payload({response, [Param]}) ->
    <<"<?xml version=\"1.0\"?><methodResponse>",
      (encode_params([Param]))/binary,
      "</methodResponse>">>.

encode_params(Params) -> encode_params(Params, <<>>).

encode_params([], <<>>) -> <<>>;
encode_params([], Acc) ->
    <<"<params>", Acc/binary, "</params>">>;
encode_params([Param | Rest], Acc) ->
    EncodedParam = encode(Param),
    NewAcc = <<Acc/binary, "<param><value>",
               EncodedParam/binary, "</value></param>">>,
    encode_params(Rest, NewAcc).

encode({struct, Struct}) ->
    Members = encode_members(Struct),
    <<"<struct>", Members/binary, "</struct>">>;
encode({array, Array}) ->
    Values = encode_values(Array),
    <<"<array><data>", Values/binary, "</data></array>">>;
encode(Integer) when is_integer(Integer) ->
    <<"<int>", (jlib:integer_to_binary(Integer))/binary, "</int>">>;
encode(true) -> <<"<boolean>1</boolean>">>; % duh!
encode(false) -> <<"<boolean>0</boolean>">>; % duh!
encode(Double) when is_float(Double) ->
    list_to_binary(
      [<<"<double>">>, io_lib:format("~p", [Double]),
       <<"</double>">>]);
encode({date, Date}) ->
    <<"<dateTime.iso8601>", Date/binary, "</dateTime.iso8601>">>;
encode({base64, Base64}) ->
    <<"<base64>", Base64/binary, "</base64>">>;
encode(Value) ->
    escape_string(Value).

escape_string(<<>>) -> <<>>;
escape_string(<<$<, Rest/binary>>) ->
    <<"&lt;", (escape_string(Rest))/binary>>;
escape_string(<<$>, Rest/binary>>) ->
    <<"&gt;", (escape_string(Rest))/binary>>;
escape_string(<<$&, Rest/binary>>) ->
    <<"&amp;", (escape_string(Rest))/binary>>;
escape_string(<<C, Rest/binary>>) -> <<C, (escape_string(Rest))/binary>>.

encode_members(Struct) -> encode_members(Struct, <<>>).

encode_members([], Acc) -> Acc;
encode_members([{Name, Value} | Rest], Acc) ->
    NewAcc = <<Acc/binary,
               "<member><name>",
               (jlib:atom_to_binary(Name))/binary,
               "</name><value>",
               (encode(Value))/binary,
               "</value></member>">>,
    encode_members(Rest, NewAcc).

encode_values(Array) -> encode_values(Array, <<>>).

encode_values([], Acc) -> Acc;
encode_values([Value | Rest], Acc) ->
    NewAcc = <<Acc/binary,
               "<value>",
               (encode(Value))/binary,
               "</value>">>,
    encode_values(Rest, NewAcc).
