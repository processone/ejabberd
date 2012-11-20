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

-module(xmlrpc_decode).
-author('jocke@gleipnir.com').
-export([payload/1]).

-include_lib("xmerl/include/xmerl.hrl").

payload(Payload) ->
    case catch xmerl_scan:string(Payload, [{encoding, latin1}]) of
        {'EXIT', Reason} -> {error, Reason};
	{E, _}  ->
	    case catch decode_element(E) of
		{'EXIT', Reason} -> {error, Reason};
		Result -> Result
	    end
    end.

decode_element(#xmlElement{name = methodCall} = MethodCall)
  when record(MethodCall, xmlElement) ->
    {MethodName, Rest} =
	match_element([methodName], MethodCall#xmlElement.content),
    TextValue = get_text_value(MethodName#xmlElement.content),
    case match_element(normal, [params], Rest) of
	{error, {missing_element, _}} ->
	    {ok, {call, list_to_atom(TextValue), []}};
	{Params, _} ->
	    DecodedParams = decode_params(Params#xmlElement.content),
	    {ok, {call, list_to_atom(TextValue), DecodedParams}}
    end;
decode_element(#xmlElement{name = methodResponse} = MethodResponse)
  when record(MethodResponse, xmlElement) ->
    case match_element([fault, params], MethodResponse#xmlElement.content) of
	{Fault, _} when Fault#xmlElement.name == fault ->
	    {Value, _} = match_element([value], Fault#xmlElement.content),
	    case decode(Value#xmlElement.content) of
		{struct, [{faultCode, Code},
			  {faultString, String}]} when integer(Code) ->
		    case xmlrpc_util:is_string(String) of
			yes -> {ok, {response, {fault, Code, String}}};
			no -> {error, {bad_string, String}}
		    end;
		{struct, [{faultString, String},
			  {faultCode, Code}]} when integer(Code) ->
                    %% This case has been found in java xmlrpc 
		    case xmlrpc_util:is_string(String) of
			yes -> {ok, {response, {fault, Code, String}}};
			no -> {error, {bad_string, String}}
		    end;
		_ ->
		    {error, {bad_element, MethodResponse}}
	    end;
	{Params, _} ->
	    case decode_params(Params#xmlElement.content) of
		[DecodedParam] -> {ok, {response, [DecodedParam]}};
		DecodedParams -> {error, {to_many_params, DecodedParams}}
	    end
    end;
decode_element(E) -> {error, {bad_element, E}}.

match_element(NameList, Content) -> match_element(throw, NameList, Content).

match_element(Type, NameList, []) ->
    return(Type, {error, {missing_element, NameList}});
match_element(Type, NameList, [E|Rest]) when record(E, xmlElement) ->
    case lists:member(E#xmlElement.name, NameList) of
	true -> {E, Rest};
	false -> return(Type, {error, {unexpected_element, E#xmlElement.name}})
    end;
match_element(Type, NameList, [T|Rest]) when record(T, xmlText) ->
    case only_whitespace(T#xmlText.value) of
	yes -> match_element(Type, NameList, Rest);
	no ->
	    return(Type, {error, {unexpected_text, T#xmlText.value, NameList}})
    end.

return(throw, Result) -> throw(Result);
return(normal, Result) -> Result.

only_whitespace([]) -> yes;
only_whitespace([$ |Rest]) -> only_whitespace(Rest);
only_whitespace([$\n|Rest]) -> only_whitespace(Rest);
only_whitespace([$\t|Rest]) -> only_whitespace(Rest);
only_whitespace(_) -> no.

get_text_value([]) -> [];
get_text_value([T|Rest]) when record(T, xmlText) ->
    T#xmlText.value++get_text_value(Rest);
get_text_value(_) -> throw({error, missing_text}).

decode_params([]) -> [];
decode_params(Content) ->
    case match_element(normal, [param], Content) of
	{error, {missing_element, _}} -> [];
	{Param, Rest} ->
	    {Value, _} = match_element([value], Param#xmlElement.content),
	    [decode(Value#xmlElement.content)|decode_params(Rest)]
    end.

decode(Content) when list(Content) ->
    case get_value(Content) of
	{text_value, TextValue} -> TextValue;
	E -> decode(E)
    end;
decode(String) when record(String, xmlText) -> String#xmlText.value;
decode(Struct) when Struct#xmlElement.name == struct ->
    {struct, decode_members(Struct#xmlElement.content)};
decode(Array) when Array#xmlElement.name == array ->
    {Data, _} = match_element([data], Array#xmlElement.content),
    {array, decode_values(Data#xmlElement.content)};
decode(Int) when Int#xmlElement.name == int; Int#xmlElement.name == i4 ->
    TextValue = get_text_value(Int#xmlElement.content),
    make_integer(TextValue);
decode(Boolean) when Boolean#xmlElement.name == boolean ->
    case get_text_value(Boolean#xmlElement.content) of
	"1" -> true;
	"0" -> false;
	TextValue -> throw({error, {invalid_boolean, TextValue}})
    end;
decode(String) when String#xmlElement.name == string ->
    get_text_value(String#xmlElement.content);
decode(Double) when Double#xmlElement.name == double ->
    TextValue = get_text_value(Double#xmlElement.content),
    make_double(TextValue);
decode(Date) when Date#xmlElement.name == 'dateTime.iso8601' ->
    TextValue = get_text_value(Date#xmlElement.content),
%    {date, ensure_iso8601_date(TextValue)}; % FIXME
    {date, TextValue};
decode(Base64) when Base64#xmlElement.name == base64 ->
    TextValue = get_text_value(Base64#xmlElement.content),
%    {base64, ensure_base64(TextValue)}; % FIXME
    {base64, TextValue};
decode(Value) -> throw({error, {bad_value, Value}}).

get_value(Content) ->
    case any_element(Content) of
	false -> {text_value, get_text_value(Content)};
	true -> get_element(Content)
    end.

any_element([]) -> false;
any_element([E|_]) when record(E, xmlElement) -> true;
any_element([_|Rest]) -> any_element(Rest).

get_element([]) -> throw({error, missing_element});
get_element([E|_]) when record(E, xmlElement) -> E;
get_element([T|Rest]) when record(T, xmlText) ->
    case only_whitespace(T#xmlText.value) of
	yes -> get_element(Rest);
	no -> throw({error, {unexpected_text, T#xmlText.value}})
    end.

decode_members(Content) ->
    case match_element(normal, [member], Content) of
	{error, {missing_element, _}} -> [];
	{Member, Rest} ->
	    {Name, Rest2} = match_element([name], Member#xmlElement.content),
	    TextValue = get_text_value(Name#xmlElement.content),
	    {Value, _} = match_element([value], Rest2),
	    [{list_to_atom(TextValue),
	      decode(Value#xmlElement.content)}|decode_members(Rest)]
    end.

decode_values([]) -> [];
decode_values(Content) ->
    case match_element(normal, [value], Content) of
	{error, {missing_element, _}} -> [];
	{Value, Rest} ->
	    [decode(Value#xmlElement.content)|decode_values(Rest)]
    end.

make_integer(Integer) ->
    case catch list_to_integer(Integer) of
	{'EXIT', _Reason} -> throw({error, {not_integer, Integer}});
	Value -> Value
    end.

make_double(Double) ->
    case catch list_to_float(Double) of
	{'EXIT', _} ->
            case catch list_to_integer(Double) of
	       {'EXIT', _} ->
                    throw({error, {not_double, Double}});
                Value -> float(Value)
        end;
	Value -> Value
    end.

% FIXME
%ensure_iso8601_date(Date) ->
%    case xmlrpc_util:is_iso8601_date(Date) of
%	no -> throw({error, {not_iso8601_date, Date}});
%	yes -> Date
%    end.
%
%ensure_base64(Base64) ->
%    case xmlrpc_util:is_base64(Base64) of
%	no -> throw({error, {not_base64, Base64}});
%	yes -> Base64
%    end.
