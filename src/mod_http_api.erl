%%%----------------------------------------------------------------------
%%% File    : mod_http_api.erl
%%% Author  : Christophe romain <christophe.romain@process-one.net>
%%% Purpose : Implements REST API for ejabberd using JSON data
%%% Created : 15 Sep 2014 by Christophe Romain <christophe.romain@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2015   ProcessOne
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

%% Example config:
%%
%%  in ejabberd_http listener
%%    request_handlers:
%%      "/api": mod_http_api
%% 
%% Access rights are defined with:
%% commands_admin_access: configure 
%% commands:
%%   - add_commands: user
%%
%%
%% add_commands allow exporting a class of commands, from
%%   open: methods is not risky and can be called by without any access check
%%   restricted (default): the same, but will appear only in ejabberdctl list.
%%   admin – auth is required with XMLRPC and HTTP API and checked for admin priviledges, works as usual in ejabberdctl.
%%   user - can be used through XMLRPC and HTTP API, even by user. Only admin can use the commands for other users.
%%
%% Then to perform an action, send a POST request to the following URL:
%% http://localhost:5280/api/<call_name>

-module(mod_http_api).

-author('cromain@process-one.net').

-behaviour(gen_mod).

-export([start/2, stop/1, process/2, mod_opt_type/1]).

-include("ejabberd.hrl").
-include("jlib.hrl").
-include("logger.hrl").
-include("ejabberd_http.hrl").

-define(CT_PLAIN,
        {<<"Content-Type">>, <<"text/plain">>}).

-define(CT_XML,
        {<<"Content-Type">>, <<"text/xml; charset=utf-8">>}).

-define(CT_JSON,
        {<<"Content-Type">>, <<"application/json">>}).

-define(AC_ALLOW_ORIGIN,
        {<<"Access-Control-Allow-Origin">>, <<"*">>}).

-define(AC_ALLOW_METHODS,
        {<<"Access-Control-Allow-Methods">>,
         <<"GET, POST, OPTIONS">>}).

-define(AC_ALLOW_HEADERS,
        {<<"Access-Control-Allow-Headers">>,
         <<"Content-Type">>}).

-define(AC_MAX_AGE,
        {<<"Access-Control-Max-Age">>, <<"86400">>}).

-define(OPTIONS_HEADER,
        [?CT_PLAIN, ?AC_ALLOW_ORIGIN, ?AC_ALLOW_METHODS,
         ?AC_ALLOW_HEADERS, ?AC_MAX_AGE]).

-define(HEADER(CType),
        [CType, ?AC_ALLOW_ORIGIN, ?AC_ALLOW_HEADERS]).

%% -------------------
%% Module control
%% -------------------

start(_Host, _Opts) ->
    ok.

stop(_Host) ->
    ok.


%% ----------
%% basic auth
%% ----------

check_permissions(#request{auth = HTTPAuth, headers = Headers}, Command)
  when HTTPAuth /= undefined ->
    case catch binary_to_existing_atom(Command, utf8) of
        Call when is_atom(Call) ->
            Admin =
                case lists:keysearch(<<"X-Admin">>, 1, Headers) of
                    {value, {_, <<"true">>}} -> true;
                    _ -> false
                end,
            Auth =
                case HTTPAuth of
                    {SJID, Pass} ->
                        case jid:from_string(SJID) of
                            #jid{user = User, server = Server} ->
                                case ejabberd_auth:check_password(User, Server, Pass) of
                                    true -> {ok, {User, Server, Pass, Admin}};
                                    false -> false
                                end;
                            _ ->
                                false
                        end;
                    {oauth, Token, _} ->
                        case ejabberd_oauth:check_token(Command, Token) of
                            {ok, User, Server} ->
                                {ok, {User, Server, {oauth, Token}, Admin}};
                            false ->
                                false
                        end;
                    _ ->
                        false
                end,
            case Auth of
                {ok, A} -> {allowed, Call, A};
                _ -> unauthorized_response()
            end;
        _ ->
            unauthorized_response()
    end;
check_permissions(_, _Command) ->
    unauthorized_response().

%% ------------------
%% command processing
%% ------------------

process(_, #request{method = 'POST', data = <<>>}) ->
    ?DEBUG("Bad Request: no data", []),
    badrequest_response();
process([Call], #request{method = 'POST', data = Data, ip = IP} = Req) ->
    try
        Args = case jiffy:decode(Data) of
            List when is_list(List) -> List;
            {List} when is_list(List) -> List;
            Other -> [Other]
        end,
        log(Call, Args, IP),
        case check_permissions(Req, Call) of
            {allowed, Cmd, Auth} ->
                {Code, Result} = handle(Cmd, Auth, Args),
                json_response(Code, jiffy:encode(Result));
            ErrorResponse ->
                ErrorResponse
        end
    catch _:Error ->
        ?DEBUG("Bad Request: ~p", [Error]),
        badrequest_response()
    end;
process([Call], #request{method = 'GET', q = Data, ip = IP} = Req) ->
    try
        Args = case Data of
            [{nokey, <<>>}] -> [];
            _ -> Data
        end,
        log(Call, Args, IP),
        case check_permissions(Req, Call) of
            {allowed, Cmd, Auth} ->
                {Code, Result} = handle(Cmd, Auth, Args),
                json_response(Code, jiffy:encode(Result));
            ErrorResponse ->
                ErrorResponse
        end
    catch _:Error ->
        ?DEBUG("Bad Request: ~p", [Error]),
        badrequest_response()
    end;
process([], #request{method = 'OPTIONS', data = <<>>}) ->
    {200, ?OPTIONS_HEADER, []};
process(_Path, Request) ->
    ?DEBUG("Bad Request: no handler ~p", [Request]),
    badrequest_response().

%% ----------------
%% command handlers
%% ----------------

% generic ejabberd command handler
handle(Call, Auth, Args) when is_atom(Call), is_list(Args) ->
    case ejabberd_commands:get_command_format(Call, Auth) of
        {ArgsSpec, _} when is_list(ArgsSpec) ->
            Args2 = [{jlib:binary_to_atom(Key), Value} || {Key, Value} <- Args],
            Spec = lists:foldr(
                    fun ({Key, binary}, Acc) ->
                            [{Key, <<>>}|Acc];
                        ({Key, string}, Acc) ->
                            [{Key, <<>>}|Acc];
                        ({Key, integer}, Acc) ->
                            [{Key, 0}|Acc];
                        ({Key, {list, _}}, Acc) ->
                            [{Key, []}|Acc];
                        ({Key, atom}, Acc) ->
                            [{Key, undefined}|Acc]
                    end, [], ArgsSpec),
            handle2(Call, Auth, match(Args2, Spec));
        {error, Msg} ->
            {400, Msg};
        _Error ->
            {400, <<"Error">>}
    end.

handle2(Call, Auth, Args) when is_atom(Call), is_list(Args) ->
    {ArgsF, _ResultF} = ejabberd_commands:get_command_format(Call, Auth),
    ArgsFormatted = format_args(Args, ArgsF),
    case ejabberd_command(Auth, Call, ArgsFormatted, 400) of
        0 -> {200, <<"OK">>};
        1 -> {500, <<"500 Internal server error">>};
        400 -> {400, <<"400 Bad Request">>};
        404 -> {404, <<"404 Not found">>};
        Res -> format_command_result(Call, Auth, Res)
    end.

get_elem_delete(A, L) ->
    case proplists:get_all_values(A, L) of
      [Value] -> {Value, proplists:delete(A, L)};
      [_, _ | _] ->
	  %% Crash reporting the error
	  exit({duplicated_attribute, A, L});
      [] ->
	  %% Report the error and then force a crash
	  exit({attribute_not_found, A, L})
    end.

format_args(Args, ArgsFormat) ->
    {ArgsRemaining, R} = lists:foldl(fun ({ArgName,
					   ArgFormat},
					  {Args1, Res}) ->
					     {ArgValue, Args2} =
						 get_elem_delete(ArgName,
								 Args1),
					     Formatted = format_arg(ArgValue,
								    ArgFormat),
					     {Args2, Res ++ [Formatted]}
				     end,
				     {Args, []}, ArgsFormat),
    case ArgsRemaining of
      [] -> R;
      L when is_list(L) -> exit({additional_unused_args, L})
    end.

format_arg({array, Elements},
	   {list, {ElementDefName, ElementDefFormat}})
    when is_list(Elements) ->
    lists:map(fun ({struct, [{ElementName, ElementValue}]}) when
                        ElementDefName == ElementName ->
		      format_arg(ElementValue, ElementDefFormat)
	      end,
	      Elements);
format_arg({array, [{struct, Elements}]},
	   {list, {ElementDefName, ElementDefFormat}})
    when is_list(Elements) ->
    lists:map(fun ({ElementName, ElementValue}) ->
		      true = ElementDefName == ElementName,
		      format_arg(ElementValue, ElementDefFormat)
	      end,
	      Elements);
format_arg({array, [{struct, Elements}]},
	   {tuple, ElementsDef})
    when is_list(Elements) ->
    FormattedList = format_args(Elements, ElementsDef),
    list_to_tuple(FormattedList);
format_arg({array, Elements}, {list, ElementsDef})
    when is_list(Elements) and is_atom(ElementsDef) ->
    [format_arg(Element, ElementsDef)
     || Element <- Elements];
format_arg(Arg, integer) when is_integer(Arg) -> Arg;
format_arg(Arg, binary) when is_list(Arg) -> process_unicode_codepoints(Arg);
format_arg(Arg, binary) when is_binary(Arg) -> Arg;
format_arg(Arg, string) when is_list(Arg) -> process_unicode_codepoints(Arg);
format_arg(Arg, string) when is_binary(Arg) -> Arg;
format_arg(undefined, binary) -> <<>>;
format_arg(undefined, string) -> <<>>;
format_arg(Arg, Format) ->
    ?ERROR_MSG("don't know how to format Arg ~p for format ~p", [Arg, Format]),
    error.

process_unicode_codepoints(Str) ->
    iolist_to_binary(lists:map(fun(X) when X > 255 -> unicode:characters_to_binary([X]);
                                  (Y) -> Y
                               end, Str)).

%% ----------------
%% internal helpers
%% ----------------

match(Args, Spec) ->
    [{Key, proplists:get_value(Key, Args, Default)} || {Key, Default} <- Spec].

ejabberd_command(Auth, Cmd, Args, Default) ->
    case catch ejabberd_commands:execute_command(undefined, Auth, Cmd, Args) of
        {'EXIT', _} -> Default;
        {error, _} -> Default;
        Result -> Result
    end.

format_command_result(Cmd, Auth, Result) ->
    {_, ResultFormat} = ejabberd_commands:get_command_format(Cmd, Auth),
    case {ResultFormat, Result} of
        {{_, rescode}, V} when V == true; V == ok ->
            {200, <<"">>};
        {{_, rescode}, _} ->
            {500, <<"">>};
        {{_, restuple}, {V1, Text1}} when V1 == true; V1 == ok ->
            {200, iolist_to_binary(Text1)};
        {{_, restuple}, {_, Text2}} ->
            {500, iolist_to_binary(Text2)};
        {{_, {list, _}}, _V} ->
            {_, L} = format_result(Result, ResultFormat),
            {200, L};
        {{_, {tuple, _}}, _V} ->
            {_, T} = format_result(Result, ResultFormat),
            {200, T};
        _ ->
            {200, {[format_result(Result, ResultFormat)]}}
    end.

format_result(Atom, {Name, atom}) ->
    {jlib:atom_to_binary(Name), jlib:atom_to_binary(Atom)};

format_result(Int, {Name, integer}) ->
    {jlib:atom_to_binary(Name), Int};

format_result(String, {Name, string}) ->
    {jlib:atom_to_binary(Name), iolist_to_binary(String)};

format_result(Code, {Name, rescode}) ->
    {jlib:atom_to_binary(Name), Code == true orelse Code == ok};

format_result({Code, Text}, {Name, restuple}) ->
    {jlib:atom_to_binary(Name),
     {[{<<"res">>, Code == true orelse Code == ok},
       {<<"text">>, iolist_to_binary(Text)}]}};

format_result(Els, {Name, {list, {_, {tuple, [{_, atom}, _]}} = Fmt}}) ->
    {jlib:atom_to_binary(Name), {[format_result(El, Fmt) || El <- Els]}};

format_result(Els, {Name, {list, Def}}) ->
    {jlib:atom_to_binary(Name), [element(2, format_result(El, Def)) || El <- Els]};

format_result(Tuple, {_Name, {tuple, [{_, atom}, ValFmt]}}) ->
    {Name2, Val} = Tuple,
    {_, Val2} = format_result(Val, ValFmt),
    {jlib:atom_to_binary(Name2), Val2};

format_result(Tuple, {Name, {tuple, Def}}) ->
    Els = lists:zip(tuple_to_list(Tuple), Def),
    {jlib:atom_to_binary(Name), {[format_result(El, ElDef) || {El, ElDef} <- Els]}};

format_result(404, {_Name, _}) ->
    "not_found".

unauthorized_response() ->
    {401, ?HEADER(?CT_XML),
     #xmlel{name = <<"h1">>, attrs = [],
            children = [{xmlcdata, <<"401 Unauthorized">>}]}}.

badrequest_response() ->
    {400, ?HEADER(?CT_XML),
     #xmlel{name = <<"h1">>, attrs = [],
            children = [{xmlcdata, <<"400 Bad Request">>}]}}.
json_response(Code, Body) when is_integer(Code) ->
    {Code, ?HEADER(?CT_JSON), Body}.

log(Call, Args, {Addr, Port}) ->
    AddrS = jlib:ip_to_list({Addr, Port}),
    ?INFO_MSG("Admin call ~s ~p from ~s:~p", [Call, Args, AddrS, Port]).

mod_opt_type(access) ->
    fun(Access) when is_atom(Access) -> Access end;
mod_opt_type(_) -> [access].
