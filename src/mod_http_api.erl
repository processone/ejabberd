%%%----------------------------------------------------------------------
%%% File    : mod_http_api.erl
%%% Author  : Christophe romain <christophe.romain@process-one.net>
%%% Purpose : Implements REST API for ejabberd using JSON data
%%% Created : 15 Sep 2014 by Christophe Romain <christophe.romain@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2016   ProcessOne
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
%% To use a specific API version N, add a vN element in the URL path:
%%  in ejabberd_http listener
%%    request_handlers:
%%      "/api/v2": mod_http_api
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
%%
%% It's also possible to enable unrestricted access to some commands from group
%% of IP addresses by using option `admin_ip_access` by having fragment like
%% this in configuration file:
%%   modules:
%%     mod_http_api:
%%       admin_ip_access: admin_ip_access_rule
%%...
%%   access:
%%     admin_ip_access_rule:
%%       admin_ip_acl:
%%         - command1
%%         - command2
%%         %% use `all` to give access to all commands
%%...
%%   acl:
%%     admin_ip_acl:
%%       ip:
%%         - "127.0.0.1/8"

-module(mod_http_api).

-author('cromain@process-one.net').

-behaviour(gen_mod).

-export([start/2, stop/1, process/2, mod_opt_type/1, depends/2]).

-include("ejabberd.hrl").
-include("jlib.hrl").
-include("logger.hrl").
-include("ejabberd_http.hrl").

-define(DEFAULT_API_VERSION, 0).

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

depends(_Host, _Opts) ->
    [].

%% ----------
%% basic auth
%% ----------

check_permissions(Request, Command) ->
    case catch binary_to_existing_atom(Command, utf8) of
        Call when is_atom(Call) ->
            {ok, CommandPolicy, Scope} = ejabberd_commands:get_command_policy_and_scope(Call),
            check_permissions2(Request, Call, CommandPolicy, Scope);
        _ ->
            %% TODO Should this be a 404 or 400 instead of 401 ?
            unauthorized_response()
    end.

check_permissions2(#request{auth = HTTPAuth, headers = Headers}, Call, _, ScopeList)
  when HTTPAuth /= undefined ->
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
                        case ejabberd_auth:check_password(User, <<"">>, Server, Pass) of
                            true -> {ok, {User, Server, Pass, Admin}};
                            false -> false
                        end;
                    _ ->
                        false
                end;
            {oauth, Token, _} ->
                case oauth_check_token(ScopeList, Token) of
                    {ok, user, {User, Server}} ->
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
check_permissions2(_Request, Call, open, _Scope) ->
    {allowed, Call, noauth};
check_permissions2(#request{ip={IP, _Port}}, Call, _Policy, _Scope) ->
    Access = gen_mod:get_module_opt(global, ?MODULE, admin_ip_access,
                                    fun(V) -> V end,
                                    none),
    Res = acl:match_rule(global, Access, IP),
    case Res of
        all ->
            {allowed, Call, admin};
        [all] ->
            {allowed, Call, admin};
        allow ->
            {allowed, Call, admin};
        Commands when is_list(Commands) ->
            case lists:member(Call, Commands) of
                true -> {allowed, Call, admin};
                _ -> unauthorized_response()
            end;
        _E ->
            {allowed, Call, noauth}
    end;
check_permissions2(_Request, _Call, _Policy, _Scope) ->
    unauthorized_response().

oauth_check_token(ScopeList, Token) when is_list(ScopeList) ->
    ejabberd_oauth:check_token(ScopeList, Token).

%% ------------------
%% command processing
%% ------------------

%process(Call, Request) ->
%    ?DEBUG("~p~n~p", [Call, Request]), ok;
process(_, #request{method = 'POST', data = <<>>}) ->
    ?DEBUG("Bad Request: no data", []),
    badrequest_response(<<"Missing POST data">>);
process([Call], #request{method = 'POST', data = Data, ip = {IP, _} = IPPort} = Req) ->
    Version = get_api_version(Req),
    try
        Args = case jiffy:decode(Data) of
            List when is_list(List) -> List;
            {List} when is_list(List) -> List;
            Other -> [Other]
        end,
        log(Call, Args, IPPort),
        case check_permissions(Req, Call) of
            {allowed, Cmd, Auth} ->
                case handle(Cmd, Auth, Args, Version, IP) of
                    {Code, Result} ->
                        json_response(Code, jiffy:encode(Result));
                    {HTMLCode, JSONErrorCode, Message} ->
                        json_error(HTMLCode, JSONErrorCode, Message)
                    end;
            %% Warning: check_permission direcly formats 401 reply if not authorized
            ErrorResponse ->
                ErrorResponse
        end
    catch _:{error,{_,invalid_json}} = _Err ->
	    ?DEBUG("Bad Request: ~p", [_Err]),
	    badrequest_response(<<"Invalid JSON input">>);
	  _:_Error ->
            ?DEBUG("Bad Request: ~p ~p", [_Error, erlang:get_stacktrace()]),
            badrequest_response()
    end;
process([Call], #request{method = 'GET', q = Data, ip = IP} = Req) ->
    Version = get_api_version(Req),
    try
        Args = case Data of
                   [{nokey, <<>>}] -> [];
                   _ -> Data
               end,
        log(Call, Args, IP),
        case check_permissions(Req, Call) of
            {allowed, Cmd, Auth} ->
                {Code, Result} = handle(Cmd, Auth, Args, Version, IP),
                json_response(Code, jiffy:encode(Result));
            %% Warning: check_permission direcly formats 401 reply if not authorized
            ErrorResponse ->
                ErrorResponse
        end
    catch _:_Error ->
        ?DEBUG("Bad Request: ~p ~p", [_Error, erlang:get_stacktrace()]),
        badrequest_response()
    end;
process([], #request{method = 'OPTIONS', data = <<>>}) ->
    {200, ?OPTIONS_HEADER, []};
process(_Path, Request) ->
    ?DEBUG("Bad Request: no handler ~p", [Request]),
    badrequest_response().

% get API version N from last "vN" element in URL path
get_api_version(#request{path = Path}) ->
    get_api_version(lists:reverse(Path));
get_api_version([<<"v", String/binary>> | Tail]) ->
    case catch jlib:binary_to_integer(String) of
	N when is_integer(N) ->
	    N;
	_ ->
	    get_api_version(Tail)
    end;
get_api_version([_Head | Tail]) ->
    get_api_version(Tail);
get_api_version([]) ->
    ?DEFAULT_API_VERSION.

%% ----------------
%% command handlers
%% ----------------

%% TODO Check accept types of request before decided format of reply.

% generic ejabberd command handler
handle(Call, Auth, Args, Version, IP) when is_atom(Call), is_list(Args) ->
    case ejabberd_commands:get_command_format(Call, Auth, Version) of
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
	    try
		handle2(Call, Auth, match(Args2, Spec), Version, IP)
	    catch throw:not_found ->
		    {404, <<"not_found">>};
		  throw:{not_found, Why} when is_atom(Why) ->
		    {404, jlib:atom_to_binary(Why)};
		  throw:{not_found, Msg} ->
		    {404, iolist_to_binary(Msg)};
		  throw:not_allowed ->
		    {401, <<"not_allowed">>};
		  throw:{not_allowed, Why} when is_atom(Why) ->
		    {401, jlib:atom_to_binary(Why)};
		  throw:{not_allowed, Msg} ->
		    {401, iolist_to_binary(Msg)};
      throw:{error, account_unprivileged} ->
        {403, 31, <<"Command need to be run with admin priviledge.">>};
		  throw:{invalid_parameter, Msg} ->
		    {400, iolist_to_binary(Msg)};
		  throw:{error, Why} when is_atom(Why) ->
		    {400, jlib:atom_to_binary(Why)};
		  throw:{error, Msg} ->
		    {400, iolist_to_binary(Msg)};
		  throw:Error when is_atom(Error) ->
		    {400, jlib:atom_to_binary(Error)};
		  throw:Msg when is_list(Msg); is_binary(Msg) ->
		    {400, iolist_to_binary(Msg)};
		  _Error ->
		    ?ERROR_MSG("REST API Error: ~p ~p", [_Error, erlang:get_stacktrace()]),
		    {500, <<"internal_error">>}
	    end;
        {error, Msg} ->
	    ?ERROR_MSG("REST API Error: ~p", [Msg]),
            {400, Msg};
        _Error ->
	    ?ERROR_MSG("REST API Error: ~p", [_Error]),
            {400, <<"Error">>}
    end.

handle2(Call, Auth, Args, Version, IP) when is_atom(Call), is_list(Args) ->
    {ArgsF, _ResultF} = ejabberd_commands:get_command_format(Call, Auth, Version),
    ArgsFormatted = format_args(Args, ArgsF),
    ejabberd_command(Auth, Call, ArgsFormatted, Version, IP).

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

format_arg(Elements,
	   {list, {_ElementDefName, ElementDefFormat}})
    when is_list(Elements) ->
    [format_arg(Element, ElementDefFormat)
     || Element <- Elements];
format_arg({[{Name, Value}]},
	   {tuple, [{_Tuple1N, Tuple1S}, {_Tuple2N, Tuple2S}]})
  when Tuple1S == binary;
       Tuple1S == string ->
    {format_arg(Name, Tuple1S), format_arg(Value, Tuple2S)};
format_arg({Elements},
	   {tuple, ElementsDef})
    when is_list(Elements) ->
    F = lists:map(fun({TElName, TElDef}) ->
			  case lists:keyfind(atom_to_binary(TElName, latin1), 1, Elements) of
			      {_, Value} ->
				  format_arg(Value, TElDef);
			      _ when TElDef == binary; TElDef == string ->
				  <<"">>;
			      _ ->
				  ?ERROR_MSG("missing field ~p in tuple ~p", [TElName, Elements]),
				  throw({invalid_parameter,
					 io_lib:format("Missing field ~w in tuple ~w", [TElName, Elements])})
			  end
		  end, ElementsDef),
    list_to_tuple(F);
format_arg(Elements, {list, ElementsDef})
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
    throw({invalid_parameter,
	   io_lib:format("Arg ~w is not in format ~w",
			 [Arg, Format])}).

process_unicode_codepoints(Str) ->
    iolist_to_binary(lists:map(fun(X) when X > 255 -> unicode:characters_to_binary([X]);
                                  (Y) -> Y
                               end, Str)).

%% ----------------
%% internal helpers
%% ----------------

match(Args, Spec) ->
    [{Key, proplists:get_value(Key, Args, Default)} || {Key, Default} <- Spec].

ejabberd_command(Auth, Cmd, Args, Version, IP) ->
    Access = case Auth of
                 admin -> [];
                 _ -> undefined
             end,
    case ejabberd_commands:execute_command(Access, Auth, Cmd, Args, Version, #{ip => IP}) of
        {error, Error} ->
            throw(Error);
        Res ->
            format_command_result(Cmd, Auth, Res, Version)
    end.

format_command_result(Cmd, Auth, Result, Version) ->
    {_, ResultFormat} = ejabberd_commands:get_command_format(Cmd, Auth, Version),
    case {ResultFormat, Result} of
	{{_, rescode}, V} when V == true; V == ok ->
	    {200, 0};
	{{_, rescode}, _} ->
	    {200, 1};
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
    json_error(401, 10, <<"Oauth Token is invalid or expired.">>).

badrequest_response() ->
    badrequest_response(<<"400 Bad Request">>).
badrequest_response(Body) ->
    json_response(400, jiffy:encode(Body)).

json_response(Code, Body) when is_integer(Code) ->
    {Code, ?HEADER(?CT_JSON), Body}.

%% HTTPCode, JSONCode = integers
%% message is binary
json_error(HTTPCode, JSONCode, Message) ->
    {HTTPCode, ?HEADER(?CT_JSON),
     jiffy:encode({[{<<"status">>, <<"error">>},
                    {<<"code">>, JSONCode},
                    {<<"message">>, Message}]})
    }.

log(Call, Args, {Addr, Port}) ->
    AddrS = jlib:ip_to_list({Addr, Port}),
    ?INFO_MSG("API call ~s ~p from ~s:~p", [Call, Args, AddrS, Port]);
log(Call, Args, IP) ->
    ?INFO_MSG("API call ~s ~p (~p)", [Call, Args, IP]).

mod_opt_type(admin_ip_access) -> fun acl:access_rules_validator/1;
mod_opt_type(_) -> [admin_ip_access].
