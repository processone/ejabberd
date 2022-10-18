%%%----------------------------------------------------------------------
%%% File    : mod_http_api.erl
%%% Author  : Christophe romain <christophe.romain@process-one.net>
%%% Purpose : Implements REST API for ejabberd using JSON data
%%% Created : 15 Sep 2014 by Christophe Romain <christophe.romain@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2022   ProcessOne
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

-module(mod_http_api).

-author('cromain@process-one.net').

-behaviour(gen_mod).

-export([start/2, stop/1, reload/3, process/2, depends/2,
         format_arg/2,
	 mod_options/1, mod_doc/0]).

-include_lib("xmpp/include/xmpp.hrl").
-include("logger.hrl").
-include("ejabberd_http.hrl").
-include("ejabberd_stacktrace.hrl").
-include("translate.hrl").

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
         <<"Content-Type, Authorization, X-Admin">>}).

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

reload(_Host, _NewOpts, _OldOpts) ->
    ok.

depends(_Host, _Opts) ->
    [].

%% ----------
%% basic auth
%% ----------

extract_auth(#request{auth = HTTPAuth, ip = {IP, _}, opts = Opts}) ->
    Info = case HTTPAuth of
	       {SJID, Pass} ->
		   try jid:decode(SJID) of
		       #jid{luser = User, lserver = Server} ->
			   case ejabberd_auth:check_password(User, <<"">>, Server, Pass) of
			       true ->
				   #{usr => {User, Server, <<"">>}, caller_server => Server};
			       false ->
				   {error, invalid_auth}
			   end
		   catch _:{bad_jid, _} ->
		       {error, invalid_auth}
		   end;
	       {oauth, Token, _} ->
		   case ejabberd_oauth:check_token(Token) of
		       {ok, {U, S}, Scope} ->
			   #{usr => {U, S, <<"">>}, oauth_scope => Scope, caller_server => S};
		       {false, Reason} ->
			   {error, Reason}
		   end;
	       invalid ->
		   {error, invalid_auth};
	       _ ->
		   #{}
	   end,
    case Info of
	Map when is_map(Map) ->
	    Tag = proplists:get_value(tag, Opts, <<>>),
	    Map#{caller_module => ?MODULE, ip => IP, tag => Tag};
	_ ->
	    ?DEBUG("Invalid auth data: ~p", [Info]),
	    Info
    end.

%% ------------------
%% command processing
%% ------------------

%process(Call, Request) ->
%    ?DEBUG("~p~n~p", [Call, Request]), ok;
process(_, #request{method = 'POST', data = <<>>}) ->
    ?DEBUG("Bad Request: no data", []),
    badrequest_response(<<"Missing POST data">>);
process([Call], #request{method = 'POST', data = Data, ip = IPPort} = Req) ->
    Version = get_api_version(Req),
    try
        Args = extract_args(Data),
        log(Call, Args, IPPort),
	perform_call(Call, Args, Req, Version)
    catch
        %% TODO We need to refactor to remove redundant error return formatting
        throw:{error, unknown_command} ->
            json_format({404, 44, <<"Command not found.">>});
        _:{error,{_,invalid_json}} = _Err ->
	    ?DEBUG("Bad Request: ~p", [_Err]),
	    badrequest_response(<<"Invalid JSON input">>);
	?EX_RULE(_Class, _Error, Stack) ->
	    StackTrace = ?EX_STACK(Stack),
            ?DEBUG("Bad Request: ~p ~p", [_Error, StackTrace]),
            badrequest_response()
    end;
process([Call], #request{method = 'GET', q = Data, ip = {IP, _}} = Req) ->
    Version = get_api_version(Req),
    try
        Args = case Data of
                   [{nokey, <<>>}] -> [];
                   _ -> Data
               end,
        log(Call, Args, IP),
	perform_call(Call, Args, Req, Version)
    catch
        %% TODO We need to refactor to remove redundant error return formatting
        throw:{error, unknown_command} ->
            json_format({404, 44, <<"Command not found.">>});
        ?EX_RULE(_, _Error, Stack) ->
	    StackTrace = ?EX_STACK(Stack),
            ?DEBUG("Bad Request: ~p ~p", [_Error, StackTrace]),
            badrequest_response()
    end;
process([_Call], #request{method = 'OPTIONS', data = <<>>}) ->
    {200, ?OPTIONS_HEADER, []};
process(_, #request{method = 'OPTIONS'}) ->
    {400, ?OPTIONS_HEADER, []};
process(_Path, Request) ->
    ?DEBUG("Bad Request: no handler ~p", [Request]),
    json_error(400, 40, <<"Missing command name.">>).

perform_call(Command, Args, Req, Version) ->
    case catch binary_to_existing_atom(Command, utf8) of
	Call when is_atom(Call) ->
	    case extract_auth(Req) of
		{error, expired} -> invalid_token_response();
		{error, not_found} -> invalid_token_response();
		{error, invalid_auth} -> unauthorized_response();
		Auth when is_map(Auth) ->
		    Result = handle(Call, Auth, Args, Version),
		    json_format(Result)
	    end;
	_ ->
	    json_error(404, 40, <<"Endpoint not found.">>)
    end.

%% Be tolerant to make API more easily usable from command-line pipe.
extract_args(<<"\n">>) -> [];
extract_args(Data) ->
    case jiffy:decode(Data) of
        List when is_list(List) -> List;
        {List} when is_list(List) -> List;
        Other -> [Other]
    end.

% get API version N from last "vN" element in URL path
get_api_version(#request{path = Path}) ->
    get_api_version(lists:reverse(Path));
get_api_version([<<"v", String/binary>> | Tail]) ->
    case catch binary_to_integer(String) of
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
handle(Call, Auth, Args, Version) when is_atom(Call), is_list(Args) ->
    Args2 = [{misc:binary_to_atom(Key), Value} || {Key, Value} <- Args],
    try handle2(Call, Auth, Args2, Version)
    catch throw:not_found ->
	    {404, <<"not_found">>};
	  throw:{not_found, Why} when is_atom(Why) ->
	    {404, misc:atom_to_binary(Why)};
	  throw:{not_found, Msg} ->
	    {404, iolist_to_binary(Msg)};
	  throw:not_allowed ->
	    {401, <<"not_allowed">>};
	  throw:{not_allowed, Why} when is_atom(Why) ->
	    {401, misc:atom_to_binary(Why)};
	  throw:{not_allowed, Msg} ->
	    {401, iolist_to_binary(Msg)};
	  throw:{error, account_unprivileged} ->
	    {403, 31, <<"Command need to be run with admin privilege.">>};
	  throw:{error, access_rules_unauthorized} ->
	    {403, 32, <<"AccessRules: Account does not have the right to perform the operation.">>};
	  throw:{invalid_parameter, Msg} ->
	    {400, iolist_to_binary(Msg)};
	  throw:{error, Why} when is_atom(Why) ->
	    {400, misc:atom_to_binary(Why)};
	  throw:{error, Msg} ->
	    {400, iolist_to_binary(Msg)};
	  throw:Error when is_atom(Error) ->
	    {400, misc:atom_to_binary(Error)};
	  throw:Msg when is_list(Msg); is_binary(Msg) ->
	    {400, iolist_to_binary(Msg)};
	  ?EX_RULE(Class, Error, Stack) ->
	    StackTrace = ?EX_STACK(Stack),
	    ?ERROR_MSG("REST API Error: "
		       "~ts(~p) -> ~p:~p ~p",
		       [Call, hide_sensitive_args(Args),
			Class, Error, StackTrace]),
	    {500, <<"internal_error">>}
    end.

handle2(Call, Auth, Args, Version) when is_atom(Call), is_list(Args) ->
    {ArgsF, ArgsR, _ResultF} = ejabberd_commands:get_command_format(Call, Auth, Version),
    ArgsFormatted = format_args(Call, rename_old_args(Args, ArgsR), ArgsF),
    case ejabberd_commands:execute_command2(Call, ArgsFormatted, Auth, Version) of
	{error, Error} ->
	    throw(Error);
	Res ->
	    format_command_result(Call, Auth, Res, Version)
    end.

rename_old_args(Args, []) ->
    Args;
rename_old_args(Args, [{OldName, NewName} | ArgsR]) ->
    Args2 = case lists:keytake(OldName, 1, Args) of
	{value, {OldName, Value}, ArgsTail} ->
	    [{NewName, Value} | ArgsTail];
	false ->
	    Args
    end,
    rename_old_args(Args2, ArgsR).

get_elem_delete(Call, A, L, F) ->
    case proplists:get_all_values(A, L) of
      [Value] -> {Value, proplists:delete(A, L)};
      [_, _ | _] ->
	  ?INFO_MSG("Command ~ts call rejected, it has duplicate attribute ~w",
		    [Call, A]),
	  throw({invalid_parameter,
		 io_lib:format("Request have duplicate argument: ~w", [A])});
      [] ->
	  case F of
	      {list, _} ->
		  {[], L};
	      _ ->
		  ?INFO_MSG("Command ~ts call rejected, missing attribute ~w",
			    [Call, A]),
		  throw({invalid_parameter,
			 io_lib:format("Request have missing argument: ~w", [A])})
	  end
    end.

format_args(Call, Args, ArgsFormat) ->
    {ArgsRemaining, R} = lists:foldl(fun ({ArgName,
					   ArgFormat},
					  {Args1, Res}) ->
					     {ArgValue, Args2} =
						 get_elem_delete(Call, ArgName,
								 Args1, ArgFormat),
					     Formatted = format_arg(ArgValue,
								    ArgFormat),
					     {Args2, Res ++ [Formatted]}
				     end,
				     {Args, []}, ArgsFormat),
    case ArgsRemaining of
      [] -> R;
      L when is_list(L) ->
	  ExtraArgs = [N || {N, _} <- L],
	  ?INFO_MSG("Command ~ts call rejected, it has unknown arguments ~w",
	      [Call, ExtraArgs]),
	  throw({invalid_parameter,
		 io_lib:format("Request have unknown arguments: ~w", [ExtraArgs])})
    end.

format_arg({Elements},
	   {list, {_ElementDefName, {tuple, [{_Tuple1N, Tuple1S}, {_Tuple2N, Tuple2S}]} = Tuple}})
    when is_list(Elements) andalso
	 (Tuple1S == binary orelse Tuple1S == string) ->
    lists:map(fun({F1, F2}) ->
		      {format_arg(F1, Tuple1S), format_arg(F2, Tuple2S)};
		 ({Val}) when is_list(Val) ->
		      format_arg({Val}, Tuple)
	      end, Elements);
format_arg(Elements,
	   {list, {_ElementDefName, {list, _} = ElementDefFormat}})
    when is_list(Elements) ->
    [{format_arg(Element, ElementDefFormat)}
     || Element <- Elements];
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
				  ?ERROR_MSG("Missing field ~p in tuple ~p", [TElName, Elements]),
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
format_arg(Arg, string) when is_list(Arg) -> Arg;
format_arg(Arg, string) when is_binary(Arg) -> binary_to_list(Arg);
format_arg(undefined, binary) -> <<>>;
format_arg(undefined, string) -> "";
format_arg(Arg, Format) ->
    ?ERROR_MSG("Don't know how to format Arg ~p for format ~p", [Arg, Format]),
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

format_command_result(Cmd, Auth, Result, Version) ->
    {_, _, ResultFormat} = ejabberd_commands:get_command_format(Cmd, Auth, Version),
    case {ResultFormat, Result} of
	{{_, rescode}, V} when V == true; V == ok ->
	    {200, 0};
	{{_, rescode}, _} ->
	    {200, 1};
        {_, {error, ErrorAtom, Code, Msg}} ->
            format_error_result(ErrorAtom, Code, Msg);
        {{_, restuple}, {V, Text}} when V == true; V == ok ->
            {200, iolist_to_binary(Text)};
        {{_, restuple}, {ErrorAtom, Msg}} ->
            format_error_result(ErrorAtom, 0, Msg);
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
    {misc:atom_to_binary(Name), misc:atom_to_binary(Atom)};

format_result(Int, {Name, integer}) ->
    {misc:atom_to_binary(Name), Int};

format_result([String | _] = StringList, {Name, string}) when is_list(String) ->
    Binarized = iolist_to_binary(string:join(StringList, "\n")),
    {misc:atom_to_binary(Name), Binarized};

format_result(String, {Name, string}) ->
    {misc:atom_to_binary(Name), iolist_to_binary(String)};

format_result(Code, {Name, rescode}) ->
    {misc:atom_to_binary(Name), Code == true orelse Code == ok};

format_result({Code, Text}, {Name, restuple}) ->
    {misc:atom_to_binary(Name),
     {[{<<"res">>, Code == true orelse Code == ok},
       {<<"text">>, iolist_to_binary(Text)}]}};

format_result(Code, {Name, restuple}) ->
    {misc:atom_to_binary(Name),
     {[{<<"res">>, Code == true orelse Code == ok},
       {<<"text">>, <<"">>}]}};

format_result(Els, {Name, {list, {_, {tuple, [{_, atom}, _]}} = Fmt}}) ->
    {misc:atom_to_binary(Name), {[format_result(El, Fmt) || El <- Els]}};

format_result(Els, {Name, {list, {_, {tuple, [{name, string}, {value, _}]}} = Fmt}}) ->
    {misc:atom_to_binary(Name), {[format_result(El, Fmt) || El <- Els]}};

format_result(Els, {Name, {list, Def}}) ->
    {misc:atom_to_binary(Name), [element(2, format_result(El, Def)) || El <- Els]};

format_result(Tuple, {_Name, {tuple, [{_, atom}, ValFmt]}}) ->
    {Name2, Val} = Tuple,
    {_, Val2} = format_result(Val, ValFmt),
    {misc:atom_to_binary(Name2), Val2};

format_result(Tuple, {_Name, {tuple, [{name, string}, {value, _} = ValFmt]}}) ->
    {Name2, Val} = Tuple,
    {_, Val2} = format_result(Val, ValFmt),
    {iolist_to_binary(Name2), Val2};

format_result(Tuple, {Name, {tuple, Def}}) ->
    Els = lists:zip(tuple_to_list(Tuple), Def),
    {misc:atom_to_binary(Name), {[format_result(El, ElDef) || {El, ElDef} <- Els]}};

format_result(404, {_Name, _}) ->
    "not_found".


format_error_result(conflict, Code, Msg) ->
    {409, Code, iolist_to_binary(Msg)};
format_error_result(not_exists, Code, Msg) ->
    {404, Code, iolist_to_binary(Msg)};
format_error_result(_ErrorAtom, Code, Msg) ->
    {500, Code, iolist_to_binary(Msg)}.

unauthorized_response() ->
    json_error(401, 10, <<"You are not authorized to call this command.">>).

invalid_token_response() ->
    json_error(401, 10, <<"Oauth Token is invalid or expired.">>).

%% outofscope_response() ->
%%     json_error(401, 11, <<"Token does not grant usage to command required scope.">>).

badrequest_response() ->
    badrequest_response(<<"400 Bad Request">>).
badrequest_response(Body) ->
    json_response(400, jiffy:encode(Body)).

json_format({Code, Result}) ->
    json_response(Code, jiffy:encode(Result));
json_format({HTMLCode, JSONErrorCode, Message}) ->
    json_error(HTMLCode, JSONErrorCode, Message).

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
    AddrS = misc:ip_to_list({Addr, Port}),
    ?INFO_MSG("API call ~ts ~p from ~ts:~p", [Call, hide_sensitive_args(Args), AddrS, Port]);
log(Call, Args, IP) ->
    ?INFO_MSG("API call ~ts ~p (~p)", [Call, hide_sensitive_args(Args), IP]).

hide_sensitive_args(Args=[_H|_T]) ->
    lists:map( fun({<<"password">>, Password}) -> {<<"password">>, ejabberd_config:may_hide_data(Password)};
         ({<<"newpass">>,NewPassword}) -> {<<"newpass">>, ejabberd_config:may_hide_data(NewPassword)};
         (E) -> E end,
         Args);
hide_sensitive_args(NonListArgs) ->
    NonListArgs.

mod_options(_) ->
    [].

mod_doc() ->
    #{desc =>
	  [?T("This module provides a ReST interface to call "
              "https://docs.ejabberd.im/developer/ejabberd-api[ejabberd API] "
	      "commands using JSON data."), "",
	   ?T("To use this module, in addition to adding it to the 'modules' "
	      "section, you must also enable it in 'listen' -> 'ejabberd_http' -> "
              "http://../listen-options/#request-handlers[request_handlers]."), "",
	   ?T("To use a specific API version N, when defining the URL path "
	      "in the request_handlers, add a 'vN'. "
	      "For example: '/api/v2: mod_http_api'"), "",
	   ?T("To run a command, send a POST request to the corresponding "
	      "URL: 'http://localhost:5280/api/<command_name>'")],
     example =>
         ["listen:",
          "  -",
          "    port: 5280",
          "    module: ejabberd_http",
          "    request_handlers:",
          "      /api: mod_http_api",
          "",
          "modules:",
          "  mod_http_api: {}"]}.
