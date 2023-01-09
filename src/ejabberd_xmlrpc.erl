%%%----------------------------------------------------------------------
%%% File    : ejabberd_xmlrpc.erl
%%% Author  : Badlop <badlop@process-one.net>
%%% Purpose : XML-RPC server that frontends ejabberd commands
%%% Created : 21 Aug 2007 by Badlop <badlop@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2023   ProcessOne
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

%%% TODO: Remove support for plaintext password

%%% TODO: commands strings should be strings without ~n

-module(ejabberd_xmlrpc).
-behaviour(ejabberd_listener).

-author('badlop@process-one.net').

-export([start/3, start_link/3, handler/2, process/2, accept/1,
	 listen_options/0]).

-include("logger.hrl").
-include("ejabberd_http.hrl").
-include("mod_roster.hrl").

-include_lib("xmpp/include/xmpp.hrl").

-record(state,
	{auth = noauth        :: noauth | map(),
         get_auth = true      :: boolean(),
	 ip                   :: inet:ip_address()}).

%% -----------------------------
%% Listener interface
%% -----------------------------

start(SockMod, Socket, Opts) ->
    Opts1 = [{request_handlers, [{[], ?MODULE}]}|Opts],
    ejabberd_http:start(SockMod, Socket, Opts1).

start_link(SockMod, Socket, Opts) ->
    Opts1 = [{request_handlers, [{[], ?MODULE}]}|Opts],
    ejabberd_http:start_link(SockMod, Socket, Opts1).

accept(Pid) ->
    ejabberd_http:accept(Pid).

%% -----------------------------
%% HTTP interface
%% -----------------------------

process(_, #request{method = 'POST', data = Data, ip = {IP, _}}) ->
    GetAuth = true,
    State = #state{get_auth = GetAuth, ip = IP},
    case fxml_stream:parse_element(Data) of
	{error, _} ->
	    {400, [],
	     #xmlel{name = <<"h1">>, attrs = [],
		    children = [{xmlcdata, <<"Malformed XML">>}]}};
	El ->
	    case fxmlrpc:decode(El) of
		{error, _} = Err ->
		    ?ERROR_MSG("XML-RPC request ~ts failed with reason: ~p",
			       [Data, Err]),
		    {400, [],
		     #xmlel{name = <<"h1">>, attrs = [],
			    children = [{xmlcdata, <<"Malformed Request">>}]}};
		{ok, RPC} ->
		    ?DEBUG("Got XML-RPC request: ~p", [RPC]),
		    {false, Result} = handler(State, RPC),
		    XML = fxml:element_to_binary(fxmlrpc:encode(Result)),
		    {200, [{<<"Content-Type">>, <<"text/xml">>}],
		     <<"<?xml version=\"1.0\"?>", XML/binary>>}
	    end
    end;
process(_, _) ->
    {400, [],
     #xmlel{name = <<"h1">>, attrs = [],
	    children = [{xmlcdata, <<"400 Bad Request">>}]}}.

%% -----------------------------
%% Access verification
%% -----------------------------

-spec extract_auth([{user | server | token | password, binary()}]) ->
			  map() | {error, not_found | expired | invalid_auth}.
extract_auth(AuthList) ->
    ?DEBUG("AUTHLIST ~p", [AuthList]),
    try get_attrs([user, server, token], AuthList) of
        [U0, S0, T] ->
	    U = jid:nodeprep(U0),
	    S = jid:nameprep(S0),
	    case ejabberd_oauth:check_token(T) of
		{ok, {U, S}, Scope} ->
		    #{usr => {U, S, <<"">>}, oauth_scope => Scope, caller_server => S};
		{false, Reason} ->
		    {error, Reason};
		_ ->
		    {error, not_found}
	    end
    catch
	exit:{attribute_not_found, _, _} ->
            try get_attrs([user, server, password], AuthList) of
                [U0, S0, P] ->
		    U = jid:nodeprep(U0),
		    S = jid:nameprep(S0),
		    case ejabberd_auth:check_password(U, <<"">>, S, P) of
			true ->
			    #{usr => {U, S, <<"">>}, caller_server => S};
			false ->
			    {error, invalid_auth}
		    end
            catch
		exit:{attribute_not_found, Attr, _} ->
		    throw({error, missing_auth_arguments, Attr})
            end
    end.

%% -----------------------------
%% Handlers
%% -----------------------------

handler(#state{get_auth = true, auth = noauth, ip = IP} = State,
	{call, Method,
	 [{struct, AuthList} | Arguments] = AllArgs}) ->
    try extract_auth(AuthList) of
	{error, invalid_auth} ->
	    build_fault_response(-118,
				 "Invalid authentication data",
				 []);
	{error, not_found} ->
	    build_fault_response(-118,
				 "Invalid oauth token",
				 []);
	{error, expired} ->
	    build_fault_response(-118,
				 "Invalid oauth token",
				 []);
        Auth ->
            handler(State#state{get_auth = false, auth = Auth#{ip => IP, caller_module => ?MODULE}},
                    {call, Method, Arguments})
    catch
      {error, missing_auth_arguments, _Attr} ->
	  handler(State#state{get_auth = false,
			      auth = #{ip => IP, caller_module => ?MODULE}},
		  {call, Method, AllArgs})
    end;

%% .............................
%%  Debug

handler(_State, {call, echothis, [A]}) ->
    {false, {response, [A]}};

handler(_State,
	{call, echothisnew, [{struct, [{sentence, A}]}]}) ->
    {false, {response, [{struct, [{repeated, A}]}]}};

handler(_State,
	{call, multhis, [{struct, [{a, A}, {b, B}]}]}) ->
    {false, {response, [A * B]}};

handler(_State,
	{call, multhisnew, [{struct, [{a, A}, {b, B}]}]}) ->
    {false, {response, [{struct, [{mu, A * B}]}]}};

%% .............................
%% ejabberd commands

handler(State, {call, Command, []}) ->
    handler(State, {call, Command, [{struct, []}]});
handler(State,
	{call, Command, [{struct, AttrL}]}) ->
    {ArgsF, ArgsR, ResultF} = ejabberd_commands:get_command_format(Command, State#state.auth),
    try_do_command(State#state.auth, Command, AttrL, ArgsF, ArgsR, ResultF);
handler(_State, Payload) ->
    build_fault_response(-112, "Unknown call: ~p",
			 [Payload]).

%% -----------------------------
%% Command
%% -----------------------------

try_do_command(Auth, Command, AttrL, ArgsF, ArgsR, ResultF) ->
    try do_command(Auth, Command, AttrL, ArgsF, ArgsR, ResultF)
    of
      {command_result, ResultFormatted} ->
	  {false, {response, [ResultFormatted]}}
    catch
      exit:{duplicated_attribute, ExitAt, ExitAtL} ->
	  build_fault_response(-114,
			       "Attribute '~p' duplicated:~n~p",
			       [ExitAt, ExitAtL]);
      exit:{attribute_not_found, ExitAt, ExitAtL} ->
	  build_fault_response(-116,
			       "Required attribute '~p' not found:~n~p",
			       [ExitAt, ExitAtL]);
      exit:{additional_unused_args, ExitAtL} ->
	  build_fault_response(-120,
			       "The call provided additional unused "
				 "arguments:~n~p",
			       [ExitAtL]);
      exit:{invalid_arg_type, Arg, Type} ->
	  build_fault_response(-122,
			       "Parameter '~p' can't be coerced to type '~p'",
			       [Arg, Type]);
      Why ->
	  build_fault_response(-118,
			       "A problem '~p' occurred executing the "
				 "command ~p with arguments~n~p",
			       [Why, Command, AttrL])
    end.

build_fault_response(Code, ParseString, ParseArgs) ->
    FaultString = "Error " ++ integer_to_list(Code) ++ "\n"
        ++ lists:flatten(io_lib:format(ParseString, ParseArgs)),
    ?WARNING_MSG(FaultString, []),
    {false, {response, {fault, Code, list_to_binary(FaultString)}}}.

do_command(Auth, Command, AttrL, ArgsF, ArgsR,
	   ResultF) ->
    ArgsFormatted = format_args(rename_old_args(AttrL, ArgsR), ArgsF),
    Result = ejabberd_commands:execute_command2(Command, ArgsFormatted, Auth),
    ResultFormatted = format_result(Result, ResultF),
    {command_result, ResultFormatted}.

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

%%-----------------------------
%% Format arguments
%%-----------------------------

get_attrs(Attribute_names, L) ->
    [get_attr(A, L) || A <- Attribute_names].

get_attr(A, L) ->
    case lists:keysearch(A, 1, L) of
      {value, {A, Value}} -> Value;
      false ->
	  exit({attribute_not_found, A, L})
    end.

get_elem_delete(A, L) ->
    case proplists:get_all_values(A, L) of
      [Value] -> {Value, proplists:delete(A, L)};
      [_, _ | _] ->
	  exit({duplicated_attribute, A, L});
      [] ->
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
format_arg(Arg, string) when is_list(Arg) -> Arg;
format_arg(Arg, string) when is_binary(Arg) -> binary_to_list(Arg);
format_arg(undefined, binary) -> <<>>;
format_arg(undefined, string) -> "";
format_arg(Arg, Format) ->
    ?ERROR_MSG("Don't know how to format Arg ~p for format ~p", [Arg, Format]),
    exit({invalid_arg_type, Arg, Format}).

process_unicode_codepoints(Str) ->
    iolist_to_binary(lists:map(fun(X) when X > 255 -> unicode:characters_to_binary([X]);
                                  (Y) -> Y
                               end, Str)).

%% -----------------------------
%% Result
%% -----------------------------

format_result({error, Error}, _) when is_list(Error) ->
    throw({error, lists:flatten(Error)});
format_result({error, Error}, _) ->
    throw({error, Error});
format_result({error, _Type, _Code, Error}, _) when is_list(Error) ->
    throw({error, lists:flatten(Error)});
format_result({error, _Type, _Code, Error}, _) ->
    throw({error, Error});
format_result(String, string) -> lists:flatten(String);
format_result(Atom, {Name, atom}) ->
    {struct,
     [{Name, iolist_to_binary(atom_to_list(Atom))}]};
format_result(Int, {Name, integer}) ->
    {struct, [{Name, Int}]};
format_result([A|_]=String, {Name, string}) when is_list(String) and is_integer(A) ->
    {struct, [{Name, lists:flatten(String)}]};
format_result(Binary, {Name, string}) when is_binary(Binary) ->
    {struct, [{Name, binary_to_list(Binary)}]};
format_result(Atom, {Name, string}) when is_atom(Atom) ->
    {struct, [{Name, atom_to_list(Atom)}]};
format_result(Integer, {Name, string}) when is_integer(Integer) ->
    {struct, [{Name, integer_to_list(Integer)}]};
format_result(Other, {Name, string}) ->
    {struct, [{Name, io_lib:format("~p", [Other])}]};
format_result(String, {Name, binary}) when is_list(String) ->
    {struct, [{Name, lists:flatten(String)}]};
format_result(Binary, {Name, binary}) when is_binary(Binary) ->
    {struct, [{Name, binary_to_list(Binary)}]};
format_result(Code, {Name, rescode}) ->
    {struct, [{Name, make_status(Code)}]};
format_result({Code, Text}, {Name, restuple}) ->
    {struct,
     [{Name, make_status(Code)},
      {text, io_lib:format("~s", [Text])}]};
format_result(Elements, {Name, {list, ElementsDef}}) ->
    FormattedList = lists:map(fun (Element) ->
				      format_result(Element, ElementsDef)
			      end,
			      Elements),
    {struct, [{Name, {array, FormattedList}}]};
format_result(ElementsTuple,
	      {Name, {tuple, ElementsDef}}) ->
    ElementsList = tuple_to_list(ElementsTuple),
    ElementsAndDef = lists:zip(ElementsList, ElementsDef),
    FormattedList = lists:map(fun ({Element, ElementDef}) ->
				      format_result(Element, ElementDef)
			      end,
			      ElementsAndDef),
    {struct, [{Name, {array, FormattedList}}]};
format_result(404, {Name, _}) ->
    {struct, [{Name, make_status(not_found)}]}.

make_status(ok) -> 0;
make_status(true) -> 0;
make_status(false) -> 1;
make_status(error) -> 1;
make_status(_) -> 1.

listen_options() ->
    ?WARNING_MSG("It is deprecated defining ejabberd_xmlrpc as a listen module "
                 "in the ejabberd configuration. Support for that configuration"
                 " method may be removed in a future ejabberd release. You are "
                 "encouraged to define ejabberd_xmlrpc inside request_handlers "
                 "option of ejabberd_http listen module. See the ejabberd "
                 "documentation for details: https://docs.ejabberd.im/admin/"
                 "configuration/listen/#ejabberd-xmlrpc", []),
    [].
