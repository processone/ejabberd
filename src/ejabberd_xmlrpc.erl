%%%----------------------------------------------------------------------
%%% File    : ejabberd_xmlrpc.erl
%%% Author  : Badlop <badlop@process-one.net>
%%% Purpose : XML-RPC server that frontends ejabberd commands
%%% Created : 21 Aug 2007 by Badlop <badlop@ono.com>
%%% Id      : $Id: ejabberd_xmlrpc.erl 595 2008-05-20 11:39:31Z badlop $
%%%----------------------------------------------------------------------

%%% TODO: Implement a command in ejabberdctl 'help COMMAND LANGUAGE' that shows
%%% a coding example to call that command in a specific language (python, php).

%%% TODO: Remove support for plaintext password

%%% TODO: commands strings should be strings without ~n

-module(ejabberd_xmlrpc).

-author('badlop@process-one.net').

-export([start/2, handler/2, socket_type/0, transform_listen_option/2]).

-include("ejabberd.hrl").
-include("logger.hrl").

-include("mod_roster.hrl").

-include("jlib.hrl").

-record(state,
	{access_commands = [] :: list(),
         auth = noauth        :: noauth | {binary(), binary(), binary()},
         get_auth = true      :: boolean()}).

%% Test:

%% xmlrpc:call({127, 0, 0, 1}, 4560, "/", {call, take_integer, [{struct, [{thisinteger, 5}]}]}).
%% {ok,{response,[{struct,[{zero,0}]}]}}
%%
%% xmlrpc:call({127, 0, 0, 1}, 4560, "/", {call, echo_string, [{struct, [{thisstring, "abcd"}]}]}).
%% {ok,{response,[{struct,[{thatstring,"abcd"}]}]}}
%%
%% xmlrpc:call({127, 0, 0, 1}, 4560, "/", {call, tell_tuple_3integer, [{struct, [{thisstring, "abcd"}]}]}).
%% {ok,{response,
%%         [{struct,
%%              [{thattuple,
%%                   {array,
%%                       [{struct,[{first,123}]},
%%                        {struct,[{second,456}]},
%%                        {struct,[{third,789}]}]}}]}]}}
%%
%% xmlrpc:call({127, 0, 0, 1}, 4560, "/", {call, pow, [{struct, [{base, 5}, {exponent, 7}]}]}).
%% {ok,{response,[{struct,[{pow,78125}]}]}}
%%
%% xmlrpc:call({127, 0, 0, 1}, 4560, "/", {call, seq, [{struct, [{from, 3}, {to, 7}]}]}).
%% {ok,{response,[{array,[{struct,[{intermediate,3}]},
%%                        {struct,[{intermediate,4}]},
%%                        {struct,[{intermediate,5}]},
%%                        {struct,[{intermediate,6}]},
%%                        {struct,[{intermediate,7}]}]}]}}
%%
%% xmlrpc:call({127, 0, 0, 1}, 4560, "/", {call, substrs, [{struct, [{word, "abcd"}]}]}).
%% NO:
%% {ok,{response,[{array,[{struct,[{miniword,"a"}]},
%%                        {struct,[{miniword,"ab"}]},
%%                        {struct,[{miniword,"abc"}]},
%%                        {struct,[{miniword,"abcd"}]}]}]}}
%% {ok,{response,
%%         [{struct,
%%              [{substrings,
%%                   {array,
%%                       [{struct,[{miniword,"a"}]},
%%                        {struct,[{miniword,"ab"}]},
%%                        {struct,[{miniword,"abc"}]},
%%                        {struct,[{miniword,"abcd"}]}]}}]}]}}
%%
%% xmlrpc:call({127, 0, 0, 1}, 4560, "/", {call, splitjid, [{struct, [{jid, "abcd@localhost/work"}]}]}).
%% {ok,{response,
%%         [{struct,
%%              [{jidparts,
%%                   {array,
%%                       [{struct,[{user,"abcd"}]},
%%                        {struct,[{server,"localhost"}]},
%%                        {struct,[{resource,"work"}]}]}}]}]}}
%%
%% xmlrpc:call({127, 0, 0, 1}, 4560, "/", {call, echo_integer_string, [{struct, [{thisstring, "abc"}, {thisinteger, 55}]}]}).
%% {ok,{response,
%%         [{struct,
%%              [{thistuple,
%%                   {array,
%%                       [{struct,[{thisinteger,55}]},
%%                        {struct,[{thisstring,"abc"}]}]}}]}]}}
%%
%%
%% xmlrpc:call({127, 0, 0, 1}, 4560, "/", {call, echo_list_integer, [{struct, [{thislist, {array, [{struct, [{thisinteger, 55}, {thisinteger, 4567}]}]}}]}]}).
%% {ok,{response,
%%         [{struct,
%%              [{thatlist,
%%                   {array,
%%                       [{struct,[{thatinteger,55}]},
%%                        {struct,[{thatinteger,4567}]}]}}]}]}}
%%
%%
%% xmlrpc:call({127, 0, 0, 1}, 4560, "/", {call, echo_integer_list_string, [{struct, [{thisinteger, 123456}, {thislist, {array, [{struct, [{thisstring, "abc"}, {thisstring, "bobo baba"}]}]}}]}]}).
%% {ok,
%%  {response,
%%   [{struct,
%%     [{thistuple,
%%       {array,
%%        [{struct,[{thatinteger,123456}]},
%%         {struct,
%%          [{thatlist,
%%            {array,
%%             [{struct,[{thatstring,"abc"}]},
%%              {struct,[{thatstring,"bobo baba"}]}]}}]}]}}]}]}}
%%
%% xmlrpc:call({127, 0, 0, 1}, 4560, "/", {call, take_tuple_2integer, [{struct, [{thistuple, {array, [{struct, [{thisinteger1, 55}, {thisinteger2, 4567}]}]}}]}]}).
%% {ok,{response,[{struct,[{zero,0}]}]}}
%%
%% xmlrpc:call({127, 0, 0, 1}, 4560, "/", {call, echo_isatils, [{struct,
%% 	                                      [{thisinteger, 123456990},
%% 			                               {thisstring, "This is ISATILS"},
%% 			                               {thisatom, "test_isatils"},
%% 			                               {thistuple, {array, [{struct, [
%%                                            {listlen, 2},
%% 			                                  {thislist, {array, [{struct, [
%%                                               {contentstring, "word1"},
%%                                               {contentstring, "word 2"}
%%                                            ]}]}}
%% 										   ]}]}}
%% 										  ]}]}).
%% {ok,{response,
%%         [{struct,
%%              [{results,
%%                   {array,
%%                       [{struct,[{thatinteger,123456990}]},
%%                        {struct,[{thatstring,"This is ISATILS"}]},
%%                        {struct,[{thatatom,"test_isatils"}]},
%%                        {struct,
%%                            [{thattuple,
%%                                 {array,
%%                                     [{struct,[{listlen,123456990}]},
%%                                      {struct,[{thatlist,...}]}]}}]}]}}]}]}}

%% ecommand doesn't exist:
%% xmlrpc:call({127, 0, 0, 1}, 4560, "/", {call, echo_integer_string2, [{struct, [{thisstring, "abc"}]}]}).
%% {ok,{response,{fault,-1, "Unknown call: {call,echo_integer_string2,[{struct,[{thisstring,\"abc\"}]}]}"}}}
%%
%% Duplicated argument:
%% xmlrpc:call({127, 0, 0, 1}, 4560, "/", {call, echo_integer_string, [{struct, [{thisstring, "abc"}, {thisinteger, 44}, {thisinteger, 55}]}]}).
%% {ok,{response,{fault,-104, "Error -104\nAttribute 'thisinteger' duplicated:\n[{thisstring,\"abc\"},{thisinteger,44},{thisinteger,55}]"}}}
%%
%% Missing argument:
%% xmlrpc:call({127, 0, 0, 1}, 4560, "/", {call, echo_integer_string, [{struct, [{thisstring, "abc"}]}]}).
%% {ok,{response,{fault,-106, "Error -106\nRequired attribute 'thisinteger' not found:\n[{thisstring,\"abc\"}]"}}}
%%
%% Duplicated tuple element:
%% xmlrpc:call({127, 0, 0, 1}, 4560, "/", {call, take_tuple_2integer, [{struct, [{thistuple, {array, [{struct, [{thisinteger1, 55}, {thisinteger1, 66}, {thisinteger2, 4567}]}]}}]}]}).
%% {ok,{response,{fault,-104, "Error -104\nAttribute 'thisinteger1' defined multiple times:\n[{thisinteger1,55},{thisinteger1,66},{thisinteger2,4567}]"}}}
%%
%% Missing element in tuple:
%% xmlrpc:call({127, 0, 0, 1}, 4560, "/", {call, take_tuple_2integer, [{struct, [{thistuple, {array, [{struct, [{thisinteger1, 55}, {thisintegerc, 66}, {thisinteger, 4567}]}]}}]}]}).
%% {ok,{response,{fault,-106, "Error -106\nRequired attribute 'thisinteger2' not found:\n[{thisintegerc,66},{thisinteger,4567}]"}}}
%%
%% The ecommand crashed:
%% xmlrpc:call({127, 0, 0, 1}, 4560, "/", {call, this_crashes, [{struct, []}]}).
%% {ok,{response,{fault,-100, "Error -100\nA problem 'error' occurred executing the command this_crashes with arguments []: badarith"}}}

%% -----------------------------
%% Listener interface
%% -----------------------------

start({gen_tcp = _SockMod, Socket}, Opts) ->
    %MaxSessions = gen_mod:get_opt(maxsessions, Opts,
    %                              fun(I) when is_integer(I), I>0 -> I end,
    %                              10),
    Timeout = gen_mod:get_opt(timeout, Opts,
                              fun(I) when is_integer(I), I>0 -> I end,
                              5000),
    AccessCommandsOpts = gen_mod:get_opt(access_commands, Opts,
                                         fun(L) when is_list(L) -> L end,
                                         []),
    AccessCommands = lists:flatmap(
                       fun({Ac, AcOpts}) ->
                               Commands = gen_mod:get_opt(
                                            commands, AcOpts,
                                            fun(A) when is_atom(A) ->
                                                    A;
                                               (L) when is_list(L) ->
                                                    true = lists:all(
                                                             fun is_atom/1,
                                                             L),
                                                    L
                                            end, all),
                               CommOpts = gen_mod:get_opt(
                                            options, AcOpts,
                                            fun(L) when is_list(L) -> L end,
                                            []),
                               [{Ac, Commands, CommOpts}];
                          (Wrong) ->
                               ?WARNING_MSG("wrong options format for ~p: ~p",
                                            [?MODULE, Wrong]),
                               []
                       end, AccessCommandsOpts),
    GetAuth = case [ACom
		    || {Ac, _, _} = ACom <- AccessCommands, Ac /= all]
		  of
		[] -> false;
		_ -> true
	      end,
    Handler = {?MODULE, handler},
    State = #state{access_commands = AccessCommands,
		   get_auth = GetAuth},
    Pid = proc_lib:spawn(xmlrpc_http, handler, [Socket, Timeout, Handler, State]),
    {ok, Pid}.

socket_type() -> raw.

%% -----------------------------
%% Access verification
%% -----------------------------

get_auth(AuthList) ->
    [User, Server, Password] = try get_attrs([user, server,
					      password],
					     AuthList)
			       of
				 [U, S, P] -> [U, S, P]
			       catch
				 exit:{attribute_not_found, Attr, _} ->
				     throw({error, missing_auth_arguments,
					    Attr})
			       end,
    {User, Server, Password}.

%% -----------------------------
%% Handlers
%% -----------------------------

%% Call:           Arguments:                                      Returns:

%% .............................
%%  Access verification

%% xmlrpc:call({127, 0, 0, 1}, 4560, "/", {call, echothis, [152]}).
%% {ok,{response,{fault,-103, "Error -103\nRequired authentication: {call,echothis,[152]}"}}}
%%
%% xmlrpc:call({127, 0, 0, 1}, 4560, "/", {call, echothis, [{struct, [{user, "badlop"}, {server, "localhost"}, {password, "ada"}]}, 152]}).
%% {ok,{response,{fault,-103,
%%      "Error -103\nAuthentication non valid: [{user,\"badlop\"},\n
%% 	 {server,\"localhost\"},\n
%% 	 {password,\"ada\"}]"}}}
%%
%% xmlrpc:call({127, 0, 0, 1}, 4560, "/", {call, echothis, [{struct, [{user, "badlop"}, {server, "localhost"}, {password, "ada90ada"}]}, 152]}).
%% {ok,{response,[152]}}
%%
%% xmlrpc:call({127, 0, 0, 1}, 4560, "/", {call, echothis, [{struct, [{user, "badlop"}, {server, "localhost"}, {password, "79C1574A43BC995F2B145A299EF97277"}]}, 152]}).
%% {ok,{response,[152]}}

handler(#state{get_auth = true, auth = noauth} = State,
	{call, Method,
	 [{struct, AuthList} | Arguments] = AllArgs}) ->
    try get_auth(AuthList) of
      Auth ->
	  handler(State#state{get_auth = false, auth = Auth},
		  {call, Method, Arguments})
    catch
      {error, missing_auth_arguments, _Attr} ->
	  handler(State#state{get_auth = false, auth = noauth},
		  {call, Method, AllArgs})
    end;
%% .............................
%%  Debug
%% echothis       String                                          String
handler(_State, {call, echothis, [A]}) ->
    {false, {response, [A]}};
%% echothisnew    struct[{sentence, String}]   struct[{repeated, String}]
handler(_State,
	{call, echothisnew, [{struct, [{sentence, A}]}]}) ->
    {false, {response, [{struct, [{repeated, A}]}]}};
%% multhis        struct[{a, Integer}, {b, Integer}]              Integer
handler(_State,
	{call, multhis, [{struct, [{a, A}, {b, B}]}]}) ->
    {false, {response, [A * B]}};
%% multhisnew     struct[{a, Integer}, {b, Integer}]    struct[{mu, Integer}]
handler(_State,
	{call, multhisnew, [{struct, [{a, A}, {b, B}]}]}) ->
    {false, {response, [{struct, [{mu, A * B}]}]}};
%% .............................
%% ejabberd commands
handler(State, {call, Command, []}) ->
    handler(State, {call, Command, [{struct, []}]});
handler(State,
	{call, Command, [{struct, AttrL}]} = Payload) ->
    case ejabberd_commands:get_command_format(Command) of
      {error, command_unknown} ->
	  build_fault_response(-112, "Unknown call: ~p",
			       [Payload]);
      {ArgsF, ResultF} ->
	  try_do_command(State#state.access_commands,
			 State#state.auth, Command, AttrL, ArgsF, ResultF)
    end;
%% If no other guard matches
handler(_State, Payload) ->
    build_fault_response(-112, "Unknown call: ~p",
			 [Payload]).

%% -----------------------------
%% Command
%% -----------------------------

try_do_command(AccessCommands, Auth, Command, AttrL,
	       ArgsF, ResultF) ->
    try do_command(AccessCommands, Auth, Command, AttrL,
		   ArgsF, ResultF)
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
    {false, {response, {fault, Code, FaultString}}}.

do_command(AccessCommands, Auth, Command, AttrL, ArgsF,
	   ResultF) ->
    ArgsFormatted = format_args(AttrL, ArgsF),
    AuthBin = convert_auth(Auth),
    Result =
	ejabberd_commands:execute_command(AccessCommands, AuthBin,
					  Command, ArgsFormatted),
    ResultFormatted = format_result(Result, ResultF),
    {command_result, ResultFormatted}.

convert_auth(noauth) ->
    noauth;
convert_auth({UserT, ServerT, PasswordT}) ->
    {list_to_binary(UserT), list_to_binary(ServerT), list_to_binary(PasswordT)}.

%%-----------------------------
%% Format arguments
%%-----------------------------

get_attrs(Attribute_names, L) ->
    [get_attr(A, L) || A <- Attribute_names].

get_attr(A, L) ->
    case lists:keysearch(A, 1, L) of
      {value, {A, Value}} -> Value;
      false ->
	  %% Report the error and then force a crash
	  exit({attribute_not_found, A, L})
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
format_arg(Arg, binary) when is_list(Arg) -> list_to_binary(Arg);
format_arg(Arg, binary) when is_binary(Arg) -> Arg;
format_arg(Arg, string) when is_list(Arg) -> list_to_binary(Arg);
format_arg(Arg, string) when is_binary(Arg) -> Arg;
format_arg(Arg, Format) ->
    ?ERROR_MSG("don't know how to format Arg ~p for format ~p", [Arg, Format]),
    throw({error_formatting_argument, Arg, Format}).

%% -----------------------------
%% Result
%% -----------------------------

format_result({error, Error}, _) ->
    throw({error, Error});
format_result(String, string) -> lists:flatten(String);
format_result(Atom, {Name, atom}) ->
    {struct,
     [{Name, iolist_to_binary(atom_to_list(Atom))}]};
format_result(Int, {Name, integer}) ->
    {struct, [{Name, Int}]};
format_result(String, {Name, string}) when is_list(String) ->
    {struct, [{Name, lists:flatten(String)}]};
format_result(Binary, {Name, string}) when is_binary(Binary) ->
    {struct, [{Name, binary_to_list(Binary)}]};
format_result(Code, {Name, rescode}) ->
    {struct, [{Name, make_status(Code)}]};
format_result({Code, Text}, {Name, restuple}) ->
    {struct,
     [{Name, make_status(Code)},
      {text, lists:flatten(Text)}]};
%% Result is a list of something: [something()]
format_result(Elements, {Name, {list, ElementsDef}}) ->
    FormattedList = lists:map(fun (Element) ->
				      format_result(Element, ElementsDef)
			      end,
			      Elements),
    {struct, [{Name, {array, FormattedList}}]};
%% Result is a tuple with several elements: {something1(), something2(), ...}
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

transform_listen_option({access_commands, ACOpts}, Opts) ->
    NewACOpts = lists:map(
                  fun({AName, ACmds, AOpts}) ->
                          {AName, [{commands, ACmds}, {options, AOpts}]};
                     (Opt) ->
                          Opt
                  end, ACOpts),
    [{access_commands, NewACOpts}|Opts];
transform_listen_option(Opt, Opts) ->
    [Opt|Opts].
