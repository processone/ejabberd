%%%----------------------------------------------------------------------
%%% File    : ejabberd_ctl.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : ejabberd command line admin tool
%%% Created : 11 Jan 2004 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2024   ProcessOne
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

-module(ejabberd_ctl).

-behaviour(gen_server).
-author('alexey@process-one.net').

-export([start/0, start_link/0, process/1, process2/2]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).
-export([get_commands_spec/0, format_arg/2,
         get_usage_command/4]).

-include("ejabberd_ctl.hrl").
-include("ejabberd_commands.hrl").
-include("logger.hrl").
-include("ejabberd_stacktrace.hrl").

-define(DEFAULT_VERSION, 1000000).

-record(state, {}).

%%-----------------------------
%% Module
%%-----------------------------

start() ->
    disable_logging(),
    [SNode, Timeout, Args] = case init:get_plain_arguments() of
                                 [SNode2, "--no-timeout" | Args2] ->
                                     [SNode2, infinity, Args2];
                                 [SNode3 | Args3] ->
                                     [SNode3, 60000, Args3];
                                 _ ->
                                     print_usage(?DEFAULT_VERSION),
                                     halt(?STATUS_USAGE)
                             end,
    SNode1 = case string:tokens(SNode, "@") of
                 [_Node, _Server] ->
                     SNode;
                 _ ->
                     case net_kernel:longnames() of
                         true ->
                             lists:flatten([SNode, "@", inet_db:gethostname(),
                                            ".", inet_db:res_option(domain)]);
                         false ->
                             lists:flatten([SNode, "@", inet_db:gethostname()]);
                         _ ->
                             SNode
                     end
             end,
    Node = list_to_atom(SNode1),
    Status = case ejabberd_cluster:call(Node, ?MODULE, process, [Args], Timeout) of
                 {badrpc, Reason} ->
                     print("Failed RPC connection to the node ~p: ~p~n",
                           [Node, Reason]),
                     %% TODO: show minimal start help
                     ?STATUS_BADRPC;
                 {invalid_version, V} ->
                     print("Invalid API version number: ~p~n", [V]),
                     ?STATUS_ERROR;
                 S ->
                     S
             end,
    halt(Status).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    ejabberd_commands:register_commands(?MODULE, get_commands_spec()),
    {ok, #state{}}.

handle_call(Request, From, State) ->
    ?WARNING_MSG("Unexpected call from ~p: ~p", [From, Request]),
    {noreply, State}.

handle_cast(Msg, State) ->
    ?WARNING_MSG("Unexpected cast: ~p", [Msg]),
    {noreply, State}.

handle_info(Info, State) ->
    ?WARNING_MSG("Unexpected info: ~p", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ejabberd_commands:unregister_commands(get_commands_spec()),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%-----------------------------
%% Process
%%-----------------------------

-spec process([string()]) -> non_neg_integer().
process(Args) ->
    process(Args, ?DEFAULT_VERSION).


-spec process([string()], non_neg_integer()) -> non_neg_integer().

%% The commands status, stop and restart are defined here to ensure
%% they are usable even if ejabberd is completely stopped.
process(["status"], _Version) ->
    {InternalStatus, ProvidedStatus} = init:get_status(),
    print("The node ~p is ~p with status: ~p~n",
	   [node(), InternalStatus, ProvidedStatus]),
    case lists:keymember(ejabberd, 1, application:which_applications()) of
        false ->
            EjabberdLogPath = ejabberd_logger:get_log_path(),
            print("ejabberd is not running in that node~n"
		   "Check for error messages: ~ts~n"
		   "or other files in that directory.~n", [EjabberdLogPath]),
            ?STATUS_ERROR;
        true ->
            print("ejabberd ~ts is running in that node~n", [ejabberd_option:version()]),
            ?STATUS_SUCCESS
    end;

%% TODO: Mnesia operations should not be hardcoded in ejabberd_ctl module.
%% For now, I leave them there to avoid breaking those commands for people that
%% may be using it (as format of response is going to change).
process(["mnesia_info_ctl"], _Version) ->
    mnesia:info(),
    ?STATUS_SUCCESS;

process(["print_sql_schema", DBType, DBVersion, NewSchema], _Version) ->
    ejabberd_sql_schema:print_schema(DBType, DBVersion, NewSchema);

%% The arguments --long and --dual are not documented because they are
%% automatically selected depending in the number of columns of the shell
process(["help" | Mode], Version) ->
    {MaxC, ShCode} = get_shell_info(),
    case Mode of
	[] ->
	    print_usage_help(MaxC, ShCode),
	    ?STATUS_SUCCESS;
	["--dual"] ->
	    print_usage(dual, MaxC, ShCode, Version),
	    ?STATUS_USAGE;
	["--long"] ->
	    print_usage(long, MaxC, ShCode, Version),
	    ?STATUS_USAGE;
	["tags"] ->
	    print_usage_tags(MaxC, ShCode, Version),
	    ?STATUS_SUCCESS;
	["--tags"] -> % deprecated in favor of "tags"
	    print_usage_tags(MaxC, ShCode, Version),
	    ?STATUS_SUCCESS;
	["commands"] ->
	    print_usage_tags_long(MaxC, ShCode, Version),
	    ?STATUS_SUCCESS;
	["--tags", Tag] -> % deprecated in favor of simply "Tag"
	    print_usage_tags(Tag, MaxC, ShCode, Version),
	    ?STATUS_SUCCESS;
	[String | _] ->
            case determine_string_type(String, Version) of
                no_idea ->
                    io:format("No tag or command matches '~ts'~n", [String]);
                both ->
                    print_usage_tags(String, MaxC, ShCode, Version),
                    print_usage_commands2(String, MaxC, ShCode, Version);
                tag ->
                    print_usage_tags(String, MaxC, ShCode, Version);
                command ->
                    print_usage_commands2(String, MaxC, ShCode, Version)
            end,
	    ?STATUS_SUCCESS
    end;

process(["--version", Arg | Args], _) ->
    Version =
	try
	    list_to_integer(Arg)
	catch _:_ ->
		throw({invalid_version, Arg})
	end,
    process(Args, Version);

process(Args, Version) ->
    {String, Code} = process2(Args, [], Version),
    case String of
	[] -> ok;
	_ ->
	    io:format("~ts~n", [String])
    end,
    Code.

-spec process2(Args::[string()], AccessCommands::any()) ->
    {String::string(), Code::integer()}.
process2(Args, AccessCommands) ->
    process2(Args, AccessCommands, ?DEFAULT_VERSION).

process2(["--auth", User, Server, Pass | Args], AccessCommands, Version) ->
    process2(Args, AccessCommands, {list_to_binary(User), list_to_binary(Server),
				    list_to_binary(Pass), true}, Version);
process2(Args, AccessCommands, Version) ->
    process2(Args, AccessCommands, noauth, Version).



process2(Args, AccessCommands, Auth, Version) ->
    case try_run_ctp(Args, Auth, AccessCommands, Version) of
	{String, wrong_command_arguments}
          when is_list(String) ->
	    io:format(lists:flatten(["\n" | String]++["\n"])),
	    [CommandString | _] = Args,
            process(["help" | [CommandString]], Version),
	    {lists:flatten(String), ?STATUS_ERROR};
	{String, Code}
          when is_list(String) and is_integer(Code) ->
	    {lists:flatten(String), Code};
	String
          when is_list(String) ->
	    {lists:flatten(String), ?STATUS_SUCCESS};
	Code
          when is_integer(Code) ->
	    {"", Code};
	Other ->
	    {"Erroneous result: " ++ io_lib:format("~p", [Other]), ?STATUS_ERROR}
    end.

determine_string_type(String, Version) ->
    TagsCommands = ejabberd_commands:get_tags_commands(Version),
    CommandsNames = case lists:keysearch(String, 1, TagsCommands) of
			{value, {String, CNs}} -> CNs;
			false -> []
		    end,
    AllCommandsNames = [atom_to_list(Name) || {Name, _, _} <- ejabberd_commands:list_commands(Version)],
    Cmds = filter_commands(AllCommandsNames, String),
    case {CommandsNames, Cmds} of
        {[], []} -> no_idea;
        {[], _} -> command;
        {_, []} -> tag;
        {_, _} -> both
    end.

%%-----------------------------
%% Command calling
%%-----------------------------

try_run_ctp(Args, Auth, AccessCommands, Version) ->
    try ejabberd_hooks:run_fold(ejabberd_ctl_process, false, [Args]) of
	false when Args /= [] ->
	    try_call_command(Args, Auth, AccessCommands, Version);
	false ->
	    print_usage(Version),
	    {"", ?STATUS_USAGE};
	Status ->
	    {"", Status}
    catch
	exit:Why ->
	    print_usage(Version),
	    {io_lib:format("Error in ejabberd ctl process: ~p", [Why]), ?STATUS_USAGE};
	Error:Why ->
            %% In this case probably ejabberd is not started, so let's show Status
            process(["status"], Version),
            print("~n", []),
	    {io_lib:format("Error in ejabberd ctl process: '~p' ~p", [Error, Why]), ?STATUS_USAGE}
    end.

try_call_command(Args, Auth, AccessCommands, Version) ->
    try call_command(Args, Auth, AccessCommands, Version) of
	{Reason, wrong_command_arguments} ->
	    {Reason, ?STATUS_ERROR};
	Res ->
	    Res
    catch
	throw:{error, unknown_command} ->
	    KnownCommands = [Cmd || {Cmd, _, _} <- ejabberd_commands:list_commands(Version)],
	    UnknownCommand = list_to_atom(hd(Args)),
	    {io_lib:format(
	       "Error: unknown command '~ts'. Did you mean '~ts'?",
	       [hd(Args), misc:best_match(UnknownCommand, KnownCommands)]),
	     ?STATUS_ERROR};
	throw:Error ->
	    {io_lib:format("~p", [Error]), ?STATUS_ERROR};
	?EX_RULE(A, Why, Stack) ->
	    StackTrace = ?EX_STACK(Stack),
	    {io_lib:format("Unhandled exception occurred executing the command:~n** ~ts",
			   [misc:format_exception(2, A, Why, StackTrace)]),
	     ?STATUS_ERROR}
    end.

-spec call_command(Args::[string()],
                   Auth::noauth | {binary(), binary(), binary(), true},
                   AccessCommands::[any()],
                   Version::integer()) ->
    string() | integer() | {string(), integer()} | {error, ErrorType::any()}.
call_command([CmdString | Args], Auth, _AccessCommands, Version) ->
    CmdStringU = ejabberd_regexp:greplace(
                   list_to_binary(CmdString), <<"-">>, <<"_">>),
    Command = list_to_atom(binary_to_list(CmdStringU)),
    {ArgsFormat, _, ResultFormat} = ejabberd_commands:get_command_format(Command, Auth, Version),
    case (catch format_args(Args, ArgsFormat)) of
	ArgsFormatted when is_list(ArgsFormatted) ->
	    CI = case Auth of
		     {U, S, _, _} -> #{usr => {U, S, <<"">>}, caller_host => S};
		     _ -> #{}
		 end,
	    CI2 = CI#{caller_module => ?MODULE},
	    Result = ejabberd_commands:execute_command2(Command,
							ArgsFormatted,
							CI2,
							Version),
	    format_result_preliminary(Result, ResultFormat, Version);
	{'EXIT', {function_clause,[{lists,zip,[A1,A2|_], _} | _]}} ->
	    {NumCompa, TextCompa} =
		case {length(A1), length(A2)} of
		    {L1, L2} when L1 < L2 -> {L2-L1, "less argument"};
		    {L1, L2} when L1 > L2 -> {L1-L2, "more argument"}
		end,
	    process(["help" | [CmdString]], Version),
	    {io_lib:format("Error: the command '~ts' requires ~p ~ts.",
			   [CmdString, NumCompa, TextCompa]),
	     wrong_command_arguments}
    end.


%%-----------------------------
%% Format arguments
%%-----------------------------

format_args(Args, ArgsFormat) ->
    lists:foldl(
      fun({{_ArgName, ArgFormat}, Arg}, Res) ->
	      Formatted = format_arg(Arg, ArgFormat),
	      Res ++ [Formatted]
      end,
      [],
      lists:zip(ArgsFormat, Args)).

format_arg(Arg, integer) ->
    format_arg2(Arg, "~d");
format_arg(Arg, binary) ->
    unicode:characters_to_binary(Arg, utf8);
format_arg("", string) ->
    "";
format_arg(Arg, string) ->
    NumChars = integer_to_list(length(Arg)),
    Parse = "~" ++ NumChars ++ "c",
    format_arg2(Arg, Parse);
format_arg(Arg, {list, {_ArgName, ArgFormat}}) ->
    [format_arg(string:trim(Element), ArgFormat) || Element <- string:tokens(Arg, ",")];
format_arg(Arg, {list, ArgFormat}) ->
    [format_arg(string:trim(Element), ArgFormat) || Element <- string:tokens(Arg, ",")];
format_arg(Arg, {tuple, Elements}) ->
    Args = string:tokens(Arg, ":"),
    list_to_tuple(format_args(Args, Elements));
format_arg(Arg, Format) ->
    S = unicode:characters_to_binary(Arg, utf8),
    JSON = misc:json_decode(S),
    mod_http_api:format_arg(JSON, Format).

format_arg2(Arg, Parse)->
    {ok, [Arg2], _RemainingArguments} = io_lib:fread(Parse, Arg),
    Arg2.

%%-----------------------------
%% Format result
%%-----------------------------

format_result_preliminary(Result, {A, {list, B}}, Version) ->
    format_result(Result, {A, {top_result_list, B}}, Version);
format_result_preliminary(Result, ResultFormat, Version) ->
    format_result(Result, ResultFormat, Version).

format_result({error, ErrorAtom}, _, _Version) ->
    {io_lib:format("Error: ~p", [ErrorAtom]), make_status(error)};

%% An error should always be allowed to return extended error to help with API.
%% Extended error is of the form:
%%  {error, type :: atom(), code :: int(), Desc :: string()}
format_result({error, ErrorAtom, Code, Msg}, _, _Version) ->
    {io_lib:format("Error: ~p: ~s", [ErrorAtom, Msg]), make_status(Code)};

format_result(Atom, {_Name, atom}, _Version) ->
    io_lib:format("~p", [Atom]);

format_result(Int, {_Name, integer}, _Version) ->
    io_lib:format("~p", [Int]);

format_result([A|_]=String, {_Name, string}, _Version) when is_list(String) and is_integer(A) ->
    io_lib:format("~ts", [String]);

format_result(Binary, {_Name, binary}, _Version) when is_binary(Binary) ->
    io_lib:format("~ts", [Binary]);

format_result(String, {_Name, binary}, _Version) when is_list(String) ->
    io_lib:format("~ts", [String]);

format_result(Binary, {_Name, string}, _Version) when is_binary(Binary) ->
    io_lib:format("~ts", [Binary]);

format_result(Atom, {_Name, string}, _Version) when is_atom(Atom) ->
    io_lib:format("~ts", [atom_to_list(Atom)]);

format_result(Integer, {_Name, string}, _Version) when is_integer(Integer) ->
    io_lib:format("~ts", [integer_to_list(Integer)]);

format_result(Other, {_Name, string}, _Version)  ->
    io_lib:format("~p", [Other]);

format_result(Code, {_Name, rescode}, _Version) ->
    make_status(Code);

format_result({Code, Text}, {_Name, restuple}, _Version) ->
    {io_lib:format("~ts", [Text]), make_status(Code)};

format_result([], {_Name, {top_result_list, _ElementsDef}}, _Version) ->
    "";
format_result([FirstElement | Elements], {_Name, {top_result_list, ElementsDef}}, Version) ->
    [format_result(FirstElement, ElementsDef, Version) |
     lists:map(
       fun(Element) ->
	       ["\n" | format_result(Element, ElementsDef, Version)]
       end,
       Elements)];

%% The result is a list of something: [something()]
format_result([], {_Name, {list, _ElementsDef}}, _Version) ->
    "";
format_result([FirstElement | Elements], {_Name, {list, ElementsDef}}, Version) ->
    Separator = case Version of
                    0 -> ";";
                    _ -> ","
                end,
    %% Start formatting the first element
    [format_result(FirstElement, ElementsDef, Version) |
     %% If there are more elements, put always first a newline character
     lists:map(
       fun(Element) ->
	       [Separator | format_result(Element, ElementsDef, Version)]
       end,
       Elements)];

%% The result is a tuple with several elements: {something1(), something2(),...}
%% NOTE: the elements in the tuple are separated with tabular characters,
%% if a string is empty, it will be difficult to notice in the shell,
%% maybe a different separation character should be used, like ;;?
format_result(ElementsTuple, {_Name, {tuple, ElementsDef}}, Version) ->
    ElementsList = tuple_to_list(ElementsTuple),
    [{FirstE, FirstD} | ElementsAndDef] = lists:zip(ElementsList, ElementsDef),
    [format_result(FirstE, FirstD, Version) |
     lists:map(
       fun({Element, ElementDef}) ->
	       ["\t" | format_result(Element, ElementDef, Version)]
       end,
       ElementsAndDef)];

format_result(404, {_Name, _}, _Version) ->
    make_status(not_found).

make_status(ok) -> ?STATUS_SUCCESS;
make_status(true) -> ?STATUS_SUCCESS;
make_status(Code) when is_integer(Code), Code > 255 -> ?STATUS_ERROR;
make_status(Code) when is_integer(Code), Code > 0 -> Code;
make_status(Error) ->
    io:format("Error: ~p~n", [Error]),
    ?STATUS_ERROR.

get_list_commands(Version) ->
    try ejabberd_commands:list_commands(Version) of
	Commands ->
	    [tuple_command_help(Command) || Command <- Commands]
    catch
	exit:_ ->
	    []
    end.

%% Return: {string(), [string()], string()}
tuple_command_help({Name, _Args, Desc}) ->
    {Args, _, _} = ejabberd_commands:get_command_format(Name, admin),
    Arguments = [atom_to_list(ArgN) || {ArgN, _ArgF} <- Args],
    CallString = atom_to_list(Name),
    {CallString, Arguments, Desc}.

has_tuple_args(Args) ->
    lists:any(
      fun({_Name, tuple}) -> true;
         ({_Name, {tuple, _}}) -> true;
         ({_Name, {list, SubArg}}) ->
            has_tuple_args([SubArg]);
         (_) -> false
      end,
      Args).

has_list_args(Args) ->
    lists:any(
      fun({_Name, list}) -> true;
         ({_Name, {list, _}}) -> true;
         (_) -> false
      end,
      Args).

%%-----------------------------
%% Print help
%%-----------------------------

%% Commands are Bold
-define(B1, "\e[1m").
-define(B2, "\e[22m").
-define(C(S), case ShCode of true -> [?B1, S, ?B2]; false -> S end).

%% Arguments are Dim
-define(D1, "\e[2m").
-define(D2, "\e[22m").
-define(A(S), case ShCode of true -> [?D1, S, ?D2]; false -> S end).

%% Tags are Underline
-define(U1, "\e[4m").
-define(U2, "\e[24m").
-define(G(S), case ShCode of true -> [?U1, S, ?U2]; false -> S end).

%% B are Nothing
-define(N1, "\e[0m").
-define(N2, "\e[0m").
-define(B(S), case ShCode of true -> [?N1, S, ?N2]; false -> S end).

print_usage(Version) ->
    {MaxC, ShCode} = get_shell_info(),
    print_usage(dual, MaxC, ShCode, Version).
print_usage(HelpMode, MaxC, ShCode, Version) ->
    AllCommands = get_list_commands(Version),

    print(
       ["Usage: ", "ejabberdctl", " [--no-timeout] [--node ", ?A("nodename"), "] [--version ", ?A("api_version"), "] ",
	?C("command"), " [", ?A("arguments"), "]\n"
	"\n"
	"Available commands in this ejabberd node:\n"], []),
    print_usage_commands(HelpMode, MaxC, ShCode, AllCommands).

print_usage_commands(HelpMode, MaxC, ShCode, Commands) ->
    CmdDescsSorted = lists:keysort(1, Commands),

    %% What is the length of the largest command?
    {CmdArgsLenDescsSorted, Lens} =
	lists:mapfoldl(
	  fun({Cmd, Args, Desc}, Lengths) ->
		  Len =
		      length(Cmd) +
		      lists:foldl(fun(Arg, R) ->
					  R + 1 + length(Arg)
				  end,
				  0,
				  Args),
		  {{Cmd, Args, Len, Desc}, [Len | Lengths]}
	  end,
	  [],
	  CmdDescsSorted),
    MaxCmdLen = case Lens of
		    [] -> 80;
		    _ -> lists:max(Lens)
		end,

    %% For each command in the list of commands
    %% Convert its definition to a line
    FmtCmdDescs = format_command_lines(CmdArgsLenDescsSorted, MaxCmdLen, MaxC, ShCode, HelpMode),

    print([FmtCmdDescs], []).


%% Get some info about the shell:
%% how many columns of width
%% and guess if it supports text formatting codes.
get_shell_info() ->
    %% This function was introduced in OTP R12B-0
    try io:columns() of
	{ok, C} -> {C-2, true};
	{error, enotsup} -> {78, false}
    catch
	_:_ -> {78, false}
    end.

%% Erlang/OTP 20.0 introduced string:find/2, but we must support old 19.3
string_find([], _SearchPattern) ->
    nomatch;
string_find([A | String], [A]) ->
    String;
string_find([_ | String], SearchPattern) ->
    string_find(String, SearchPattern).

%% Split this command description in several lines of proper length
prepare_description(DescInit, MaxC, Desc) ->
    case string_find(Desc, "\n") of
        nomatch ->
            prepare_description2(DescInit, MaxC, Desc);
        _ ->
            Desc
    end.

prepare_description2(DescInit, MaxC, Desc) ->
    Words = string:tokens(Desc, " "),
    prepare_long_line(DescInit, MaxC, Words).

prepare_long_line(DescInit, MaxC, Words) ->
    MaxSegmentLen = MaxC - DescInit,
    MarginString = lists:duplicate(DescInit, $\s), % Put spaces
    [FirstSegment | MoreSegments] = split_desc_segments(MaxSegmentLen, Words),
    MoreSegmentsMixed = mix_desc_segments(MarginString, MoreSegments),
    [FirstSegment | MoreSegmentsMixed].

mix_desc_segments(MarginString, Segments) ->
    [["\n", MarginString, Segment] || Segment <- Segments].

split_desc_segments(MaxL, Words) ->
    join(MaxL, Words).

%% Join words in a segment,
%% but stop adding to a segment if adding this word would pass L
join(L, Words) ->
    join(L, Words, 0, [], []).

join(_Len, [], _CurSegLen, CurSeg, AllSegs) ->
    lists:reverse([CurSeg | AllSegs]);
join(Len, [Word | Tail], CurSegLen, CurSeg, AllSegs) ->
    WordLen = length(Word),
    SegSize = WordLen + CurSegLen + 1,
    {NewCurSeg, NewAllSegs, NewCurSegLen} =
        if SegSize < Len ->
                {[CurSeg, " ", Word], AllSegs, SegSize};
           true ->
                {Word, [CurSeg | AllSegs], WordLen}
        end,
    NewLen = case string:str(Word, "\n") of
                 0 ->
                     NewCurSegLen;
                 _ ->
                     0
             end,
    join(Len, Tail, NewLen, NewCurSeg, NewAllSegs).


format_command_lines(CALD, MaxCmdLen, MaxC, ShCode, dual)
  when MaxC - MaxCmdLen < 40 ->
    %% If the space available for descriptions is too narrow, enforce long help mode
    format_command_lines(CALD, MaxCmdLen, MaxC, ShCode, long);

format_command_lines(CALD, _MaxCmdLen, _MaxC, ShCode, short) ->
    lists:map(
      fun({Cmd, Args, _CmdArgsL, _Desc}) ->
	      ["    ", ?C(Cmd), [[" ", ?A(Arg)] || Arg <- Args], "\n"]
      end, CALD);

format_command_lines(CALD, MaxCmdLen, MaxC, ShCode, dual) ->
    lists:map(
      fun({Cmd, Args, CmdArgsL, Desc}) ->
	      DescFmt = prepare_description(MaxCmdLen+4, MaxC, Desc),
	      ["  ", ?C(Cmd), [[" ", ?A(Arg)] || Arg <- Args],
               lists:duplicate(MaxCmdLen - CmdArgsL + 1, $\s),
	       DescFmt, "\n"]
      end, CALD);

format_command_lines(CALD, _MaxCmdLen, MaxC, ShCode, long) ->
    lists:map(
      fun({Cmd, Args, _CmdArgsL, Desc}) ->
	      DescFmt = prepare_description(13, MaxC, Desc),
	      ["  ", ?C(Cmd), [[" ", ?A(Arg)] || Arg <- Args], "\n",
               "            ", DescFmt, "\n"]
      end, CALD).


%%-----------------------------
%% Print Tags
%%-----------------------------

print_usage_tags(MaxC, ShCode, Version) ->
    print("Available tags and list of commands:", []),
    TagsCommands = ejabberd_commands:get_tags_commands(Version),
    lists:foreach(
      fun({Tag, Commands} = _TagCommands) ->
	      print(["\n\n  ", ?G(Tag), "\n    "], []),
	      Words = lists:sort(Commands),
	      Desc = prepare_long_line(5, MaxC, Words),
	      print(?C(Desc), [])
      end,
      TagsCommands),
    print("\n\n", []).

print_usage_tags_long(MaxC, ShCode, Version) ->
    print("Available tags and commands details:", []),
    TagsCommands = ejabberd_commands:get_tags_commands(Version),
    print("\n", []),
    lists:foreach(
      fun({Tag, CommandsNames} = _TagCommands) ->
	      print(["\n  ", ?G(Tag), "\n"], []),
                CommandsList = lists:map(
                                 fun(NameString) ->
                                         C = ejabberd_commands:get_command_definition(
                                               list_to_atom(NameString), Version),
                                         #ejabberd_commands{name = Name,
                                                            args = Args,
                                                            desc = Desc} = C,
                                         tuple_command_help({Name, Args, Desc})
                                 end,
                                 CommandsNames),
                print_usage_commands(short, MaxC, ShCode, CommandsList)
      end,
      TagsCommands),
    print("\n", []).

print_usage_tags(Tag, MaxC, ShCode, Version) ->
    print(["Available commands with tag ", ?G(Tag), ":", "\n", "\n"], []),
    HelpMode = long,
    TagsCommands = ejabberd_commands:get_tags_commands(Version),
    CommandsNames = case lists:keysearch(Tag, 1, TagsCommands) of
			{value, {Tag, CNs}} -> CNs;
			false -> []
		    end,
    CommandsList = lists:map(
		     fun(NameString) ->
			     C = ejabberd_commands:get_command_definition(
                                   list_to_atom(NameString), Version),
			     #ejabberd_commands{name = Name,
						args = Args,
						desc = Desc} = C,
			     tuple_command_help({Name, Args, Desc})
		     end,
		     CommandsNames),
    print_usage_commands(HelpMode, MaxC, ShCode, CommandsList),
    print("\n", []).


%%-----------------------------
%% Print usage of 'help' command
%%-----------------------------

print_usage_help(MaxC, ShCode) ->
    LongDesc =
	["This special ", ?C("help"), " command provides help of ejabberd commands.\n\n"
	 "The format is:\n  ", ?B("ejabberdctl"), " ", ?C("help"),
         " [", ?A("tags"), " | ", ?A("commands"), " | ", ?G("tag"), " | ", ?C("command"), " | ", ?C("com?*"), "]\n\n"
	 "The optional arguments:\n"
	 "  ",?A("tags"),"         Show all tags and commands names in each tag\n"
	 "  ",?A("commands"),"     Show all tags and commands details in each tag\n"
	 "  ",?G("tag"),"          Show commands related to this tag\n"
	 "  ",?C("command"),"      Show detailed description of this command\n"
	 "  ",?C("com?*"),"        Show commands that match this glob.\n"
	 "               (? will match a simple character, and\n"
	 "                * will match several characters)\n"
	 "\n",
	 "Some example usages:\n",
	 "  ejabberdctl ", ?C("help"), "\n",
	 "  ejabberdctl ", ?C("help"), " ", ?A("tags"), "\n",
	 "  ejabberdctl ", ?C("help"), " ", ?A("commands"), "\n",
	 "  ejabberdctl ", ?C("help"), " ", ?G("accounts"), "\n",
	 "  ejabberdctl ", ?C("help"), " ", ?C("register"), "\n",
	 "  ejabberdctl ", ?C("help"), " ", ?C("regist*"), "\n",
	 "\n",
	 "Some command arguments are lists or tuples, like add_rosteritem and create_room_with_opts.\n",
	 "Separate the elements in a list with the , character.\n",
	 "Separate the elements in a tuple with the : character.\n",
	 "\n",
	 "Some commands results are lists or tuples, like get_roster and get_user_subscriptions.\n",
	 "The elements in a list are separated with a , character.\n",
	 "The elements in a tuple are separated with a tabular character.\n"],
    ArgsDef = [],
    C = #ejabberd_commands{
	   name = help,
	   desc = "Show help of ejabberd commands",
	   longdesc = lists:flatten(LongDesc),
	   args = ArgsDef,
	   result = {help, string}},
    print(get_usage_command2("help", C, MaxC, ShCode), []).


%%-----------------------------
%% Print usage command
%%-----------------------------

-spec print_usage_commands2(CmdSubString::string(), MaxC::integer(),
                            ShCode::boolean(), Version::integer()) -> ok.
print_usage_commands2(CmdSubString, MaxC, ShCode, Version) ->
    %% Get which command names match this substring
    AllCommandsNames = [atom_to_list(Name) || {Name, _, _} <- ejabberd_commands:list_commands(Version)],
    Cmds = filter_commands(AllCommandsNames, CmdSubString),
    case Cmds of
	[] -> io:format("Error: no command found that match '~ts'~n", [CmdSubString]);
	_ -> print_usage_commands3(lists:sort(Cmds), MaxC, ShCode, Version)
    end.

print_usage_commands3([Cmd], MaxC, ShCode, Version) ->
    print_usage_command(Cmd, MaxC, ShCode, Version);
print_usage_commands3(Cmds, MaxC, ShCode, Version) ->
        CommandsList = lists:map(
		     fun(NameString) ->
			     C = ejabberd_commands:get_command_definition(
                                   list_to_atom(NameString), Version),
			     #ejabberd_commands{name = Name,
						args = Args,
						desc = Desc} = C,
			     tuple_command_help({Name, Args, Desc})
		     end,
		     Cmds),

	      print_usage_commands(long, MaxC, ShCode, CommandsList), %% que aqui solo muestre un par de lineas
              ok.

filter_commands(All, SubString) ->
    case lists:member(SubString, All) of
	true -> [SubString];
	false -> filter_commands_regexp(All, SubString)
    end.

filter_commands_regexp(All, Glob) ->
    RegExp = ejabberd_regexp:sh_to_awk(list_to_binary(Glob)),
    lists:filter(
      fun(Command) ->
	      case ejabberd_regexp:run(list_to_binary(Command), RegExp) of
		  match ->
		      true;
		  nomatch ->
		      false
	      end
      end,
      All).

maybe_add_policy_arguments(Args, user) ->
    [{user, binary}, {host, binary} | Args];
maybe_add_policy_arguments(Args, _) ->
    Args.

-spec print_usage_command(Cmd::string(), MaxC::integer(),
                          ShCode::boolean(), Version::integer()) -> ok.
print_usage_command(Cmd, MaxC, ShCode, Version) ->
    print(get_usage_command(Cmd, MaxC, ShCode, Version), []).

get_usage_command(Cmd, MaxC, ShCode, Version) ->
    Name = list_to_atom(Cmd),
    C = ejabberd_commands:get_command_definition(Name, Version),
    get_usage_command2(Cmd, C, MaxC, ShCode).

get_usage_command2(Cmd, C, MaxC, ShCode) ->
    #ejabberd_commands{
		     tags = TagsAtoms,
		     definer = Definer,
		     desc = Desc,
		     args = ArgsDefPreliminary,
		     args_desc = ArgsDesc,
		     args_example = ArgsExample,
		     result_example = ResultExample,
		     policy = Policy,
		     longdesc = LongDesc,
		     note = Note,
		     result = ResultDef} = C,

    NameFmt = ["  ", ?B("Command Name"), ": ", ?C(Cmd), "\n"],

    %% Initial indentation of result is 13 = length("  Arguments: ")
    ArgsDef = maybe_add_policy_arguments(ArgsDefPreliminary, Policy),
    ArgsDetailed = add_args_desc(ArgsDef, ArgsDesc),
    Args = [format_usage_ctype1(ArgDetailed, 13, ShCode) || ArgDetailed <- ArgsDetailed],

    ArgsMargin = lists:duplicate(13, $\s),
    ArgsListFmt = case Args of
		      [] -> "\n";
		      _ -> [ [Arg, "\n", ArgsMargin] || Arg <- Args]
		  end,
    ArgsFmt = ["  ", ?B("Arguments"), ": ", ArgsListFmt],

    %% Initial indentation of result is 11 = length("  Returns: ")
    ResultFmt = format_usage_ctype(ResultDef, 11),
    ReturnsFmt = ["  ",?B("Result"),": ", ResultFmt],

    ExampleMargin = lists:duplicate(11, $\s),
    Example = format_usage_example(Cmd, ArgsExample, ResultExample, ExampleMargin),
    ExampleFmt = case Example of
		      [] ->
                            "";
		      _ ->
                            ExampleListFmt = [ [Ex, "\n", ExampleMargin] || Ex <- Example],
                            ["  ",?B("Example"),": ", ExampleListFmt, "\n"]
		  end,

    TagsFmt = ["  ",?B("Tags"),":", prepare_long_line(8, MaxC, [?G(atom_to_list(TagA)) || TagA <- TagsAtoms])],

    IsDefinerMod = case Definer of
                     unknown -> true;
                     _ -> lists:member([gen_mod], proplists:get_all_values(behaviour, Definer:module_info(attributes)))
                 end,
    ModuleFmt = case IsDefinerMod of
                    true -> ["  ",?B("Module"),": ", atom_to_list(Definer), "\n\n"];
                    false -> []
                end,

    NoteFmt = case Note of
                    "" -> [];
                    _ -> ["  ",?B("Note"),": ", Note, "\n\n"]
                end,

    DescFmt = ["  ",?B("Description"),":", prepare_description(15, MaxC, Desc)],

    LongDescFmt = case LongDesc of
		      "" -> "";
		      _ -> ["", prepare_description(0, MaxC, LongDesc), "\n\n"]
		  end,

    NoteEjabberdctlList = case has_list_args(ArgsDefPreliminary) of
			  true -> ["  ", ?B("Note:"), " In a list argument, separate the elements using the , character for example: one,two,three\n\n"];
			  false -> ""
		      end,
    NoteEjabberdctlTuple = case has_tuple_args(ArgsDefPreliminary) of
			  true -> ["  ", ?B("Note:"), " In a tuple argument, separate the elements using the : character for example: members_only:true\n\n"];
			  false -> ""
		      end,

    First = case Cmd of
        "help" -> "";
        _ -> [NameFmt, "\n", ArgsFmt, "\n", ReturnsFmt,
                    "\n\n", ExampleFmt, TagsFmt, "\n\n", ModuleFmt, NoteFmt, DescFmt, "\n\n"]
    end,
    [First, LongDescFmt, NoteEjabberdctlList, NoteEjabberdctlTuple].

%%-----------------------------
%% Format Arguments Help
%%-----------------------------

add_args_desc(Definitions, none) ->
    Descriptions = lists:duplicate(length(Definitions), ""),
    add_args_desc(Definitions, Descriptions);
add_args_desc(Definitions, Descriptions) ->
    lists:zipwith(fun({Name, Type}, Description) ->
                          {Name, Type, Description} end,
                  Definitions,
                  Descriptions).

format_usage_ctype1({_Name, _Type} = Definition, Indentation, ShCode) ->
    [Arg] = add_args_desc([Definition], none),
    format_usage_ctype1(Arg, Indentation, ShCode);
format_usage_ctype1({Name, Type, Description}, Indentation, ShCode) ->
    TypeString = case Type of
        {list, ElementDef} ->
            NameFmt = atom_to_list(Name),
            Indentation2 = Indentation + length(NameFmt) + 4,
            ElementFmt = format_usage_ctype1(ElementDef, Indentation2, ShCode),
            io_lib:format("[ ~s ]", [lists:flatten(ElementFmt)]);
        {tuple, ElementsDef} ->
            NameFmt = atom_to_list(Name),
            Indentation2 = Indentation + length(NameFmt) + 4,
            ElementsFmt = format_usage_tuple(ElementsDef, Indentation2),
            io_lib:format("{ ~s }", [lists:flatten(ElementsFmt)]);
        _ ->
            Type
    end,
    DescriptionText = case Description of
                          "" -> "";
                          Description -> " : "++Description
                      end,
    io_lib:format("~p::~s~s", [Name, TypeString, DescriptionText]).


format_usage_ctype(Type, _Indentation)
  when (Type==atom) or (Type==integer) or (Type==string) or (Type==binary)
       or (Type==rescode) or (Type==restuple) ->
    io_lib:format("~p", [Type]);

format_usage_ctype({Name, Type}, _Indentation)
  when (Type==atom) or (Type==integer) or (Type==string) or (Type==binary)
       or (Type==rescode) or (Type==restuple)
       or (Type==any) ->
    io_lib:format("~p::~p", [Name, Type]);

format_usage_ctype({Name, {list, ElementDef}}, Indentation) ->
    NameFmt = atom_to_list(Name),
    Indentation2 = Indentation + length(NameFmt) + 4,
    ElementFmt = format_usage_ctype(ElementDef, Indentation2),
    [NameFmt, "::[ ", ElementFmt, " ]"];

format_usage_ctype({Name, {tuple, ElementsDef}}, Indentation) ->
    NameFmt = atom_to_list(Name),
    Indentation2 = Indentation + length(NameFmt) + 4,
    ElementsFmt = format_usage_tuple(ElementsDef, Indentation2),
    [NameFmt, "::{ "] ++ ElementsFmt ++ [" }"].


format_usage_tuple([], _Indentation) ->
    [];
format_usage_tuple([ElementDef], Indentation) ->
    format_usage_ctype(ElementDef, Indentation);
format_usage_tuple([ElementDef | ElementsDef], Indentation) ->
    ElementFmt = format_usage_ctype(ElementDef, Indentation),
    MarginString = lists:duplicate(Indentation, $\s), % Put spaces
    [ElementFmt, ",\n", MarginString, format_usage_tuple(ElementsDef, Indentation)].

print(Format, Args) ->
    io:format(lists:flatten(Format), Args).

-ifdef(LAGER).
disable_logging() ->
    ok.
-else.
disable_logging() ->
    logger:set_primary_config(level, none).
-endif.

%%-----------------------------
%% Format Example Help
%%-----------------------------

format_usage_example(_Cmd, none, _ResultExample, _Indentation) ->
    "";
format_usage_example(Cmd, ArgsExample, ResultExample, Indentation) ->
    Arguments = format_usage_arguments(ArgsExample, []),
    Result = format_usage_result([ResultExample], [], Indentation),
    [lists:join(" ", ["ejabberdctl", Cmd] ++ Arguments) | Result].

format_usage_arguments([], R) ->
    lists:reverse(R);

format_usage_arguments([Argument | Arguments], R)
  when is_integer(Argument) ->
    format_usage_arguments(Arguments, [integer_to_list(Argument) | R]);

format_usage_arguments([[Integer|_] = Argument | Arguments], R)
  when is_list(Argument) and is_integer(Integer) ->
    Result = case contains_more_than_letters(Argument) of
                 true -> ["\"", Argument, "\""];
                 false -> [Argument]
             end,
    format_usage_arguments(Arguments, [Result | R]);

format_usage_arguments([[Element | _] = Argument | Arguments], R)
  when is_list(Argument) and is_tuple(Element) ->
    ArgumentFmt = format_usage_arguments(Argument, []),
    format_usage_arguments(Arguments, [lists:join(",", ArgumentFmt) | R]);

format_usage_arguments([Argument | Arguments], R)
  when is_list(Argument) ->
    Result = format_usage_arguments(Argument, []),
    format_usage_arguments(Arguments, [lists:join(",", Result) | R]);

format_usage_arguments([Argument | Arguments], R)
  when is_tuple(Argument) ->
    Result = format_usage_arguments(tuple_to_list(Argument), []),
    format_usage_arguments(Arguments, [lists:join(":", Result) | R]);

format_usage_arguments([Argument | Arguments], R)
  when is_binary(Argument) ->
    Result = case contains_more_than_letters(binary_to_list(Argument)) of
                 true -> ["\"", Argument, "\""];
                 false -> [Argument]
             end,
    format_usage_arguments(Arguments, [Result | R]);

format_usage_arguments([Argument | Arguments], R) ->
    format_usage_arguments(Arguments, [Argument | R]).

format_usage_result([none], _R, _Indentation) ->
    "";
format_usage_result([], R, _Indentation) ->
    lists:reverse(R);

format_usage_result([{Code, Text} | Arguments], R, Indentation)
  when is_atom(Code) and is_binary(Text) ->
    format_usage_result(Arguments, [Text | R], Indentation);

format_usage_result([Argument | Arguments], R, Indentation)
  when is_atom(Argument) ->
    format_usage_result(Arguments, [["\'", atom_to_list(Argument), "\'"] | R], Indentation);

format_usage_result([Argument | Arguments], R, Indentation)
  when is_integer(Argument) ->
    format_usage_result(Arguments, [integer_to_list(Argument) | R], Indentation);

format_usage_result([[Integer|_] = Argument | Arguments], R, Indentation)
  when is_list(Argument) and is_integer(Integer) ->
    format_usage_result(Arguments, [Argument | R], Indentation);

format_usage_result([[Element | _] = Argument | Arguments], R, Indentation)
  when is_list(Argument) and is_tuple(Element) ->
    ArgumentFmt = format_usage_result(Argument, [], Indentation),
    format_usage_result(Arguments, [lists:join("\n"++Indentation, ArgumentFmt) | R], Indentation);

format_usage_result([Argument | Arguments], R, Indentation)
  when is_list(Argument) ->
    format_usage_result(Arguments, [lists:join("\n"++Indentation, Argument) | R], Indentation);

format_usage_result([Argument | Arguments], R, Indentation)
  when is_tuple(Argument) ->
    Result = format_usage_result(tuple_to_list(Argument), [], Indentation),
    format_usage_result(Arguments, [lists:join("\t", Result) | R], Indentation);

format_usage_result([Argument | Arguments], R, Indentation) ->
    format_usage_result(Arguments, [Argument | R], Indentation).

contains_more_than_letters(Argument) ->
    lists:any(fun(I) when (I < $A) -> true;
                 (I) when (I > $z) -> true;
                 (_) -> false end,
              Argument).

%%-----------------------------
%% Register commands
%%-----------------------------

get_commands_spec() ->
    [
     #ejabberd_commands{name = help, tags = [ejabberdctl],
			desc = "Get list of commands, or help of a command (only ejabberdctl)",
			longdesc = "This command is exclusive for the ejabberdctl command-line script, "
			"don't attempt to execute it using any other API frontend."},
     #ejabberd_commands{name = mnesia_info_ctl, tags = [ejabberdctl, mnesia],
			desc = "Show information of Mnesia system (only ejabberdctl)",
			note = "renamed in 24.02",
			longdesc = "This command is exclusive for the ejabberdctl command-line script, "
			"don't attempt to execute it using any other API frontend."},
     #ejabberd_commands{name = print_sql_schema, tags = [ejabberdctl, sql],
			desc = "Print SQL schema for the given RDBMS (only ejabberdctl)",
			longdesc = "This command is exclusive for the ejabberdctl command-line script, "
			"don't attempt to execute it using any other API frontend.",
			note = "added in 24.02",
			args = [{db_type, string}, {db_version, string}, {new_schema, string}],
                        args_desc = ["Database type: pgsql | mysql | sqlite",
                                     "Your database version: 16.1, 8.2.0...",
                                     "Use new schema: 0, false, 1 or true"],
                        args_example = ["pgsql", "16.1", "true"]}
    ].
