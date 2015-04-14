%%%----------------------------------------------------------------------
%%% File    : ejabberd_ctl.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : ejabberd command line admin tool
%%% Created : 11 Jan 2004 by Alexey Shchepin <alexey@process-one.net>
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

%%% @headerfile "ejabberd_ctl.hrl"

%%% @doc Management of ejabberdctl commands and frontend to ejabberd commands.
%%%
%%% An ejabberdctl command is an abstract function identified by a
%%% name, with a defined number of calling arguments, that can be
%%% defined in any Erlang module and executed using ejabberdctl
%%% administration script.
%%%
%%% Note: strings cannot have blankspaces
%%%
%%% Does not support commands that have arguments with ctypes: list, tuple
%%%
%%% TODO: Update the guide
%%% TODO: Mention this in the release notes
%%% Note: the commands with several words use now the underline: _
%%% It is still possible to call the commands with dash: -
%%% but this is deprecated, and may be removed in a future version.


-module(ejabberd_ctl).
-author('alexey@process-one.net').

-export([start/0,
	 init/0,
	 process/1,
	 process2/2,
	 register_commands/3,
	 unregister_commands/3]).

-include("ejabberd_ctl.hrl").
-include("ejabberd_commands.hrl").
-include("ejabberd.hrl").
-include("logger.hrl").


%%-----------------------------
%% Module
%%-----------------------------

start() ->
    case init:get_plain_arguments() of
	[SNode | Args] ->
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
	    Status = case rpc:call(Node, ?MODULE, process, [Args]) of
			 {badrpc, Reason} ->
			     print("Failed RPC connection to the node ~p: ~p~n",
				    [Node, Reason]),
			     %% TODO: show minimal start help
			     ?STATUS_BADRPC;
			 S ->
			     S
		     end,
	    halt(Status);
	_ ->
	    print_usage(),
	    halt(?STATUS_USAGE)
    end.

init() ->
    ets:new(ejabberd_ctl_cmds, [named_table, set, public]),
    ets:new(ejabberd_ctl_host_cmds, [named_table, set, public]).


%%-----------------------------
%% ejabberdctl Command managment
%%-----------------------------

register_commands(CmdDescs, Module, Function) ->
    ets:insert(ejabberd_ctl_cmds, CmdDescs),
    ejabberd_hooks:add(ejabberd_ctl_process,
		       Module, Function, 50),
    ok.

unregister_commands(CmdDescs, Module, Function) ->
    lists:foreach(fun(CmdDesc) ->
			  ets:delete_object(ejabberd_ctl_cmds, CmdDesc)
		  end, CmdDescs),
    ejabberd_hooks:delete(ejabberd_ctl_process,
			  Module, Function, 50),
    ok.


%%-----------------------------
%% Process
%%-----------------------------

-spec process([string()]) -> non_neg_integer().

%% The commands status, stop and restart are defined here to ensure
%% they are usable even if ejabberd is completely stopped.
process(["status"]) ->
    {InternalStatus, ProvidedStatus} = init:get_status(),
    print("The node ~p is ~p with status: ~p~n",
	   [node(), InternalStatus, ProvidedStatus]),
    case lists:keysearch(ejabberd, 1, application:which_applications()) of
        false ->
            EjabberdLogPath = ejabberd_logger:get_log_path(),
            print("ejabberd is not running in that node~n"
		   "Check for error messages: ~s~n"
		   "or other files in that directory.~n", [EjabberdLogPath]),
            ?STATUS_ERROR;
        {value, {_, _, Version}} ->
            print("ejabberd ~s is running in that node~n", [Version]),
            ?STATUS_SUCCESS
    end;

process(["stop"]) ->
    %%ejabberd_cover:stop(),
    init:stop(),
    ?STATUS_SUCCESS;

process(["restart"]) ->
    init:restart(),
    ?STATUS_SUCCESS;

process(["mnesia"]) ->
    print("~p~n", [mnesia:system_info(all)]),
    ?STATUS_SUCCESS;

process(["mnesia", "info"]) ->
    mnesia:info(),
    ?STATUS_SUCCESS;

process(["mnesia", Arg]) ->
    case catch mnesia:system_info(list_to_atom(Arg)) of
	{'EXIT', Error} -> print("Error: ~p~n", [Error]);
	Return -> print("~p~n", [Return])
    end,
    ?STATUS_SUCCESS;

%% The arguments --long and --dual are not documented because they are
%% automatically selected depending in the number of columns of the shell
process(["help" | Mode]) ->
    {MaxC, ShCode} = get_shell_info(),
    case Mode of
	[] ->
	    print_usage(dual, MaxC, ShCode),
	    ?STATUS_USAGE;
	["--dual"] ->
	    print_usage(dual, MaxC, ShCode),
	    ?STATUS_USAGE;
	["--long"] ->
	    print_usage(long, MaxC, ShCode),
	    ?STATUS_USAGE;
	["--tags"] ->
	    print_usage_tags(MaxC, ShCode),
	    ?STATUS_SUCCESS;
	["--tags", Tag] ->
	    print_usage_tags(Tag, MaxC, ShCode),
	    ?STATUS_SUCCESS;
	["help"] ->
	    print_usage_help(MaxC, ShCode),
	    ?STATUS_SUCCESS;
	[CmdString | _] ->
	    CmdStringU = ejabberd_regexp:greplace(
                           list_to_binary(CmdString), <<"-">>, <<"_">>),
	    print_usage_commands(binary_to_list(CmdStringU), MaxC, ShCode),
	    ?STATUS_SUCCESS
    end;

process(Args) ->
    AccessCommands = get_accesscommands(),
    {String, Code} = process2(Args, AccessCommands),
    case String of
	[] -> ok;
	_ ->
	    io:format("~s~n", [String])
    end,
    Code.

%% @spec (Args::[string()], AccessCommands) -> {String::string(), Code::integer()}
process2(["--auth", User, Server, Pass | Args], AccessCommands) ->
    process2(Args, {list_to_binary(User), list_to_binary(Server), list_to_binary(Pass)}, AccessCommands);
process2(Args, AccessCommands) ->
    process2(Args, noauth, AccessCommands).

process2(Args, Auth, AccessCommands) ->
    case try_run_ctp(Args, Auth, AccessCommands) of
	{String, wrong_command_arguments}
          when is_list(String) ->
	    io:format(lists:flatten(["\n" | String]++["\n"])),
	    [CommandString | _] = Args,
            process(["help" | [CommandString]]),
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

get_accesscommands() ->
    ejabberd_config:get_option(ejabberdctl_access_commands,
                                     fun(V) when is_list(V) -> V end, []).

%%-----------------------------
%% Command calling
%%-----------------------------

%% @spec (Args::[string()], Auth, AccessCommands) -> string() | integer() | {string(), integer()}
try_run_ctp(Args, Auth, AccessCommands) ->
    try ejabberd_hooks:run_fold(ejabberd_ctl_process, false, [Args]) of
	false when Args /= [] ->
	    try_call_command(Args, Auth, AccessCommands);
	false ->
	    print_usage(),
	    {"", ?STATUS_USAGE};
	Status ->
	    {"", Status}
    catch
	exit:Why ->
	    print_usage(),
	    {io_lib:format("Error in ejabberd ctl process: ~p", [Why]), ?STATUS_USAGE};
	Error:Why ->
            %% In this case probably ejabberd is not started, so let's show Status
            process(["status"]),
            print("~n", []),
	    {io_lib:format("Error in ejabberd ctl process: '~p' ~p", [Error, Why]), ?STATUS_USAGE}
    end.

%% @spec (Args::[string()], Auth, AccessCommands) -> string() | integer() | {string(), integer()}
try_call_command(Args, Auth, AccessCommands) ->
    try call_command(Args, Auth, AccessCommands) of
	{error, command_unknown} ->
	    {io_lib:format("Error: command ~p not known.", [hd(Args)]), ?STATUS_ERROR};
	{error, wrong_command_arguments} ->
	    {"Error: wrong arguments", ?STATUS_ERROR};
	Res ->
	    Res
    catch
	A:Why ->
	    Stack = erlang:get_stacktrace(),
	    {io_lib:format("Problem '~p ~p' occurred executing the command.~nStacktrace: ~p", [A, Why, Stack]), ?STATUS_ERROR}
    end.

%% @spec (Args::[string()], Auth, AccessCommands) -> string() | integer() | {string(), integer()} | {error, ErrorType}
call_command([CmdString | Args], Auth, AccessCommands) ->
    CmdStringU = ejabberd_regexp:greplace(
                   list_to_binary(CmdString), <<"-">>, <<"_">>),
    Command = list_to_atom(binary_to_list(CmdStringU)),
    case ejabberd_commands:get_command_format(Command) of
	{error, command_unknown} ->
	    {error, command_unknown};
	{ArgsFormat, ResultFormat} ->
	    case (catch format_args(Args, ArgsFormat)) of
		ArgsFormatted when is_list(ArgsFormatted) ->
		    Result = ejabberd_commands:execute_command(AccessCommands, Auth, Command,
							       ArgsFormatted),
		    format_result(Result, ResultFormat);
		{'EXIT', {function_clause,[{lists,zip,[A1, A2], _} | _]}} ->
		    {NumCompa, TextCompa} =
			case {length(A1), length(A2)} of
			    {L1, L2} when L1 < L2 -> {L2-L1, "less argument"};
			    {L1, L2} when L1 > L2 -> {L1-L2, "more argument"}
			end,
		    {io_lib:format("Error: the command ~p requires ~p ~s.",
				   [CmdString, NumCompa, TextCompa]),
		     wrong_command_arguments}
	    end
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
    list_to_binary(format_arg(Arg, string));
format_arg("", string) ->
    "";
format_arg(Arg, string) ->
    NumChars = integer_to_list(length(Arg)),
    Parse = "~" ++ NumChars ++ "c",
    format_arg2(Arg, Parse).

format_arg2(Arg, Parse)->
    {ok, [Arg2], _RemainingArguments} = io_lib:fread(Parse, Arg),
    Arg2.

%%-----------------------------
%% Format result
%%-----------------------------

format_result({error, ErrorAtom}, _) ->
    {io_lib:format("Error: ~p", [ErrorAtom]), make_status(error)};

format_result(Atom, {_Name, atom}) ->
    io_lib:format("~p", [Atom]);

format_result(Int, {_Name, integer}) ->
    io_lib:format("~p", [Int]);

format_result(String, {_Name, string}) when is_list(String) ->
    io_lib:format("~s", [String]);

format_result(Binary, {_Name, string}) when is_binary(Binary) ->
    io_lib:format("~s", [binary_to_list(Binary)]);

format_result(Code, {_Name, rescode}) ->
    make_status(Code);

format_result({Code, Text}, {_Name, restuple}) ->
    {io_lib:format("~s", [Text]), make_status(Code)};

%% The result is a list of something: [something()]
format_result([], {_Name, {list, _ElementsDef}}) ->
    "";
format_result([FirstElement | Elements], {_Name, {list, ElementsDef}}) ->
    %% Start formatting the first element
    [format_result(FirstElement, ElementsDef) |
     %% If there are more elements, put always first a newline character
     lists:map(
       fun(Element) ->
	       ["\n" | format_result(Element, ElementsDef)]
       end,
       Elements)];

%% The result is a tuple with several elements: {something1(), something2(),...}
%% NOTE: the elements in the tuple are separated with tabular characters,
%% if a string is empty, it will be difficult to notice in the shell,
%% maybe a different separation character should be used, like ;;?
format_result(ElementsTuple, {_Name, {tuple, ElementsDef}}) ->
    ElementsList = tuple_to_list(ElementsTuple),
    [{FirstE, FirstD} | ElementsAndDef] = lists:zip(ElementsList, ElementsDef),
    [format_result(FirstE, FirstD) |
     lists:map(
       fun({Element, ElementDef}) ->
	       ["\t" | format_result(Element, ElementDef)]
       end,
       ElementsAndDef)].

make_status(ok) -> ?STATUS_SUCCESS;
make_status(true) -> ?STATUS_SUCCESS;
make_status(_Error) -> ?STATUS_ERROR.

get_list_commands() ->
    try ejabberd_commands:list_commands() of
	Commands ->
	    [tuple_command_help(Command)
	     || {N,_,_}=Command <- Commands,
		%% Don't show again those commands, because they are already
		%% announced by ejabberd_ctl itself
		N /= status, N /= stop, N /= restart]
    catch
	exit:_ ->
	    []
    end.

%% Return: {string(), [string()], string()}
tuple_command_help({Name, Args, Desc}) ->
    Arguments = [atom_to_list(ArgN) || {ArgN, _ArgF} <- Args],
    Prepend = case is_supported_args(Args) of
		  true -> "";
		  false -> "*"
	      end,
    CallString = atom_to_list(Name),
    {CallString, Arguments, Prepend ++ Desc}.

is_supported_args(Args) ->
    lists:all(
      fun({_Name, Format}) ->
	      (Format == integer)
		  or (Format == string)
		      or (Format == binary)
      end,
      Args).

get_list_ctls() ->
    case catch ets:tab2list(ejabberd_ctl_cmds) of
	{'EXIT', _} -> [];
	Cs -> [{NameArgs, [], Desc} || {NameArgs, Desc} <- Cs]
    end.


%%-----------------------------
%% Print help
%%-----------------------------

%% Bold
-define(B1, "\e[1m").
-define(B2, "\e[22m").
-define(B(S), case ShCode of true -> [?B1, S, ?B2]; false -> S end).

%% Underline
-define(U1, "\e[4m").
-define(U2, "\e[24m").
-define(U(S), case ShCode of true -> [?U1, S, ?U2]; false -> S end).

print_usage() ->
    {MaxC, ShCode} = get_shell_info(),
    print_usage(dual, MaxC, ShCode).
print_usage(HelpMode, MaxC, ShCode) ->
    AllCommands =
	[
	 {"status", [], "Get ejabberd status"},
	 {"stop", [], "Stop ejabberd"},
	 {"restart", [], "Restart ejabberd"},
	 {"help", ["[--tags [tag] | com?*]"], "Show help (try: ejabberdctl help help)"},
	 {"mnesia", ["[info]"], "show information of Mnesia system"}] ++
	get_list_commands() ++
	get_list_ctls(),

    print(
       ["Usage: ", ?B("ejabberdctl"), " [--node ", ?U("nodename"), "] [--auth ",
	?U("user"), " ", ?U("host"), " ", ?U("password"), "] ",
	?U("command"), " [", ?U("options"), "]\n"
	"\n"
	"Available commands in this ejabberd node:\n"], []),
    print_usage_commands(HelpMode, MaxC, ShCode, AllCommands),
    print(
       ["\n"
	"Examples:\n"
	"  ejabberdctl restart\n"
	"  ejabberdctl --node ejabberd@host restart\n"],
       []).

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

%% Split this command description in several lines of proper length
prepare_description(DescInit, MaxC, Desc) ->
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

format_command_lines(CALD, MaxCmdLen, MaxC, ShCode, dual) ->
    lists:map(
      fun({Cmd, Args, CmdArgsL, Desc}) ->
	      DescFmt = prepare_description(MaxCmdLen+4, MaxC, Desc),
	      ["   ", ?B(Cmd), " ", [[?U(Arg), " "] || Arg <- Args],
               string:chars($\s, MaxCmdLen - CmdArgsL + 1),
	       DescFmt, "\n"]
      end, CALD);

format_command_lines(CALD, _MaxCmdLen, MaxC, ShCode, long) ->
    lists:map(
      fun({Cmd, Args, _CmdArgsL, Desc}) ->
	      DescFmt = prepare_description(8, MaxC, Desc),
	      ["\n   ", ?B(Cmd), " ", [[?U(Arg), " "] || Arg <- Args], "\n", "        ",
	       DescFmt, "\n"]
      end, CALD).


%%-----------------------------
%% Print Tags
%%-----------------------------

print_usage_tags(MaxC, ShCode) ->
    print("Available tags and commands:", []),
    TagsCommands = ejabberd_commands:get_tags_commands(),
    lists:foreach(
      fun({Tag, Commands} = _TagCommands) ->
	      print(["\n\n  ", ?B(Tag), "\n     "], []),
	      Words = lists:sort(Commands),
	      Desc = prepare_long_line(5, MaxC, Words),
	      print(Desc, [])
      end,
      TagsCommands),
    print("\n\n", []).

print_usage_tags(Tag, MaxC, ShCode) ->
    print(["Available commands with tag ", ?B(Tag), ":", "\n"], []),
    HelpMode = long,
    TagsCommands = ejabberd_commands:get_tags_commands(),
    CommandsNames = case lists:keysearch(Tag, 1, TagsCommands) of
			{value, {Tag, CNs}} -> CNs;
			false -> []
		    end,
    CommandsList = lists:map(
		     fun(NameString) ->
			     C = ejabberd_commands:get_command_definition(
                                   list_to_atom(NameString)),
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
	["The special 'help' ejabberdctl command provides help of ejabberd commands.\n\n"
	 "The format is:\n  ", ?B("ejabberdctl"), " ", ?B("help"), " [", ?B("--tags"), " ", ?U("[tag]"), " | ", ?U("com?*"), "]\n\n"
	 "The optional arguments:\n"
	 "  ",?B("--tags"),"      Show all tags and the names of commands in each tag\n"
	 "  ",?B("--tags"), " ", ?U("tag"),"  Show description of commands in this tag\n"
	 "  ",?U("command"),"     Show detailed description of the command\n"
	 "  ",?U("com?*"),"       Show detailed description of commands that match this glob.\n"
	 "              You can use ? to match a simple character,\n"
	 "              and * to match several characters.\n"
	 "\n",
	 "Some example usages:\n",
	 "  ejabberdctl help\n",
	 "  ejabberdctl help --tags\n",
	 "  ejabberdctl help --tags accounts\n",
	 "  ejabberdctl help register\n",
	 "  ejabberdctl help regist*\n",
	 "\n",
	 "Please note that 'ejabberdctl help' shows all ejabberd commands,\n",
	 "even those that cannot be used in the shell with ejabberdctl.\n",
	 "Those commands can be identified because the description starts with: *"],
    ArgsDef = [],
    C = #ejabberd_commands{
      desc = "Show help of ejabberd commands",
      longdesc = lists:flatten(LongDesc),
      args = ArgsDef,
      result = {help, string}},
    print_usage_command("help", C, MaxC, ShCode).


%%-----------------------------
%% Print usage command
%%-----------------------------

%% @spec (CmdSubString::string(), MaxC::integer(), ShCode::boolean()) -> ok
print_usage_commands(CmdSubString, MaxC, ShCode) ->
    %% Get which command names match this substring
    AllCommandsNames = [atom_to_list(Name) || {Name, _, _} <- ejabberd_commands:list_commands()],
    Cmds = filter_commands(AllCommandsNames, CmdSubString),
    case Cmds of
    	[] -> io:format("Error: not command found that match: ~p~n", [CmdSubString]);
	_ -> print_usage_commands2(lists:sort(Cmds), MaxC, ShCode)
    end.

print_usage_commands2(Cmds, MaxC, ShCode) ->
    %% Then for each one print it
    lists:mapfoldl(
      fun(Cmd, Remaining) ->
	      print_usage_command(Cmd, MaxC, ShCode),
	      case Remaining > 1 of
		  true -> print([" ", lists:duplicate(MaxC, 126), " \n"], []);
		  false -> ok
	      end,
	      {ok, Remaining-1}
      end,
      length(Cmds),
      Cmds).

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

%% @spec (Cmd::string(), MaxC::integer(), ShCode::boolean()) -> ok
print_usage_command(Cmd, MaxC, ShCode) ->
    Name = list_to_atom(Cmd),
    case ejabberd_commands:get_command_definition(Name) of
	command_not_found ->
	    io:format("Error: command ~p not known.~n", [Cmd]);
	C ->
	    print_usage_command(Cmd, C, MaxC, ShCode)
    end.

print_usage_command(Cmd, C, MaxC, ShCode) ->
    #ejabberd_commands{
		     tags = TagsAtoms,
		     desc = Desc,
		     longdesc = LongDesc,
		     args = ArgsDef,
		     result = ResultDef} = C,

    NameFmt = ["  ", ?B("Command Name"), ": ", Cmd, "\n"],

    %% Initial indentation of result is 13 = length("  Arguments: ")
    Args = [format_usage_ctype(ArgDef, 13) || ArgDef <- ArgsDef],
    ArgsMargin = lists:duplicate(13, $\s),
    ArgsListFmt = case Args of
		      [] -> "\n";
		      _ -> [ [Arg, "\n", ArgsMargin] || Arg <- Args]
		  end,
    ArgsFmt = ["  ", ?B("Arguments"), ": ", ArgsListFmt],

    %% Initial indentation of result is 11 = length("  Returns: ")
    ResultFmt = format_usage_ctype(ResultDef, 11),
    ReturnsFmt = ["  ",?B("Returns"),": ", ResultFmt],

    XmlrpcFmt = "", %%+++ ["  ",?B("XML-RPC"),": ", format_usage_xmlrpc(ArgsDef, ResultDef), "\n\n"],

    TagsFmt = ["  ",?B("Tags"),": ", prepare_long_line(8, MaxC, [atom_to_list(TagA) || TagA <- TagsAtoms])],

    DescFmt = ["  ",?B("Description"),": ", prepare_description(15, MaxC, Desc)],

    LongDescFmt = case LongDesc of
		      "" -> "";
		      _ -> ["", prepare_description(0, MaxC, LongDesc), "\n\n"]
		  end,

    NoteEjabberdctl = case is_supported_args(ArgsDef) of
			  true -> "";
			  false -> ["  ", ?B("Note:"), " This command cannot be executed using ejabberdctl. Try ejabberd_xmlrpc.\n\n"]
		      end,

    print(["\n", NameFmt, "\n", ArgsFmt, "\n", ReturnsFmt, "\n\n", XmlrpcFmt, TagsFmt, "\n\n", DescFmt, "\n\n", LongDescFmt, NoteEjabberdctl], []).

format_usage_ctype(Type, _Indentation)
  when (Type==atom) or (Type==integer) or (Type==string) or (Type==binary) or (Type==rescode) or (Type==restuple)->
    io_lib:format("~p", [Type]);

format_usage_ctype({Name, Type}, _Indentation)
  when (Type==atom) or (Type==integer) or (Type==string) or (Type==binary) or (Type==rescode) or (Type==restuple)->
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
    [NameFmt, "::{ " | ElementsFmt].

format_usage_tuple([], _Indentation) ->
    [];
format_usage_tuple([ElementDef], Indentation) ->
    [format_usage_ctype(ElementDef, Indentation) , " }"];
format_usage_tuple([ElementDef | ElementsDef], Indentation) ->
    ElementFmt = format_usage_ctype(ElementDef, Indentation),
    MarginString = lists:duplicate(Indentation, $\s), % Put spaces
    [ElementFmt, ",\n", MarginString, format_usage_tuple(ElementsDef, Indentation)].

print(Format, Args) ->
    io:format(lists:flatten(Format), Args).

%%-----------------------------
%% Command managment
%%-----------------------------

%%+++
%% Struct(Integer res) create_account(Struct(String user, String server, String password))
%%format_usage_xmlrpc(ArgsDef, ResultDef) ->
%%    ["aaaa bbb ccc"].

