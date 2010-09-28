%%% ====================================================================
%%% This software is copyright 2006-2010, ProcessOne.
%%%
%%% mod_support
%%% allow automatic build of support archive to be sent to Process-One
%%%
%%% @copyright 2006-2010 ProcessOne
%%% @author Christophe Romain <christophe.romain@process-one.net>
%%%   [http://www.process-one.net/]
%%% @version {@vsn}, {@date} {@time}
%%% @end
%%% ====================================================================


-module(mod_support).
-author('christophe.romain@process-one.net').

-behaviour(gen_mod).
%-behaviour(gen_server). 

% module functions
-export([start/2,stop/1,is_loaded/0,loop/1,dump/0]).
-compile(export_all).

-include("ejabberd.hrl").
-include("jlib.hrl").
-include("licence.hrl").

-include_lib("kernel/include/file.hrl").

-define(LOG_FETCH_SIZE, 1000000).
-define(RPC_TIMEOUT, 10000). % 10
-define(MAX_FILE_SIZE, 2147483648). %%2Gb

start(Host, Opts) ->
    case ?IS_VALID of
    true ->
	case gen_mod:get_opt(dump_freq, Opts, 0) of
	0 -> no_dump;
	Freq -> spawn(?MODULE, loop, [Freq*60000])
	end,
	ok;
    false ->
	not_started
    end.
    
stop(Host) ->
    ok.
    
is_loaded() ->
    ok.
    
loop(Timeout) ->
    receive
    quit -> ok
    after Timeout ->
	Dump = dump(),
	BaseName = get_base_name(),
	%%{Data,EjabberdLog,SaslLog,ECrash} = Dump,
	write_logs(tuple_to_list(Dump),BaseName,["_memory.bin",
						 "_ejabberd.log.gz",
						 "_sasl.log.gz",
						 "_erlang_crash_dump.log.gz"]),
	loop(Timeout)
    end.

get_base_name() ->
	{{Y,M,D},{Hr,Mn,_Sc}} = calendar:local_time(),
	case os:getenv("EJABBERD_LOG_PATH") of
	    false ->
		filename:join(filename:dirname(filename:absname("")),
		    lists:flatten(io_lib:format("~b~b~b~b~b",[Y,M,D,Hr,Mn])));
	    Path -> 
		filename:join(filename:dirname(Path),
		    lists:flatten(io_lib:format("~b~b~b~b~b",[Y,M,D,Hr,Mn])))
	end.
    
write_logs([BinaryData|T],BaseName,[Filename|Filenames]) ->
        Log = BaseName++Filename,
	file:write_file(Log, BinaryData),
        write_logs(T,BaseName,Filenames);

write_logs([],BaseName,_)-> ok.

dump() ->
    Dump = lists:map(fun(LogFile)  ->
	Content = case file:open(LogFile,[read,raw]) of
	{ok, IO} ->  
            Size = case file:read_file_info(LogFile) of
            {ok, FileInfo} -> FileInfo#file_info.size;
            _ -> ?LOG_FETCH_SIZE
            end,
	    case Size>?MAX_FILE_SIZE of
              true -> io_lib:format("File ~s is too big: ~p bytes.",[LogFile, Size]);
              false -> 
   		if Size>?LOG_FETCH_SIZE ->
                      file:position(IO, Size-?LOG_FETCH_SIZE),
                      case file:read(IO, ?LOG_FETCH_SIZE) of
                      {ok, Data1} -> Data1;
                      Error1 -> io_lib:format("can not read log file (~s): ~p",[LogFile, Error1])
                      end;
                   true ->
                       case file:read(IO, Size) of
                       {ok, Data2} -> Data2;
                       Error2 -> io_lib:format("can not read log file (~s): ~p",[LogFile, Error2])
                       end
                end
	    end;
        {error, Reason} -> 
            io_lib:format("can not open log file (~s): ~p",[LogFile, Reason])
        end,
        zlib:gzip(list_to_binary(Content))
    end, [ejabberd_logs(), sasl_logs(), erl_crash()]),
    NodeState = get_node_state(),
    list_to_tuple([NodeState|Dump]).

ejabberd_logs() ->
    LogPath = case application:get_env(log_path) of
    {ok, Path} -> 
        Path;
    undefined ->
        case os:getenv("EJABBERD_LOG_PATH") of
        false -> ?LOG_PATH;
        Path -> Path
        end
    end.

sasl_logs() ->
    case os:getenv("SASL_LOG_PATH") of
    false -> filename:join([filename:dirname(ejabberd_logs()),"sasl.log"]);
    Path -> Path
    end.

erl_crash() ->
    LogsDir = filename:dirname(ejabberd_logs()),
    CrashDumpWildcard =  filename:join([LogsDir,"erl_crash*dump"]),
    FileName = case filelib:wildcard(CrashDumpWildcard) of
	     [Files] -> [LastFile|T] = lists:reverse([Files]),
			LastFile;
	     _ -> case os:getenv("ERL_CRASH_DUMP") of
		  false -> "erl_crash.dump";
		  Path -> Path
		  end
	     end.


proc_info(Pid) ->
    Info = process_info(Pid),
    lists:map(fun(Elem) ->
	List = proplists:get_value(Elem, Info),
	{Elem, size(term_to_binary(List))}
	end, [messages, dictionary])
    ++ [X || X <- Info, 
        lists:member(element(1,X),
            [heap_size,stack_size,reductions,links,status,initial_call,current_function])].

environment() ->
    {ok, KE} = application:get_key(kernel,env),
    {ok, EE} = application:get_key(ejabberd,env),
    Env = [{inetrc, os:getenv("ERL_INETRC")},
	   {sopath, os:getenv("EJABBERD_SO_PATH")},
	   {maxports, os:getenv("ERL_MAX_PORTS")},
	   {maxtables, os:getenv("ERL_MAX_ETS_TABLES")},
	   {crashdump, os:getenv("ERL_CRASH_DUMP")},
	   {archdir, os:getenv("ARCHDIR")},
	   {mnesia, mnesia:system_info(all)}],
	Args = [{args, init:get_arguments()}, {plain, init:get_plain_arguments()}],
    KE++EE++Env++Args.

memtop(N) ->
    E = lists:sublist(lists:reverse(lists:keysort(2,lists:map(fun(Tab) -> {Tab, ets:info(Tab,memory)} end, ets:all()))),N),
    M = lists:sublist(lists:reverse(lists:keysort(2,lists:map(fun(Tab) -> {Tab, mnesia:table_info(Tab,memory)} end, mnesia:system_info(tables)))),N),
    E++M.

maxmsgqueue() ->
    lists:max(lists:map(fun(Pid) -> proplists:get_value(message_queue_len,process_info(Pid)) end, erlang:processes())).

msgqueue(N) ->
    lists:filter(fun(L) -> proplists:get_value(message_queue_len, L) > N
    end, lists:map(fun(Pid) -> process_info(Pid) end, erlang:processes())).

%lists:sublist(lists:reverse(lists:keysort(2,lists:map(fun(Pid) -> {E,L} = process_info(Pid, dictionary), {E,length(L)} end, erlang:processes()))), 10)

%%Entry point to invoke mod_support via command line. 
%%Example: erl -sname debug@localhost -s mod_support report ejabberd@localhost
%%See issue #TECH-286.
report(Node) ->
    [NodeId|T]=Node,
    UploadResult = force_load_code_into_node(NodeId, ?MODULE),
    case UploadResult of
	ok -> NodeState = rpc:call(NodeId,mod_support,get_node_state,[],?RPC_TIMEOUT),
	      Dump = rpc:call(NodeId,mod_support,dump,[],?RPC_TIMEOUT),
	      BaseName = get_base_name(),
	      %%{Data,EjabberdLog,SaslLog,ECrash} = Dump,
	      write_logs(tuple_to_list(Dump),BaseName,["_memory.bin",
						       "_ejabberd.log.gz",
						       "_sasl.log.gz",
						       "_erlang_crash_dump.log.gz"]),
	      error_logger:info_msg("State in node ~p was written to log~n",[NodeId]),
	      error_logger:info_msg("Unloading module ~s from node ~p. ",[?MODULE,NodeId]),
	      force_unload_code_from_node(NodeId, ?MODULE);
        _  -> error_logger:info_msg("Error uploading module ~s from node ~p~n",[?MODULE,NodeId])
    end.

%%Load Module into the ejabberd Node specified.
force_load_code_into_node(Node, Module) ->
    CodeFile = code:where_is_file(atom_to_list(Module)++".beam"),
    case file:read_file(CodeFile) of
    {ok, Code} ->
        rpc:call(Node, code, purge, [Module], ?RPC_TIMEOUT),
        rpc:call(Node, code, delete, [Module], ?RPC_TIMEOUT),
        case rpc:call(Node, code, load_binary, [Module, CodeFile, Code], ?RPC_TIMEOUT) of
        {module, _} ->
            error_logger:info_msg("Loading ~s module into ~p : success ~n", [Module,Node]),
            rpc:block_call(Node, Module, is_loaded, [], ?RPC_TIMEOUT);
        {error, badfile} ->
            error_logger:info_msg("Loading ~s module into ~p : incorrect format ~n", [Module,Node]),
            {error, badfile};
        {error, not_purged} ->
            % this should never happen anyway..
            error_logger:info_msg("Loading ~s module into ~p : old code already exists ~n", [Module,Node]),
            {error, not_purged};
        {badrpc, Reason} ->
            error_logger:info_msg("Loading ~s module into ~p: badrpc ~p ~n", [Module,Node,Reason]),
            {badrpc, Reason}
        end;
    Error ->
        error_logger:error_msg("Cannot read module file ~s ~p : ~p ~n", [Module, CodeFile, Error]),
        Error
    end.

%%Unload erlang Module from the Node specified. Used to ensure cleanup after rpc calls.
force_unload_code_from_node(Node, Module) ->
    rpc:call(Node, code, purge, [Module], ?RPC_TIMEOUT),
    rpc:call(Node, code, delete, [Module], ?RPC_TIMEOUT).

%%Retrieve system state and pack it into Data
%%TODO enhance state info. See #TECH-286.
get_node_state() ->
    Mem = erlang:memory(),
    Ets = lists:map(fun(Tab) -> ets:info(Tab) end, ets:all()),
    Mnesia = lists:map(fun(Tab) -> mnesia:table_info(Tab,all) end, mnesia:system_info(tables)),
    Procs = lists:map(fun(Pid) -> proc_info(Pid) end, erlang:processes()),
    Data = term_to_binary({Mem, Ets, Mnesia, Procs}).

crash_dump() ->
    SystemInfo = [erlang:system_info(X) || X<-[info,loaded,procs]],
    [zlib:gzip(list_to_binary(lists:flatten(SystemInfo)))].

crash_dump(Node) ->
    [NodeId|T]=Node,
    UploadResult = force_load_code_into_node(NodeId, ?MODULE),
    case UploadResult of
	ok -> Dump = rpc:call(NodeId,mod_support,crash_dump,[],?RPC_TIMEOUT),
	      BaseName = get_base_name(),
	      write_logs(Dump,BaseName,["_realtime_crash_dump.gz"]),
	      error_logger:info_msg("Unloading module ~s from node ~p. ",[?MODULE,NodeId]),
	      force_unload_code_from_node(NodeId, ?MODULE);
        _  -> error_logger:info_msg("Error uploading module ~s from node ~p~n",[?MODULE,NodeId])
    end.
