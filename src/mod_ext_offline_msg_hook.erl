%%%----------------------------------------------------------------------
%%% File    : extoffmsghook.erl
%%% Purpose : External offline message hook
%%% Created : Sun Apr  1 14:00:35 IST 2012
%%%----------------------------------------------------------------------

-module(mod_ext_offline_msg_hook).

-behaviour(gen_mod).

-export([start/2,
         stop/1,
         init/2,
         push_message/3]).

-include("ejabberd.hrl").

-define(INIT_TIMEOUT, 60000). % Timeout is in milliseconds: 60 seconds == 60000
-define(CALL_TIMEOUT, 10000). % Timeout is in milliseconds: 10 seconds == 10000

start(Host, _Opts) ->
    ExtPrg = ejabberd_config:get_local_option({extoffmsghook_program, Host}),
    ?INFO_MSG("EXTERNAL OFFLINE MESSAGE MODULE LOADING", []),
    lists:foreach(
      fun(This) ->
              start_instance(get_process_name(Host, This), ExtPrg)
      end,
      lists:seq(0, get_instances(Host)-1)
     ),
    ejabberd_hooks : add ( offline_message_hook , Host, ?MODULE ,push_message , 50).

start_instance(ProcessName, ExtPrg) ->
    spawn(?MODULE, init, [ProcessName, ExtPrg]).

restart_instance(ProcessName, ExtPrg) ->
    unregister(ProcessName),
    start_instance(ProcessName, ExtPrg).

init(ProcessName, ExtPrg) ->
    register(ProcessName, self()),
    process_flag(trap_exit,true),
    Port = open_port({spawn, ExtPrg}, [{packet,2}]),
    loop(Port, ?INIT_TIMEOUT, ProcessName, ExtPrg).

stop(Host) ->
    lists:foreach(
      fun(This) ->
              get_process_name(Host, This) ! stop
      end,
      lists:seq(0, get_instances(Host)-1)
     ),
    ?INFO_MSG("STOPPING EXTERNAL OFFLINE MSG  MODULE", []),    
	ejabberd_hooks:delete(offline_message_hook, Host, ?MODULE, push_message, 50).

get_process_name(Host, Integer) ->
    gen_mod:get_module_proc(lists:append([Host, integer_to_list(Integer)]), eoffmsg).

push_message(_From, _To, Packet) ->
    Type  =  xml:get_tag_attr_s( "type" ,  Packet ),
    FromS  =  xml:get_tag_attr_s ( "from" ,  Packet ),
    ToS  =  xml:get_tag_attr_s ( "to" ,  Packet ),
    Body = xml:get_path_s(Packet, [{elem, "body"}, cdata]),
    if (Type == "chat") ->
            call_port("localhost", ["offline", FromS, ToS, Body])
    end.

call_port(Server, Msg) ->
    LServer = jlib:nameprep(Server),
    ProcessName = get_process_name(LServer, random_instance(get_instances(LServer))),
    ProcessName ! {call, self(), Msg},
    receive
        {eoffmsg,Result} ->
            Result
    end.

random_instance(MaxNum) ->
    {A1,A2,A3} = now(),
    random:seed(A1, A2, A3),
    random:uniform(MaxNum) - 1.

get_instances(Server) ->
    case ejabberd_config:get_local_option({extoffmsghook_instances, Server}) of
        Num when is_integer(Num) -> Num;
        _ -> 1
    end.

loop(Port, Timeout, ProcessName, ExtPrg) ->
    receive
        {call, Caller, Msg} ->
            port_command(Port, encode(Msg)),
            receive
                {Port, {data, Data}} ->
                    ?DEBUG("extoffmsg call '~p' received data response:~n~p", [Msg, Data]),
                    Caller ! {eoffmsg, decode(Data)},
                    loop(Port, ?CALL_TIMEOUT, ProcessName, ExtPrg);
                {Port, Other} ->
                    ?ERROR_MSG("extoffmsg call '~p' received strange response:~n~p", [Msg, Other]),
                    Caller ! {eoffmsg, false},
                    loop(Port, ?CALL_TIMEOUT, ProcessName, ExtPrg)
            after
                Timeout ->
                    ?ERROR_MSG("extoffmsg call '~p' didn't receive response", [Msg]),
                    Caller ! {eoffmsg, false},
                    Pid = restart_instance(ProcessName, ExtPrg),
                    flush_buffer_and_forward_messages(Pid),
                    exit(port_terminated)
            end;
        stop ->
            Port ! {self(), close},
            receive
                {Port, closed} ->
                    exit(normal)
            end;
        {'EXIT', Port, Reason} ->
            ?CRITICAL_MSG("extoffmsg script has exitted abruptly with reason '~p'", [Reason]),
            Pid = restart_instance(ProcessName, ExtPrg),
            flush_buffer_and_forward_messages(Pid),
            exit(port_terminated)
    end.

flush_buffer_and_forward_messages(Pid) ->
    receive
        Message ->
            Pid ! Message,
            flush_buffer_and_forward_messages(Pid)
    after 0 ->
            true
    end.

join(List, Sep) ->
    lists:foldl(fun(A, "") -> A;
                   (A, Acc) -> Acc ++ Sep ++ A
                end, "", List).

encode(L) ->
    join(L,":").

decode([0,0]) ->
    false;
decode([0,1]) ->
    true.
