-module (mod_http_fileserver_log).

-behaviour (gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export ([start_link/2,start/2, stop/1, add_to_log/4,reopen_log/1]).

-include("ejabberd.hrl").
-include("jlib.hrl").
-include("ejabberd_http.hrl").
-include_lib("kernel/include/file.hrl").

-define(PROCNAME, ejabberd_mod_http_fileserver_log).

-record(state, {host,accesslog, accesslogfd}).
%% Public API

start(Host, Filename) ->
    Proc =gen_mod:get_module_proc(Host, ?PROCNAME),
    ChildSpec =
    {Proc,
     {?MODULE, start_link, [Host, Filename]},
     transient, % if process crashes abruptly, it gets restarted
     1000,
     worker,
     [?MODULE]},
    supervisor:start_child(ejabberd_sup, ChildSpec).

stop(Host) ->
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    gen_server:call(Proc, stop),
    supervisor:terminate_child(ejabberd_sup, Proc),
    supervisor:delete_child(ejabberd_sup, Proc).

start_link(Host, Filename) ->
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    gen_server:start_link({local, Proc}, ?MODULE, [Host, Filename], []).

add_to_log(Host,FileSize, Code, Request) ->
    gen_server:cast(gen_mod:get_module_proc(Host, ?PROCNAME),
		    {add_to_log, FileSize, Code, Request}).
		    
reopen_log(Host) ->
    gen_server:cast(gen_mod:get_module_proc(Host, ?PROCNAME), reopen_log).

%% Server implementation, a.k.a.: callbacks

init([Host, Filename]) ->
    try try_open_log(Filename, Host) of
	AccessLogFD ->
	    ?DEBUG("File opened !", []),
	    {ok, #state{host = Host,
			accesslog = Filename,
			accesslogfd = AccessLogFD}}
    catch
	throw:Reason ->
	    {stop, Reason}
    end.

try_open_log(FN, Host) ->
    FD = try open_log(FN) of
	     FD1 -> FD1
	 catch
	     throw:{cannot_open_accesslog, FN, Reason} ->
		 ?ERROR_MSG("Cannot open access log file: ~p~nReason: ~p", [FN, Reason]),
		 undefined
	 end,
    %HostB = list_to_binary(Host),
    ejabberd_hooks:add(reopen_log_hook, Host, ?MODULE, reopen_log, 50),
    FD.
     
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast({add_to_log, FileSize, Code, Request}, State) ->
    add_to_log2(State#state.accesslogfd, FileSize, Code, Request),
    {noreply, State};
handle_cast(reopen_log, State) ->
    FD2 = reopen_log(State#state.accesslog, State#state.accesslogfd),
    {noreply, State#state{accesslogfd = FD2}};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    close_log(State#state.accesslogfd),
    ejabberd_hooks:delete(reopen_log_hook, State#state.host, ?MODULE, reopen_log, 50),
    ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
  
  
%%----------------------------------------------------------------------
%% Log file
%%----------------------------------------------------------------------

open_log(FN) ->
    case file:open(FN, [append]) of
	{ok, FD} ->
	    FD;
	{error, Reason} ->
	    throw({cannot_open_accesslog, FN, Reason})
    end.

close_log(FD) ->
    file:close(FD).

reopen_log(undefined, undefined) ->
    ok;
reopen_log(FN, FD) ->
    ?DEBUG("reopening logs", []),
    close_log(FD),
    open_log(FN).



add_to_log2(undefined, _FileSize, _Code, _Request) ->
    ok;
add_to_log2(File, FileSize, Code, Request) ->
    {{Year, Month, Day}, {Hour, Minute, Second}} = calendar:local_time(),
    IP = ip_to_string(element(1, Request#request.ip)),
    Path = join(Request#request.path, "/"),
    Query = case join(lists:map(fun(E) -> lists:concat([element(1, E), "=", element(2, E)]) end,
				Request#request.q), "&") of
		[] ->
		    "";
		String ->
		    [$? | String]
	    end,
    UserAgent = find_header('User-Agent', Request#request.headers, "-"),
    Referer = find_header('Referer', Request#request.headers, "-"),
    %% Pseudo Combined Apache log format:
    %% 127.0.0.1 - - [28/Mar/2007:18:41:55 +0200] "GET / HTTP/1.1" 302 303 "-" "tsung"
    %% TODO some fields are harcoded/missing:
    %%   The date/time integers should have always 2 digits. For example day "7" should be "07"
    %%   Month should be 3*letter, not integer 1..12
    %%   Missing time zone = (`+' | `-') 4*digit
    %%   Missing protocol version: HTTP/1.1
    %% For reference: http://httpd.apache.org/docs/2.2/logs.html
    io:format(File, "~s - - [~p/~p/~p:~p:~p:~p] \"~s /~s~s\" ~p ~p ~p ~p~n",
	      [IP, Day, Month, Year, Hour, Minute, Second, Request#request.method, Path, Query, Code,
               FileSize, Referer, UserAgent]).
               
find_header(Header, Headers, Default) ->
    case lists:keysearch(Header, 1, Headers) of
        {value, {_, Value}} -> Value;
        false               -> Default
    end.
                   
join([], _) ->
    "";
join([E], _) ->
    E;
join([H | T], Separator) ->
    lists:foldl(fun(E, Acc) -> lists:concat([Acc, Separator, E]) end, H, T).
    
%% Convert IP address tuple to string representation. Accepts either
%% IPv4 or IPv6 address tuples.
ip_to_string(Address) when size(Address) == 4 ->
    join(tuple_to_list(Address), ".");
ip_to_string(Address) when size(Address) == 8 ->
    Parts = lists:map(fun (Int) -> io_lib:format("~.16B", [Int]) end, tuple_to_list(Address)),
    string:to_lower(lists:flatten(join(Parts, ":"))).