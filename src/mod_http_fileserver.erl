%%%-------------------------------------------------------------------
%%% File    : mod_http_fileserver.erl
%%% Author  : Massimiliano Mirra <mmirra [at] process-one [dot] net>
%%% Purpose : Simple file server plugin for embedded ejabberd web server
%%% Created :
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

-module(mod_http_fileserver).

-author('mmirra@process-one.net').

-behaviour(gen_mod).
-behaviour(gen_server).

%% gen_mod callbacks
-export([start/2, stop/1]).

%% API
-export([start_link/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

%% request_handlers callbacks
-export([process/2]).

%% utility for other http modules
-export([content_type/3]).

-export([reopen_log/1, mod_opt_type/1]).

-include("ejabberd.hrl").
-include("logger.hrl").
-include("ejabberd_http.hrl").

-include("jlib.hrl").

-include_lib("kernel/include/file.hrl").

-record(state,
	{host, docroot, accesslog, accesslogfd,
	 directory_indices, custom_headers, default_content_type,
	 content_types = []}).

-define(PROCNAME, ejabberd_mod_http_fileserver).

%% Response is {DataSize, Code, [{HeaderKey, HeaderValue}], Data}
-define(HTTP_ERR_FILE_NOT_FOUND,
	{-1, 404, [], <<"Not found">>}).

-define(HTTP_ERR_FORBIDDEN,
	{-1, 403, [], <<"Forbidden">>}).

-define(DEFAULT_CONTENT_TYPE,
	<<"application/octet-stream">>).

-define(DEFAULT_CONTENT_TYPES,
	[{<<".css">>, <<"text/css">>},
	 {<<".gif">>, <<"image/gif">>},
	 {<<".html">>, <<"text/html">>},
	 {<<".jar">>, <<"application/java-archive">>},
	 {<<".jpeg">>, <<"image/jpeg">>},
	 {<<".jpg">>, <<"image/jpeg">>},
	 {<<".js">>, <<"text/javascript">>},
	 {<<".png">>, <<"image/png">>},
	 {<<".svg">>, <<"image/svg+xml">>},
	 {<<".txt">>, <<"text/plain">>},
	 {<<".xml">>, <<"application/xml">>},
	 {<<".xpi">>, <<"application/x-xpinstall">>},
	 {<<".xul">>, <<"application/vnd.mozilla.xul+xml">>}]).

%%====================================================================
%% gen_mod callbacks
%%====================================================================

start(Host, Opts) ->
    Proc = get_proc_name(Host),
    ChildSpec =
	{Proc,
	 {?MODULE, start_link, [Host, Opts]},
	 transient, % if process crashes abruptly, it gets restarted
	 1000,
	 worker,
	 [?MODULE]},
    supervisor:start_child(ejabberd_sup, ChildSpec).

stop(Host) ->
    Proc = get_proc_name(Host),
    gen_server:call(Proc, stop),
    supervisor:terminate_child(ejabberd_sup, Proc),
    supervisor:delete_child(ejabberd_sup, Proc).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(Host, Opts) ->
    Proc = get_proc_name(Host),
    gen_server:start_link({local, Proc}, ?MODULE, [Host, Opts], []).

%%====================================================================
%% gen_server callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([Host, Opts]) ->
    try initialize(Host, Opts) of
	{DocRoot, AccessLog, AccessLogFD, DirectoryIndices,
         CustomHeaders, DefaultContentType, ContentTypes} ->
	    {ok, #state{host = Host,
			accesslog = AccessLog,
			accesslogfd = AccessLogFD,
			docroot = DocRoot,
                        directory_indices = DirectoryIndices,
                        custom_headers = CustomHeaders,
                        default_content_type = DefaultContentType,
                        content_types = ContentTypes}}
    catch
	throw:Reason ->
	    {stop, Reason}
    end.

initialize(Host, Opts) ->
    DocRoot = gen_mod:get_opt(docroot, Opts, fun(A) -> A end, undefined),
    check_docroot_defined(DocRoot, Host),
    DRInfo = check_docroot_exists(DocRoot),
    check_docroot_is_dir(DRInfo, DocRoot),
    check_docroot_is_readable(DRInfo, DocRoot),
    AccessLog = gen_mod:get_opt(accesslog, Opts,
                                fun iolist_to_binary/1,
                                undefined),
    AccessLogFD = try_open_log(AccessLog, Host),
    DirectoryIndices = gen_mod:get_opt(directory_indices, Opts,
                                       fun(L) when is_list(L) -> L end,
                                       []),
    CustomHeaders = gen_mod:get_opt(custom_headers, Opts,
                                    fun(L) when is_list(L) -> L end,
                                    []),
    DefaultContentType = gen_mod:get_opt(default_content_type, Opts,
                                         fun iolist_to_binary/1,
                                         ?DEFAULT_CONTENT_TYPE),
    ContentTypes = build_list_content_types(
                     gen_mod:get_opt(content_types, Opts,
                                     fun(L) when is_list(L) ->
					     lists:map(
					       fun({K, V}) ->
						       {iolist_to_binary(K),
							iolist_to_binary(V)}
					       end, L)
				     end, []),
                     ?DEFAULT_CONTENT_TYPES),
    ?INFO_MSG("known content types: ~s",
	      [str:join([[$*, K, " -> ", V] || {K, V} <- ContentTypes],
			<<", ">>)]),
    {DocRoot, AccessLog, AccessLogFD, DirectoryIndices,
     CustomHeaders, DefaultContentType, ContentTypes}.


%% @spec (AdminCTs::[CT], Default::[CT]) -> [CT]
%% where CT = {Extension::string(), Value}
%%       Value = string() | undefined
%% @doc Return a unified list without duplicates.
%% Elements of AdminCTs have more priority.
%% If a CT is declared as 'undefined', then it is not included in the result.
build_list_content_types(AdminCTsUnsorted, DefaultCTsUnsorted) ->
    AdminCTs = lists:ukeysort(1, AdminCTsUnsorted),
    DefaultCTs = lists:ukeysort(1, DefaultCTsUnsorted),
    CTsUnfiltered = lists:ukeymerge(1, AdminCTs,
				    DefaultCTs),
    [{Extension, Value}
     || {Extension, Value} <- CTsUnfiltered,
	Value /= undefined].

check_docroot_defined(DocRoot, Host) ->
    case DocRoot of
      undefined -> throw({undefined_docroot_option, Host});
      _ -> ok
    end.

check_docroot_exists(DocRoot) ->
    case file:read_file_info(DocRoot) of
      {error, Reason} ->
	  throw({error_access_docroot, DocRoot, Reason});
      {ok, FI} -> FI
    end.

check_docroot_is_dir(DRInfo, DocRoot) ->
    case DRInfo#file_info.type of
      directory -> ok;
      _ -> throw({docroot_not_directory, DocRoot})
    end.

check_docroot_is_readable(DRInfo, DocRoot) ->
    case DRInfo#file_info.access of
      read -> ok;
      read_write -> ok;
      _ -> throw({docroot_not_readable, DocRoot})
    end.

try_open_log(undefined, _Host) ->
    undefined;
try_open_log(FN, Host) ->
    FD = try open_log(FN) of
	     FD1 -> FD1
	 catch
	     throw:{cannot_open_accesslog, FN, Reason} ->
		 ?ERROR_MSG("Cannot open access log file: ~p~nReason: ~p", [FN, Reason]),
		 undefined
	 end,
    ejabberd_hooks:add(reopen_log_hook, Host, ?MODULE, reopen_log, 50),
    FD.

%%--------------------------------------------------------------------
%% Function: handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({serve, LocalPath}, _From, State) ->
    Reply = serve(LocalPath, State#state.docroot, State#state.directory_indices,
		  State#state.custom_headers,
                  State#state.default_content_type, State#state.content_types),
    {reply, Reply, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast({add_to_log, FileSize, Code, Request}, State) ->
    add_to_log(State#state.accesslogfd, FileSize, Code, Request),
    {noreply, State};
handle_cast(reopen_log, State) ->
    FD2 = reopen_log(State#state.accesslog, State#state.accesslogfd),
    {noreply, State#state{accesslogfd = FD2}};
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, State) ->
    close_log(State#state.accesslogfd),
    ejabberd_hooks:delete(reopen_log_hook, State#state.host, ?MODULE, reopen_log, 50),
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% request_handlers callbacks
%%====================================================================

%% @spec (LocalPath, Request) -> {HTTPCode::integer(), [Header], Page::string()}
%% @doc Handle an HTTP request.
%% LocalPath is the part of the requested URL path that is "local to the module".
%% Returns the page to be sent back to the client and/or HTTP status code.
process(LocalPath, Request) ->
    ?DEBUG("Requested ~p", [LocalPath]),
    try gen_server:call(get_proc_name(Request#request.host), {serve, LocalPath}) of
	{FileSize, Code, Headers, Contents} ->
	    add_to_log(FileSize, Code, Request),
	    {Code, Headers, Contents}
    catch
	exit:{noproc, _} ->
	    ?ERROR_MSG("Received an HTTP request with Host ~p, but couldn't find the related "
		       "ejabberd virtual host", [Request#request.host]),
	    ejabberd_web:error(not_found)
    end.

serve(LocalPath, DocRoot, DirectoryIndices, CustomHeaders, DefaultContentType, ContentTypes) ->
    FileName = filename:join(filename:split(DocRoot) ++ LocalPath),
    case file:read_file_info(FileName) of
        {error, enoent}                    -> ?HTTP_ERR_FILE_NOT_FOUND;
        {error, enotdir}                   -> ?HTTP_ERR_FILE_NOT_FOUND;
        {error, eacces}                    -> ?HTTP_ERR_FORBIDDEN;
        {ok, #file_info{type = directory}} -> serve_index(FileName,
                                                          DirectoryIndices,
                                                          CustomHeaders,
                                                          DefaultContentType,
                                                          ContentTypes);
        {ok, FileInfo}                     -> serve_file(FileInfo, FileName,
                                                         CustomHeaders,
                                                         DefaultContentType,
                                                         ContentTypes)
    end.

%% Troll through the directory indices attempting to find one which
%% works, if none can be found, return a 404.
serve_index(_FileName, [], _CH, _DefaultContentType, _ContentTypes) ->
    ?HTTP_ERR_FILE_NOT_FOUND;
serve_index(FileName, [Index | T], CH, DefaultContentType, ContentTypes) ->
    IndexFileName = filename:join([FileName] ++ [Index]),
    case file:read_file_info(IndexFileName) of
        {error, _Error}                    -> serve_index(FileName, T, CH, DefaultContentType, ContentTypes);
        {ok, #file_info{type = directory}} -> serve_index(FileName, T, CH, DefaultContentType, ContentTypes);
        {ok, FileInfo}                     -> serve_file(FileInfo, IndexFileName, CH, DefaultContentType, ContentTypes)
    end.

%% Assume the file exists if we got this far and attempt to read it in
%% and serve it up.
serve_file(FileInfo, FileName, CustomHeaders, DefaultContentType, ContentTypes) ->
    ?DEBUG("Delivering: ~s", [FileName]),
    ContentType = content_type(FileName, DefaultContentType,
			       ContentTypes),
    {ok, FileContents} = file:read_file(FileName),
    {FileInfo#file_info.size, 200,
     [{<<"Server">>, <<"ejabberd">>},
      {<<"Last-Modified">>, last_modified(FileInfo)},
      {<<"Content-Type">>, ContentType}
      | CustomHeaders],
     FileContents}.

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
    close_log(FD),
    open_log(FN).

reopen_log(Host) ->
    gen_server:cast(get_proc_name(Host), reopen_log).

add_to_log(FileSize, Code, Request) ->
    gen_server:cast(get_proc_name(Request#request.host),
		    {add_to_log, FileSize, Code, Request}).

add_to_log(undefined, _FileSize, _Code, _Request) ->
    ok;
add_to_log(File, FileSize, Code, Request) ->
    {{Year, Month, Day}, {Hour, Minute, Second}} = calendar:local_time(),
    IP = ip_to_string(element(1, Request#request.ip)),
    Path = join(Request#request.path, "/"),
    Query = case join(lists:map(fun(E) -> lists:concat([element(1, E), "=", binary_to_list(element(2, E))]) end,
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
      false -> Default
    end.

%%----------------------------------------------------------------------
%% Utilities
%%----------------------------------------------------------------------

get_proc_name(Host) -> gen_mod:get_module_proc(Host, ?PROCNAME).

join([], _) ->
    <<"">>;
join([E], _) ->
    E;
join([H | T], Separator) ->
    [H2 | T2] = case is_binary(H) of true -> [binary_to_list(I)||I<-[H|T]]; false -> [H | T] end,
    Res=lists:foldl(fun(E, Acc) -> lists:concat([Acc, Separator, E]) end, H2, T2),
    case is_binary(H) of true -> list_to_binary(Res); false -> Res end.

content_type(Filename, DefaultContentType, ContentTypes) ->
    Extension = str:to_lower(filename:extension(Filename)),
    case lists:keysearch(Extension, 1, ContentTypes) of
      {value, {_, ContentType}} -> ContentType;
      false -> DefaultContentType
    end.

last_modified(FileInfo) ->
    Then = FileInfo#file_info.mtime,
    httpd_util:rfc1123_date(Then).

%% Convert IP address tuple to string representation. Accepts either
%% IPv4 or IPv6 address tuples.
ip_to_string(Address) when size(Address) == 4 ->
    join(tuple_to_list(Address), ".");
ip_to_string(Address) when size(Address) == 8 ->
    Parts = lists:map(fun (Int) -> io_lib:format("~.16B", [Int]) end, tuple_to_list(Address)),
    string:to_lower(lists:flatten(join(Parts, ":"))).

mod_opt_type(accesslog) -> fun iolist_to_binary/1;
mod_opt_type(content_types) ->
    fun (L) when is_list(L) -> L end;
mod_opt_type(custom_headers) ->
    fun (L) when is_list(L) -> L end;
mod_opt_type(default_content_type) ->
    fun iolist_to_binary/1;
mod_opt_type(directory_indices) ->
    fun (L) when is_list(L) -> L end;
mod_opt_type(docroot) -> fun (A) -> A end;
mod_opt_type(_) ->
    [accesslog, content_types, custom_headers,
     default_content_type, directory_indices, docroot].
