%%%-------------------------------------------------------------------
%%% File    : mod_http_fileserver.erl
%%% Author  : Massimiliano Mirra <mmirra [at] process-one [dot] net>
%%% Purpose : Simple file server plugin for embedded ejabberd web server
%%% Created :
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2020   ProcessOne
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
-export([start/2, stop/1, reload/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

%% request_handlers callbacks
-export([process/2]).

%% utility for other http modules
-export([content_type/3]).

-export([reopen_log/0, mod_opt_type/1, mod_options/1, depends/2, mod_doc/0]).

-include("logger.hrl").
-include("ejabberd_http.hrl").
-include_lib("kernel/include/file.hrl").
-include("translate.hrl").

-record(state,
	{host, docroot, accesslog, accesslogfd,
	 directory_indices, custom_headers, default_content_type,
	 content_types = [], user_access = none}).

%% Response is {DataSize, Code, [{HeaderKey, HeaderValue}], Data}
-define(HTTP_ERR_FILE_NOT_FOUND,
	{-1, 404, [], <<"Not found">>}).

-define(REQUEST_AUTH_HEADERS,
	[{<<"WWW-Authenticate">>, <<"Basic realm=\"ejabberd\"">>}]).

-define(HTTP_ERR_FORBIDDEN,
	{-1, 403, [], <<"Forbidden">>}).
-define(HTTP_ERR_REQUEST_AUTH,
	{-1, 401, ?REQUEST_AUTH_HEADERS, <<"Unauthorized">>}).
-define(HTTP_ERR_HOST_UNKNOWN,
	{-1, 410, [], <<"Host unknown">>}).

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
    gen_mod:start_child(?MODULE, Host, Opts).

stop(Host) ->
    gen_mod:stop_child(?MODULE, Host).

reload(Host, NewOpts, OldOpts) ->
    Proc = get_proc_name(Host),
    gen_server:cast(Proc, {reload, Host, NewOpts, OldOpts}).

depends(_Host, _Opts) ->
    [].

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
init([Host|_]) ->
    Opts = gen_mod:get_module_opts(Host, ?MODULE),
    try initialize(Host, Opts) of
	State ->
	    process_flag(trap_exit, true),
	    {ok, State}
    catch
	throw:Reason ->
	    {stop, Reason}
    end.

initialize(Host, Opts) ->
    DocRoot = mod_http_fileserver_opt:docroot(Opts),
    AccessLog = mod_http_fileserver_opt:accesslog(Opts),
    AccessLogFD = try_open_log(AccessLog, Host),
    DirectoryIndices = mod_http_fileserver_opt:directory_indices(Opts),
    CustomHeaders = mod_http_fileserver_opt:custom_headers(Opts),
    DefaultContentType = mod_http_fileserver_opt:default_content_type(Opts),
    UserAccess0 = mod_http_fileserver_opt:must_authenticate_with(Opts),
    UserAccess = case UserAccess0 of
		     [] -> none;
		     _ ->
			 maps:from_list(UserAccess0)
		 end,
    ContentTypes = build_list_content_types(
                     mod_http_fileserver_opt:content_types(Opts),
                     ?DEFAULT_CONTENT_TYPES),
    ?DEBUG("Known content types: ~ts",
	   [str:join([[$*, K, " -> ", V] || {K, V} <- ContentTypes],
		     <<", ">>)]),
    #state{host = Host,
	   accesslog = AccessLog,
	   accesslogfd = AccessLogFD,
	   docroot = DocRoot,
	   directory_indices = DirectoryIndices,
	   custom_headers = CustomHeaders,
	   default_content_type = DefaultContentType,
	   content_types = ContentTypes,
	   user_access = UserAccess}.

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

try_open_log(undefined, _Host) ->
    undefined;
try_open_log(FN, _Host) ->
    FD = try open_log(FN) of
	     FD1 -> FD1
	 catch
	     throw:{cannot_open_accesslog, FN, Reason} ->
		 ?ERROR_MSG("Cannot open access log file: ~p~nReason: ~p", [FN, Reason]),
		 undefined
	 end,
    ejabberd_hooks:add(reopen_log_hook, ?MODULE, reopen_log, 50),
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
handle_call({serve, LocalPath, Auth, RHeaders}, _From, State) ->
    IfModifiedSince = case find_header('If-Modified-Since', RHeaders, bad_date) of
			  bad_date ->
			      bad_date;
			  Val ->
			      httpd_util:convert_request_date(binary_to_list(Val))
		      end,
    Reply = serve(LocalPath, Auth, State#state.docroot, State#state.directory_indices,
		  State#state.custom_headers,
		  State#state.default_content_type, State#state.content_types,
		  State#state.user_access, IfModifiedSince),
    {reply, Reply, State};
handle_call(Request, From, State) ->
    ?WARNING_MSG("Unexpected call from ~p: ~p", [From, Request]),
    {noreply, State}.

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
handle_cast({reload, Host, NewOpts, _OldOpts}, OldState) ->
    try initialize(Host, NewOpts) of
	NewState ->
	    FD = reopen_log(NewState#state.accesslog, OldState#state.accesslogfd),
	    {noreply, NewState#state{accesslogfd = FD}}
    catch throw:_ ->
	    {noreply, OldState}
    end;
handle_cast(Msg, State) ->
    ?WARNING_MSG("Unexpected cast: ~p", [Msg]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(Info, State) ->
    ?WARNING_MSG("Unexpected info: ~p", [Info]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, #state{host = Host} = State) ->
    close_log(State#state.accesslogfd),
    case gen_mod:is_loaded_elsewhere(Host, ?MODULE) of
	false ->
	    ejabberd_hooks:delete(reopen_log_hook, ?MODULE, reopen_log, 50);
	true ->
	    ok
    end.

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
process(LocalPath, #request{host = Host, auth = Auth, headers = RHeaders} = Request) ->
    ?DEBUG("Requested ~p", [LocalPath]),
    try
	VHost = ejabberd_router:host_of_route(Host),
	{FileSize, Code, Headers, Contents} =
	    gen_server:call(get_proc_name(VHost),
			    {serve, LocalPath, Auth, RHeaders}),
	add_to_log(FileSize, Code, Request#request{host = VHost}),
	{Code, Headers, Contents}
    catch _:{Why, _} when Why == noproc; Why == invalid_domain; Why == unregistered_route ->
	    ?DEBUG("Received an HTTP request with Host: ~ts, "
		   "but couldn't find the related "
		   "ejabberd virtual host", [Host]),
	    {FileSize1, Code1, Headers1, Contents1} = ?HTTP_ERR_HOST_UNKNOWN,
	    add_to_log(FileSize1, Code1, Request#request{host = ejabberd_config:get_myname()}),
	    {Code1, Headers1, Contents1}
    end.

serve(LocalPath, Auth, DocRoot, DirectoryIndices, CustomHeaders, DefaultContentType,
    ContentTypes, UserAccess, IfModifiedSince) ->
    CanProceed = case {UserAccess, Auth} of
		     {none, _} -> true;
		     {_, {User, Pass}} ->
			 case maps:find(User, UserAccess) of
			     {ok, Pass} -> true;
			     _ -> false
			 end;
		     _ ->
			 false
		 end,
    case CanProceed of
	false ->
	    ?HTTP_ERR_REQUEST_AUTH;
	true ->
	    FileName = filename:join(filename:split(DocRoot) ++ LocalPath),
	    case file:read_file_info(FileName) of
		{error, enoent} ->
		    ?HTTP_ERR_FILE_NOT_FOUND;
		{error, enotdir} ->
		    ?HTTP_ERR_FILE_NOT_FOUND;
		{error, eacces} ->
		    ?HTTP_ERR_FORBIDDEN;
		{ok, #file_info{type = directory}} -> serve_index(FileName,
								  DirectoryIndices,
								  CustomHeaders,
								  DefaultContentType,
								  ContentTypes);
		{ok, #file_info{mtime = MTime} = FileInfo} ->
		    case calendar:local_time_to_universal_time_dst(MTime) of
			[IfModifiedSince | _] ->
			    serve_not_modified(FileInfo, FileName,
					       CustomHeaders);
			_ ->
			    serve_file(FileInfo, FileName,
				       CustomHeaders,
				       DefaultContentType,
				       ContentTypes)
		    end
	    end
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

serve_not_modified(FileInfo, FileName, CustomHeaders) ->
    ?DEBUG("Delivering not modified: ~ts", [FileName]),
    {0, 304,
     ejabberd_http:apply_custom_headers(
	 [{<<"Server">>, <<"ejabberd">>},
	  {<<"Last-Modified">>, last_modified(FileInfo)}],
	 CustomHeaders), <<>>}.

%% Assume the file exists if we got this far and attempt to read it in
%% and serve it up.
serve_file(FileInfo, FileName, CustomHeaders, DefaultContentType, ContentTypes) ->
    ?DEBUG("Delivering: ~ts", [FileName]),
    ContentType = content_type(FileName, DefaultContentType,
			       ContentTypes),
    {FileInfo#file_info.size, 200,
     ejabberd_http:apply_custom_headers(
	 [{<<"Server">>, <<"ejabberd">>},
	  {<<"Last-Modified">>, last_modified(FileInfo)},
	  {<<"Content-Type">>, ContentType}],
	 CustomHeaders),
     {file, FileName}}.

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

reopen_log() ->
    lists:foreach(
      fun(Host) ->
	      gen_server:cast(get_proc_name(Host), reopen_log)
      end, ejabberd_option:hosts()).

add_to_log(FileSize, Code, Request) ->
    gen_server:cast(get_proc_name(Request#request.host),
		    {add_to_log, FileSize, Code, Request}).

add_to_log(undefined, _FileSize, _Code, _Request) ->
    ok;
add_to_log(File, FileSize, Code, Request) ->
    {{Year, Month, Day}, {Hour, Minute, Second}} = calendar:local_time(),
    IP = ip_to_string(element(1, Request#request.ip)),
    Path = join(Request#request.path, "/"),
    Query = case stringify_query(Request#request.q) of
		<<"">> ->
		    "";
		String ->
		    [$? | String]
	    end,
    UserAgent = find_header('User-Agent', Request#request.headers, "-"),
    Referer = find_header('Referer', Request#request.headers, "-"),
    %% Pseudo Combined Apache log format:
    %% 127.0.0.1 - - [28/Mar/2007:18:41:55 +0200] "GET / HTTP/1.1" 302 303 "-" "tsung"
    %% TODO some fields are hardcoded/missing:
    %%   The date/time integers should have always 2 digits. For example day "7" should be "07"
    %%   Month should be 3*letter, not integer 1..12
    %%   Missing time zone = (`+' | `-') 4*digit
    %%   Missing protocol version: HTTP/1.1
    %% For reference: http://httpd.apache.org/docs/2.2/logs.html
    io:format(File, "~ts - - [~p/~p/~p:~p:~p:~p] \"~ts /~ts~ts\" ~p ~p ~p ~p~n",
	      [IP, Day, Month, Year, Hour, Minute, Second, Request#request.method, Path, Query, Code,
               FileSize, Referer, UserAgent]).

stringify_query(Q) ->
    stringify_query(Q, []).
stringify_query([], Res) ->
    join(lists:reverse(Res), "&");
stringify_query([{nokey, _B} | Q], Res) ->
    stringify_query(Q, Res);
stringify_query([{A, B} | Q], Res) ->
    stringify_query(Q, [join([A,B], "=") | Res]).

find_header(Header, Headers, Default) ->
    case lists:keysearch(Header, 1, Headers) of
      {value, {_, Value}} -> Value;
      false -> Default
    end.

%%----------------------------------------------------------------------
%% Utilities
%%----------------------------------------------------------------------

get_proc_name(Host) -> gen_mod:get_module_proc(Host, ?MODULE).

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

mod_opt_type(accesslog) ->
    econf:file(write);
mod_opt_type(content_types) ->
    econf:map(econf:binary(), econf:binary());
mod_opt_type(custom_headers) ->
    econf:map(econf:binary(), econf:binary());
mod_opt_type(default_content_type) ->
    econf:binary();
mod_opt_type(directory_indices) ->
    econf:list(econf:binary());
mod_opt_type(docroot) ->
    econf:directory(write);
mod_opt_type(must_authenticate_with) ->
    econf:list(
      econf:and_then(
	econf:and_then(
	  econf:binary("^[^:]+:[^:]+$"),
	  econf:binary_sep(":")),
	fun([K, V]) -> {K, V} end)).

-spec mod_options(binary()) -> [{must_authenticate_with, [{binary(), binary()}]} |
				{atom(), any()}].
mod_options(_) ->
    [{accesslog, undefined},
     {content_types, []},
     {default_content_type, <<"application/octet-stream">>},
     {custom_headers, []},
     {directory_indices, []},
     {must_authenticate_with, []},
     %% Required option
     docroot].

mod_doc() ->
    #{desc =>
          ?T("This simple module serves files from the local disk over HTTP."),
      opts =>
          [{accesslog,
            #{value => ?T("Path"),
              desc =>
                  ?T("File to log accesses using an Apache-like format. "
                     "No log will be recorded if this option is not specified.")}},
           {docroot,
            #{value => ?T("Path"),
              desc =>
                  ?T("Directory to serve the files from. "
                     "This is a mandatory option.")}},
           {content_types,
            #{value => "{Extension: Type}",
              desc =>
                  ?T("Specify mappings of extension to content type. "
                     "There are several content types already defined. "
                     "With this option you can add new definitions "
                     "or modify existing ones. The default values are:"),
              example =>
                  ["content_types:"|
                     ["  " ++ binary_to_list(E) ++ ": " ++ binary_to_list(T)
                      || {E, T} <- ?DEFAULT_CONTENT_TYPES]]}},
           {default_content_type,
            #{value => ?T("Type"),
              desc =>
                  ?T("Specify the content type to use for unknown extensions. "
                     "The default value is 'application/octet-stream'.")}},
           {custom_headers,
            #{value => "{Name: Value}",
              desc =>
                  ?T("Indicate custom HTTP headers to be included in all responses. "
                     "There are no custom headers by default.")}},
           {directory_indices,
            #{value => "[Index, ...]",
              desc =>
                  ?T("Indicate one or more directory index files, "
                     "similarly to Apache's 'DirectoryIndex' variable. "
                     "When an HTTP request hits a directory instead of a "
                     "regular file, those directory indices are looked in order, "
                     "and the first one found is returned. "
                     "The default value is an empty list.")}},
           {must_authenticate_with,
            #{value => ?T("[{Username, Hostname}, ...]"),
              desc =>
                  ?T("List of accounts that are allowed to use this service. "
		     "Default value: '[]'.")}}],
      example =>
          [{?T("This example configuration will serve the files from the "
	       "local directory '/var/www' in the address "
	       "'http://example.org:5280/pub/archive/'. In this example a new "
	       "content type 'ogg' is defined, 'png' is redefined, and 'jpg' "
	       "definition is deleted:"),
	   ["listen:",
           "  ...",
           "  -",
           "    port: 5280",
           "    module: ejabberd_http",
           "    request_handlers:",
           "      ...",
           "      /pub/archive: mod_http_fileserver",
           "      ...",
           "  ...",
           "",
           "modules:",
           "  ...",
           "  mod_http_fileserver:",
           "    docroot: /var/www",
           "    accesslog: /var/log/ejabberd/access.log",
           "    directory_indices:",
           "      - index.html",
           "      - main.htm",
           "    custom_headers:",
           "      X-Powered-By: Erlang/OTP",
           "      X-Fry: \"It's a widely-believed fact!\"",
           "    content_types:",
           "      .ogg: audio/ogg",
           "      .png: image/png",
           "    default_content_type: text/html",
           "  ..."]}]}.
