%%%----------------------------------------------------------------------
%%% File    : mod_http_upload.erl
%%% Author  : Holger Weiss <holger@zedat.fu-berlin.de>
%%% Purpose : HTTP File Upload (XEP-0363)
%%% Created : 20 Aug 2015 by Holger Weiss <holger@zedat.fu-berlin.de>
%%%
%%%
%%% ejabberd, Copyright (C) 2015-2025   ProcessOne
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

-module(mod_http_upload).
-author('holger@zedat.fu-berlin.de').
-behaviour(gen_server).
-behaviour(gen_mod).
-protocol({xep, 363, '0.3.0', '15.10', "complete", ""}).

-define(SERVICE_REQUEST_TIMEOUT, 5000). % 5 seconds.
-define(CALL_TIMEOUT, 60000). % 1 minute.
-define(SLOT_TIMEOUT, timer:hours(5)).
-define(DEFAULT_CONTENT_TYPE, <<"application/octet-stream">>).
-define(CONTENT_TYPES,
	[{<<".avi">>, <<"video/avi">>},
	 {<<".bmp">>, <<"image/bmp">>},
	 {<<".bz2">>, <<"application/x-bzip2">>},
	 {<<".gif">>, <<"image/gif">>},
	 {<<".gz">>, <<"application/x-gzip">>},
	 {<<".jpeg">>, <<"image/jpeg">>},
	 {<<".jpg">>, <<"image/jpeg">>},
	 {<<".m4a">>, <<"audio/mp4">>},
	 {<<".mp3">>, <<"audio/mpeg">>},
	 {<<".mp4">>, <<"video/mp4">>},
	 {<<".mpeg">>, <<"video/mpeg">>},
	 {<<".mpg">>, <<"video/mpeg">>},
	 {<<".ogg">>, <<"application/ogg">>},
	 {<<".pdf">>, <<"application/pdf">>},
	 {<<".png">>, <<"image/png">>},
	 {<<".rtf">>, <<"application/rtf">>},
	 {<<".svg">>, <<"image/svg+xml">>},
	 {<<".tiff">>, <<"image/tiff">>},
	 {<<".txt">>, <<"text/plain">>},
	 {<<".wav">>, <<"audio/wav">>},
	 {<<".webp">>, <<"image/webp">>},
	 {<<".xz">>, <<"application/x-xz">>},
	 {<<".zip">>, <<"application/zip">>}]).

%% gen_mod/supervisor callbacks.
-export([start/2,
	 stop/1,
	 reload/3,
	 depends/2,
         mod_doc/0,
	 mod_opt_type/1,
	 mod_options/1]).

%% gen_server callbacks.
-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3]).

%% ejabberd_http callback.
-export([process/2]).

%% ejabberd_hooks callback.
-export([remove_user/2]).

%% Utility functions.
-export([get_proc_name/2,
	 expand_home/1,
	 expand_host/2]).

-include("ejabberd_http.hrl").
-include_lib("xmpp/include/xmpp.hrl").
-include("logger.hrl").
-include("translate.hrl").

-record(state,
	{server_host = <<>>     :: binary(),
	 hosts = []             :: [binary()],
	 name = <<>>            :: binary(),
	 access = none          :: atom(),
	 max_size = infinity    :: pos_integer() | infinity,
	 secret_length = 40     :: pos_integer(),
	 jid_in_url = sha1      :: sha1 | node,
	 file_mode              :: integer() | undefined,
	 dir_mode               :: integer() | undefined,
	 docroot = <<>>         :: binary(),
	 put_url = <<>>         :: binary(),
	 get_url = <<>>         :: binary(),
	 service_url            :: binary() | undefined,
	 thumbnail = false      :: boolean(),
	 custom_headers = []    :: [{binary(), binary()}],
	 slots = #{}            :: slots(),
	 external_secret = <<>> :: binary()}).

-record(media_info,
	{path   :: binary(),
	 type   :: atom(),
	 height :: integer(),
	 width  :: integer()}).

-type state() :: #state{}.
-type slot() :: [binary(), ...].
-type slots() :: #{slot() => {pos_integer(), reference()}}.
-type media_info() :: #media_info{}.

%%--------------------------------------------------------------------
%% gen_mod/supervisor callbacks.
%%--------------------------------------------------------------------
-spec start(binary(), gen_mod:opts()) -> {ok, pid()} | {error, term()}.
start(ServerHost, Opts) ->
    Proc = get_proc_name(ServerHost, ?MODULE),
    case gen_mod:start_child(?MODULE, ServerHost, Opts, Proc) of
	{ok, _} = Ret -> Ret;
	{error, {already_started, _}} = Err ->
	    ?ERROR_MSG("Multiple virtual hosts can't use a single 'put_url' "
		       "without the @HOST@ keyword", []),
	    Err;
	Err ->
	    Err
    end.

-spec stop(binary()) -> ok | {error, any()}.
stop(ServerHost) ->
    Proc = get_proc_name(ServerHost, ?MODULE),
    gen_mod:stop_child(Proc).

-spec reload(binary(), gen_mod:opts(), gen_mod:opts()) -> ok | {ok, pid()} | {error, term()}.
reload(ServerHost, NewOpts, OldOpts) ->
    NewURL = mod_http_upload_opt:put_url(NewOpts),
    OldURL = mod_http_upload_opt:put_url(OldOpts),
    OldProc = get_proc_name(ServerHost, ?MODULE, OldURL),
    NewProc = get_proc_name(ServerHost, ?MODULE, NewURL),
    if OldProc /= NewProc ->
	    gen_mod:stop_child(OldProc),
	    start(ServerHost, NewOpts);
       true ->
	    gen_server:cast(NewProc, {reload, NewOpts, OldOpts})
    end.

-spec mod_opt_type(atom()) -> econf:validator().
mod_opt_type(name) ->
    econf:binary();
mod_opt_type(access) ->
    econf:acl();
mod_opt_type(max_size) ->
    econf:pos_int(infinity);
mod_opt_type(secret_length) ->
    econf:int(8, 1000);
mod_opt_type(jid_in_url) ->
    econf:enum([sha1, node]);
mod_opt_type(file_mode) ->
    econf:octal();
mod_opt_type(dir_mode) ->
    econf:octal();
mod_opt_type(docroot) ->
    econf:binary();
mod_opt_type(put_url) ->
    econf:url();
mod_opt_type(get_url) ->
    econf:url();
mod_opt_type(service_url) ->
    econf:url();
mod_opt_type(custom_headers) ->
    econf:map(econf:binary(), econf:binary());
mod_opt_type(rm_on_unregister) ->
    econf:bool();
mod_opt_type(thumbnail) ->
    econf:and_then(
      econf:bool(),
      fun(true) ->
	      case eimp:supported_formats() of
		  [] -> econf:fail(eimp_error);
		  [_|_] -> true
	      end;
	 (false) ->
	      false
      end);
mod_opt_type(external_secret) ->
    econf:binary();
mod_opt_type(host) ->
    econf:host();
mod_opt_type(hosts) ->
    econf:hosts();
mod_opt_type(vcard) ->
    econf:vcard_temp().

-spec mod_options(binary()) -> [{thumbnail, boolean()} |
				{atom(), any()}].
mod_options(Host) ->
    [{host, <<"upload.", Host/binary>>},
     {hosts, []},
     {name, ?T("HTTP File Upload")},
     {vcard, undefined},
     {access, local},
     {max_size, 104857600},
     {secret_length, 40},
     {jid_in_url, sha1},
     {file_mode, undefined},
     {dir_mode, undefined},
     {docroot, <<"@HOME@/upload">>},
     {put_url, <<"https://", Host/binary, ":5443/upload">>},
     {get_url, undefined},
     {service_url, undefined},
     {external_secret, <<"">>},
     {custom_headers, []},
     {rm_on_unregister, true},
     {thumbnail, false}].

mod_doc() ->
    #{desc =>
          [?T("This module allows for requesting permissions to "
              "upload a file via HTTP as described in "
              "https://xmpp.org/extensions/xep-0363.html"
              "[XEP-0363: HTTP File Upload]. If the request is accepted, "
              "the client receives a URL for uploading the file and "
              "another URL from which that file can later be downloaded."), "",
           ?T("In order to use this module, it must be enabled "
              "in 'listen' -> 'ejabberd_http' -> "
              "_`listen-options.md#request_handlers|request_handlers`_.")],
      opts =>
          [{host,
            #{desc => ?T("Deprecated. Use 'hosts' instead.")}},
           {hosts,
            #{value => ?T("[Host, ...]"),
              desc =>
                  ?T("This option defines the Jabber IDs of the service. "
                     "If the 'hosts' option is not specified, the only Jabber ID will "
                     "be the hostname of the virtual host with the prefix '\"upload.\"'. "
                     "The keyword '@HOST@' is replaced with the real virtual host name.")}},
           {name,
            #{value => ?T("Name"),
              desc =>
                  ?T("A name of the service in the Service Discovery. "
                     "The default value is '\"HTTP File Upload\"'. "
                     "Please note this will only be displayed by some XMPP clients.")}},
           {access,
            #{value => ?T("AccessName"),
              desc =>
                  ?T("This option defines the access rule to limit who is "
                     "permitted to use the HTTP upload service. "
                     "The default value is 'local'. If no access rule of "
                     "that name exists, no user will be allowed to use the service.")}},
           {max_size,
            #{value => ?T("Size"),
              desc =>
                  ?T("This option limits the acceptable file size. "
                     "Either a number of bytes (larger than zero) or "
                     "'infinity' must be specified. "
                     "The default value is '104857600'.")}},
           {secret_length,
            #{value => ?T("Length"),
              desc =>
                  ?T("This option defines the length of the random "
                     "string included in the GET and PUT URLs generated "
                     "by 'mod_http_upload'. The minimum length is '8' characters, "
                     "but it is recommended to choose a larger value. "
                     "The default value is '40'.")}},
           {jid_in_url,
            #{value => "node | sha1",
              desc =>
                  ?T("When this option is set to 'node', the node identifier "
                     "of the user's JID (i.e., the user name) is included in "
                     "the GET and PUT URLs generated by 'mod_http_upload'. "
                     "Otherwise, a SHA-1 hash of the user's bare JID is "
                     "included instead. The default value is 'sha1'.")}},
           {thumbnail,
            #{value => "true | false",
              desc =>
                  ?T("This option specifies whether ejabberd should create "
                     "thumbnails of uploaded images. If a thumbnail is created, "
                     "a <thumbnail/> element that contains the download <uri/> "
                     "and some metadata is returned with the PUT response. "
                     "The default value is 'false'.")}},
           {file_mode,
            #{value => ?T("Permission"),
              desc =>
                  ?T("This option defines the permission bits of uploaded files. "
                     "The bits are specified as an octal number (see the 'chmod(1)' "
                     "manual page) within double quotes. For example: '\"0644\"'. "
                     "The default is undefined, which means no explicit permissions "
                     "will be set.")}},
           {dir_mode,
            #{value => ?T("Permission"),
              desc =>
                  ?T("This option defines the permission bits of the 'docroot' "
                     "directory and any directories created during file uploads. "
                     "The bits are specified as an octal number (see the 'chmod(1)' "
                     "manual page) within double quotes. For example: '\"0755\"'. "
                     "The default is undefined, which means no explicit permissions "
                     "will be set.")}},
           {docroot,
            #{value => ?T("Path"),
              desc =>
                  ?T("Uploaded files are stored below the directory specified "
                       "(as an absolute path) with this option. The keyword "
                     "'@HOME@' is replaced with the home directory of the user "
                     "running ejabberd, and the keyword '@HOST@' with the virtual "
                     "host name. The default value is '\"@HOME@/upload\"'.")}},
           {put_url,
            #{value => ?T("URL"),
              desc =>
                  ?T("This option specifies the initial part of the PUT URLs "
                     "used for file uploads. The keyword '@HOST@' is replaced "
                     "with the virtual host name. "
                     "And '@HOST_URL_ENCODE@' is replaced with the host name encoded for URL, "
                     "useful when your virtual hosts contain non-latin characters. "
                     "NOTE: different virtual hosts cannot use the same PUT URL. "
                     "The default value is '\"https://@HOST@:5443/upload\"'.")}},
           {get_url,
            #{value => ?T("URL"),
              desc =>
                  ?T("This option specifies the initial part of the GET URLs "
                     "used for downloading the files. The default value is 'undefined'. "
                     "When this option is 'undefined', this option is set "
                     "to the same value as 'put_url'. The keyword '@HOST@' is "
                     "replaced with the virtual host name. NOTE: if GET requests "
                     "are handled by this module, the 'get_url' must match the "
                     "'put_url'. Setting it to a different value only makes "
                     "sense if an external web server or _`mod_http_fileserver`_ "
                     "is used to serve the uploaded files.")}},
           {service_url,
            #{desc => ?T("Deprecated.")}},
           {custom_headers,
            #{value => "{Name: Value}",
              desc =>
                  ?T("This option specifies additional header fields to be "
                     "included in all HTTP responses. By default no custom "
                     "headers are included.")}},
           {external_secret,
            #{value => ?T("Text"),
              desc =>
                  ?T("This option makes it possible to offload all HTTP "
                     "Upload processing to a separate HTTP server. "
                     "Both ejabberd and the HTTP server should share this "
                     "secret and behave exactly as described at "
                     "https://modules.prosody.im/mod_http_upload_external.html#implementation"
                     "[Prosody's mod_http_upload_external: Implementation]. "
                     "There is no default value.")}},
           {rm_on_unregister,
            #{value => "true | false",
              desc =>
                  ?T("This option specifies whether files uploaded by a user "
                     "should be removed when that user is unregistered. "
                     "The default value is 'true'.")}},
           {vcard,
            #{value => ?T("vCard"),
              desc =>
                  ?T("A custom vCard of the service that will be displayed "
                     "by some XMPP clients in Service Discovery. The value of "
                     "'vCard' is a YAML map constructed from an XML representation "
                     "of vCard. Since the representation has no attributes, "
                     "the mapping is straightforward."),
              example =>
                  ["# This XML representation of vCard:",
                   "#   <vCard xmlns='vcard-temp'>",
                   "#     <FN>Conferences</FN>",
                   "#     <ADR>",
                   "#       <WORK/>",
                   "#       <STREET>Elm Street</STREET>",
                   "#     </ADR>",
                   "#   </vCard>",
                   "# ",
                   "# is translated to:",
                   "vcard:",
                   "  fn: Conferences",
                   "  adr:",
                   "    -",
                   "      work: true",
                   "      street: Elm Street"]}}],
      example =>
          ["listen:",
           "  -",
           "    port: 5443",
           "    module: ejabberd_http",
           "    tls: true",
           "    request_handlers:",
           "      /upload: mod_http_upload",
           "",
           "modules:",
           "  mod_http_upload:",
           "    docroot: /ejabberd/upload",
           "    put_url: \"https://@HOST@:5443/upload\""]}.

-spec depends(binary(), gen_mod:opts()) -> [{module(), hard | soft}].
depends(_Host, _Opts) ->
    [].

%%--------------------------------------------------------------------
%% gen_server callbacks.
%%--------------------------------------------------------------------
-spec init(list()) -> {ok, state()}.
init([ServerHost|_]) ->
    process_flag(trap_exit, true),
    Opts = gen_mod:get_module_opts(ServerHost, ?MODULE),
    Hosts = gen_mod:get_opt_hosts(Opts),
    case mod_http_upload_opt:rm_on_unregister(Opts) of
	true ->
	    ejabberd_hooks:add(remove_user, ServerHost, ?MODULE,
			       remove_user, 50);
	false ->
	    ok
    end,
    State = init_state(ServerHost, Hosts, Opts),
    {ok, State}.

-spec handle_call(_, {pid(), _}, state())
      -> {reply, {ok, pos_integer(), binary(),
		      pos_integer() | undefined,
		      pos_integer() | undefined}, state()} |
	 {reply, {error, atom()}, state()} | {noreply, state()}.
handle_call({use_slot, Slot, Size}, _From,
	    #state{file_mode = FileMode,
		   dir_mode = DirMode,
		   get_url = GetPrefix,
		   thumbnail = Thumbnail,
		   custom_headers = CustomHeaders,
		   docroot = DocRoot} = State) ->
    case get_slot(Slot, State) of
	{ok, {Size, TRef}} ->
	    misc:cancel_timer(TRef),
	    NewState = del_slot(Slot, State),
	    Path = str:join([DocRoot | Slot], <<$/>>),
	    {reply,
	     {ok, Path, FileMode, DirMode, GetPrefix, Thumbnail, CustomHeaders},
	     NewState};
	{ok, {_WrongSize, _TRef}} ->
	    {reply, {error, size_mismatch}, State};
	error ->
	    {reply, {error, invalid_slot}, State}
    end;
handle_call(get_conf, _From,
	    #state{docroot = DocRoot,
	           custom_headers = CustomHeaders} = State) ->
    {reply, {ok, DocRoot, CustomHeaders}, State};
handle_call(Request, From, State) ->
    ?WARNING_MSG("Unexpected call from ~p: ~p", [From, Request]),
    {noreply, State}.

-spec handle_cast(_, state()) -> {noreply, state()}.
handle_cast({reload, NewOpts, OldOpts},
	    #state{server_host = ServerHost} = State) ->
    case {mod_http_upload_opt:rm_on_unregister(NewOpts),
	  mod_http_upload_opt:rm_on_unregister(OldOpts)} of
	{true, false} ->
	    ejabberd_hooks:add(remove_user, ServerHost, ?MODULE,
			       remove_user, 50);
	{false, true} ->
	    ejabberd_hooks:delete(remove_user, ServerHost, ?MODULE,
				  remove_user, 50);
	_ ->
	    ok
    end,
    NewHosts = gen_mod:get_opt_hosts(NewOpts),
    OldHosts = gen_mod:get_opt_hosts(OldOpts),
    lists:foreach(fun ejabberd_router:unregister_route/1, OldHosts -- NewHosts),
    NewState = init_state(State#state{hosts = NewHosts -- OldHosts}, NewOpts),
    {noreply, NewState};
handle_cast(Request, State) ->
    ?WARNING_MSG("Unexpected cast: ~p", [Request]),
    {noreply, State}.

-spec handle_info(timeout | _, state()) -> {noreply, state()}.
handle_info({route, #iq{lang = Lang} = Packet}, State) ->
    try xmpp:decode_els(Packet) of
	IQ ->
	    {Reply, NewState} = case process_iq(IQ, State) of
				    R when is_record(R, iq) ->
					{R, State};
				    {R, S} ->
					{R, S};
				    not_request ->
					{none, State}
				end,
	    if Reply /= none ->
		    ejabberd_router:route(Reply);
	       true ->
		    ok
	    end,
	    {noreply, NewState}
    catch _:{xmpp_codec, Why} ->
	    Txt = xmpp:io_format_error(Why),
	    Err = xmpp:err_bad_request(Txt, Lang),
	    ejabberd_router:route_error(Packet, Err),
	    {noreply, State}
    end;
handle_info({timeout, _TRef, Slot}, State) ->
    NewState = del_slot(Slot, State),
    {noreply, NewState};
handle_info(Info, State) ->
    ?WARNING_MSG("Unexpected info: ~p", [Info]),
    {noreply, State}.

-spec terminate(normal | shutdown | {shutdown, _} | _, state()) -> ok.
terminate(Reason, #state{server_host = ServerHost, hosts = Hosts}) ->
    ?DEBUG("Stopping HTTP upload process for ~ts: ~p", [ServerHost, Reason]),
    ejabberd_hooks:delete(remove_user, ServerHost, ?MODULE, remove_user, 50),
    lists:foreach(fun ejabberd_router:unregister_route/1, Hosts).

-spec code_change({down, _} | _, state(), _) -> {ok, state()}.
code_change(_OldVsn, #state{server_host = ServerHost} = State, _Extra) ->
    ?DEBUG("Updating HTTP upload process for ~ts", [ServerHost]),
    {ok, State}.

%%--------------------------------------------------------------------
%% ejabberd_http callback.
%%--------------------------------------------------------------------
-spec process([binary()], #request{})
      -> {pos_integer(), [{binary(), binary()}], binary()}.
process(LocalPath, #request{method = Method, host = Host, ip = IP})
    when length(LocalPath) < 3,
	 Method == 'PUT' orelse
	 Method == 'GET' orelse
	 Method == 'HEAD' ->
    ?DEBUG("Rejecting ~ts request from ~ts for ~ts: Too few path components",
	   [Method, encode_addr(IP), Host]),
    http_response(404);
process(_LocalPath, #request{method = 'PUT', host = Host, ip = IP,
			     length = Length} = Request0) ->
    Request = Request0#request{host = redecode_url(Host)},
    {Proc, Slot} = parse_http_request(Request),
    try gen_server:call(Proc, {use_slot, Slot, Length}, ?CALL_TIMEOUT) of
	{ok, Path, FileMode, DirMode, GetPrefix, Thumbnail, CustomHeaders} ->
	    ?DEBUG("Storing file from ~ts for ~ts: ~ts",
		   [encode_addr(IP), Host, Path]),
	    case store_file(Path, Request, FileMode, DirMode,
			    GetPrefix, Slot, Thumbnail) of
		ok ->
		    http_response(201, CustomHeaders);
		{ok, Headers, OutData} ->
		    http_response(201, ejabberd_http:apply_custom_headers(Headers, CustomHeaders), OutData);
		{error, closed} ->
		    ?DEBUG("Cannot store file ~ts from ~ts for ~ts: connection closed",
			   [Path, encode_addr(IP), Host]),
		    http_response(404);
		{error, Error} ->
		    ?ERROR_MSG("Cannot store file ~ts from ~ts for ~ts: ~ts",
			       [Path, encode_addr(IP), Host, format_error(Error)]),
		    http_response(500)
	    end;
	{error, size_mismatch} ->
	    ?WARNING_MSG("Rejecting file ~ts from ~ts for ~ts: Unexpected size (~B)",
		      [lists:last(Slot), encode_addr(IP), Host, Length]),
	    http_response(413);
	{error, invalid_slot} ->
	    ?WARNING_MSG("Rejecting file ~ts from ~ts for ~ts: Invalid slot",
		      [lists:last(Slot), encode_addr(IP), Host]),
	    http_response(403)
    catch
	exit:{noproc, _} ->
	    ?WARNING_MSG("Cannot handle PUT request from ~ts for ~ts: "
			 "Upload not configured for this host",
			 [encode_addr(IP), Host]),
	    http_response(404);
	_:Error ->
	    ?ERROR_MSG("Cannot handle PUT request from ~ts for ~ts: ~p",
		       [encode_addr(IP), Host, Error]),
	    http_response(500)
    end;
process(_LocalPath, #request{method = Method, host = Host, ip = IP} = Request0)
    when Method == 'GET';
	 Method == 'HEAD' ->
    Request = Request0#request{host = redecode_url(Host)},
    {Proc, [_UserDir, _RandDir, FileName] = Slot} = parse_http_request(Request),
    try gen_server:call(Proc, get_conf, ?CALL_TIMEOUT) of
	{ok, DocRoot, CustomHeaders} ->
	    Path = str:join([DocRoot | Slot], <<$/>>),
	    case file:open(Path, [read]) of
		{ok, Fd} ->
		    file:close(Fd),
		    ?INFO_MSG("Serving ~ts to ~ts", [Path, encode_addr(IP)]),
		    ContentType = guess_content_type(FileName),
		    Headers1 = case ContentType of
				 <<"image/", _SubType/binary>> -> [];
				 <<"text/", _SubType/binary>> -> [];
				 _ ->
				     [{<<"Content-Disposition">>,
				       <<"attachment; filename=",
					 $", FileName/binary, $">>}]
			       end,
		    Headers2 = [{<<"Content-Type">>, ContentType} | Headers1],
		    Headers3 = ejabberd_http:apply_custom_headers(Headers2, CustomHeaders),
		    http_response(200, Headers3, {file, Path});
		{error, eacces} ->
		    ?WARNING_MSG("Cannot serve ~ts to ~ts: Permission denied",
			      [Path, encode_addr(IP)]),
		    http_response(403);
		{error, enoent} ->
		    ?WARNING_MSG("Cannot serve ~ts to ~ts: No such file",
			      [Path, encode_addr(IP)]),
		    http_response(404);
		{error, eisdir} ->
		    ?WARNING_MSG("Cannot serve ~ts to ~ts: Is a directory",
			      [Path, encode_addr(IP)]),
		    http_response(404);
		{error, Error} ->
		    ?WARNING_MSG("Cannot serve ~ts to ~ts: ~ts",
			      [Path, encode_addr(IP), format_error(Error)]),
		    http_response(500)
	    end
    catch
	exit:{noproc, _} ->
	    ?WARNING_MSG("Cannot handle ~ts request from ~ts for ~ts: "
			 "Upload not configured for this host",
			 [Method, encode_addr(IP), Host]),
	    http_response(404);
	_:Error ->
	    ?ERROR_MSG("Cannot handle ~ts request from ~ts for ~ts: ~p",
		       [Method, encode_addr(IP), Host, Error]),
	    http_response(500)
    end;
process(_LocalPath, #request{method = 'OPTIONS', host = Host,
			     ip = IP} = Request) ->
    ?DEBUG("Responding to OPTIONS request from ~ts for ~ts",
	   [encode_addr(IP), Host]),
    {Proc, _Slot} = parse_http_request(Request),
    try gen_server:call(Proc, get_conf, ?CALL_TIMEOUT) of
	{ok, _DocRoot, CustomHeaders} ->
	    AllowHeader = {<<"Allow">>, <<"OPTIONS, HEAD, GET, PUT">>},
	    http_response(200, ejabberd_http:apply_custom_headers([AllowHeader], CustomHeaders))
    catch
	exit:{noproc, _} ->
	    ?WARNING_MSG("Cannot handle OPTIONS request from ~ts for ~ts: "
			 "Upload not configured for this host",
			 [encode_addr(IP), Host]),
	    http_response(404);
	_:Error ->
	    ?ERROR_MSG("Cannot handle OPTIONS request from ~ts for ~ts: ~p",
		       [encode_addr(IP), Host, Error]),
	    http_response(500)
    end;
process(_LocalPath, #request{method = Method, host = Host, ip = IP}) ->
    ?DEBUG("Rejecting ~ts request from ~ts for ~ts",
	   [Method, encode_addr(IP), Host]),
    http_response(405, [{<<"Allow">>, <<"OPTIONS, HEAD, GET, PUT">>}]).

%%--------------------------------------------------------------------
%% State initialization
%%--------------------------------------------------------------------
-spec init_state(binary(), [binary()], gen_mod:opts()) -> state().
init_state(ServerHost, Hosts, Opts) ->
    init_state(#state{server_host = ServerHost, hosts = Hosts}, Opts).

-spec init_state(state(), gen_mod:opts()) -> state().
init_state(#state{server_host = ServerHost, hosts = Hosts} = State, Opts) ->
    Name = mod_http_upload_opt:name(Opts),
    Access = mod_http_upload_opt:access(Opts),
    MaxSize = mod_http_upload_opt:max_size(Opts),
    SecretLength = mod_http_upload_opt:secret_length(Opts),
    JIDinURL = mod_http_upload_opt:jid_in_url(Opts),
    DocRoot = mod_http_upload_opt:docroot(Opts),
    FileMode = mod_http_upload_opt:file_mode(Opts),
    DirMode = mod_http_upload_opt:dir_mode(Opts),
    PutURL = mod_http_upload_opt:put_url(Opts),
    GetURL = case mod_http_upload_opt:get_url(Opts) of
		 undefined -> PutURL;
		 URL -> URL
	     end,
    ServiceURL = mod_http_upload_opt:service_url(Opts),
    Thumbnail = mod_http_upload_opt:thumbnail(Opts),
    ExternalSecret = mod_http_upload_opt:external_secret(Opts),
    CustomHeaders = mod_http_upload_opt:custom_headers(Opts),
    DocRoot1 = expand_home(str:strip(DocRoot, right, $/)),
    DocRoot2 = expand_host(DocRoot1, ServerHost),
    case DirMode of
	undefined ->
	    ok;
	Mode ->
	    file:change_mode(DocRoot2, Mode)
    end,
    lists:foreach(
      fun(Host) ->
	      ejabberd_router:register_route(Host, ServerHost)
      end, Hosts),
    State#state{server_host = ServerHost, hosts = Hosts, name = Name,
		access = Access, max_size = MaxSize,
		secret_length = SecretLength, jid_in_url = JIDinURL,
		file_mode = FileMode, dir_mode = DirMode,
		thumbnail = Thumbnail,
		docroot = DocRoot2,
		put_url = expand_host(str:strip(PutURL, right, $/), ServerHost),
		get_url = expand_host(str:strip(GetURL, right, $/), ServerHost),
		service_url = ServiceURL,
		external_secret = ExternalSecret,
		custom_headers = CustomHeaders}.

%%--------------------------------------------------------------------
%% Exported utility functions.
%%--------------------------------------------------------------------
-spec get_proc_name(binary(), atom()) -> atom().
get_proc_name(ServerHost, ModuleName) ->
    PutURL = mod_http_upload_opt:put_url(ServerHost),
    get_proc_name(ServerHost, ModuleName, PutURL).

-spec get_proc_name(binary(), atom(), binary()) -> atom().
get_proc_name(ServerHost, ModuleName, PutURL) ->
    %% Once we depend on OTP >= 20.0, we can use binaries with http_uri.
    {ok, _Scheme, _UserInfo, Host0, _Port, Path0, _Query} =
        misc:uri_parse(expand_host(PutURL, ServerHost)),
    Host = jid:nameprep(iolist_to_binary(Host0)),
    Path = str:strip(iolist_to_binary(Path0), right, $/),
    ProcPrefix = <<Host/binary, Path/binary>>,
    gen_mod:get_module_proc(ProcPrefix, ModuleName).

-spec expand_home(binary()) -> binary().
expand_home(Input) ->
    Home = misc:get_home(),
    misc:expand_keyword(<<"@HOME@">>, Input, Home).

-spec expand_host(binary(), binary()) -> binary().
expand_host(Input, Host) ->
    misc:expand_keyword(<<"@HOST@">>, Input, Host).

%%--------------------------------------------------------------------
%% Internal functions.
%%--------------------------------------------------------------------

%% XMPP request handling.

-spec process_iq(iq(), state()) -> {iq(), state()} | iq() | not_request.
process_iq(#iq{type = get, lang = Lang, sub_els = [#disco_info{}]} = IQ,
	   #state{server_host = ServerHost, name = Name}) ->
    AddInfo = ejabberd_hooks:run_fold(disco_info, ServerHost, [],
				      [ServerHost, ?MODULE, <<"">>, <<"">>]),
    xmpp:make_iq_result(IQ, iq_disco_info(ServerHost, Lang, Name, AddInfo));
process_iq(#iq{type = get, sub_els = [#disco_items{}]} = IQ, _State) ->
    xmpp:make_iq_result(IQ, #disco_items{});
process_iq(#iq{type = get, sub_els = [#vcard_temp{}], lang = Lang} = IQ,
	   #state{server_host = ServerHost}) ->
    VCard = case mod_http_upload_opt:vcard(ServerHost) of
		undefined ->
		    #vcard_temp{fn = <<"ejabberd/mod_http_upload">>,
				url = ejabberd_config:get_uri(),
				desc = misc:get_descr(
					 Lang, ?T("ejabberd HTTP Upload service"))};
		V ->
		    V
	    end,
    xmpp:make_iq_result(IQ, VCard);
process_iq(#iq{type = get, sub_els = [#upload_request{filename = File,
						      size = Size,
						      'content-type' = CType,
						      xmlns = XMLNS}]} = IQ,
	   State) ->
    process_slot_request(IQ, File, Size, CType, XMLNS, State);
process_iq(#iq{type = get, sub_els = [#upload_request_0{filename = File,
							size = Size,
							'content-type' = CType,
							xmlns = XMLNS}]} = IQ,
	   State) ->
    process_slot_request(IQ, File, Size, CType, XMLNS, State);
process_iq(#iq{type = T, lang = Lang} = IQ, _State) when T == get; T == set ->
    Txt = ?T("No module is handling this query"),
    xmpp:make_error(IQ, xmpp:err_service_unavailable(Txt, Lang));
process_iq(#iq{}, _State) ->
    not_request.

-spec process_slot_request(iq(), binary(), pos_integer(), binary(), binary(),
			   state()) -> {iq(), state()} | iq().
process_slot_request(#iq{lang = Lang, from = From} = IQ,
		     File, Size, CType, XMLNS,
		     #state{server_host = ServerHost,
			    access = Access} = State) ->
    case acl:match_rule(ServerHost, Access, From) of
	allow ->
	    ContentType = yield_content_type(CType),
	    case create_slot(State, From, File, Size, ContentType, XMLNS,
			     Lang) of
		{ok, Slot} ->
		    Query = make_query_string(Slot, Size, State),
		    NewState = add_slot(Slot, Size, State),
		    NewSlot = mk_slot(Slot, State, XMLNS, Query),
		    {xmpp:make_iq_result(IQ, NewSlot), NewState};
		{ok, PutURL, GetURL} ->
		    Slot = mk_slot(PutURL, GetURL, XMLNS, <<"">>),
		    xmpp:make_iq_result(IQ, Slot);
		{error, Error} ->
		    xmpp:make_error(IQ, Error)
	    end;
	deny ->
	    ?DEBUG("Denying HTTP upload slot request from ~ts",
		   [jid:encode(From)]),
	    Txt = ?T("Access denied by service policy"),
	    xmpp:make_error(IQ, xmpp:err_forbidden(Txt, Lang))
    end.

-spec create_slot(state(), jid(), binary(), pos_integer(), binary(), binary(),
		  binary())
      -> {ok, slot()} | {ok, binary(), binary()} | {error, xmpp_element()}.
create_slot(#state{service_url = undefined, max_size = MaxSize},
	    JID, File, Size, _ContentType, XMLNS, Lang)
  when MaxSize /= infinity,
       Size > MaxSize ->
    Text = {?T("File larger than ~w bytes"), [MaxSize]},
    ?WARNING_MSG("Rejecting file ~ts from ~ts (too large: ~B bytes)",
	      [File, jid:encode(JID), Size]),
    Error = xmpp:err_not_acceptable(Text, Lang),
    Els = xmpp:get_els(Error),
    Els1 = [#upload_file_too_large{'max-file-size' = MaxSize,
				   xmlns = XMLNS} | Els],
    Error1 = xmpp:set_els(Error, Els1),
    {error, Error1};
create_slot(#state{service_url = undefined,
		   jid_in_url = JIDinURL,
		   secret_length = SecretLength,
		   server_host = ServerHost,
		   docroot = DocRoot},
	    JID, File, Size, _ContentType, _XMLNS, Lang) ->
    UserStr = make_user_string(JID, JIDinURL),
    UserDir = <<DocRoot/binary, $/, UserStr/binary>>,
    case ejabberd_hooks:run_fold(http_upload_slot_request, ServerHost, allow,
				 [ServerHost, JID, UserDir, Size, Lang]) of
	allow ->
	    RandStr = p1_rand:get_alphanum_string(SecretLength),
	    FileStr = make_file_string(File),
	    ?INFO_MSG("Got HTTP upload slot for ~ts (file: ~ts, size: ~B)",
		      [jid:encode(JID), File, Size]),
	    {ok, [UserStr, RandStr, FileStr]};
	deny ->
	    {error, xmpp:err_service_unavailable()};
	#stanza_error{} = Error ->
	    {error, Error}
    end;
create_slot(#state{service_url = ServiceURL},
	    #jid{luser = U, lserver = S} = JID,
	    File, Size, ContentType, _XMLNS, Lang) ->
    Options = [{body_format, binary}, {full_result, false}],
    HttpOptions = [{timeout, ?SERVICE_REQUEST_TIMEOUT}],
    SizeStr = integer_to_binary(Size),
    JidStr = jid:encode({U, S, <<"">>}),
    GetRequest = <<ServiceURL/binary,
		   "?jid=", (misc:url_encode(JidStr))/binary,
		   "&name=", (misc:url_encode(File))/binary,
		   "&size=", (misc:url_encode(SizeStr))/binary,
		   "&content_type=", (misc:url_encode(ContentType))/binary>>,
    case httpc:request(get, {binary_to_list(GetRequest), []},
		       HttpOptions, Options) of
	{ok, {Code, Body}} when Code >= 200, Code =< 299 ->
	    case binary:split(Body, <<$\n>>, [global, trim]) of
		[<<"http", _/binary>> = PutURL,
		 <<"http", _/binary>> = GetURL] ->
		    ?INFO_MSG("Got HTTP upload slot for ~ts (file: ~ts, size: ~B)",
			      [jid:encode(JID), File, Size]),
		    {ok, PutURL, GetURL};
		Lines ->
		    ?ERROR_MSG("Can't parse data received for ~ts from <~ts>: ~p",
			       [jid:encode(JID), ServiceURL, Lines]),
		    Txt = ?T("Failed to parse HTTP response"),
		    {error, xmpp:err_service_unavailable(Txt, Lang)}
	    end;
	{ok, {402, _Body}} ->
	    ?WARNING_MSG("Got status code 402 for ~ts from <~ts>",
		      [jid:encode(JID), ServiceURL]),
	    {error, xmpp:err_resource_constraint()};
	{ok, {403, _Body}} ->
	    ?WARNING_MSG("Got status code 403 for ~ts from <~ts>",
		      [jid:encode(JID), ServiceURL]),
	    {error, xmpp:err_not_allowed()};
	{ok, {413, _Body}} ->
	    ?WARNING_MSG("Got status code 413 for ~ts from <~ts>",
		      [jid:encode(JID), ServiceURL]),
	    {error, xmpp:err_not_acceptable()};
	{ok, {Code, _Body}} ->
	    ?ERROR_MSG("Unexpected status code for ~ts from <~ts>: ~B",
		       [jid:encode(JID), ServiceURL, Code]),
	    {error, xmpp:err_service_unavailable()};
	{error, Reason} ->
	    ?ERROR_MSG("Error requesting upload slot for ~ts from <~ts>: ~p",
		       [jid:encode(JID), ServiceURL, Reason]),
	    {error, xmpp:err_service_unavailable()}
    end.

-spec add_slot(slot(), pos_integer(), state()) -> state().
add_slot(Slot, Size, #state{external_secret = <<>>, slots = Slots} = State) ->
    TRef = erlang:start_timer(?SLOT_TIMEOUT, self(), Slot),
    NewSlots = maps:put(Slot, {Size, TRef}, Slots),
    State#state{slots = NewSlots};
add_slot(_Slot, _Size, State) ->
    State.

-spec get_slot(slot(), state()) -> {ok, {pos_integer(), reference()}} | error.
get_slot(Slot, #state{slots = Slots}) ->
    maps:find(Slot, Slots).

-spec del_slot(slot(), state()) -> state().
del_slot(Slot, #state{slots = Slots} = State) ->
    NewSlots = maps:remove(Slot, Slots),
    State#state{slots = NewSlots}.

-spec mk_slot(slot(), state(), binary(), binary()) -> upload_slot();
	     (binary(), binary(), binary(), binary()) -> upload_slot().
mk_slot(Slot, #state{put_url = PutPrefix, get_url = GetPrefix}, XMLNS, Query) ->
    PutURL = str:join([PutPrefix | Slot], <<$/>>),
    GetURL = str:join([GetPrefix | Slot], <<$/>>),
    mk_slot(PutURL, GetURL, XMLNS, Query);
mk_slot(PutURL, GetURL, XMLNS, Query) ->
    PutURL1 = <<(reencode_url(PutURL))/binary, Query/binary>>,
    GetURL1 = reencode_url(GetURL),
    case XMLNS of
	?NS_HTTP_UPLOAD_0 ->
	    #upload_slot_0{get = GetURL1, put = PutURL1, xmlns = XMLNS};
	_ ->
	    #upload_slot{get = GetURL1, put = PutURL1, xmlns = XMLNS}
    end.

reencode_url(UrlString) ->
    {ok, _, _, Host, _, _, _} = yconf:parse_uri(misc:url_encode(UrlString)),
    HostDecoded = uri_string:percent_decode(Host),
    HostIdna = idna:encode(uri_string:percent_decode(HostDecoded)),
    re:replace(UrlString, HostDecoded, HostIdna, [{return, binary}]).

redecode_url(UrlString) ->
    {ok, _, _, HostIdna, _, _, _} = yconf:parse_uri(<<"http://", UrlString/binary>>),
    HostDecoded = idna:decode(HostIdna),
    Host = uri_string:quote(HostDecoded),
    re:replace(UrlString, HostIdna, Host, [{return, binary}]).

-spec make_user_string(jid(), sha1 | node) -> binary().
make_user_string(#jid{luser = U, lserver = S}, sha1) ->
    str:sha(<<U/binary, $@, S/binary>>);
make_user_string(#jid{luser = U}, node) ->
    replace_special_chars(U).

-spec make_file_string(binary()) -> binary().
make_file_string(File) ->
    replace_special_chars(File).

-spec make_query_string(slot(), non_neg_integer(), state()) -> binary().
make_query_string(Slot, Size, #state{external_secret = Key}) when Key /= <<>> ->
    UrlPath = str:join(Slot, <<$/>>),
    SizeStr = integer_to_binary(Size),
    Data = <<UrlPath/binary, " ", SizeStr/binary>>,
    HMAC = str:to_hexlist(crypto:mac(hmac, sha256, Key, Data)),
    <<"?v=", HMAC/binary>>;
make_query_string(_Slot, _Size, _State) ->
    <<>>.

-spec replace_special_chars(binary()) -> binary().
replace_special_chars(S) ->
    re:replace(S, <<"[^\\p{Xan}_.-]">>, <<$_>>,
	       [unicode, global, {return, binary}]).

-spec yield_content_type(binary()) -> binary().
yield_content_type(<<"">>) -> ?DEFAULT_CONTENT_TYPE;
yield_content_type(Type) -> Type.

-spec encode_addr(inet:ip_address() | {inet:ip_address(), inet:port_number()} |
		  undefined) -> binary().
encode_addr(IP) ->
    ejabberd_config:may_hide_data(misc:ip_to_list(IP)).

-spec iq_disco_info(binary(), binary(), binary(), [xdata()]) -> disco_info().
iq_disco_info(Host, Lang, Name, AddInfo) ->
    Form = case mod_http_upload_opt:max_size(Host) of
	       infinity ->
		   AddInfo;
	       MaxSize ->
		   lists:foldl(
		     fun(NS, Acc) ->
			     Fs = http_upload:encode(
				    [{'max-file-size', MaxSize}], NS, Lang),
			     [#xdata{type = result, fields = Fs}|Acc]
		     end, AddInfo, [?NS_HTTP_UPLOAD_0, ?NS_HTTP_UPLOAD])
	   end,
    #disco_info{identities = [#identity{category = <<"store">>,
					type = <<"file">>,
					name = translate:translate(Lang, Name)}],
		features = [?NS_HTTP_UPLOAD,
			    ?NS_HTTP_UPLOAD_0,
			    ?NS_HTTP_UPLOAD_OLD,
			    ?NS_VCARD,
			    ?NS_DISCO_INFO,
			    ?NS_DISCO_ITEMS],
		xdata = Form}.

%% HTTP request handling.

-spec parse_http_request(#request{}) -> {atom(), slot()}.
parse_http_request(#request{host = Host0, path = Path}) ->
    Host = jid:nameprep(Host0),
    PrefixLength = length(Path) - 3,
    {ProcURL, Slot} = if PrefixLength > 0 ->
			      Prefix = lists:sublist(Path, PrefixLength),
			      {str:join([Host | Prefix], $/),
			       lists:nthtail(PrefixLength, Path)};
			 true ->
			      {Host, Path}
		      end,
    {gen_mod:get_module_proc(ProcURL, ?MODULE), Slot}.

-spec store_file(binary(), http_request(),
		 integer() | undefined,
		 integer() | undefined,
		 binary(), slot(), boolean())
      -> ok | {ok, [{binary(), binary()}], binary()} | {error, term()}.
store_file(Path, Request, FileMode, DirMode, GetPrefix, Slot, Thumbnail) ->
    case do_store_file(Path, Request, FileMode, DirMode) of
	ok when Thumbnail ->
	    case read_image(Path) of
		{ok, Data, MediaInfo} ->
		    case convert(Data, MediaInfo) of
			{ok, #media_info{path = OutPath} = OutMediaInfo} ->
			    [UserDir, RandDir | _] = Slot,
			    FileName = filename:basename(OutPath),
			    URL = str:join([GetPrefix, UserDir,
					    RandDir, FileName], <<$/>>),
			    ThumbEl = thumb_el(OutMediaInfo, URL),
			    {ok,
			     [{<<"Content-Type">>,
			       <<"text/xml; charset=utf-8">>}],
			     fxml:element_to_binary(ThumbEl)};
			pass ->
			    ok
		    end;
		pass ->
		    ok
	    end;
	ok ->
	    ok;
	Err ->
	    Err
    end.

-spec do_store_file(file:filename_all(), http_request(),
		    integer() | undefined,
		    integer() | undefined)
      -> ok | {error, term()}.
do_store_file(Path, Request, FileMode, DirMode) ->
    try
	ok = filelib:ensure_dir(Path),
	ok = ejabberd_http:recv_file(Request, Path),
	if is_integer(FileMode) ->
		ok = file:change_mode(Path, FileMode);
	   FileMode == undefined ->
		ok
	end,
	if is_integer(DirMode) ->
		RandDir = filename:dirname(Path),
		UserDir = filename:dirname(RandDir),
		ok = file:change_mode(RandDir, DirMode),
		ok = file:change_mode(UserDir, DirMode);
	   DirMode == undefined ->
		ok
	end
    catch
	_:{badmatch, {error, Error}} ->
	    {error, Error}
    end.

-spec guess_content_type(binary()) -> binary().
guess_content_type(FileName) ->
    mod_http_fileserver:content_type(FileName,
				     ?DEFAULT_CONTENT_TYPE,
				     ?CONTENT_TYPES).

-spec http_response(100..599)
      -> {pos_integer(), [{binary(), binary()}], binary()}.
http_response(Code) ->
    http_response(Code, []).

-spec http_response(100..599, [{binary(), binary()}])
      -> {pos_integer(), [{binary(), binary()}], binary()}.
http_response(Code, ExtraHeaders) ->
    Message = <<(code_to_message(Code))/binary, $\n>>,
    http_response(Code, ExtraHeaders, Message).

-type http_body() :: binary() | {file, file:filename_all()}.
-spec http_response(100..599, [{binary(), binary()}], http_body())
      -> {pos_integer(), [{binary(), binary()}], http_body()}.
http_response(Code, ExtraHeaders, Body) ->
    Headers = case proplists:is_defined(<<"Content-Type">>, ExtraHeaders) of
		  true ->
		      ExtraHeaders;
		  false ->
		      [{<<"Content-Type">>, <<"text/plain">>} | ExtraHeaders]
	      end,
    {Code, Headers, Body}.

-spec code_to_message(100..599) -> binary().
code_to_message(201) -> <<"Upload successful.">>;
code_to_message(403) -> <<"Forbidden.">>;
code_to_message(404) -> <<"Not found.">>;
code_to_message(405) -> <<"Method not allowed.">>;
code_to_message(413) -> <<"File size doesn't match requested size.">>;
code_to_message(500) -> <<"Internal server error.">>;
code_to_message(_Code) -> <<"">>.

-spec format_error(atom()) -> string().
format_error(Reason) ->
    case file:format_error(Reason) of
	"unknown POSIX error" ->
	    case inet:format_error(Reason) of
		"unknown POSIX error" ->
		    atom_to_list(Reason);
		Txt ->
		    Txt
	    end;
	Txt ->
	    Txt
    end.

%%--------------------------------------------------------------------
%% Image manipulation stuff.
%%--------------------------------------------------------------------
-spec read_image(binary()) -> {ok, binary(), media_info()} | pass.
read_image(Path) ->
    case file:read_file(Path) of
	{ok, Data} ->
	    case eimp:identify(Data) of
		{ok, Info} ->
		    {ok, Data,
		     #media_info{
			path = Path,
			type = proplists:get_value(type, Info),
			width = proplists:get_value(width, Info),
			height = proplists:get_value(height, Info)}};
		{error, Why} ->
		    ?DEBUG("Cannot identify type of ~ts: ~ts",
			   [Path, eimp:format_error(Why)]),
		    pass
	    end;
	{error, Reason} ->
	    ?DEBUG("Failed to read file ~ts: ~ts",
		   [Path, format_error(Reason)]),
	    pass
    end.

-spec convert(binary(), media_info()) -> {ok, media_info()} | pass.
convert(InData, #media_info{path = Path, type = T, width = W, height = H} = Info) ->
    if W * H >= 25000000 ->
	    ?DEBUG("The image ~ts is more than 25 Mpix", [Path]),
	    pass;
       W =< 300, H =< 300 ->
	    {ok, Info};
       true ->
	    Dir = filename:dirname(Path),
	    Ext = atom_to_binary(T, latin1),
	    FileName = <<(p1_rand:get_string())/binary, $., Ext/binary>>,
	    OutPath = filename:join(Dir, FileName),
	    {W1, H1} = if W > H -> {300, round(H*300/W)};
			  H > W -> {round(W*300/H), 300};
			  true -> {300, 300}
		       end,
	    OutInfo = #media_info{path = OutPath, type = T, width = W1, height = H1},
	    case eimp:convert(InData, T, [{scale, {W1, H1}}]) of
		{ok, OutData} ->
		    case file:write_file(OutPath, OutData) of
			ok ->
			    {ok, OutInfo};
			{error, Why} ->
			    ?ERROR_MSG("Failed to write to ~ts: ~ts",
				       [OutPath, format_error(Why)]),
			    pass
		    end;
		{error, Why} ->
		    ?ERROR_MSG("Failed to convert ~ts to ~ts: ~ts",
			       [Path, OutPath, eimp:format_error(Why)]),
		    pass
	    end
    end.

-spec thumb_el(media_info(), binary()) -> xmlel().
thumb_el(#media_info{type = T, height = H, width = W}, URI) ->
    MimeType = <<"image/", (atom_to_binary(T, latin1))/binary>>,
    Thumb = #thumbnail{'media-type' = MimeType, uri = URI,
		       height = H, width = W},
    xmpp:encode(Thumb).

%%--------------------------------------------------------------------
%% Remove user.
%%--------------------------------------------------------------------
-spec remove_user(binary(), binary()) -> ok.
remove_user(User, Server) ->
    ServerHost = jid:nameprep(Server),
    DocRoot = mod_http_upload_opt:docroot(ServerHost),
    JIDinURL = mod_http_upload_opt:jid_in_url(ServerHost),
    DocRoot1 = expand_host(expand_home(DocRoot), ServerHost),
    UserStr = make_user_string(jid:make(User, Server), JIDinURL),
    UserDir = str:join([DocRoot1, UserStr], <<$/>>),
    case misc:delete_dir(UserDir) of
	ok ->
	    ?INFO_MSG("Removed HTTP upload directory of ~ts@~ts", [User, Server]);
	{error, enoent} ->
	    ?DEBUG("Found no HTTP upload directory of ~ts@~ts", [User, Server]);
	{error, Error} ->
	    ?ERROR_MSG("Cannot remove HTTP upload directory of ~ts@~ts: ~ts",
		       [User, Server, format_error(Error)])
    end,
    ok.
