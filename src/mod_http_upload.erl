%%%----------------------------------------------------------------------
%%% File    : mod_http_upload.erl
%%% Author  : Holger Weiss <holger@zedat.fu-berlin.de>
%%% Purpose : HTTP File Upload (XEP-0363)
%%% Created : 20 Aug 2015 by Holger Weiss <holger@zedat.fu-berlin.de>
%%%
%%%
%%% ejabberd, Copyright (C) 2015-2017   ProcessOne
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

-protocol({xep, 363, '0.1'}).

-define(SERVICE_REQUEST_TIMEOUT, 5000). % 5 seconds.
-define(SLOT_TIMEOUT, 18000000). % 5 hours.
-define(FORMAT(Error), file:format_error(Error)).
-define(URL_ENC(URL), binary_to_list(ejabberd_http:url_encode(URL))).
-define(ADDR_TO_STR(IP), ejabberd_config:may_hide_data(misc:ip_to_list(IP))).
-define(STR_TO_INT(Str, B), binary_to_integer(iolist_to_binary(Str), B)).
-define(DEFAULT_CONTENT_TYPE, <<"application/octet-stream">>).
-define(CONTENT_TYPES,
	[{<<".avi">>, <<"video/avi">>},
	 {<<".bmp">>, <<"image/bmp">>},
	 {<<".bz2">>, <<"application/x-bzip2">>},
	 {<<".gif">>, <<"image/gif">>},
	 {<<".gz">>, <<"application/x-gzip">>},
	 {<<".jpeg">>, <<"image/jpeg">>},
	 {<<".jpg">>, <<"image/jpeg">>},
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

-behaviour(gen_server).
-behaviour(gen_mod).

%% gen_mod/supervisor callbacks.
-export([start/2,
	 stop/1,
	 depends/2,
	 mod_opt_type/1]).

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

-include("ejabberd.hrl").
-include("ejabberd_http.hrl").
-include("xmpp.hrl").
-include("logger.hrl").

-record(state,
	{server_host            :: binary(),
	 host                   :: binary(),
	 name                   :: binary(),
	 access                 :: atom(),
	 max_size               :: pos_integer() | infinity,
	 secret_length          :: pos_integer(),
	 jid_in_url             :: sha1 | node,
	 file_mode              :: integer() | undefined,
	 dir_mode               :: integer() | undefined,
	 docroot                :: binary(),
	 put_url                :: binary(),
	 get_url                :: binary(),
	 service_url            :: binary() | undefined,
	 thumbnail              :: boolean(),
	 slots = #{}            :: map()}).

-record(media_info,
	{type   :: binary(),
	 height :: integer(),
	 width  :: integer()}).

-type state() :: #state{}.
-type slot() :: [binary(), ...].
-type media_info() :: #media_info{}.

%%--------------------------------------------------------------------
%% gen_mod/supervisor callbacks.
%%--------------------------------------------------------------------
-spec start(binary(), gen_mod:opts()) -> {ok, pid()}.

start(ServerHost, Opts) ->
    case gen_mod:get_opt(rm_on_unregister, Opts,
			 fun(B) when is_boolean(B) -> B end,
			 true) of
	true ->
	    ejabberd_hooks:add(remove_user, ServerHost, ?MODULE,
			       remove_user, 50);
	false ->
	    ok
    end,
    Proc = get_proc_name(ServerHost, ?MODULE),
    gen_mod:start_child(?MODULE, ServerHost, Opts, Proc).

-spec stop(binary()) -> ok | {error, any()}.

stop(ServerHost) ->
    case gen_mod:get_module_opt(ServerHost, ?MODULE, rm_on_unregister,
				fun(B) when is_boolean(B) -> B end,
			        true) of
	true ->
	    ejabberd_hooks:delete(remove_user, ServerHost, ?MODULE,
				  remove_user, 50);
	false ->
	    ok
    end,
    Proc = get_proc_name(ServerHost, ?MODULE),
    gen_mod:stop_child(Proc).

-spec mod_opt_type(atom()) -> fun((term()) -> term()) | [atom()].

mod_opt_type(host) ->
    fun iolist_to_binary/1;
mod_opt_type(name) ->
    fun iolist_to_binary/1;
mod_opt_type(access) ->
    fun acl:access_rules_validator/1;
mod_opt_type(max_size) ->
    fun(I) when is_integer(I), I > 0 -> I;
       (infinity) -> infinity
    end;
mod_opt_type(secret_length) ->
    fun(I) when is_integer(I), I >= 8 -> I end;
mod_opt_type(jid_in_url) ->
    fun(sha1) -> sha1;
       (node) -> node
    end;
mod_opt_type(file_mode) ->
    fun(Mode) -> ?STR_TO_INT(Mode, 8) end;
mod_opt_type(dir_mode) ->
    fun(Mode) -> ?STR_TO_INT(Mode, 8) end;
mod_opt_type(docroot) ->
    fun iolist_to_binary/1;
mod_opt_type(put_url) ->
    fun(<<"http://", _/binary>> = URL) -> URL;
       (<<"https://", _/binary>> = URL) -> URL
    end;
mod_opt_type(get_url) ->
    fun(<<"http://", _/binary>> = URL) -> URL;
       (<<"https://", _/binary>> = URL) -> URL
    end;
mod_opt_type(service_url) ->
    fun(<<"http://", _/binary>> = URL) -> URL;
       (<<"https://", _/binary>> = URL) -> URL
    end;
mod_opt_type(custom_headers) ->
    fun(Headers) ->
	    lists:map(fun({K, V}) ->
			      {iolist_to_binary(K), iolist_to_binary(V)}
		      end, Headers)
    end;
mod_opt_type(rm_on_unregister) ->
    fun(B) when is_boolean(B) -> B end;
mod_opt_type(thumbnail) ->
    fun(B) when is_boolean(B) -> B end;
mod_opt_type(_) ->
    [host, name, access, max_size, secret_length, jid_in_url, file_mode,
     dir_mode, docroot, put_url, get_url, service_url, custom_headers,
     rm_on_unregister, thumbnail].

-spec depends(binary(), gen_mod:opts()) -> [{module(), hard | soft}].

depends(_Host, _Opts) ->
    [].

%%--------------------------------------------------------------------
%% gen_server callbacks.
%%--------------------------------------------------------------------

-spec init(list()) -> {ok, state()}.

init([ServerHost, Opts]) ->
    process_flag(trap_exit, true),
    Host = gen_mod:get_opt_host(ServerHost, Opts, <<"upload.@HOST@">>),
    Name = gen_mod:get_opt(name, Opts,
			   fun iolist_to_binary/1,
			   <<"HTTP File Upload">>),
    Access = gen_mod:get_opt(access, Opts,
			     fun acl:access_rules_validator/1,
			     local),
    MaxSize = gen_mod:get_opt(max_size, Opts,
			      fun(I) when is_integer(I), I > 0 -> I;
				 (infinity) -> infinity
			      end,
			      104857600),
    SecretLength = gen_mod:get_opt(secret_length, Opts,
				   fun(I) when is_integer(I), I >= 8 -> I end,
				   40),
    JIDinURL = gen_mod:get_opt(jid_in_url, Opts,
				   fun(sha1) -> sha1;
				      (node) -> node
				   end,
				   sha1),
    DocRoot = gen_mod:get_opt(docroot, Opts,
			      fun iolist_to_binary/1,
			      <<"@HOME@/upload">>),
    FileMode = gen_mod:get_opt(file_mode, Opts,
			       fun(Mode) -> ?STR_TO_INT(Mode, 8) end),
    DirMode = gen_mod:get_opt(dir_mode, Opts,
			      fun(Mode) -> ?STR_TO_INT(Mode, 8) end),
    PutURL = gen_mod:get_opt(put_url, Opts,
			     fun(<<"http://", _/binary>> = URL) -> URL;
				(<<"https://", _/binary>> = URL) -> URL
			     end,
			     <<"http://@HOST@:5444">>),
    GetURL = gen_mod:get_opt(get_url, Opts,
			     fun(<<"http://", _/binary>> = URL) -> URL;
				(<<"https://", _/binary>> = URL) -> URL
			     end,
			     PutURL),
    ServiceURL = gen_mod:get_opt(service_url, Opts,
				 fun(<<"http://", _/binary>> = URL) -> URL;
				    (<<"https://", _/binary>> = URL) -> URL
				 end),
    Thumbnail = gen_mod:get_opt(thumbnail, Opts,
				fun(B) when is_boolean(B) -> B end,
				true),
    DocRoot1 = expand_home(str:strip(DocRoot, right, $/)),
    DocRoot2 = expand_host(DocRoot1, ServerHost),
    case ServiceURL of
	undefined ->
	    ok;
	<<"http://", _/binary>> ->
	    application:start(inets);
	<<"https://", _/binary>> ->
	    application:start(inets),
	    application:start(crypto),
	    application:start(asn1),
	    application:start(public_key),
	    application:start(ssl)
    end,
    case DirMode of
	undefined ->
	    ok;
	Mode ->
	    file:change_mode(DocRoot2, Mode)
    end,
    case Thumbnail of
	true ->
	    case string:str(os:cmd("identify"), "Magick") of
	      0 ->
		  ?ERROR_MSG("Cannot find 'identify' command, please install "
			     "ImageMagick or disable thumbnail creation", []);
	      _ ->
		  ok
	    end;
	false ->
	    ok
    end,
    ejabberd_router:register_route(Host, ServerHost),
    {ok, #state{server_host = ServerHost, host = Host, name = Name,
		access = Access, max_size = MaxSize,
		secret_length = SecretLength, jid_in_url = JIDinURL,
		file_mode = FileMode, dir_mode = DirMode,
		thumbnail = Thumbnail,
		docroot = DocRoot2,
		put_url = expand_host(str:strip(PutURL, right, $/), ServerHost),
		get_url = expand_host(str:strip(GetURL, right, $/), ServerHost),
		service_url = ServiceURL}}.

-spec handle_call(_, {pid(), _}, state())
      -> {reply, {ok, pos_integer(), binary(),
		      pos_integer() | undefined,
		      pos_integer() | undefined}, state()} |
	 {reply, {error, atom()}, state()} | {noreply, state()}.

handle_call({use_slot, Slot, Size}, _From, #state{file_mode = FileMode,
						  dir_mode = DirMode,
						  get_url = GetPrefix,
						  thumbnail = Thumbnail,
						  docroot = DocRoot} = State) ->
    case get_slot(Slot, State) of
	{ok, {Size, Timer}} ->
	    timer:cancel(Timer),
	    NewState = del_slot(Slot, State),
	    Path = str:join([DocRoot | Slot], <<$/>>),
	    {reply, {ok, Path, FileMode, DirMode, GetPrefix, Thumbnail},
	     NewState};
	{ok, {_WrongSize, _Timer}} ->
	    {reply, {error, size_mismatch}, State};
	error ->
	    {reply, {error, invalid_slot}, State}
    end;
handle_call(get_docroot, _From, #state{docroot = DocRoot} = State) ->
    {reply, {ok, DocRoot}, State};
handle_call(Request, From, State) ->
    ?ERROR_MSG("Got unexpected request from ~p: ~p", [From, Request]),
    {noreply, State}.

-spec handle_cast(_, state()) -> {noreply, state()}.

handle_cast(Request, State) ->
    ?ERROR_MSG("Got unexpected request: ~p", [Request]),
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
handle_info({slot_timed_out, Slot}, State) ->
    NewState = del_slot(Slot, State),
    {noreply, NewState};
handle_info(Info, State) ->
    ?ERROR_MSG("Got unexpected info: ~p", [Info]),
    {noreply, State}.

-spec terminate(normal | shutdown | {shutdown, _} | _, state()) -> ok.

terminate(Reason, #state{server_host = ServerHost, host = Host}) ->
    ?DEBUG("Stopping HTTP upload process for ~s: ~p", [ServerHost, Reason]),
    ejabberd_router:unregister_route(Host),
    ok.

-spec code_change({down, _} | _, state(), _) -> {ok, state()}.

code_change(_OldVsn, #state{server_host = ServerHost} = State, _Extra) ->
    ?DEBUG("Updating HTTP upload process for ~s", [ServerHost]),
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
    ?DEBUG("Rejecting ~s request from ~s for ~s: Too few path components",
	   [Method, ?ADDR_TO_STR(IP), Host]),
    http_response(Host, 404);
process(_LocalPath, #request{method = 'PUT', host = Host, ip = IP,
			     data = Data} = Request) ->
    {Proc, Slot} = parse_http_request(Request),
    case catch gen_server:call(Proc, {use_slot, Slot, byte_size(Data)}) of
	{ok, Path, FileMode, DirMode, GetPrefix, Thumbnail} ->
	    ?DEBUG("Storing file from ~s for ~s: ~s",
		   [?ADDR_TO_STR(IP), Host, Path]),
	    case store_file(Path, Data, FileMode, DirMode,
			    GetPrefix, Slot, Thumbnail) of
		ok ->
		    http_response(Host, 201);
		{ok, Headers, OutData} ->
		    http_response(Host, 201, Headers, OutData);
		{error, Error} ->
		    ?ERROR_MSG("Cannot store file ~s from ~s for ~s: ~p",
			       [Path, ?ADDR_TO_STR(IP), Host, ?FORMAT(Error)]),
		    http_response(Host, 500)
	    end;
	{error, size_mismatch} ->
	    ?INFO_MSG("Rejecting file from ~s for ~s: Unexpected size (~B)",
		      [?ADDR_TO_STR(IP), Host, byte_size(Data)]),
	    http_response(Host, 413);
	{error, invalid_slot} ->
	    ?INFO_MSG("Rejecting file from ~s for ~s: Invalid slot",
		      [?ADDR_TO_STR(IP), Host]),
	    http_response(Host, 403);
	Error ->
	    ?ERROR_MSG("Cannot handle PUT request from ~s for ~s: ~p",
		       [?ADDR_TO_STR(IP), Host, Error]),
	    http_response(Host, 500)
    end;
process(_LocalPath, #request{method = Method, host = Host, ip = IP} = Request)
    when Method == 'GET';
	 Method == 'HEAD' ->
    {Proc, [_UserDir, _RandDir, FileName] = Slot} = parse_http_request(Request),
    case catch gen_server:call(Proc, get_docroot) of
	{ok, DocRoot} ->
	    Path = str:join([DocRoot | Slot], <<$/>>),
	    case file:read_file(Path) of
		{ok, Data} ->
		    ?INFO_MSG("Serving ~s to ~s", [Path, ?ADDR_TO_STR(IP)]),
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
		    http_response(Host, 200, Headers2, Data);
		{error, eacces} ->
		    ?INFO_MSG("Cannot serve ~s to ~s: Permission denied",
			      [Path, ?ADDR_TO_STR(IP)]),
		    http_response(Host, 403);
		{error, enoent} ->
		    ?INFO_MSG("Cannot serve ~s to ~s: No such file",
			      [Path, ?ADDR_TO_STR(IP)]),
		    http_response(Host, 404);
		{error, eisdir} ->
		    ?INFO_MSG("Cannot serve ~s to ~s: Is a directory",
			      [Path, ?ADDR_TO_STR(IP)]),
		    http_response(Host, 404);
		{error, Error} ->
		    ?INFO_MSG("Cannot serve ~s to ~s: ~s",
			      [Path, ?ADDR_TO_STR(IP), ?FORMAT(Error)]),
		    http_response(Host, 500)
	    end;
	Error ->
	    ?ERROR_MSG("Cannot handle ~s request from ~s for ~s: ~p",
		       [Method, ?ADDR_TO_STR(IP), Host, Error]),
	    http_response(Host, 500)
    end;
process(_LocalPath, #request{method = 'OPTIONS', host = Host, ip = IP}) ->
    ?DEBUG("Responding to OPTIONS request from ~s for ~s",
	   [?ADDR_TO_STR(IP), Host]),
    http_response(Host, 200);
process(_LocalPath, #request{method = Method, host = Host, ip = IP}) ->
    ?DEBUG("Rejecting ~s request from ~s for ~s",
	   [Method, ?ADDR_TO_STR(IP), Host]),
    http_response(Host, 405, [{<<"Allow">>, <<"OPTIONS, HEAD, GET, PUT">>}]).

%%--------------------------------------------------------------------
%% Exported utility functions.
%%--------------------------------------------------------------------

-spec get_proc_name(binary(), atom()) -> atom().

get_proc_name(ServerHost, ModuleName) ->
    PutURL = gen_mod:get_module_opt(ServerHost, ?MODULE, put_url,
				    fun(<<"http://", _/binary>> = URL) -> URL;
				       (<<"https://", _/binary>> = URL) -> URL;
				       (_) -> <<"http://@HOST@">>
				    end,
				    <<"http://@HOST@">>),
    {ok, {_Scheme, _UserInfo, Host, _Port, Path, _Query}} =
	http_uri:parse(binary_to_list(expand_host(PutURL, ServerHost))),
    ProcPrefix = list_to_binary(string:strip(Host ++ Path, right, $/)),
    gen_mod:get_module_proc(ProcPrefix, ModuleName).

-spec expand_home(binary()) -> binary().

expand_home(Input) ->
    {ok, [[Home]]} = init:get_argument(home),
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
process_iq(#iq{type = get, lang = Lang, from = From,
	       sub_els = [#upload_request{filename = File,
					  size = Size,
					  'content-type' = CType,
					  xmlns = XMLNS}]} = IQ,
	   #state{server_host = ServerHost, access = Access} = State) ->
    case acl:match_rule(ServerHost, Access, From) of
	allow ->
	    ContentType = yield_content_type(CType),
	    case create_slot(State, From, File, Size, ContentType, Lang) of
		{ok, Slot} ->
		    {ok, Timer} = timer:send_after(?SLOT_TIMEOUT,
						   {slot_timed_out,
						    Slot}),
		    NewState = add_slot(Slot, Size, Timer, State),
		    NewSlot = mk_slot(Slot, State, XMLNS),
		    {xmpp:make_iq_result(IQ, NewSlot), NewState};
		{ok, PutURL, GetURL} ->
		    Slot = mk_slot(PutURL, GetURL, XMLNS),
		    xmpp:make_iq_result(IQ, Slot);
		{error, Error} ->
		    xmpp:make_error(IQ, Error)
	    end;
	deny ->
	    ?DEBUG("Denying HTTP upload slot request from ~s",
		   [jid:encode(From)]),
	    Txt = <<"Denied by ACL">>,
	    xmpp:make_error(IQ, xmpp:err_forbidden(Txt, Lang))
    end;
process_iq(#iq{type = T, lang = Lang} = IQ, _State) when T == get; T == set ->
    Txt = <<"No module is handling this query">>,
    xmpp:make_error(IQ, xmpp:err_service_unavailable(Txt, Lang));
process_iq(#iq{}, _State) ->
    not_request.

-spec create_slot(state(), jid(), binary(), pos_integer(), binary(), binary())
      -> {ok, slot()} | {ok, binary(), binary()} | {error, xmlel()}.

create_slot(#state{service_url = undefined, max_size = MaxSize},
	    JID, File, Size, _ContentType, Lang) when MaxSize /= infinity,
						      Size > MaxSize ->
    Text = {<<"File larger than ~w bytes">>, [MaxSize]},
    ?INFO_MSG("Rejecting file ~s from ~s (too large: ~B bytes)",
	      [File, jid:encode(JID), Size]),
    {error, xmpp:err_not_acceptable(Text, Lang)};
create_slot(#state{service_url = undefined,
		   jid_in_url = JIDinURL,
		   secret_length = SecretLength,
		   server_host = ServerHost,
		   docroot = DocRoot},
	    JID, File, Size, _ContentType, Lang) ->
    UserStr = make_user_string(JID, JIDinURL),
    UserDir = <<DocRoot/binary, $/, UserStr/binary>>,
    case ejabberd_hooks:run_fold(http_upload_slot_request, ServerHost, allow,
				 [JID, UserDir, Size, Lang]) of
	allow ->
	    RandStr = make_rand_string(SecretLength),
	    FileStr = make_file_string(File),
	    ?INFO_MSG("Got HTTP upload slot for ~s (file: ~s)",
		      [jid:encode(JID), File]),
	    {ok, [UserStr, RandStr, FileStr]};
	deny ->
	    {error, xmpp:err_service_unavailable()};
	#stanza_error{} = Error ->
	    {error, Error}
    end;
create_slot(#state{service_url = ServiceURL},
	    #jid{luser = U, lserver = S} = JID, File, Size, ContentType,
	    Lang) ->
    Options = [{body_format, binary}, {full_result, false}],
    HttpOptions = [{timeout, ?SERVICE_REQUEST_TIMEOUT}],
    SizeStr = integer_to_binary(Size),
    GetRequest = binary_to_list(ServiceURL) ++
		     "?jid=" ++ ?URL_ENC(jid:encode({U, S, <<"">>})) ++
		     "&name=" ++ ?URL_ENC(File) ++
		     "&size=" ++ ?URL_ENC(SizeStr) ++
		     "&content_type=" ++ ?URL_ENC(ContentType),
    case httpc:request(get, {GetRequest, []}, HttpOptions, Options) of
	{ok, {Code, Body}} when Code >= 200, Code =< 299 ->
	    case binary:split(Body, <<$\n>>, [global, trim]) of
		[<<"http", _/binary>> = PutURL,
		 <<"http", _/binary>> = GetURL] ->
		    ?INFO_MSG("Got HTTP upload slot for ~s (file: ~s)",
			      [jid:encode(JID), File]),
		    {ok, PutURL, GetURL};
		Lines ->
		    ?ERROR_MSG("Can't parse data received for ~s from <~s>: ~p",
			       [jid:encode(JID), ServiceURL, Lines]),
		    Txt = <<"Failed to parse HTTP response">>,
		    {error, xmpp:err_service_unavailable(Txt, Lang)}
	    end;
	{ok, {402, _Body}} ->
	    ?INFO_MSG("Got status code 402 for ~s from <~s>",
		      [jid:encode(JID), ServiceURL]),
	    {error, xmpp:err_resource_constraint()};
	{ok, {403, _Body}} ->
	    ?INFO_MSG("Got status code 403 for ~s from <~s>",
		      [jid:encode(JID), ServiceURL]),
	    {error, xmpp:err_not_allowed()};
	{ok, {413, _Body}} ->
	    ?INFO_MSG("Got status code 413 for ~s from <~s>",
		      [jid:encode(JID), ServiceURL]),
	    {error, xmpp:err_not_acceptable()};
	{ok, {Code, _Body}} ->
	    ?ERROR_MSG("Got unexpected status code for ~s from <~s>: ~B",
		       [jid:encode(JID), ServiceURL, Code]),
	    {error, xmpp:err_service_unavailable()};
	{error, Reason} ->
	    ?ERROR_MSG("Error requesting upload slot for ~s from <~s>: ~p",
		       [jid:encode(JID), ServiceURL, Reason]),
	    {error, xmpp:err_service_unavailable()}
    end.

-spec add_slot(slot(), pos_integer(), timer:tref(), state()) -> state().

add_slot(Slot, Size, Timer, #state{slots = Slots} = State) ->
    NewSlots = maps:put(Slot, {Size, Timer}, Slots),
    State#state{slots = NewSlots}.

-spec get_slot(slot(), state()) -> {ok, {pos_integer(), timer:tref()}} | error.

get_slot(Slot, #state{slots = Slots}) ->
    maps:find(Slot, Slots).

-spec del_slot(slot(), state()) -> state().

del_slot(Slot, #state{slots = Slots} = State) ->
    NewSlots = maps:remove(Slot, Slots),
    State#state{slots = NewSlots}.

-spec mk_slot(slot(), state(), binary()) -> upload_slot();
	     (binary(), binary(), binary()) -> upload_slot().

mk_slot(Slot, #state{put_url = PutPrefix, get_url = GetPrefix}, XMLNS) ->
    PutURL = str:join([PutPrefix | Slot], <<$/>>),
    GetURL = str:join([GetPrefix | Slot], <<$/>>),
    mk_slot(PutURL, GetURL, XMLNS);
mk_slot(PutURL, GetURL, XMLNS) ->
    #upload_slot{get = GetURL, put = PutURL, xmlns = XMLNS}.

-spec make_user_string(jid(), sha1 | node) -> binary().

make_user_string(#jid{luser = U, lserver = S}, sha1) ->
    str:sha(<<U/binary, $@, S/binary>>);
make_user_string(#jid{luser = U}, node) ->
    re:replace(U, <<"[^a-zA-Z0-9_.-]">>, <<$_>>, [global, {return, binary}]).

-spec make_file_string(binary()) -> binary().

make_file_string(File) ->
    re:replace(File, <<"[^a-zA-Z0-9_.-]">>, <<$_>>, [global, {return, binary}]).

-spec make_rand_string(non_neg_integer()) -> binary().

make_rand_string(Length) ->
    list_to_binary(make_rand_string([], Length)).

-spec make_rand_string(string(), non_neg_integer()) -> string().

make_rand_string(S, 0) -> S;
make_rand_string(S, N) -> make_rand_string([make_rand_char() | S], N - 1).

-spec make_rand_char() -> char().

make_rand_char() ->
    map_int_to_char(crypto:rand_uniform(0, 62)).

-spec map_int_to_char(0..61) -> char().

map_int_to_char(N) when N =<  9 -> N + 48; % Digit.
map_int_to_char(N) when N =< 35 -> N + 55; % Upper-case character.
map_int_to_char(N) when N =< 61 -> N + 61. % Lower-case character.

-spec yield_content_type(binary()) -> binary().

yield_content_type(<<"">>) -> ?DEFAULT_CONTENT_TYPE;
yield_content_type(Type) -> Type.

-spec iq_disco_info(binary(), binary(), binary(), [xdata()]) -> disco_info().

iq_disco_info(Host, Lang, Name, AddInfo) ->
    Form = case gen_mod:get_module_opt(Host, ?MODULE, max_size,
				       fun(I) when is_integer(I), I > 0 -> I;
					  (infinity) -> infinity
				       end,
				       104857600) of
	       infinity ->
		   AddInfo;
	       MaxSize ->
		   MaxSizeStr = integer_to_binary(MaxSize),
		   Fields = [#xdata_field{type = hidden,
					  var = <<"FORM_TYPE">>,
					  values = [?NS_HTTP_UPLOAD]},
			     #xdata_field{var = <<"max-file-size">>,
					  values = [MaxSizeStr]}],
		   [#xdata{type = result, fields = Fields}|AddInfo]
	   end,
    #disco_info{identities = [#identity{category = <<"store">>,
					type = <<"file">>,
					name = translate:translate(Lang, Name)}],
		features = [?NS_HTTP_UPLOAD, ?NS_HTTP_UPLOAD_OLD,
			    ?NS_DISCO_INFO, ?NS_DISCO_ITEMS],
		xdata = Form}.

%% HTTP request handling.

-spec parse_http_request(#request{}) -> {atom(), slot()}.

parse_http_request(#request{host = Host, path = Path}) ->
    PrefixLength = length(Path) - 3,
    {ProcURL, Slot} = if PrefixLength > 0 ->
			      Prefix = lists:sublist(Path, PrefixLength),
			      {str:join([Host | Prefix], $/),
			       lists:nthtail(PrefixLength, Path)};
			 true ->
			      {Host, Path}
		      end,
    {gen_mod:get_module_proc(ProcURL, ?MODULE), Slot}.

-spec store_file(binary(), binary(),
		 integer() | undefined,
		 integer() | undefined,
		 binary(), slot(), boolean())
      -> ok | {ok, [{binary(), binary()}], binary()} | {error, term()}.

store_file(Path, Data, FileMode, DirMode, GetPrefix, Slot, Thumbnail) ->
    case do_store_file(Path, Data, FileMode, DirMode) of
	ok when Thumbnail ->
	    case identify(Path) of
		{ok, MediaInfo} ->
		    case convert(Path, MediaInfo) of
			{ok, OutPath} ->
			    [UserDir, RandDir | _] = Slot,
			    FileName = filename:basename(OutPath),
			    URL = str:join([GetPrefix, UserDir,
					    RandDir, FileName], <<$/>>),
			    ThumbEl = thumb_el(OutPath, URL),
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

-spec do_store_file(file:filename_all(), binary(),
		    integer() | undefined,
		    integer() | undefined)
      -> ok | {error, term()}.

do_store_file(Path, Data, FileMode, DirMode) ->
    try
	ok = filelib:ensure_dir(Path),
	{ok, Io} = file:open(Path, [write, exclusive, raw]),
	Ok = file:write(Io, Data),
	ok = file:close(Io),
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
	end,
	ok = Ok % Raise an exception if file:write/2 failed.
    catch
	_:{badmatch, {error, Error}} ->
	    {error, Error};
	_:Error ->
	    {error, Error}
    end.

-spec guess_content_type(binary()) -> binary().

guess_content_type(FileName) ->
    mod_http_fileserver:content_type(FileName,
				     ?DEFAULT_CONTENT_TYPE,
				     ?CONTENT_TYPES).

-spec http_response(binary(), 100..599)
      -> {pos_integer(), [{binary(), binary()}], binary()}.

http_response(Host, Code) ->
    http_response(Host, Code, []).

-spec http_response(binary(), 100..599, [{binary(), binary()}])
      -> {pos_integer(), [{binary(), binary()}], binary()}.

http_response(Host, Code, ExtraHeaders) ->
    Message = <<(code_to_message(Code))/binary, $\n>>,
    http_response(Host, Code, ExtraHeaders, Message).

-spec http_response(binary(), 100..599, [{binary(), binary()}], binary())
      -> {pos_integer(), [{binary(), binary()}], binary()}.

http_response(Host, Code, ExtraHeaders, Body) ->
    CustomHeaders =
	gen_mod:get_module_opt(Host, ?MODULE, custom_headers,
			       fun(Headers) ->
				       lists:map(fun({K, V}) ->
							 {iolist_to_binary(K),
							  iolist_to_binary(V)}
						 end, Headers)
			       end,
			       []),
    Headers = case proplists:is_defined(<<"Content-Type">>, ExtraHeaders) of
		  true ->
		      ExtraHeaders;
		  false ->
		      [{<<"Content-Type">>, <<"text/plain">>} | ExtraHeaders]
	      end ++ CustomHeaders,
    {Code, Headers, Body}.

-spec code_to_message(100..599) -> binary().

code_to_message(201) -> <<"Upload successful.">>;
code_to_message(403) -> <<"Forbidden.">>;
code_to_message(404) -> <<"Not found.">>;
code_to_message(405) -> <<"Method not allowed.">>;
code_to_message(413) -> <<"File size doesn't match requested size.">>;
code_to_message(500) -> <<"Internal server error.">>;
code_to_message(_Code) -> <<"">>.

%%--------------------------------------------------------------------
%% Image manipulation stuff.
%%--------------------------------------------------------------------

-spec identify(binary()) -> {ok, media_info()} | pass.

identify(Path) ->
    Cmd = io_lib:format("identify -format 'ok %m %h %w' ~s", [Path]),
    Res = string:strip(os:cmd(Cmd), right, $\n),
    case string:tokens(Res, " ") of
	["ok", T, H, W] ->
	    {ok, #media_info{type = list_to_binary(string:to_lower(T)),
			     height = list_to_integer(H),
			     width = list_to_integer(W)}};
	_ ->
	    ?DEBUG("Cannot identify type of ~s: ~s", [Path, Res]),
	    pass
    end.

-spec convert(binary(), media_info()) -> {ok, binary()} | pass.

convert(Path, #media_info{type = T, width = W, height = H}) ->
    if W * H >= 25000000 ->
	    ?DEBUG("The image ~s is more than 25 Mpix", [Path]),
	    pass;
       W =< 300, H =< 300 ->
	    {ok, Path};
       T == <<"gif">>; T == <<"jpeg">>; T == <<"png">>; T == <<"webp">> ->
	    Dir = filename:dirname(Path),
	    FileName = <<(randoms:get_string())/binary, $., T/binary>>,
	    OutPath = filename:join(Dir, FileName),
	    Cmd = io_lib:format("convert -resize 300 ~s ~s", [Path, OutPath]),
	    case os:cmd(Cmd) of
		"" ->
		    {ok, OutPath};
		Err ->
		    ?ERROR_MSG("Failed to convert ~s to ~s: ~s",
			       [Path, OutPath, string:strip(Err, right, $\n)]),
		    pass
	    end;
       true ->
	    ?DEBUG("Won't call 'convert' for unknown type ~s", [T]),
	    pass
    end.

-spec thumb_el(binary(), binary()) -> xmlel().

thumb_el(Path, URI) ->
    ContentType = guess_content_type(Path),
    xmpp:encode(
      case identify(Path) of
	  {ok, #media_info{height = H, width = W}} ->
	      #thumbnail{'media-type' = ContentType, uri = URI,
			 height = H, width = W};
	  pass ->
	      #thumbnail{uri = URI, 'media-type' = ContentType}
      end).

%%--------------------------------------------------------------------
%% Remove user.
%%--------------------------------------------------------------------

-spec remove_user(binary(), binary()) -> ok.

remove_user(User, Server) ->
    ServerHost = jid:nameprep(Server),
    DocRoot = gen_mod:get_module_opt(ServerHost, ?MODULE, docroot,
				     fun iolist_to_binary/1,
				     <<"@HOME@/upload">>),
    JIDinURL = gen_mod:get_module_opt(ServerHost, ?MODULE, jid_in_url,
				      fun(sha1) -> sha1;
					 (node) -> node
				      end,
				      sha1),
    DocRoot1 = expand_host(expand_home(DocRoot), ServerHost),
    UserStr = make_user_string(jid:make(User, Server), JIDinURL),
    UserDir = str:join([DocRoot1, UserStr], <<$/>>),
    case del_tree(UserDir) of
	ok ->
	    ?INFO_MSG("Removed HTTP upload directory of ~s@~s", [User, Server]);
	{error, enoent} ->
	    ?DEBUG("Found no HTTP upload directory of ~s@~s", [User, Server]);
	{error, Error} ->
	    ?ERROR_MSG("Cannot remove HTTP upload directory of ~s@~s: ~p",
		       [User, Server, ?FORMAT(Error)])
    end,
    ok.

-spec del_tree(file:filename_all()) -> ok | {error, term()}.

del_tree(Dir) when is_binary(Dir) ->
    del_tree(binary_to_list(Dir));
del_tree(Dir) ->
    try
	{ok, Entries} = file:list_dir(Dir),
	lists:foreach(fun(Path) ->
			      case filelib:is_dir(Path) of
				  true ->
				      ok = del_tree(Path);
				  false ->
				      ok = file:delete(Path)
			      end
		      end, [Dir ++ "/" ++ Entry || Entry <- Entries]),
	ok = file:del_dir(Dir)
    catch
	_:{badmatch, {error, Error}} ->
	    {error, Error};
	_:Error ->
	    {error, Error}
    end.
