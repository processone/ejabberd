%%%-------------------------------------------------------------------
%%% File    : mod_http_fileserver.erl
%%% Author  : Massimiliano Mirra <mmirra [at] process-one [dot] net>
%%% Purpose : Simple file server plugin for embedded ejabberd web server
%%% Created :
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2010   ProcessOne
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
%%% You should have received a copy of the GNU General Public License
%%% along with this program; if not, write to the Free Software
%%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
%%% 02111-1307 USA
%%%
%%%----------------------------------------------------------------------

-module(mod_http_fileserver).
-author('mmirra@process-one.net').
-author('ecestari@process-one.net').
-behaviour(gen_mod).

%% gen_mod callbacks
-export([start/2, stop/1]).

%% request_handlers callbacks
-export([process/2]).

-include("ejabberd.hrl").
-include("jlib.hrl").
-include_lib("kernel/include/file.hrl").

-include("ejabberd_http.hrl").

-ifdef(SSL40).
-define(STRING2LOWER, string).
-else.
-ifdef(SSL39).
-define(STRING2LOWER, string).
-else.
-define(STRING2LOWER, httpd_util).
-endif.
-endif.

%% Response is {DataSize, Code, [{HeaderKey, HeaderValue}], Data}
-define(HTTP_ERR_FILE_NOT_FOUND, {-1, 404, [], "Not found"}).
-define(HTTP_ERR_FORBIDDEN,      {-1, 403, [], "Forbidden"}).

-define(DEFAULT_CONTENT_TYPE, "application/octet-stream").
-define(DEFAULT_CONTENT_TYPES, [{".css",  "text/css"},
                                {".gif",  "image/gif"},
                                {".html", "text/html"},
                                {".jar",  "application/java-archive"},
                                {".jpeg", "image/jpeg"},
				                {".jpg",  "image/jpeg"},
                                {".js",   "text/javascript"},
                                {".png",  "image/png"},
                                {".txt",  "text/plain"},
                                {".xml",  "application/xml"},
                                {".xpi",  "application/x-xpinstall"},
                                {".xul",  "application/vnd.mozilla.xul+xml"}]).

-compile(export_all).


start(Host, Opts) ->
    DocRoot = gen_mod:get_opt(docroot, Opts, undefined),
    set_default_host(Host, Opts),
    conf_store(Host, docroot, DocRoot),
    check_docroot_defined(DocRoot, Host),
    DRInfo = check_docroot_exists(DocRoot),
    check_docroot_is_dir(DRInfo, DocRoot),
    check_docroot_is_readable(DRInfo, DocRoot),
    AccessLog = gen_mod:get_opt(accesslog, Opts, undefined),
    start_log(Host, AccessLog),
    DirectoryIndices = gen_mod:get_opt(directory_indices, Opts, []),
    conf_store(Host, directory_indices, DirectoryIndices),
    ServeStaticGzip = gen_mod:get_opt(serve_gzip, Opts, false),
    conf_store(Host, serve_gzip, ServeStaticGzip),
    CustomHeaders = gen_mod:get_opt(custom_headers, Opts, []),
    conf_store(Host, custom_headers, CustomHeaders),
    DefaultContentType = gen_mod:get_opt(default_content_type, Opts,
                                         ?DEFAULT_CONTENT_TYPE),
    conf_store(Host, default_content_type, DefaultContentType),
    ContentTypes = build_list_content_types(gen_mod:get_opt(content_types, Opts, []), ?DEFAULT_CONTENT_TYPES),
    conf_store(Host, content_types, ContentTypes),
    ?INFO_MSG("initialize: ~n ~p", [ContentTypes]),
    ok.

% Defines host that will answer request if hostname is not recognized.
% The first configured host will be used.
set_default_host(Host, _Opts)->
    case mochiglobal:get(http_default_host) of
        undefined ->
            ?DEBUG("Setting default host to ~p", [Host]),
            mochiglobal:put(http_default_host, Host);
        _ ->
            ok
    end.
% Returns current host if it exists or default host
get_host(Host)->    
    DCT = mochiglobal:get(default_content_type),
    case lists:keymember(Host, 1, DCT) of
        true -> Host;
        false -> mochiglobal:get(http_default_host)
    end.
    
conf_store(Host, Key, Value)->
    R = case mochiglobal:get(Key) of
        undefined -> [{Host, Value}];
        A ->
            case lists:keymember(Host, 1, A) of
                true -> lists:keyreplace(Host, 1, A,{Host, Value});
                false -> [{Host, Value}|A]
            end
    end,
    mochiglobal:put(Key, R).
    
conf_get(Host, Key) ->
    case mochiglobal:get(Key) of
        undefined-> undefined;
        A ->
            case lists:keyfind(Host, 1, A) of
                {Host, Val} -> Val;
                false -> 
                    case mochiglobal:get(http_default_host) of
                        Host -> % stop recursion here
                            undefined;
                        DefaultHost ->
                            conf_get(DefaultHost, Key)
                    end
            end
    end.


%% @spec (AdminCTs::[CT], Default::[CT]) -> [CT]
%% where CT = {Extension::string(), Value}
%%       Value = string() | undefined
%% @doc Return a unified list without duplicates.
%% Elements of AdminCTs have more priority.
%% If a CT is declared as 'undefined', then it is not included in the result.

start_log(_Host, undefined)->
    ok;
start_log(Host, FileName) ->
    mod_http_fileserver_log:start(Host, FileName).
    
build_list_content_types(AdminCTsUnsorted, DefaultCTsUnsorted) ->
    AdminCTs = lists:ukeysort(1, AdminCTsUnsorted),
    DefaultCTs = lists:ukeysort(1, DefaultCTsUnsorted),
    CTsUnfiltered = lists:ukeymerge(1, AdminCTs, DefaultCTs),
    [{Extension, Value} || {Extension, Value} <- CTsUnfiltered, Value /= undefined].

check_docroot_defined(DocRoot, Host) ->
    case DocRoot of
	undefined -> throw({undefined_docroot_option, Host});
	_ -> ok
    end.

check_docroot_exists(DocRoot) ->
    case file:read_file_info(DocRoot) of
	{error, Reason} -> throw({error_access_docroot, DocRoot, Reason});
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
    
stop(_Host) ->
    ok.


%%====================================================================
%% request_handlers callbacks
%%====================================================================

%% @spec (LocalPath, Request) -> {HTTPCode::integer(), [Header], Page::string()}
%% @doc Handle an HTTP request.
%% LocalPath is the part of the requested URL path that is "local to the module".
%% Returns the page to be sent back to the client and/or HTTP status code.

process(LocalPath, Request) ->
    ?DEBUG("Requested ~p", [LocalPath]),
    Host = get_host(Request#request.host),
    ClientHeaders = Request#request.headers,
    DirectoryIndices = conf_get(Host, directory_indices),
    CustomHeaders = conf_get(Host, custom_headers),
    DefaultContentType = conf_get(Host, default_content_type),
    ContentTypes = conf_get(Host, content_types),
    Encoding = conf_get(Host, serve_gzip),
    Static = select_encoding(ClientHeaders, Encoding),
    DocRoot = conf_get(Host, docroot),
    FileName = filename:join(filename:split(DocRoot) ++ LocalPath),
    {FileSize, Code, Headers, Contents} = case file:read_file_info(FileName) of
        {error, enoent}                    -> ?HTTP_ERR_FILE_NOT_FOUND;
        {error, eacces}                    -> ?HTTP_ERR_FORBIDDEN;
        {ok, #file_info{type = directory}} -> serve_index(FileName,
                                                          DirectoryIndices,
                                                          CustomHeaders,
                                                          DefaultContentType,
                                                          ContentTypes, Static);
        {ok, FileInfo}                     -> 
            case should_serve(FileInfo, ClientHeaders) of
                true ->serve_file(FileInfo, FileName,
                                  CustomHeaders,
                                  DefaultContentType,
                                  ContentTypes, Static);
                false ->
                    {0, 304, [], []}
            end 
    end,
    mod_http_fileserver_log:add_to_log(Host,FileSize, Code, Request),
    {Code, Headers, Contents}.

should_serve(FileInfo, Headers) ->
    lists:foldl(fun({Header, Fun}, Acc)->
        case lists:keyfind(Header, 1, Headers) of
            {_, Val} ->
                Fun(FileInfo,Val);
            _O ->
                Acc
        end
    end, true, [{'If-None-Match',fun etag/2}
                ]).
etag(FileInfo, Etag)->
    case httpd_util:create_etag(FileInfo) of
        Etag ->
            false;
        _ ->
            true
    end.
modified(FileInfo, LastModified)->
    AfterDate = calendar:datetime_to_gregorian_seconds(
                    httpd_util:convert_request_date(LastModified)),
    Mtime = calendar:datetime_to_gregorian_seconds(FileInfo#file_info.mtime),
    ?DEBUG("Modified : ~p > ~p (serving: ~p)", [Mtime, AfterDate,Mtime > AfterDate]),
    Mtime > AfterDate.

select_encoding(_Headers, false)->
    false;
select_encoding(Headers, Configuration)->
    Value =  find_header('Accept-Encoding', Headers, ""),
    Schemes = string:tokens(Value, ","),
    case lists:member("gzip",Schemes) of
        true -> Configuration;
        false -> false
    end.

%% Troll through the directory indices attempting to find one which
%% works, if none can be found, return a 404.
serve_index(_FileName, [], _CH, _DefaultContentType, _ContentTypes, _Static) ->
    ?HTTP_ERR_FILE_NOT_FOUND;
serve_index(FileName, [Index | T], CH, DefaultContentType, ContentTypes, Static) ->
    IndexFileName = filename:join([FileName] ++ [Index]),
    case file:read_file_info(IndexFileName) of
        {error, _Error}                    -> serve_index(FileName, T, CH, DefaultContentType, ContentTypes, Static);
        {ok, #file_info{type = directory}} -> serve_index(FileName, T, CH, DefaultContentType, ContentTypes, Static);
        {ok, FileInfo}                     -> serve_file(FileInfo, IndexFileName, CH, DefaultContentType, ContentTypes, Static)
    end.

%% Assume the file exists if we got this far and attempt to read it in
%% and serve it up.
     
serve_file(FileInfo, FileName, CustomHeaders, DefaultContentType, ContentTypes, false) ->
    ?DEBUG("Delivering: ~s", [FileName]),
    ContentType = content_type(FileName, DefaultContentType, ContentTypes),
    {ok, FileContents} = file:read_file(FileName),
    {FileInfo#file_info.size,
     200, [{"Server", "ejabberd"},
           {"Last-Modified", last_modified(FileInfo)},
           {"Content-Type", ContentType} | CustomHeaders],
     FileContents};
     
serve_file(FileInfo, FileName, CustomHeaders, DefaultContentType, ContentTypes, Gzip) ->
    ?DEBUG("Delivering: ~s", [FileName]),
    ContentType = content_type(FileName, DefaultContentType, ContentTypes),
    CompressedFileName = FileName ++ ".gz",
    case file:read_file_info(CompressedFileName) of
        {ok, FileInfoCompressed} -> %Found compressed
            ?INFO_MSG("Found compressed: ~s", [FileName]),
            {ok, FileContents} = file:read_file(CompressedFileName),
            {FileInfoCompressed#file_info.size,
             200, [{"Server", "ejabberd"},
                   {"Last-Modified", last_modified(FileInfoCompressed)},
                   {"Content-Type", ContentType},
                   {"Etag", httpd_util:create_etag(FileInfoCompressed)},
                   {"Content-Encoding", "gzip"} | CustomHeaders],
             FileContents};
        {error, _} ->
            {FileContents, Size} = case Gzip of
                static ->
                    {ok, Content} = file:read_file(FileName),
                    {Content, FileInfo#file_info.size};
                always ->
                    {ok, Content} = file:read_file(FileName),
                    Compressed = zlib:gzip(Content),
                    {Compressed, size(Compressed)}
                end,
            {Size,
             200, [{"Server", "ejabberd"},
                   {"Last-Modified", last_modified(FileInfo)},
                   {"Etag", httpd_util:create_etag(FileInfo)},
                   {"Content-Type", ContentType},
                   {"Content-Encoding", "gzip"} | CustomHeaders],
             FileContents}
    end.

find_header(Header, Headers, Default) ->
    case lists:keysearch(Header, 1, Headers) of
        {value, {_, Value}} -> Value;
        false               -> Default
    end.

%%----------------------------------------------------------------------
%% Utilities
%%----------------------------------------------------------------------

content_type(Filename, DefaultContentType, ContentTypes) ->
    Extension = ?STRING2LOWER:to_lower(filename:extension(Filename)),
    case lists:keysearch(Extension, 1, ContentTypes) of
        {value, {_, ContentType}} -> ContentType;
        false                     -> DefaultContentType
    end.

last_modified(FileInfo) ->
    Then = FileInfo#file_info.mtime,
    httpd_util:rfc1123_date(Then).
