%%%----------------------------------------------------------------------
%%% File    : mod_http_fileserver.erl
%%% Author  : Massimiliano Mirra <mmirra [at] process-one [dot] net>
%%% Purpose : Simple file server plugin for embedded ejabberd web server
%%% Created :
%%% Id      :
%%%----------------------------------------------------------------------

-module(mod_http_fileserver).
-author('mmirra@process-one.net').
-vsn('').
-define(ejabberd_debug, true).
-compile([export_all]).

-behaviour(gen_mod).

-export([
	start/2,
	stop/1,
	process/2
	]).

-include("ejabberd.hrl").
-include("jlib.hrl").
-include("ejabberd_http.hrl").

%%%----------------------------------------------------------------------
%%% REQUEST HANDLERS
%%%----------------------------------------------------------------------

%%-----------------------------------------------------------------------
%% FUNCTION
%% 
%%   process/2
%%
%% PURPOSE
%%
%%   Handle an HTTP request.
%%
%% RETURNS
%%
%%   Page to be sent back to the client and/or HTTP status code.
%%
%% ARGUMENTS
%%
%% - LocalPath: part of the requested URL path that is "local to the
%%   module".
%%
%%-----------------------------------------------------------------------

process(LocalPath, _Request) ->
    ?DEBUG("Requested ~p", [LocalPath]),
    [{docroot, DocRoot}] = ets:lookup(mod_http_fileserver, docroot),
    FileName = filename:join(filename:split(DocRoot) ++ LocalPath),
    case file:read_file(FileName) of
        {ok, FileContents} ->
            ?DEBUG("Delivering content.", []),
            {200,
             [{"Server", "ejabberd"},
              {"Content-type", content_type(FileName)}],
             FileContents};
        {error, Error} ->
            ?DEBUG("Delivering error: ~p", [Error]),
            case Error of
                eacces -> {403, [], "Forbidden"};
                enoent -> {404, [], "Not found"};
                _Else -> {404, [], atom_to_list(Error)}
            end
    end.


%%%----------------------------------------------------------------------
%%% UTILITIES
%%%----------------------------------------------------------------------

content_type(Filename) ->
    case httpd_util:to_lower(filename:extension(Filename)) of
        ".jpg"  -> "image/jpeg";
        ".jpeg" -> "image/jpeg";
        ".gif"  -> "image/gif";
        ".png"  -> "image/png";
        ".html" -> "text/html";
        ".css"  -> "text/css";
        ".txt"  -> "text/plain";
        ".xul"  -> "application/vnd.mozilla.xul+xml";
        ".jar"  -> "application/java-archive";
        ".xpi"  -> "application/x-xpinstall";
        _Else   -> "application/octet-stream"
    end.


%%%----------------------------------------------------------------------
%%% BEHAVIOUR CALLBACKS
%%%----------------------------------------------------------------------

start(_Host, Opts) ->
    case gen_mod:get_opt(docroot, Opts, undefined) of
        undefined ->
            {'EXIT', {missing_document_root, ?MODULE}};
        DocRoot ->
            case filelib:is_dir(DocRoot) of
                true ->
                    ets:new(mod_http_fileserver, [named_table, bag]),
                    ets:insert(mod_http_fileserver, [{docroot, DocRoot}]),
                    ok;
                _Else ->
                    {'EXIT', {unaccessible_document_root, ?MODULE}}
            end    
    end.

stop(_Host) ->
	ok.
