%% Generated automatically
%% DO NOT EDIT: run `make options` instead

-module(mod_http_fileserver_opt).

-export([accesslog/1]).
-export([content_types/1]).
-export([custom_headers/1]).
-export([default_content_type/1]).
-export([directory_indices/1]).
-export([docroot/1]).
-export([must_authenticate_with/1]).

-spec accesslog(gen_mod:opts() | global | binary()) -> 'undefined' | binary().
accesslog(Opts) when is_map(Opts) ->
    gen_mod:get_opt(accesslog, Opts);
accesslog(Host) ->
    gen_mod:get_module_opt(Host, mod_http_fileserver, accesslog).

-spec content_types(gen_mod:opts() | global | binary()) -> [{binary(),binary()}].
content_types(Opts) when is_map(Opts) ->
    gen_mod:get_opt(content_types, Opts);
content_types(Host) ->
    gen_mod:get_module_opt(Host, mod_http_fileserver, content_types).

-spec custom_headers(gen_mod:opts() | global | binary()) -> [{binary(),binary()}].
custom_headers(Opts) when is_map(Opts) ->
    gen_mod:get_opt(custom_headers, Opts);
custom_headers(Host) ->
    gen_mod:get_module_opt(Host, mod_http_fileserver, custom_headers).

-spec default_content_type(gen_mod:opts() | global | binary()) -> binary().
default_content_type(Opts) when is_map(Opts) ->
    gen_mod:get_opt(default_content_type, Opts);
default_content_type(Host) ->
    gen_mod:get_module_opt(Host, mod_http_fileserver, default_content_type).

-spec directory_indices(gen_mod:opts() | global | binary()) -> [binary()].
directory_indices(Opts) when is_map(Opts) ->
    gen_mod:get_opt(directory_indices, Opts);
directory_indices(Host) ->
    gen_mod:get_module_opt(Host, mod_http_fileserver, directory_indices).

-spec docroot(gen_mod:opts() | global | binary()) -> binary().
docroot(Opts) when is_map(Opts) ->
    gen_mod:get_opt(docroot, Opts);
docroot(Host) ->
    gen_mod:get_module_opt(Host, mod_http_fileserver, docroot).

-spec must_authenticate_with(gen_mod:opts() | global | binary()) -> [{binary(),binary()}].
must_authenticate_with(Opts) when is_map(Opts) ->
    gen_mod:get_opt(must_authenticate_with, Opts);
must_authenticate_with(Host) ->
    gen_mod:get_module_opt(Host, mod_http_fileserver, must_authenticate_with).

