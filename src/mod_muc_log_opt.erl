%% Generated automatically
%% DO NOT EDIT: run `make options` instead

-module(mod_muc_log_opt).

-export([access_log/1]).
-export([cssfile/1]).
-export([dirname/1]).
-export([dirtype/1]).
-export([file_format/1]).
-export([file_permissions/1]).
-export([outdir/1]).
-export([spam_prevention/1]).
-export([timezone/1]).
-export([top_link/1]).
-export([url/1]).

-spec access_log(gen_mod:opts() | global | binary()) -> 'muc_admin' | acl:acl().
access_log(Opts) when is_map(Opts) ->
    gen_mod:get_opt(access_log, Opts);
access_log(Host) ->
    gen_mod:get_module_opt(Host, mod_muc_log, access_log).

-spec cssfile(gen_mod:opts() | global | binary()) -> {'file',binary()} | {'url',binary()}.
cssfile(Opts) when is_map(Opts) ->
    gen_mod:get_opt(cssfile, Opts);
cssfile(Host) ->
    gen_mod:get_module_opt(Host, mod_muc_log, cssfile).

-spec dirname(gen_mod:opts() | global | binary()) -> 'room_jid' | 'room_name'.
dirname(Opts) when is_map(Opts) ->
    gen_mod:get_opt(dirname, Opts);
dirname(Host) ->
    gen_mod:get_module_opt(Host, mod_muc_log, dirname).

-spec dirtype(gen_mod:opts() | global | binary()) -> 'plain' | 'subdirs'.
dirtype(Opts) when is_map(Opts) ->
    gen_mod:get_opt(dirtype, Opts);
dirtype(Host) ->
    gen_mod:get_module_opt(Host, mod_muc_log, dirtype).

-spec file_format(gen_mod:opts() | global | binary()) -> 'html' | 'plaintext'.
file_format(Opts) when is_map(Opts) ->
    gen_mod:get_opt(file_format, Opts);
file_format(Host) ->
    gen_mod:get_module_opt(Host, mod_muc_log, file_format).

-spec file_permissions(gen_mod:opts() | global | binary()) -> {non_neg_integer(),non_neg_integer()}.
file_permissions(Opts) when is_map(Opts) ->
    gen_mod:get_opt(file_permissions, Opts);
file_permissions(Host) ->
    gen_mod:get_module_opt(Host, mod_muc_log, file_permissions).

-spec outdir(gen_mod:opts() | global | binary()) -> binary().
outdir(Opts) when is_map(Opts) ->
    gen_mod:get_opt(outdir, Opts);
outdir(Host) ->
    gen_mod:get_module_opt(Host, mod_muc_log, outdir).

-spec spam_prevention(gen_mod:opts() | global | binary()) -> boolean().
spam_prevention(Opts) when is_map(Opts) ->
    gen_mod:get_opt(spam_prevention, Opts);
spam_prevention(Host) ->
    gen_mod:get_module_opt(Host, mod_muc_log, spam_prevention).

-spec timezone(gen_mod:opts() | global | binary()) -> 'local' | 'universal'.
timezone(Opts) when is_map(Opts) ->
    gen_mod:get_opt(timezone, Opts);
timezone(Host) ->
    gen_mod:get_module_opt(Host, mod_muc_log, timezone).

-spec top_link(gen_mod:opts() | global | binary()) -> {binary(),binary()}.
top_link(Opts) when is_map(Opts) ->
    gen_mod:get_opt(top_link, Opts);
top_link(Host) ->
    gen_mod:get_module_opt(Host, mod_muc_log, top_link).

-spec url(gen_mod:opts() | global | binary()) -> 'undefined' | binary().
url(Opts) when is_map(Opts) ->
    gen_mod:get_opt(url, Opts);
url(Host) ->
    gen_mod:get_module_opt(Host, mod_muc_log, url).

