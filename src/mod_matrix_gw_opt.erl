%% Generated automatically
%% DO NOT EDIT: run `make options` instead

-module(mod_matrix_gw_opt).

-export([host/1]).
-export([key/1]).
-export([key_name/1]).
-export([leave_timeout/1]).
-export([matrix_domain/1]).
-export([matrix_encrypt_dms/1]).
-export([matrix_id_as_jid/1]).
-export([media_max_bytes/1]).
-export([media_max_entries/1]).
-export([notary_servers/1]).
-export([tls_cacertfile/1]).
-export([todevice_max/1]).
-export([upload_url/1]).

-spec host(gen_mod:opts() | global | binary()) -> binary().
host(Opts) when is_map(Opts) ->
    gen_mod:get_opt(host, Opts);
host(Host) ->
    gen_mod:get_module_opt(Host, mod_matrix_gw, host).

-spec key(gen_mod:opts() | global | binary()) -> {binary(),binary()}.
key(Opts) when is_map(Opts) ->
    gen_mod:get_opt(key, Opts);
key(Host) ->
    gen_mod:get_module_opt(Host, mod_matrix_gw, key).

-spec key_name(gen_mod:opts() | global | binary()) -> binary().
key_name(Opts) when is_map(Opts) ->
    gen_mod:get_opt(key_name, Opts);
key_name(Host) ->
    gen_mod:get_module_opt(Host, mod_matrix_gw, key_name).

-spec leave_timeout(gen_mod:opts() | global | binary()) -> non_neg_integer().
leave_timeout(Opts) when is_map(Opts) ->
    gen_mod:get_opt(leave_timeout, Opts);
leave_timeout(Host) ->
    gen_mod:get_module_opt(Host, mod_matrix_gw, leave_timeout).

-spec matrix_domain(gen_mod:opts() | global | binary()) -> binary().
matrix_domain(Opts) when is_map(Opts) ->
    gen_mod:get_opt(matrix_domain, Opts);
matrix_domain(Host) ->
    gen_mod:get_module_opt(Host, mod_matrix_gw, matrix_domain).

-spec matrix_encrypt_dms(gen_mod:opts() | global | binary()) -> boolean().
matrix_encrypt_dms(Opts) when is_map(Opts) ->
    gen_mod:get_opt(matrix_encrypt_dms, Opts);
matrix_encrypt_dms(Host) ->
    gen_mod:get_module_opt(Host, mod_matrix_gw, matrix_encrypt_dms).

-spec matrix_id_as_jid(gen_mod:opts() | global | binary()) -> boolean().
matrix_id_as_jid(Opts) when is_map(Opts) ->
    gen_mod:get_opt(matrix_id_as_jid, Opts);
matrix_id_as_jid(Host) ->
    gen_mod:get_module_opt(Host, mod_matrix_gw, matrix_id_as_jid).

-spec media_max_bytes(gen_mod:opts() | global | binary()) -> pos_integer().
media_max_bytes(Opts) when is_map(Opts) ->
    gen_mod:get_opt(media_max_bytes, Opts);
media_max_bytes(Host) ->
    gen_mod:get_module_opt(Host, mod_matrix_gw, media_max_bytes).

-spec media_max_entries(gen_mod:opts() | global | binary()) -> pos_integer().
media_max_entries(Opts) when is_map(Opts) ->
    gen_mod:get_opt(media_max_entries, Opts);
media_max_entries(Host) ->
    gen_mod:get_module_opt(Host, mod_matrix_gw, media_max_entries).

-spec notary_servers(gen_mod:opts() | global | binary()) -> [binary()].
notary_servers(Opts) when is_map(Opts) ->
    gen_mod:get_opt(notary_servers, Opts);
notary_servers(Host) ->
    gen_mod:get_module_opt(Host, mod_matrix_gw, notary_servers).

-spec tls_cacertfile(gen_mod:opts() | global | binary()) -> 'undefined' | binary().
tls_cacertfile(Opts) when is_map(Opts) ->
    gen_mod:get_opt(tls_cacertfile, Opts);
tls_cacertfile(Host) ->
    gen_mod:get_module_opt(Host, mod_matrix_gw, tls_cacertfile).

-spec todevice_max(gen_mod:opts() | global | binary()) -> 'unlimited' | pos_integer().
todevice_max(Opts) when is_map(Opts) ->
    gen_mod:get_opt(todevice_max, Opts);
todevice_max(Host) ->
    gen_mod:get_module_opt(Host, mod_matrix_gw, todevice_max).

-spec upload_url(gen_mod:opts() | global | binary()) -> 'undefined' | binary().
upload_url(Opts) when is_map(Opts) ->
    gen_mod:get_opt(upload_url, Opts);
upload_url(Host) ->
    gen_mod:get_module_opt(Host, mod_matrix_gw, upload_url).

