%% Generated automatically
%% DO NOT EDIT: run `make options` instead

-module(mod_matrix_gw_opt).

-export([host/1]).
-export([key/1]).
-export([key_name/1]).
-export([leave_timeout/1]).
-export([matrix_domain/1]).
-export([matrix_id_as_jid/1]).
-export([notary_servers/1]).


-spec host(gen_mod:opts() | global | binary()) -> binary().
host(Opts) when is_map(Opts) ->
    gen_mod:get_opt(host, Opts);
host(Host) ->
    gen_mod:get_module_opt(Host, mod_matrix_gw, host).


-spec key(gen_mod:opts() | global | binary()) -> {binary(), binary()}.
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


-spec matrix_id_as_jid(gen_mod:opts() | global | binary()) -> boolean().
matrix_id_as_jid(Opts) when is_map(Opts) ->
    gen_mod:get_opt(matrix_id_as_jid, Opts);
matrix_id_as_jid(Host) ->
    gen_mod:get_module_opt(Host, mod_matrix_gw, matrix_id_as_jid).


-spec notary_servers(gen_mod:opts() | global | binary()) -> [binary()].
notary_servers(Opts) when is_map(Opts) ->
    gen_mod:get_opt(notary_servers, Opts);
notary_servers(Host) ->
    gen_mod:get_module_opt(Host, mod_matrix_gw, notary_servers).
