%% Generated automatically
%% DO NOT EDIT: run `make options` instead

-module(mod_vcard_ldap_opt).

-export([ldap_backups/1]).
-export([ldap_base/1]).
-export([ldap_deref_aliases/1]).
-export([ldap_encrypt/1]).
-export([ldap_filter/1]).
-export([ldap_password/1]).
-export([ldap_port/1]).
-export([ldap_rootdn/1]).
-export([ldap_search_fields/1]).
-export([ldap_search_reported/1]).
-export([ldap_servers/1]).
-export([ldap_tls_cacertfile/1]).
-export([ldap_tls_certfile/1]).
-export([ldap_tls_depth/1]).
-export([ldap_tls_verify/1]).
-export([ldap_uids/1]).
-export([ldap_vcard_map/1]).

-spec ldap_backups(gen_mod:opts() | global | binary()) -> [binary()].
ldap_backups(Opts) when is_map(Opts) ->
    gen_mod:get_opt(ldap_backups, Opts);
ldap_backups(Host) ->
    gen_mod:get_module_opt(Host, mod_vcard_ldap, ldap_backups).

-spec ldap_base(gen_mod:opts() | global | binary()) -> binary().
ldap_base(Opts) when is_map(Opts) ->
    gen_mod:get_opt(ldap_base, Opts);
ldap_base(Host) ->
    gen_mod:get_module_opt(Host, mod_vcard_ldap, ldap_base).

-spec ldap_deref_aliases(gen_mod:opts() | global | binary()) -> 'always' | 'finding' | 'never' | 'searching'.
ldap_deref_aliases(Opts) when is_map(Opts) ->
    gen_mod:get_opt(ldap_deref_aliases, Opts);
ldap_deref_aliases(Host) ->
    gen_mod:get_module_opt(Host, mod_vcard_ldap, ldap_deref_aliases).

-spec ldap_encrypt(gen_mod:opts() | global | binary()) -> 'none' | 'starttls' | 'tls'.
ldap_encrypt(Opts) when is_map(Opts) ->
    gen_mod:get_opt(ldap_encrypt, Opts);
ldap_encrypt(Host) ->
    gen_mod:get_module_opt(Host, mod_vcard_ldap, ldap_encrypt).

-spec ldap_filter(gen_mod:opts() | global | binary()) -> binary().
ldap_filter(Opts) when is_map(Opts) ->
    gen_mod:get_opt(ldap_filter, Opts);
ldap_filter(Host) ->
    gen_mod:get_module_opt(Host, mod_vcard_ldap, ldap_filter).

-spec ldap_password(gen_mod:opts() | global | binary()) -> binary().
ldap_password(Opts) when is_map(Opts) ->
    gen_mod:get_opt(ldap_password, Opts);
ldap_password(Host) ->
    gen_mod:get_module_opt(Host, mod_vcard_ldap, ldap_password).

-spec ldap_port(gen_mod:opts() | global | binary()) -> 1..1114111.
ldap_port(Opts) when is_map(Opts) ->
    gen_mod:get_opt(ldap_port, Opts);
ldap_port(Host) ->
    gen_mod:get_module_opt(Host, mod_vcard_ldap, ldap_port).

-spec ldap_rootdn(gen_mod:opts() | global | binary()) -> binary().
ldap_rootdn(Opts) when is_map(Opts) ->
    gen_mod:get_opt(ldap_rootdn, Opts);
ldap_rootdn(Host) ->
    gen_mod:get_module_opt(Host, mod_vcard_ldap, ldap_rootdn).

-spec ldap_search_fields(gen_mod:opts() | global | binary()) -> [{binary(),binary()}].
ldap_search_fields(Opts) when is_map(Opts) ->
    gen_mod:get_opt(ldap_search_fields, Opts);
ldap_search_fields(Host) ->
    gen_mod:get_module_opt(Host, mod_vcard_ldap, ldap_search_fields).

-spec ldap_search_reported(gen_mod:opts() | global | binary()) -> [{binary(),binary()}].
ldap_search_reported(Opts) when is_map(Opts) ->
    gen_mod:get_opt(ldap_search_reported, Opts);
ldap_search_reported(Host) ->
    gen_mod:get_module_opt(Host, mod_vcard_ldap, ldap_search_reported).

-spec ldap_servers(gen_mod:opts() | global | binary()) -> [binary()].
ldap_servers(Opts) when is_map(Opts) ->
    gen_mod:get_opt(ldap_servers, Opts);
ldap_servers(Host) ->
    gen_mod:get_module_opt(Host, mod_vcard_ldap, ldap_servers).

-spec ldap_tls_cacertfile(gen_mod:opts() | global | binary()) -> binary().
ldap_tls_cacertfile(Opts) when is_map(Opts) ->
    gen_mod:get_opt(ldap_tls_cacertfile, Opts);
ldap_tls_cacertfile(Host) ->
    gen_mod:get_module_opt(Host, mod_vcard_ldap, ldap_tls_cacertfile).

-spec ldap_tls_certfile(gen_mod:opts() | global | binary()) -> binary().
ldap_tls_certfile(Opts) when is_map(Opts) ->
    gen_mod:get_opt(ldap_tls_certfile, Opts);
ldap_tls_certfile(Host) ->
    gen_mod:get_module_opt(Host, mod_vcard_ldap, ldap_tls_certfile).

-spec ldap_tls_depth(gen_mod:opts() | global | binary()) -> non_neg_integer().
ldap_tls_depth(Opts) when is_map(Opts) ->
    gen_mod:get_opt(ldap_tls_depth, Opts);
ldap_tls_depth(Host) ->
    gen_mod:get_module_opt(Host, mod_vcard_ldap, ldap_tls_depth).

-spec ldap_tls_verify(gen_mod:opts() | global | binary()) -> 'false' | 'hard' | 'soft'.
ldap_tls_verify(Opts) when is_map(Opts) ->
    gen_mod:get_opt(ldap_tls_verify, Opts);
ldap_tls_verify(Host) ->
    gen_mod:get_module_opt(Host, mod_vcard_ldap, ldap_tls_verify).

-spec ldap_uids(gen_mod:opts() | global | binary()) -> [{binary(),binary()}].
ldap_uids(Opts) when is_map(Opts) ->
    gen_mod:get_opt(ldap_uids, Opts);
ldap_uids(Host) ->
    gen_mod:get_module_opt(Host, mod_vcard_ldap, ldap_uids).

-spec ldap_vcard_map(gen_mod:opts() | global | binary()) -> [{binary(),[{binary(),[binary()]}]}].
ldap_vcard_map(Opts) when is_map(Opts) ->
    gen_mod:get_opt(ldap_vcard_map, Opts);
ldap_vcard_map(Host) ->
    gen_mod:get_module_opt(Host, mod_vcard_ldap, ldap_vcard_map).

