%% Generated automatically
%% DO NOT EDIT: run `make options` instead

-module(mod_shared_roster_ldap_opt).

-export([cache_life_time/1]).
-export([cache_missed/1]).
-export([cache_size/1]).
-export([ldap_auth_check/1]).
-export([ldap_backups/1]).
-export([ldap_base/1]).
-export([ldap_deref_aliases/1]).
-export([ldap_encrypt/1]).
-export([ldap_filter/1]).
-export([ldap_gfilter/1]).
-export([ldap_groupattr/1]).
-export([ldap_groupdesc/1]).
-export([ldap_memberattr/1]).
-export([ldap_memberattr_format/1]).
-export([ldap_memberattr_format_re/1]).
-export([ldap_password/1]).
-export([ldap_port/1]).
-export([ldap_rfilter/1]).
-export([ldap_rootdn/1]).
-export([ldap_servers/1]).
-export([ldap_tls_cacertfile/1]).
-export([ldap_tls_certfile/1]).
-export([ldap_tls_depth/1]).
-export([ldap_tls_verify/1]).
-export([ldap_ufilter/1]).
-export([ldap_uids/1]).
-export([ldap_userdesc/1]).
-export([ldap_useruid/1]).
-export([use_cache/1]).

-spec cache_life_time(gen_mod:opts() | global | binary()) -> 'infinity' | pos_integer().
cache_life_time(Opts) when is_map(Opts) ->
    gen_mod:get_opt(cache_life_time, Opts);
cache_life_time(Host) ->
    gen_mod:get_module_opt(Host, mod_shared_roster_ldap, cache_life_time).

-spec cache_missed(gen_mod:opts() | global | binary()) -> boolean().
cache_missed(Opts) when is_map(Opts) ->
    gen_mod:get_opt(cache_missed, Opts);
cache_missed(Host) ->
    gen_mod:get_module_opt(Host, mod_shared_roster_ldap, cache_missed).

-spec cache_size(gen_mod:opts() | global | binary()) -> 'infinity' | pos_integer().
cache_size(Opts) when is_map(Opts) ->
    gen_mod:get_opt(cache_size, Opts);
cache_size(Host) ->
    gen_mod:get_module_opt(Host, mod_shared_roster_ldap, cache_size).

-spec ldap_auth_check(gen_mod:opts() | global | binary()) -> boolean().
ldap_auth_check(Opts) when is_map(Opts) ->
    gen_mod:get_opt(ldap_auth_check, Opts);
ldap_auth_check(Host) ->
    gen_mod:get_module_opt(Host, mod_shared_roster_ldap, ldap_auth_check).

-spec ldap_backups(gen_mod:opts() | global | binary()) -> [binary()].
ldap_backups(Opts) when is_map(Opts) ->
    gen_mod:get_opt(ldap_backups, Opts);
ldap_backups(Host) ->
    gen_mod:get_module_opt(Host, mod_shared_roster_ldap, ldap_backups).

-spec ldap_base(gen_mod:opts() | global | binary()) -> binary().
ldap_base(Opts) when is_map(Opts) ->
    gen_mod:get_opt(ldap_base, Opts);
ldap_base(Host) ->
    gen_mod:get_module_opt(Host, mod_shared_roster_ldap, ldap_base).

-spec ldap_deref_aliases(gen_mod:opts() | global | binary()) -> 'always' | 'finding' | 'never' | 'searching'.
ldap_deref_aliases(Opts) when is_map(Opts) ->
    gen_mod:get_opt(ldap_deref_aliases, Opts);
ldap_deref_aliases(Host) ->
    gen_mod:get_module_opt(Host, mod_shared_roster_ldap, ldap_deref_aliases).

-spec ldap_encrypt(gen_mod:opts() | global | binary()) -> 'none' | 'starttls' | 'tls'.
ldap_encrypt(Opts) when is_map(Opts) ->
    gen_mod:get_opt(ldap_encrypt, Opts);
ldap_encrypt(Host) ->
    gen_mod:get_module_opt(Host, mod_shared_roster_ldap, ldap_encrypt).

-spec ldap_filter(gen_mod:opts() | global | binary()) -> binary().
ldap_filter(Opts) when is_map(Opts) ->
    gen_mod:get_opt(ldap_filter, Opts);
ldap_filter(Host) ->
    gen_mod:get_module_opt(Host, mod_shared_roster_ldap, ldap_filter).

-spec ldap_gfilter(gen_mod:opts() | global | binary()) -> binary().
ldap_gfilter(Opts) when is_map(Opts) ->
    gen_mod:get_opt(ldap_gfilter, Opts);
ldap_gfilter(Host) ->
    gen_mod:get_module_opt(Host, mod_shared_roster_ldap, ldap_gfilter).

-spec ldap_groupattr(gen_mod:opts() | global | binary()) -> binary().
ldap_groupattr(Opts) when is_map(Opts) ->
    gen_mod:get_opt(ldap_groupattr, Opts);
ldap_groupattr(Host) ->
    gen_mod:get_module_opt(Host, mod_shared_roster_ldap, ldap_groupattr).

-spec ldap_groupdesc(gen_mod:opts() | global | binary()) -> 'undefined' | binary().
ldap_groupdesc(Opts) when is_map(Opts) ->
    gen_mod:get_opt(ldap_groupdesc, Opts);
ldap_groupdesc(Host) ->
    gen_mod:get_module_opt(Host, mod_shared_roster_ldap, ldap_groupdesc).

-spec ldap_memberattr(gen_mod:opts() | global | binary()) -> binary().
ldap_memberattr(Opts) when is_map(Opts) ->
    gen_mod:get_opt(ldap_memberattr, Opts);
ldap_memberattr(Host) ->
    gen_mod:get_module_opt(Host, mod_shared_roster_ldap, ldap_memberattr).

-spec ldap_memberattr_format(gen_mod:opts() | global | binary()) -> binary().
ldap_memberattr_format(Opts) when is_map(Opts) ->
    gen_mod:get_opt(ldap_memberattr_format, Opts);
ldap_memberattr_format(Host) ->
    gen_mod:get_module_opt(Host, mod_shared_roster_ldap, ldap_memberattr_format).

-spec ldap_memberattr_format_re(gen_mod:opts() | global | binary()) -> 'undefined' | re:mp().
ldap_memberattr_format_re(Opts) when is_map(Opts) ->
    gen_mod:get_opt(ldap_memberattr_format_re, Opts);
ldap_memberattr_format_re(Host) ->
    gen_mod:get_module_opt(Host, mod_shared_roster_ldap, ldap_memberattr_format_re).

-spec ldap_password(gen_mod:opts() | global | binary()) -> binary().
ldap_password(Opts) when is_map(Opts) ->
    gen_mod:get_opt(ldap_password, Opts);
ldap_password(Host) ->
    gen_mod:get_module_opt(Host, mod_shared_roster_ldap, ldap_password).

-spec ldap_port(gen_mod:opts() | global | binary()) -> 1..1114111.
ldap_port(Opts) when is_map(Opts) ->
    gen_mod:get_opt(ldap_port, Opts);
ldap_port(Host) ->
    gen_mod:get_module_opt(Host, mod_shared_roster_ldap, ldap_port).

-spec ldap_rfilter(gen_mod:opts() | global | binary()) -> binary().
ldap_rfilter(Opts) when is_map(Opts) ->
    gen_mod:get_opt(ldap_rfilter, Opts);
ldap_rfilter(Host) ->
    gen_mod:get_module_opt(Host, mod_shared_roster_ldap, ldap_rfilter).

-spec ldap_rootdn(gen_mod:opts() | global | binary()) -> binary().
ldap_rootdn(Opts) when is_map(Opts) ->
    gen_mod:get_opt(ldap_rootdn, Opts);
ldap_rootdn(Host) ->
    gen_mod:get_module_opt(Host, mod_shared_roster_ldap, ldap_rootdn).

-spec ldap_servers(gen_mod:opts() | global | binary()) -> [binary()].
ldap_servers(Opts) when is_map(Opts) ->
    gen_mod:get_opt(ldap_servers, Opts);
ldap_servers(Host) ->
    gen_mod:get_module_opt(Host, mod_shared_roster_ldap, ldap_servers).

-spec ldap_tls_cacertfile(gen_mod:opts() | global | binary()) -> binary().
ldap_tls_cacertfile(Opts) when is_map(Opts) ->
    gen_mod:get_opt(ldap_tls_cacertfile, Opts);
ldap_tls_cacertfile(Host) ->
    gen_mod:get_module_opt(Host, mod_shared_roster_ldap, ldap_tls_cacertfile).

-spec ldap_tls_certfile(gen_mod:opts() | global | binary()) -> binary().
ldap_tls_certfile(Opts) when is_map(Opts) ->
    gen_mod:get_opt(ldap_tls_certfile, Opts);
ldap_tls_certfile(Host) ->
    gen_mod:get_module_opt(Host, mod_shared_roster_ldap, ldap_tls_certfile).

-spec ldap_tls_depth(gen_mod:opts() | global | binary()) -> non_neg_integer().
ldap_tls_depth(Opts) when is_map(Opts) ->
    gen_mod:get_opt(ldap_tls_depth, Opts);
ldap_tls_depth(Host) ->
    gen_mod:get_module_opt(Host, mod_shared_roster_ldap, ldap_tls_depth).

-spec ldap_tls_verify(gen_mod:opts() | global | binary()) -> 'false' | 'hard' | 'soft'.
ldap_tls_verify(Opts) when is_map(Opts) ->
    gen_mod:get_opt(ldap_tls_verify, Opts);
ldap_tls_verify(Host) ->
    gen_mod:get_module_opt(Host, mod_shared_roster_ldap, ldap_tls_verify).

-spec ldap_ufilter(gen_mod:opts() | global | binary()) -> binary().
ldap_ufilter(Opts) when is_map(Opts) ->
    gen_mod:get_opt(ldap_ufilter, Opts);
ldap_ufilter(Host) ->
    gen_mod:get_module_opt(Host, mod_shared_roster_ldap, ldap_ufilter).

-spec ldap_uids(gen_mod:opts() | global | binary()) -> [{binary(),binary()}].
ldap_uids(Opts) when is_map(Opts) ->
    gen_mod:get_opt(ldap_uids, Opts);
ldap_uids(Host) ->
    gen_mod:get_module_opt(Host, mod_shared_roster_ldap, ldap_uids).

-spec ldap_userdesc(gen_mod:opts() | global | binary()) -> binary().
ldap_userdesc(Opts) when is_map(Opts) ->
    gen_mod:get_opt(ldap_userdesc, Opts);
ldap_userdesc(Host) ->
    gen_mod:get_module_opt(Host, mod_shared_roster_ldap, ldap_userdesc).

-spec ldap_useruid(gen_mod:opts() | global | binary()) -> binary().
ldap_useruid(Opts) when is_map(Opts) ->
    gen_mod:get_opt(ldap_useruid, Opts);
ldap_useruid(Host) ->
    gen_mod:get_module_opt(Host, mod_shared_roster_ldap, ldap_useruid).

-spec use_cache(gen_mod:opts() | global | binary()) -> boolean().
use_cache(Opts) when is_map(Opts) ->
    gen_mod:get_opt(use_cache, Opts);
use_cache(Host) ->
    gen_mod:get_module_opt(Host, mod_shared_roster_ldap, use_cache).

