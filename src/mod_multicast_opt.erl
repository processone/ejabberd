%% Generated automatically
%% DO NOT EDIT: run `make options` instead

-module(mod_multicast_opt).

-export([access/1]).
-export([host/1]).
-export([hosts/1]).
-export([limits/1]).
-export([name/1]).
-export([vcard/1]).

-spec access(gen_mod:opts() | global | binary()) -> 'all' | acl:acl().
access(Opts) when is_map(Opts) ->
    gen_mod:get_opt(access, Opts);
access(Host) ->
    gen_mod:get_module_opt(Host, mod_multicast, access).

-spec host(gen_mod:opts() | global | binary()) -> binary().
host(Opts) when is_map(Opts) ->
    gen_mod:get_opt(host, Opts);
host(Host) ->
    gen_mod:get_module_opt(Host, mod_multicast, host).

-spec hosts(gen_mod:opts() | global | binary()) -> [binary()].
hosts(Opts) when is_map(Opts) ->
    gen_mod:get_opt(hosts, Opts);
hosts(Host) ->
    gen_mod:get_module_opt(Host, mod_multicast, hosts).

-spec limits(gen_mod:opts() | global | binary()) -> [{'local',[{'message','infinite' | non_neg_integer()} | {'presence','infinite' | non_neg_integer()}]} | {'remote',[{'message','infinite' | non_neg_integer()} | {'presence','infinite' | non_neg_integer()}]}].
limits(Opts) when is_map(Opts) ->
    gen_mod:get_opt(limits, Opts);
limits(Host) ->
    gen_mod:get_module_opt(Host, mod_multicast, limits).

-spec name(gen_mod:opts() | global | binary()) -> binary().
name(Opts) when is_map(Opts) ->
    gen_mod:get_opt(name, Opts);
name(Host) ->
    gen_mod:get_module_opt(Host, mod_multicast, name).

-spec vcard(gen_mod:opts() | global | binary()) -> 'undefined' | tuple().
vcard(Opts) when is_map(Opts) ->
    gen_mod:get_opt(vcard, Opts);
vcard(Host) ->
    gen_mod:get_module_opt(Host, mod_multicast, vcard).

