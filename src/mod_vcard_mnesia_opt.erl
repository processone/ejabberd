%% Generated automatically
%% DO NOT EDIT: run `make options` instead

-module(mod_vcard_mnesia_opt).

-export([search_all_hosts/1]).

-spec search_all_hosts(gen_mod:opts() | global | binary()) -> boolean().
search_all_hosts(Opts) when is_map(Opts) ->
    gen_mod:get_opt(search_all_hosts, Opts);
search_all_hosts(Host) ->
    gen_mod:get_module_opt(Host, mod_vcard_mnesia, search_all_hosts).

