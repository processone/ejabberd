%% Generated automatically
%% DO NOT EDIT: run `make options` instead

-module(mod_disco_opt).

-export([extra_domains/1]).
-export([name/1]).
-export([server_info/1]).

-spec extra_domains(gen_mod:opts() | global | binary()) -> [binary()].
extra_domains(Opts) when is_map(Opts) ->
    gen_mod:get_opt(extra_domains, Opts);
extra_domains(Host) ->
    gen_mod:get_module_opt(Host, mod_disco, extra_domains).

-spec name(gen_mod:opts() | global | binary()) -> binary().
name(Opts) when is_map(Opts) ->
    gen_mod:get_opt(name, Opts);
name(Host) ->
    gen_mod:get_module_opt(Host, mod_disco, name).

-spec server_info(gen_mod:opts() | global | binary()) -> [{'all' | [module()],binary(),[binary()]}].
server_info(Opts) when is_map(Opts) ->
    gen_mod:get_opt(server_info, Opts);
server_info(Host) ->
    gen_mod:get_module_opt(Host, mod_disco, server_info).

