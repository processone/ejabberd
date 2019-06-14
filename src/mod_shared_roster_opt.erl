%% Generated automatically
%% DO NOT EDIT: run `make options` instead

-module(mod_shared_roster_opt).

-export([db_type/1]).

-spec db_type(gen_mod:opts() | global | binary()) -> atom().
db_type(Opts) when is_map(Opts) ->
    gen_mod:get_opt(db_type, Opts);
db_type(Host) ->
    gen_mod:get_module_opt(Host, mod_shared_roster, db_type).

