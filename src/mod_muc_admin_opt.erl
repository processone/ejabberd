%% Generated automatically
%% DO NOT EDIT: run `make options` instead

-module(mod_muc_admin_opt).

-export([subscribe_room_many_max_users/1]).

-spec subscribe_room_many_max_users(gen_mod:opts() | global | binary()) -> integer().
subscribe_room_many_max_users(Opts) when is_map(Opts) ->
    gen_mod:get_opt(subscribe_room_many_max_users, Opts);
subscribe_room_many_max_users(Host) ->
    gen_mod:get_module_opt(Host, mod_muc_admin, subscribe_room_many_max_users).

