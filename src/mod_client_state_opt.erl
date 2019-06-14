%% Generated automatically
%% DO NOT EDIT: run `make options` instead

-module(mod_client_state_opt).

-export([queue_chat_states/1]).
-export([queue_pep/1]).
-export([queue_presence/1]).

-spec queue_chat_states(gen_mod:opts() | global | binary()) -> boolean().
queue_chat_states(Opts) when is_map(Opts) ->
    gen_mod:get_opt(queue_chat_states, Opts);
queue_chat_states(Host) ->
    gen_mod:get_module_opt(Host, mod_client_state, queue_chat_states).

-spec queue_pep(gen_mod:opts() | global | binary()) -> boolean().
queue_pep(Opts) when is_map(Opts) ->
    gen_mod:get_opt(queue_pep, Opts);
queue_pep(Host) ->
    gen_mod:get_module_opt(Host, mod_client_state, queue_pep).

-spec queue_presence(gen_mod:opts() | global | binary()) -> boolean().
queue_presence(Opts) when is_map(Opts) ->
    gen_mod:get_opt(queue_presence, Opts);
queue_presence(Host) ->
    gen_mod:get_module_opt(Host, mod_client_state, queue_presence).

