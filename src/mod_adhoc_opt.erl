%% Generated automatically
%% DO NOT EDIT: run `make options` instead

-module(mod_adhoc_opt).

-export([report_commands_node/1]).

-spec report_commands_node(gen_mod:opts() | global | binary()) -> boolean().
report_commands_node(Opts) when is_map(Opts) ->
    gen_mod:get_opt(report_commands_node, Opts);
report_commands_node(Host) ->
    gen_mod:get_module_opt(Host, mod_adhoc, report_commands_node).

