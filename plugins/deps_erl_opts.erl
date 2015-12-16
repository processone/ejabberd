-module(deps_erl_opts).
-export([preprocess/2]).

preprocess(Config, Dirs) ->
    ExtraOpts = rebar_config:get(Config, deps_erl_opts, []),
    Opts = rebar_config:get(Config, erl_opts, []),
    NewOpts = lists:foldl(fun(Opt, Acc) when is_tuple(Opt) ->
                                  lists:keystore(element(1, Opt), 1, Acc, Opt);
                             (Opt, Acc) ->
                                  [Opt | lists:delete(Opt, Acc)]
                          end, Opts, ExtraOpts),
    {ok, rebar_config:set(Config, erl_opts, NewOpts), []}.
