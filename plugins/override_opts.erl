-module(override_opts).
-export([preprocess/2]).

override_opts(override, Config, Opts) ->
    lists:foldl(fun({Opt, Value}, Conf) ->
			rebar_config:set(Conf, Opt, Value)
		end, Config, Opts);
override_opts(add, Config, Opts) ->
    lists:foldl(fun({Opt, Value}, Conf) ->
			V = rebar_config:get_local(Conf, Opt, []),
			rebar_config:set(Conf, Opt, V ++ Value)
		end, Config, Opts);
override_opts(del, Config, Opts) ->
    lists:foldl(fun({Opt, Value}, Conf) ->
			V = rebar_config:get_local(Conf, Opt, []),
			rebar_config:set(Conf, Opt, V -- Value)
		end, Config, Opts).

preprocess(Config, _Dirs) ->
    Overrides = rebar_config:get_local(Config, overrides, []),
    TopOverrides = case rebar_config:get_xconf(Config, top_overrides, []) of
		  [] -> Overrides;
		  Val -> Val
	      end,
    Config2 = rebar_config:set_xconf(Config, top_overrides, TopOverrides),
    try
        Config3 = case rebar_app_utils:load_app_file(Config2, _Dirs) of
		  {ok, C, AppName, _AppData} ->
		      lists:foldl(fun({Type, AppName2, Opts}, Conf1) when
					    AppName2 == AppName ->
					  override_opts(Type, Conf1, Opts);
				     ({Type, Opts}, Conf1a) ->
					  override_opts(Type, Conf1a, Opts);
				     (_, Conf2) ->
					  Conf2
				  end, C, TopOverrides);
		  _ ->
		      Config2
	end,
	{ok, Config3, []}
    catch
        error:badarg -> {ok, Config2, []}
    end.
