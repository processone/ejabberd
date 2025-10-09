defmodule Ejabberd.MixProject do
  use Mix.Project

  def project do
    [app: :ejabberd,
     source_url: "https://github.com/processone/ejabberd",
     version: version(),
     description: description(),
     elixir: elixir_required_version(),
     elixirc_paths: ["lib"],
     compile_path: ".",
     compilers: [:asn1, :yecc] ++ Mix.compilers(),
     erlc_options: erlc_options(),
     erlc_paths: ["asn1", "src"],
     # Elixir tests are starting the part of ejabberd they need
     aliases: [test: "test --no-start"],
     start_permanent: Mix.env() == :prod,
     language: :erlang,
     dialyzer: dialyzer(),
     releases: releases(),
     package: package(),
     docs: docs(),
     deps: deps()]
  end

  def version do
    case config(:vsn) do
      :false -> "0.0.0" # ./configure wasn't run: vars.config not created
      ~c"0.0" -> "0.0.0" # the full git repository wasn't downloaded
      ~c"latest.0" -> "0.0.0" # running 'docker-ejabberd/ecs/build.sh latest'
      [_, _, ?., _, _] = x ->
        head = String.replace(:erlang.list_to_binary(x), ~r/\.0+([0-9])/, ".\\1")
        "#{head}.0"
      vsn -> String.replace(:erlang.list_to_binary(vsn), ~r/\.0+([0-9])/, ".\\1")
    end
  end

  def description do
    """
    Robust, Ubiquitous and Massively Scalable Messaging Platform (XMPP, MQTT, SIP Server)
    """
  end

  def application do
    [mod: {:ejabberd_app, []},
     extra_applications: [:inets, :kernel, :sasl, :ssl, :stdlib, :syntax_tools,
                          :logger, :mix]
     ++ cond_apps(),
     included_applications: [:mnesia, :os_mon,
                             :cache_tab, :eimp, :mqtree, :p1_acme,
                             :p1_oauth2, :pkix]
     ++ cond_included_apps()]
  end

  defp dialyzer do
    [
      plt_add_apps: [
        :mnesia, :odbc, :os_mon, :stdlib,
        :eredis, :luerl,
        :cache_tab, :eimp, :epam, :esip, :ezlib, :mqtree,
        :p1_acme, :p1_mysql, :p1_oauth2, :p1_pgsql, :pkix,
        :sqlite3, :stun, :xmpp],
    ]
  end

  defp if_version_above(ver, okResult) do
    if :erlang.system_info(:otp_release) > ver do
      okResult
    else
      []
    end
  end

  defp if_version_below(ver, okResult) do
    if :erlang.system_info(:otp_release) < ver do
      okResult
    else
      []
    end
  end

  defp erlc_options do
    # Use our own includes + includes from all dependencies
    includes = ["include", deps_include()]
    result = [{:d, :ELIXIR_ENABLED}] ++
             cond_options() ++
             Enum.map(includes, fn (path) -> {:i, path} end) ++
             if_version_above(~c"20", [{:d, :HAVE_URI_STRING}]) ++
             if_version_above(~c"20", [{:d, :HAVE_ERL_ERROR}]) ++
             if_version_below(~c"21", [{:d, :USE_OLD_HTTP_URI}]) ++
             if_version_below(~c"22", [{:d, :LAGER}]) ++
             if_version_below(~c"21", [{:d, :NO_CUSTOMIZE_HOSTNAME_CHECK}]) ++
             if_version_below(~c"23", [{:d, :USE_OLD_CRYPTO_HMAC}]) ++
             if_version_below(~c"23", [{:d, :USE_OLD_PG2}]) ++
             if_version_below(~c"24", [{:d, :COMPILER_REPORTS_ONLY_LINES}]) ++
             if_version_below(~c"24", [{:d, :SYSTOOLS_APP_DEF_WITHOUT_OPTIONAL}]) ++
             if_version_below(~c"24", [{:d, :OTP_BELOW_24}]) ++
             if_version_below(~c"25", [{:d, :OTP_BELOW_25}]) ++
             if_version_below(~c"26", [{:d, :OTP_BELOW_26}]) ++
             if_version_below(~c"27", [{:d, :OTP_BELOW_27}]) ++
             if_version_below(~c"27", [{:feature, :maybe_expr, :enable}]) ++
             if_version_below(~c"28", [{:d, :OTP_BELOW_28}])
    defines = for {:d, value} <- result, do: {:d, value}
    result ++ [{:d, :ALL_DEFS, defines}]
  end

  defp cond_options do
    for {:true, option} <- [{config(:sip), {:d, :SIP}},
                            {config(:stun), {:d, :STUN}},
                            {config(:debug), :debug_info},
                            {not config(:debug), {:debug_info, false}},
                            {config(:roster_gateway_workaround), {:d, :ROSTER_GATEWAY_WORKAROUND}},
                            {config(:multihost_sql_schema), {:d, :MULTIHOST_SQL_SCHEMA}}
                           ], do:
    option
  end

  defp deps do
    [{:cache_tab, "~> 1.0"},
     {:dialyxir, "~> 1.2", only: [:test], runtime: false},
     {:eimp, "~> 1.0"},
     {:ex_doc, "~> 0.31", only: [:edoc], runtime: false},
     {:fast_tls, "~> 1.1.24"},
     {:fast_xml, "~> 1.1.56"},
     {:fast_yaml, "~> 1.0"},
     {:idna, "~> 6.0"},
     {:mqtree, "~> 1.0"},
     {:p1_acme, ">= 1.0.28"},
     {:p1_oauth2, "~> 0.6"},
     {:p1_utils, "~> 1.0"},
     {:pkix, "~> 1.0"},
     {:stringprep, ">= 1.0.26"},
     {:xmpp, git: "https://github.com/processone/xmpp", ref: "e9d901ea84fd3910ad32b715853397eb1155b41c", override: true},
     {:yconf, git: "https://github.com/processone/yconf", ref: "95692795a8a8d950ba560e5b07e6b80660557259", override: true}]
    ++ cond_deps()
  end

  defp deps_include() do
    if Mix.Project.umbrella?() do
      "../../deps"
    else
      case Mix.Project.deps_paths()[:ejabberd] do
        nil -> "deps"
        _ -> ".."
      end
    end
  end

  defp cond_deps do
    for {:true, dep} <- [{config(:pam), {:epam, "~> 1.0"}},
                         {Mix.env() == :translations,
                          {:ejabberd_po, git: "https://github.com/processone/ejabberd-po.git"}},
                         {Mix.env() == :dev,
                          {:exsync, "~> 0.2", optional: true, runtime: false}},
                         {config(:redis), {:eredis, "~> 1.7.1"}},
                         {config(:sip), {:esip, "~> 1.0"}},
                         {config(:zlib), {:ezlib, "~> 1.0"}},
                         {if_version_above(~c"23", true), {:jose, "~> 1.11.10"}},
                         {if_version_below(~c"24", true), {:jose, "1.11.1", override: true}},
                         {if_version_below(~c"27", true), {:jiffy, "~> 1.1.1"}},
                         {if_version_below(~c"22", true), {:lager, "~> 3.9.1"}},
                         {config(:lua), {:luerl, "~> 1.2.0"}},
                         {config(:mysql), {:p1_mysql, ">= 1.0.24"}},
                         {config(:pgsql), {:p1_pgsql, ">= 1.1.32"}},
                         {config(:sqlite), {:sqlite3, "~> 1.1"}},
                         {config(:stun), {:stun, "~> 1.0"}}], do:
      dep
  end

  defp cond_apps do
    for {:true, app} <- [{config(:stun), :stun},
                         {if_version_below(~c"27", true), :jiffy},
                         {config(:tools), :debugger},
                         {config(:tools), :observer},
                         {config(:tools), :wx}], do:
      app
  end

  defp cond_included_apps do
    for {:true, app} <- [{config(:pam), :epam},
                         {config(:lua), :luerl},
                         {config(:redis), :eredis},
                         {Mix.env() == :edoc, :ex_doc},
                         {Mix.env() == :test, :dialyxir},
                         {if_version_below(~c"22", true), :lager},
                         {config(:mysql), :p1_mysql},
                         {config(:sip), :esip},
                         {config(:odbc), :odbc},
                         {config(:pgsql), :p1_pgsql},
                         {config(:sqlite), :sqlite3}], do:
      app
  end

  defp package do
    [# These are the default files included in the package
      files: ["include", "lib", "priv", "sql", "src",
              "COPYING", "README.md",
              "ejabberd.yml.example", "config/runtime.exs",
              "mix.exs", "rebar.config", "rebar.config.script", "vars.config"],
      maintainers: ["ProcessOne"],
      licenses: ["GPL-2.0-or-later"],
      links: %{"ejabberd.im" => "https://www.ejabberd.im",
               "ejabberd Docs" => "https://docs.ejabberd.im",
               "GitHub" => "https://github.com/processone/ejabberd",
               "ProcessOne" => "https://www.process-one.net/"}]
  end

  defp vars do
    filepath = case Application.fetch_env(:ejabberd, :vars_config_path) do
      :error ->
        "vars.config"
      {:ok, path} ->
        path
    end
    config2 = case :file.consult(filepath) do
      {:ok,config} -> config
      _ -> [stun: true, zlib: true]
    end
    case Mix.env() do
      :dev -> List.keystore(config2, :tools, 0, {:tools, true})
      _ -> config2
    end
  end

  defp config(key) do
    case vars()[key] do
      nil -> false
      value -> value
    end
  end

  defp elixir_required_version do
    case {Map.get(System.get_env(), "RELIVE", "false"),
          MapSet.member?(MapSet.new(System.argv()), "release")}
      do
      {"true", _} ->
        case Version.match?(System.version(), "~> 1.11") do
          false ->
            IO.puts("ERROR: To use 'make relive', Elixir 1.11.0 or higher is required.")
          _ -> :ok
        end
        "~> 1.11"
      {_, true} ->
        case Version.match?(System.version(), "~> 1.10") do
          false ->
            IO.puts("ERROR: To build releases, Elixir 1.10.0 or higher is required.")
          _ -> :ok
        end
        case Version.match?(System.version(), "< 1.11.4")
          and :erlang.system_info(:otp_release) > ~c"23" do
          true ->
            IO.puts("ERROR: To build releases with Elixir lower than 1.11.4, Erlang/OTP lower than 24 is required.")
          _ -> :ok
        end
        "~> 1.10"
      _ ->
        "~> 1.4"
    end
  end

  defp releases do
    maybe_tar = case Mix.env() do
      :prod -> [:tar]
      _ -> []
    end
    [
      ejabberd: [
        include_executables_for: [:unix],
        # applications: [runtime_tools: :permanent]
        strip_beams: Mix.env() != :dev,
        steps: [&copy_extra_files/1, :assemble | maybe_tar]
      ]
    ]
  end

  defp copy_extra_files(release) do
    assigns = [
      version: version(),
      rootdir: config(:rootdir),
      installuser: config(:installuser),
      libdir: config(:libdir),
      sysconfdir: config(:sysconfdir),
      localstatedir: config(:localstatedir),
      config_dir: config(:config_dir),
      logs_dir: config(:logs_dir),
      spool_dir: config(:spool_dir),
      vsn: version(),
      iexpath: config(:iexpath),
      erl: config(:erl),
      epmd: config(:epmd),
      bindir: Path.join([config(:release_dir), "releases", version()]),
      release_dir: config(:release_dir),
      erts_dir: config(:erts_dir),
      erts_vsn: "erts-#{release.erts_version}"
    ]
    ro = "rel/overlays"
    File.rm_rf(ro)

    # Elixir lower than 1.12.0 don't have System.shell
    execute = fn(command) ->
      case function_exported?(System, :shell, 1) do
        true ->
          System.shell(command, into: IO.stream())
        false ->
          :os.cmd(to_charlist(command))
      end
    end

    # Mix/Elixir lower than 1.11.0 use config/releases.exs instead of runtime.exs
    case Version.match?(System.version(), "~> 1.11") do
      true ->
        :ok
      false ->
        execute.("cp config/runtime.exs config/releases.exs")
    end

    execute.("sed -e 's|{{\\(\[_a-z\]*\\)}}|<%= @\\1 %>|g' ejabberdctl.template > ejabberdctl.example1")
    Mix.Generator.copy_template("ejabberdctl.example1", "ejabberdctl.example2", assigns)
    execute.("sed -e 's|{{\\(\[_a-z\]*\\)}}|<%= @\\1 %>|g' ejabberdctl.example2> ejabberdctl.example2a")
    Mix.Generator.copy_template("ejabberdctl.example2a", "ejabberdctl.example2b", assigns)
    execute.("sed -e 's|{{\\(\[_a-z\]*\\)}}|<%= @\\1 %>|g' ejabberdctl.example2b > ejabberdctl.example4")
    execute.("sed -e 's|^ERLANG_OPTS=\"|ERLANG_OPTS=\"-boot ../releases/#{release.version}/start_clean -boot_var RELEASE_LIB ../lib |' ejabberdctl.example4 > ejabberdctl.example5")
    execute.("sed -e 's|^INSTALLUSER=|ERL_OPTIONS=\"-setcookie \\$\\(cat \"\\${SCRIPT_DIR%/*}/releases/COOKIE\")\"\\nINSTALLUSER=|g' ejabberdctl.example5 > ejabberdctl.example6")
    Mix.Generator.copy_template("ejabberdctl.example6", "#{ro}/bin/ejabberdctl", assigns)
    File.chmod("#{ro}/bin/ejabberdctl", 0o755)

    File.rm("ejabberdctl.example1")
    File.rm("ejabberdctl.example2")
    File.rm("ejabberdctl.example2a")
    File.rm("ejabberdctl.example2b")
    File.rm("ejabberdctl.example3")
    File.rm("ejabberdctl.example4")
    File.rm("ejabberdctl.example5")
    File.rm("ejabberdctl.example6")

    suffix = case Mix.env() do
      :dev ->
        Mix.Generator.copy_file("test/ejabberd_SUITE_data/ca.pem", "#{ro}/conf/ca.pem")
        Mix.Generator.copy_file("test/ejabberd_SUITE_data/cert.pem", "#{ro}/conf/cert.pem")
        ".example"
      _ -> ""
    end

    Mix.Generator.copy_file("ejabberd.yml.example", "#{ro}/conf/ejabberd.yml#{suffix}")
    Mix.Generator.copy_file("ejabberdctl.cfg.example", "#{ro}/conf/ejabberdctl.cfg#{suffix}")
    Mix.Generator.copy_file("inetrc", "#{ro}/conf/inetrc")

    Enum.each(File.ls!("sql"),
      fn x ->
        Mix.Generator.copy_file("sql/#{x}", "#{ro}/lib/ejabberd-#{release.version}/priv/sql/#{x}")
      end)

    File.cp_r!("include", "#{ro}/lib/ejabberd-#{release.version}/include")
    for {name, details} <- Map.to_list(release.applications) do
      {_, is_otp_app} = List.keyfind(details, :otp_app?, 0)
      {_, vsn} = List.keyfind(details, :vsn, 0)
      {_, path} = List.keyfind(details, :path, 0)
      source_dir = case is_otp_app do
        :true -> "#{path}/include"
        :false -> "deps/#{name}/include"
      end
      target_dir = "#{ro}/lib/#{name}-#{vsn}/include"
      File.exists?(source_dir)
        && File.mkdir_p(target_dir)
        && File.cp_r!(source_dir, target_dir)
    end

    case Mix.env() do
      :dev -> execute.("REL_DIR_TEMP=$PWD/rel/overlays/ rel/setup-dev.sh mix")
      _ -> :ok
    end

    release
  end

  defp docs do
    [
      main: "readme",
      logo: "_build/edoc/logo.png",
      source_ref: "master",
      extra_section: "", # No need for Pages section name, it's the only one
      api_reference: false, # API section has just Elixir, hide it
      filter_modules: "aaaaa", # Module section has just Elixir modules, hide them
      extras: [
        "README.md": [title: "Readme"],
        "COMPILE.md": [title: "Compile and Install"],
        "CONTAINER.md": [title: "Container Image"],
        "CONTRIBUTING.md": [title: "Contributing"],
        "CONTRIBUTORS.md": [title: "Contributors"],
        "CODE_OF_CONDUCT.md": [title: "Code of Conduct"],
        "CHANGELOG.md": [title: "ChangeLog"],
        "COPYING": [title: "Copying License"],
        "_build/edoc/docs.md": [title: "&xrArr; ejabberd Docs"]
      ],
      groups_for_extras: [
        "": Path.wildcard("*.md") ++ ["COPYING"],
        "For more documentation": "_build/edoc/docs.md"
      ]
    ]
  end
end

defmodule Mix.Tasks.Compile.Asn1 do
  use Mix.Task
  alias Mix.Compilers.Erlang

  @recursive true
  @manifest ".compile.asn1"

  def run(args) do
    {opts, _, _} = OptionParser.parse(args, switches: [force: :boolean])

    project      = Mix.Project.config()
    source_paths = project[:asn1_paths] || ["asn1"]
    dest_paths    = project[:asn1_target] || ["src"]
    mappings     = Enum.zip(source_paths, dest_paths)
    options      = project[:asn1_options] || []

    force = case opts[:force] do
        :true -> [force: true]
        _ -> [force: false]
    end

    Erlang.compile(manifest(), mappings, :asn1, :erl, force, fn
      input, output ->
        options = options ++ [:noobj, outdir: Erlang.to_erl_file(Path.dirname(output))]
        case :asn1ct.compile(Erlang.to_erl_file(input), options) do
          :ok -> {:ok, :done}
          error -> error
        end
    end)
  end

  def manifests, do: [manifest()]
  defp manifest, do: Path.join(Mix.Project.manifest_path(), @manifest)

  def clean, do: Erlang.clean(manifest())
end
