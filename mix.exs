defmodule Ejabberd.Mixfile do
  use Mix.Project

  def project do
    [app: :ejabberd,
     version: "17.03.0",
     description: description,
     elixir: "~> 1.3",
     elixirc_paths: ["lib"],
     compile_path: ".",
     compilers: [:asn1] ++ Mix.compilers,
     erlc_options: erlc_options,
     erlc_paths: ["asn1", "src"],
     # Elixir tests are starting the part of ejabberd they need
     aliases: [test: "test --no-start"],
     package: package,
     deps: deps]
  end

  def description do
    """
    Robust, ubiquitous and massively scalable Jabber / XMPP Instant Messaging platform.
    """
  end

  def application do
    [mod: {:ejabberd_app, []},
     applications: [:ssl],
     included_applications: [:lager, :mnesia, :inets, :p1_utils, :cache_tab,
                             :fast_tls, :stringprep, :fast_xml, :xmpp,
                             :stun, :fast_yaml, :esip, :jiffy, :p1_oauth2]
                         ++ cond_apps]
  end

  defp erlc_options do
    # Use our own includes + includes from all dependencies
    includes = ["include"] ++ deps_include(["fast_xml", "xmpp", "p1_utils"])
    [:debug_info, {:d, :ELIXIR_ENABLED}] ++ Enum.map(includes, fn(path) -> {:i, path} end)
  end

  defp deps do
    [{:lager, "~> 3.2"},
     {:p1_utils, "~> 1.0"},
     {:fast_xml, "~> 1.1"},
     {:xmpp, "~> 1.1"},
     {:cache_tab, "~> 1.0"},
     {:stringprep, "~> 1.0"},
     {:fast_yaml, "~> 1.0"},
     {:fast_tls, "~> 1.0"},
     {:stun, "~> 1.0"},
     {:esip, "~> 1.0"},
     {:jiffy, "~> 0.14.7"},
     {:p1_oauth2, "~> 0.6.1"},
     {:distillery, "~> 1.0"},
     {:ex_doc, ">= 0.0.0", only: :dev}]
    ++ cond_deps
  end

  defp deps_include(deps) do
    base = case Mix.Project.deps_paths()[:ejabberd] do
      nil -> "deps"
      _ -> ".."
    end
    Enum.map(deps, fn dep -> base<>"/#{dep}/include" end)
  end

  defp cond_deps do
    for {:true, dep} <- [{config(:mysql), {:p1_mysql, "~> 1.0"}},
                         {config(:pgsql), {:p1_pgsql, "~> 1.1"}},
                         {config(:sqlite), {:sqlite3, "~> 1.1"}},
                         {config(:riak), {:riakc, "~> 2.4"}},
                         {config(:redis), {:eredis, "~> 1.0"}},
                         {config(:zlib), {:ezlib, "~> 1.0"}},
                         {config(:iconv), {:iconv, "~> 1.0"}},
                         {config(:pam), {:epam, "~> 1.0"}},
                         {config(:tools), {:luerl, github: "rvirding/luerl", tag: "v0.2"}},
                         {config(:tools), {:meck, "~> 0.8.4"}},
                         {config(:tools), {:moka, github: "processone/moka", tag: "1.0.5c"}}], do:
      dep
  end

  defp cond_apps do
    for {:true, app} <- [{config(:redis), :eredis},
                         {config(:mysql), :p1_mysql},
                         {config(:pgsql), :p1_pgsql},
                         {config(:sqlite), :sqlite3},
                         {config(:zlib), :ezlib},
                         {config(:iconv), :iconv}], do:
      app
  end

  def package do
    [# These are the default files included in the package
      files: ["lib", "src", "priv", "mix.exs", "include", "README.md", "COPYING"],
      maintainers: ["ProcessOne"],
      licenses: ["GPLv2"],
      links: %{"Site" => "https://www.ejabberd.im",
               "Documentation" => "http://docs.ejabberd.im",
               "Source" => "https://github.com/processone/ejabberd",
               "ProcessOne" => "http://www.process-one.net/"}]
  end

  def vars do
    case :file.consult("vars.config") do
      {:ok,config} -> config
      _ -> [zlib: true, iconv: true]
    end
  end

  defp config(key) do
    case vars[key] do
      nil -> false
      value -> value
    end
  end

end

defmodule Mix.Tasks.Compile.Asn1 do
  use Mix.Task
  alias Mix.Compilers.Erlang

  @recursive true
  @manifest ".compile.asn1"

  def run(args) do
    {opts, _, _} = OptionParser.parse(args, switches: [force: :boolean])

    project      = Mix.Project.config
    source_paths = project[:asn1_paths] || ["asn1"]
    dest_paths    = project[:asn1_target] || ["src"]
    mappings     = Enum.zip(source_paths, dest_paths)
    options      = project[:asn1_options] || []

    Erlang.compile(manifest(), mappings, :asn1, :erl, opts[:force], fn
      input, output ->
        options = options ++ [:noobj, outdir: Erlang.to_erl_file(Path.dirname(output))]
        case :asn1ct.compile(Erlang.to_erl_file(input), options) do
          :ok -> {:ok, :done}
          error -> error
        end
    end)
  end

  def manifests, do: [manifest]
  defp manifest, do: Path.join(Mix.Project.manifest_path, @manifest)

  def clean, do: Erlang.clean(manifest())
end
