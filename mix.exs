defmodule Ejabberd.Mixfile do
  use Mix.Project

  def project do
    [app: :ejabberd,
     version: "16.01.0",
     description: description,
     elixir: "~> 1.1",
     elixirc_paths: ["lib"],
     compile_path: ".",
     compilers: [:asn1] ++ Mix.compilers,
     erlc_options: erlc_options,
     erlc_paths: ["asn1", "src"],
     package: package,
     deps: deps]
  end

  defp description do
    """
    Robust, ubiquitous and massively scalable Jabber / XMPP Instant Messaging platform.
    """
  end

  def application do
    [mod: {:ejabberd_app, []},
     applications: [:ssl],
     included_applications: [:lager, :mnesia, :p1_utils, :cache_tab,
                             :p1_tls, :stringprep, :p1_xml,
                             :p1_stun, :p1_yaml, :p1_zlib, :p1_iconv,
                             :esip, :jiffy, :oauth2, :xmlrpc, :eredis,
                             :p1_mysql, :p1_pgsql, :sqlite3]]
  end

  defp erlc_options do
    # Use our own includes + includes from all dependencies
    includes = ["include"] ++ Path.wildcard(Path.join("..", "/*/include"))
    [:debug_info] ++ Enum.map(includes, fn(path) -> {:i, path} end)
  end

  defp deps do
    [{:lager, "~> 3.0"},
     {:p1_utils, "~> 1.0", override: true},
     {:cache_tab, "~> 1.0"},
     {:stringprep, "~> 1.0"},
     {:fast_yaml, "~> 1.0"},
     {:p1_tls, git: "https://github.com/processone/tls", tag: "1.0.0"},
     {:p1_xml, git: "https://github.com/processone/xml", tag: "1.1.2"},
     {:p1_stun, git: "https://github.com/processone/stun", tag: "0.9.1"},
     {:esip, git: "https://github.com/processone/p1_sip", tag: "1.0.1"},
     {:jiffy, "~> 0.14.7"},
     {:p1_oauth2, "~> 0.6.1"},
     {:p1_xmlrpc, "~> 1.15"},
     {:p1_mysql, "~> 1.0"},
     {:p1_pgsql, "~> 1.0"},
     {:sqlite3, "~> 1.1"},
     {:p1_zlib, git: "https://github.com/processone/zlib", tag: "1.0.0"},
     {:p1_iconv, git: "https://github.com/processone/eiconv", tag: "0.9.0"},
     {:eredis, "~> 1.0"},
     {:exrm, "~> 1.0.0-rc7", only: :dev}]
  end

  defp package do
    [# These are the default files included in the package
      files: ["lib", "src", "priv", "mix.exs", "include", "README.md", "COPYING"],
      maintainers: ["ProcessOne"],
      licenses: ["GPLv2"],
      links: %{"Site" => "https://www.ejabberd.im",
               "Documentation" => "http://docs.ejabberd.im",
               "Source" => "https://github.com/processone/ejabberd"}]
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
