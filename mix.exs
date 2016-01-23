defmodule Ejabberd.Mixfile do
  use Mix.Project

  def project do
    [app: :ejabberd,
     version: "16.01.0",
     elixir: "~> 1.1",
     elixirc_paths: ["lib"],
     compile_path: ".",
     compilers: [:asn1] ++ Mix.compilers,
     erlc_options: erlc_options,
     erlc_paths: ["asn1", "src"],
     package: package,
     deps: deps]
  end

  def application do
    [mod: {:ejabberd_app, []},
     applications: [:ssl, :mnesia],
     included_applications: [:lager, :p1_utils, :cache_tab,
                             :p1_tls, :p1_stringprep, :p1_xml,
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
    [{:lager, git: "https://github.com/basho/lager", tag: "3.0.2"},
     {:p1_utils, git: "https://github.com/processone/p1_utils", tag: "1.0.3", override: true},
     {:cache_tab, git: "https://github.com/processone/cache_tab", tag: "1.0.2"},
     {:p1_tls, git: "https://github.com/processone/tls", tag: "1.0.0"},
     {:p1_stringprep, git: "https://github.com/processone/stringprep", tag: "1.0.1"},
     {:p1_xml, git: "https://github.com/processone/xml", tag: "1.1.2"},
     {:p1_stun, git: "https://github.com/processone/stun", tag: "0.9.1"},
     {:esip, git: "https://github.com/processone/p1_sip", tag: "1.0.1"},
     {:p1_yaml, git: "https://github.com/processone/p1_yaml", tag: "1.0.1"},
     {:jiffy, git: "https://github.com/davisp/jiffy", tag: "0.14.5"},
     {:oauth2, git: "https://github.com/kivra/oauth2", ref: "8d129fbf8866930b4ffa6dd84e65bd2b32b9acb8"},
     {:xmlrpc, git: "https://github.com/rds13/xmlrpc.git", tag: "1.15"},
     {:p1_mysql, git: "https://github.com/processone/mysql", tag: "1.0.0"},
     {:p1_pgsql, git: "https://github.com/processone/pgsql", tag: "1.0.0"},
     {:sqlite3, git: "https://github.com/alexeyr/erlang-sqlite3", ref: "cbc3505f7a131254265d3ef56191b2581b8cc172"},
     {:p1_zlib, git: "https://github.com/processone/zlib", tag: "1.0.0"},
     {:p1_iconv, git: "https://github.com/processone/eiconv", tag: "0.9.0"},
     {:eredis, git: "https://github.com/wooga/eredis", tag: "v1.0.8"},
     {:exrm, "0.19.9"}]
  end

  defp package do
    [licenses: ["GPLv2"],
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
