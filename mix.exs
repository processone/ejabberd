defmodule Ejabberd.Mixfile do
  use Mix.Project

  def project do
    [app: :ejabberd,
     version: "15.07.0",
     elixir: "~> 1.0",
     elixirc_paths: ["lib"],
     compile_path: ".",
     compilers: [:asn1] ++ Mix.compilers,
     erlc_options: erlc_options,
     erlc_paths: ["asn1", "src"],
     deps: deps]
  end

  def application do
    [mod: {:ejabberd_app, []},
     applications: [:ssl],
     included_applications: [:p1_logger,:p1_yaml,:p1_tls,:p1_xml,:p1_stringprep,:p1_zlib,:p1_cache_tab,:mnesia,:p1_utils,
                             :p1_iconv,:esip,:p1_stun,:ehyperloglog,:p1_mysql,:p1_pgsql,:eredis]]
  end
  
  defp erlc_options do
    includes = Path.wildcard(Path.join("..", "/*/include"))
    [:debug_info, {:d, :NO_EXT_LIB}] ++ Enum.map(includes, fn(path) -> {:i, path} end)
  end
  
  defp deps do
    [
        {:p1_xml, git: "https://github.com/processone/xml"},
        {:p1_logger, git: "https://github.com/processone/p1_logger"},
        {:p1_yaml, git: "https://github.com/processone/p1_yaml"},
        {:p1_tls, git: "https://github.com/processone/tls"},
        {:p1_stringprep, git: "https://github.com/processone/stringprep"},
        {:p1_zlib, git: "https://github.com/processone/zlib"},
        {:p1_cache_tab, git: "https://github.com/processone/cache_tab"},
        {:p1_utils, git: "https://github.com/processone/p1_utils"},
        {:p1_iconv, git: "https://github.com/processone/eiconv"},
        {:esip, git: "https://github.com/processone/p1_sip"},
        {:p1_stun, git: "https://github.com/processone/stun"},
        {:ehyperloglog, git: "https://github.com/vaxelfel/eHyperLogLog"},
        {:p1_mysql, git: "https://github.com/processone/mysql"},
        {:p1_pgsql, git: "https://github.com/processone/pgsql"},
        {:eredis, git: "https://github.com/wooga/eredis"},
        {:exrm, "~> 0.19.2"}
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
