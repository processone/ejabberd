defmodule Ejabberd.Mixfile do
  use Mix.Project

  def project do
    [app: :ejabberd,
     version: "15.03.0",
     elixir: "~> 1.0",
     elixirc_paths: ["lib"],
     compile_path: ".",
     compilers: Mix.compilers,
     erlc_options: erlc_options,
     deps: deps]
  end

  def application do
    [mod: {:ejabberd_app, []},
     applications: [:kernel, :stdlib]]
  end
  
  defp erlc_options do
    includes = Path.wildcard(Path.join("..", "/*/include"))
    [:debug_info, {:d, :NO_EXT_LIB}] ++ Enum.map(includes, fn(path) -> {:i, path} end)
  end
  
  defp deps do
    [
        {:p1_xml, github: "processone/xml"},
        {:p1_logger, github: "processone/p1_logger"},
        {:p1_yaml, github: "processone/p1_yaml"},
        {:p1_tls, github: "processone/tls"},
        {:p1_stringprep, github: "processone/stringprep"},
        {:p1_zlib, github: "processone/zlib"},
        {:p1_cache_tab, github: "processone/cache_tab"},
        {:p1_utils, github: "processone/p1_utils"},
        {:p1_iconv, github: "processone/eiconv"},
	      {:esip, github: "processone/p1_sip"},
	      {:p1_stun, github: "processone/stun"},
        {:ehyperloglog, github: "vaxelfel/eHyperLogLog"},
        {:p1_mysql, github: "processone/mysql"},
        {:p1_pgsql, github: "processone/pgsql"},
		    {:eredis, github: "wooga/eredis"}
     ]
  end
end
