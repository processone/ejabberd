defmodule Mix.Tasks.Ejabberd.Deps.Tree do
  use Mix.Task

  alias Ejabberd.Config.EjabberdModule

  @shortdoc "Lists all ejabberd modules and their dependencies"

  @moduledoc """
  Lists all ejabberd modules and their dependencies.

  The project must have ejabberd as a dependency.
  """

  def run(_argv) do
    # First we need to start manually the store to be available
    # during the compilation of the config file.
    Ejabberd.Config.Store.start_link
    Ejabberd.Config.init(:ejabberd_config.get_ejabberd_config_path())

    Mix.shell.info "ejabberd modules"

    Ejabberd.Config.Store.get(:modules)
    |> Enum.reverse # Because of how mods are stored inside the store
    |> format_mods
    |> Mix.shell.info
  end

  defp format_mods(mods) when is_list(mods) do
    deps_tree = build_dependency_tree(mods)
    mods_used_as_dependency = get_mods_used_as_dependency(deps_tree)

    keep_only_mods_not_used_as_dep(deps_tree, mods_used_as_dependency)
    |> format_mods_into_string
  end

  defp build_dependency_tree(mods) do
    Enum.map mods, fn %EjabberdModule{module: mod, attrs: attrs} ->
      deps = attrs[:dependency]
      build_dependency_tree(mods, mod, deps)
    end
  end

  defp build_dependency_tree(mods, mod, []), do: %{module: mod, dependency: []}
  defp build_dependency_tree(mods, mod, deps) when is_list(deps) do
    dependencies = Enum.map deps, fn dep ->
      dep_deps = get_dependencies_of_mod(mods, dep)
      build_dependency_tree(mods, dep, dep_deps)
    end

    %{module: mod, dependency: dependencies}
  end

  defp get_mods_used_as_dependency(mods) when is_list(mods) do
    Enum.reduce mods, [], fn(mod, acc) ->
      case mod do
        %{dependency: []} -> acc
        %{dependency: deps} -> get_mod_names(deps) ++ acc
      end
    end
  end

  defp get_mod_names([]), do: []
  defp get_mod_names(mods) when is_list(mods), do: Enum.map(mods, &get_mod_names/1) |> List.flatten
  defp get_mod_names(%{module: mod, dependency: deps}), do: [mod | get_mod_names(deps)]

  defp keep_only_mods_not_used_as_dep(mods, mods_used_as_dep) do
    Enum.filter mods, fn %{module: mod} ->
      not mod in mods_used_as_dep
    end
  end

  defp get_dependencies_of_mod(deps, mod_name) do
    Enum.find(deps, &(Map.get(&1, :module) == mod_name))
    |> Map.get(:attrs)
    |> Keyword.get(:dependency)
  end

  defp format_mods_into_string(mods), do: format_mods_into_string(mods, 0)
  defp format_mods_into_string([], _indentation), do: ""
  defp format_mods_into_string(mods, indentation) when is_list(mods) do
    Enum.reduce mods, "", fn(mod, acc) ->
      acc <> format_mods_into_string(mod, indentation)
    end
  end

  defp format_mods_into_string(%{module: mod, dependency: deps}, 0) do
    "\n├── #{mod}" <> format_mods_into_string(deps,  2)
  end

  defp format_mods_into_string(%{module: mod, dependency: deps}, indentation) do
    spaces = Enum.reduce 0..indentation, "", fn(_, acc) -> " " <> acc end
    "\n│#{spaces}└── #{mod}" <> format_mods_into_string(deps, indentation + 4)
  end
end
