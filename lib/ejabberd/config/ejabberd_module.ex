defmodule Ejabberd.Config.EjabberdModule do
  @moduledoc """
  Module representing a module block in the configuration file.
  It offers functions for validation and for starting the modules.

  Warning: The name is EjabberdModule to not collide with
  the already existing Elixir.Module.
  """

  @type t :: %{module: atom, attrs: [Attr.t]}

  defstruct [:module, :attrs]

  alias Ejabberd.Config.EjabberdModule
  alias Ejabberd.Config.Attr
  alias Ejabberd.Config.Validation

  @doc """
  Given a list of modules / single module
  it runs different validators on them.

  For each module, returns a {:ok, mod} or {:error, mod, errors}
  """
  def validate(modules) do
    Validation.validate(modules)
  end

  @doc """
  Given a list of modules, it takes only the ones with
  a git attribute and tries to fetch the repo,
  then, it install them through :ext_mod.install/1
  """
  @spec fetch_git_repos([EjabberdModule.t]) :: none()
  def fetch_git_repos(modules) do
    modules
    |> Enum.filter(&is_git_module?/1)
    |> Enum.each(&fetch_and_install_git_module/1)
  end

  # Private API

  defp is_git_module?(%EjabberdModule{attrs: attrs}) do
    case Keyword.get(attrs, :git) do
      "" -> false
      repo -> String.match?(repo, ~r/((git|ssh|http(s)?)|(git@[\w\.]+))(:(\/\/)?)([\w\.@\:\/\-~]+)(\.git)(\/)?/)
    end
  end

  defp fetch_and_install_git_module(%EjabberdModule{attrs: attrs}) do
    repo = Keyword.get(attrs, :git)
    mod_name = case Keyword.get(attrs, :name) do
      "" -> infer_mod_name_from_git_url(repo)
      name -> name
    end

    path = "#{:ext_mod.modules_dir()}/sources/ejabberd-contrib\/#{mod_name}"
    fetch_and_store_repo_source_if_not_exists(path, repo)
    :ext_mod.install(mod_name) # Have to check if overwrites an already present mod
  end

  defp fetch_and_store_repo_source_if_not_exists(path, repo) do
    unless File.exists?(path) do
      IO.puts "[info] Fetching: #{repo}"
      :os.cmd('git clone #{repo} #{path}')
    end
  end

  defp infer_mod_name_from_git_url(repo),
    do: String.split(repo, "/") |> List.last |> String.replace(".git", "")
end
