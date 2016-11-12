defmodule Ejabberd.Config do
  @moduledoc """
  Base module for configuration file.

  Imports macros for the config DSL and contains functions
  for working/starting the configuration parsed.
  """

  alias Ejabberd.Config.EjabberdModule
  alias Ejabberd.Config.Attr
  alias Ejabberd.Config.EjabberdLogger

  defmacro __using__(_opts) do
    quote do
      import Ejabberd.Config, only: :macros
      import Ejabberd.Logger

      @before_compile Ejabberd.Config
    end
  end

  # Validate the modules parsed and log validation errors at compile time.
  # Could be also possible to interrupt the compilation&execution by throwing
  # an exception if necessary.
  def __before_compile__(_env) do
    get_modules_parsed_in_order
    |> EjabberdModule.validate
    |> EjabberdLogger.log_errors
  end

  @doc """
  Given the path of the config file, it evaluates it.
  """
  def init(file_path, force \\ false) do
    init_already_executed = Ejabberd.Config.Store.get(:module_name) != []

    case force do
      true ->
        Ejabberd.Config.Store.stop
        Ejabberd.Config.Store.start_link
        do_init(file_path)
      false ->
        if not init_already_executed, do: do_init(file_path)
    end
  end

  @doc """
  Returns a list with all the opts, formatted for ejabberd.
  """
  def get_ejabberd_opts do
    get_general_opts
    |> Dict.put(:modules, get_modules_parsed_in_order())
    |> Dict.put(:listeners, get_listeners_parsed_in_order())
    |> Ejabberd.Config.OptsFormatter.format_opts_for_ejabberd
  end

  @doc """
  Register the hooks defined inside the elixir config file.
  """
  def start_hooks do
    get_hooks_parsed_in_order()
    |> Enum.each(&Ejabberd.Config.EjabberdHook.start/1)
  end

  ###
  ### MACROS
  ###

  defmacro listen(module, do: block) do
    attrs = Attr.extract_attrs_from_block_with_defaults(block)

    quote do
      Ejabberd.Config.Store.put(:listeners, %EjabberdModule{
        module: unquote(module),
        attrs: unquote(attrs)
      })
    end
  end

  defmacro module(module, do: block) do
    attrs = Attr.extract_attrs_from_block_with_defaults(block)

    quote do
      Ejabberd.Config.Store.put(:modules, %EjabberdModule{
        module: unquote(module),
        attrs: unquote(attrs)
      })
    end
  end

  defmacro hook(hook_name, opts, fun) do
    quote do
      Ejabberd.Config.Store.put(:hooks, %Ejabberd.Config.EjabberdHook{
        hook: unquote(hook_name),
        opts: unquote(opts),
        fun: unquote(fun)
      })
    end
  end

  # Private API

  defp do_init(file_path) do
    # File evaluation
    Code.eval_file(file_path) |> extract_and_store_module_name()

    # Getting start/0 config
    Ejabberd.Config.Store.get(:module_name)
    |> case do
      nil -> IO.puts "[ ERR ] Configuration module not found."
      [module] -> call_start_func_and_store_data(module)
    end

    # Fetching git modules and install them
    get_modules_parsed_in_order()
    |> EjabberdModule.fetch_git_repos
  end

  # Returns the modules from the store
  defp get_modules_parsed_in_order,
    do: Ejabberd.Config.Store.get(:modules) |> Enum.reverse

  # Returns the listeners from the store
  defp get_listeners_parsed_in_order,
    do: Ejabberd.Config.Store.get(:listeners) |> Enum.reverse

  defp get_hooks_parsed_in_order,
    do: Ejabberd.Config.Store.get(:hooks) |> Enum.reverse

  # Returns the general config options
  defp get_general_opts,
    do: Ejabberd.Config.Store.get(:general) |> List.first

  # Gets the general ejabberd options calling
  # the start/0 function and stores them.
  defp call_start_func_and_store_data(module) do
    opts = apply(module, :start, [])
    Ejabberd.Config.Store.put(:general, opts)
  end

  # Stores the configuration module name
  defp extract_and_store_module_name({{:module, mod, _bytes, _}, _}) do
    Ejabberd.Config.Store.put(:module_name, mod)
  end
end
