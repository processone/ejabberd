defmodule Ejabberd.Config do
  @moduledoc """
  Base module for configuration file.

  Imports macros for the config DSL and contains functions
  for working/starting the configuration parsed.
  """

  alias Ejabberd.Config.{EjabberdModule, Attr, EjabberdLogger}

  defmacro __using__(_opts) do
    quote do
      Ejabberd.Config.Store.start_link

      import Ejabberd.Config, only: :macros
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
  Retrieves the path of the configuration file
  and tries to evaluate it.
  """
  def init do
    # File evaluation
    Application.get_env(:ejabberd, :file)
    |> case do
      nil -> IO.puts "[ ERR ] Missing :file for ejabberd configuration."
      path -> Code.eval_file(path) |> extract_and_store_module_name()
    end

    # Getting start/0 config
    Ejabberd.Config.Store.get(:config_module)
    |> case do
      nil -> IO.puts "[ ERR ] Configuration module not found."
      [module] -> call_start_on_config_and_store_data(module)
    end
  end

  # TODO: Start modules parsed ? Ejabberd needs that?
  # (For now it seems to work because gen_mod uses mods stored in ets)
  @doc """
  Returns a list with all the opts, formatted for ejabberd.
  """
  def get_ejabberd_opts do
    get_general_opts
    |> Dict.put(:modules, get_modules_parsed_in_order())
    |> Dict.put(:listeners, get_listeners_parsed_in_order())
    |> Ejabberd.Config.OptsFormatter.format_opts_for_ejabberd
  end

  defmacro listen(module, do: block) do
    attrs = Attr.extract_attrs_from_block_with_defaults(block)

    quote do
      Ejabberd.Config.Store.put(:listeners, %EjabberdModule{
        module: unquote(module),
        attrs: unquote(attrs)
      })
    end
  end

  defmacro module(module) do
    do_module(module, do: nil)
  end

  defmacro module(module, block) do
    do_module(module, block)
  end

  defp do_module(module, do: block) do
    attrs = Attr.extract_attrs_from_block_with_defaults(block)

    quote do
      Ejabberd.Config.Store.put(:modules, %EjabberdModule{
        module: unquote(module),
        attrs: unquote(attrs)
      })
    end
  end

  # Private API

  # Returns the modules from the store
  defp get_modules_parsed_in_order,
    do: Ejabberd.Config.Store.get(:modules) |> Enum.reverse

  # Returns the listeners from the store
  defp get_listeners_parsed_in_order,
    do: Ejabberd.Config.Store.get(:listeners) |> Enum.reverse

  # Returns the general config options
  defp get_general_opts,
    do: Ejabberd.Config.Store.get(:general) |> List.first

  # Gets the general ejabberd options calling
  # the start/0 function and stores them.
  defp call_start_on_config_and_store_data(module) do
    opts = apply(module, :start, [])
    Ejabberd.Config.Store.put(:general, opts)
  end

  # Stores the configuration module name
  defp extract_and_store_module_name({{:module, mod, _, :ok}, _}) do
    Ejabberd.Config.Store.put(:config_module, mod)
  end
end
