defmodule Ejabberd.Config.EjabberdModule do
  @moduledoc """
  Module representing a module block in the configuration file.
  It offers functions for validation and for starting the modules.

  Warning: The name is EjabberdModule to not collide with
  the already existing Elixir.Module.
  """

  @type t :: %{module: atom, attrs: [Attr.t]}

  defstruct [:module, :attrs]
  alias Ejabberd.Config.{EjabberdModule, Attr, Validation}

  @doc """
  Given a list of modules / single module
  it runs different validators on them.

  For each module, returns a {:ok, mod} or {:error, mod, errors}
  """
  def validate(modules) do
    Validation.validate(modules)
  end

  @doc """
  Start each module in the order specified in the list,
  also call the before/after hook if specified.
  """
  def start_modules(modules) when is_list(modules), do:
    Enum.each(modules, &start_modules/1)

  def start_modules(%EjabberdModule{module: mod, attrs: attrs}) do
    opts = attrs[:opts]

    invoke_hook(attrs[:before_hook])
    apply(mod, :start, ["localhost", opts])
    invoke_hook(attrs[:after_hook])
  end

  # Private API

  defp invoke_hook(nil), do: nil
  defp invoke_hook(func), do: func.()
end
