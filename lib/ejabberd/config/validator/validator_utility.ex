defmodule Ejabberd.Config.ValidatorUtility do
  @moduledoc """
  Module used as a base validator for validation modules.
  Imports utility functions for working with validation structures.
  """

  alias Ejabberd.Config.EjabberdModule

  @doc """
  Inserts an error inside the errors collection, for the given key.
  If the key doesn't exists then it creates an empty collection
  and inserts the value passed.
  """
  @spec put_error(map, atom, any) :: map
  def put_error(errors, key, val) do
    Map.update errors, key, [val], fn coll ->
      [val | coll]
    end
  end

  @doc """
  Given a list of modules it extracts and returns a list
  of the module names (which are Elixir.Module).
  """
  @spec extract_module_names(EjabberdModule.t) :: [atom]
  def extract_module_names(modules) when is_list(modules) do
    modules
    |> Enum.map(&Map.get(&1, :module))
  end
end
