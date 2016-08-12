defmodule Ejabberd.Config.Validation do
  @moduledoc """
  Module used to validate a list of modules.
  """

  @type mod_validation :: {[EjabberdModule.t], EjabberdModule.t, map}
  @type mod_validation_result :: {:ok, EjabberdModule.t} | {:error, EjabberdModule.t, map}

  alias Ejabberd.Config.EjabberdModule
  alias Ejabberd.Config.Attr
  alias Ejabberd.Config.Validator
  alias Ejabberd.Config.ValidatorUtility

  @doc """
  Given a module or a list of modules it runs validators on them
  and returns {:ok, mod} or {:error, mod, errors}, for each
  of them.
  """
  @spec validate([EjabberdModule.t] | EjabberdModule.t) :: [mod_validation_result]
  def validate(modules) when is_list(modules), do: Enum.map(modules, &do_validate(modules, &1))
  def validate(module), do: validate([module])

  # Private API

  @spec do_validate([EjabberdModule.t], EjabberdModule.t) :: mod_validation_result
  defp do_validate(modules, mod) do
    {modules, mod, %{}}
    |> Validator.Attrs.validate
    |> Validator.Dependencies.validate
    |> resolve_validation_result
  end

  @spec resolve_validation_result(mod_validation) :: mod_validation_result
  defp resolve_validation_result({_modules, mod, errors}) do
    case errors do
      err when err == %{} -> {:ok, mod}
      err -> {:error, mod, err}
    end
  end
end
