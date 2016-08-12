defmodule Ejabberd.Config.Validator.Attrs do
  @moduledoc """
  Validator module used to validate attributes.
  """

  # TODO: Duplicated from validator.ex !!!
  @type mod_validation :: {[EjabberdModule.t], EjabberdModule.t, map}

  import Ejabberd.Config.ValidatorUtility
  alias Ejabberd.Config.Attr

  @doc """
  Given a module (with the form used for validation)
  it runs Attr.validate/1 on each attribute and
  returns the validation tuple with the errors updated, if found.
  """
  @spec validate(mod_validation) :: mod_validation
  def validate({modules, mod, errors}) do
    errors = Enum.reduce mod.attrs, errors, fn(attr, err) ->
      case Attr.validate(attr) do
        {:ok, attr} -> err
        {:error, attr, cause} -> put_error(err, :attribute, {attr, cause})
      end
    end

    {modules, mod, errors}
  end
end
