defmodule Ejabberd.Config.Validator.Dependencies do
  @moduledoc """
  Validator module used to validate dependencies specified
  with the @dependency annotation.
  """

  # TODO: Duplicated from validator.ex !!!
  @type mod_validation :: {[EjabberdModule.t], EjabberdModule.t, map}
  import Ejabberd.Config.ValidatorUtility

  @doc """
  Given a module (with the form used for validation)
  it checks if the @dependency annotation is respected and
  returns the validation tuple with the errors updated, if found.
  """
  @spec validate(mod_validation) :: mod_validation
  def validate({modules, mod, errors}) do
    module_names = extract_module_names(modules)
    dependencies = mod.attrs[:dependency]

    errors = Enum.reduce dependencies, errors, fn(req_module, err) ->
      case req_module in module_names do
        true -> err
        false -> put_error(err, :dependency, {req_module, :not_found})
      end
    end

    {modules, mod, errors}
  end
end
