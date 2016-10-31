defmodule Ejabberd.Config.EjabberdLogger do
  @moduledoc """
  Module used to log validation errors given validated modules
  given validated modules.
  """

  alias Ejabberd.Config.EjabberdModule

  @doc """
  Given a list of modules validated, in the form of {:ok, mod} or
  {:error, mod, errors}, it logs to the user the errors found.
  """
  @spec log_errors([EjabberdModule.t]) :: [EjabberdModule.t]
  def log_errors(modules_validated) when is_list(modules_validated) do
    Enum.each modules_validated, &do_log_errors/1
    modules_validated
  end

  defp do_log_errors({:ok, _mod}), do: nil
  defp do_log_errors({:error, _mod, errors}), do: Enum.each errors, &do_log_errors/1
  defp do_log_errors({:attribute, errors}), do: Enum.each errors, &log_attribute_error/1
  defp do_log_errors({:dependency, errors}), do: Enum.each errors, &log_dependency_error/1

  defp log_attribute_error({{attr_name, val}, :attr_not_supported}), do:
    IO.puts "[ WARN ] Annotation @#{attr_name} is not supported."

  defp log_attribute_error({{attr_name, val}, :type_not_supported}), do:
    IO.puts "[ WARN ] Annotation @#{attr_name} with value #{inspect val} is not supported (type mismatch)."

  defp log_dependency_error({module, :not_found}), do:
    IO.puts "[ WARN ] Module #{inspect module} was not found, but is required as a dependency."
end
