defmodule Ejabberd.ConfigUtil do
  @moduledoc """
  Module containing utility functions for
  the config file.
  """

  @doc """
  Returns true when the config file is based on elixir.
  """
  @spec is_elixir_config(list) :: boolean
  def is_elixir_config(filename) when is_list(filename) do
    is_elixir_config(to_string(filename))
  end

  def is_elixir_config(filename) do
    String.ends_with?(filename, "exs")
  end
end
