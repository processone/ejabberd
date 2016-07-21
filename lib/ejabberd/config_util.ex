defmodule Ejabberd.ConfigUtil do

  def is_elixir_config(filename) when is_list(filename) do
    is_elixir_config(to_string(filename))
  end

  def is_elixir_config(filename) do
    String.ends_with?(filename, "exs")
  end
end
