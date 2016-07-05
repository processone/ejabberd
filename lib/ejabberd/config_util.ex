defmodule Ejabberd.ConfigUtil do

  def is_elixir_config(filename) do
    String.ends_with?(filename, "exs")
  end
end
