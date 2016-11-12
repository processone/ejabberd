defmodule Ejabberd.Module do

  defmacro __using__(opts) do
    logger_enabled = Keyword.get(opts, :logger, true)

    quote do
      @behaviour :gen_mod
      import Ejabberd.Module

      unquote(if logger_enabled do
        quote do: import Ejabberd.Logger
      end)
    end
  end

  # gen_mod callbacks
  def depends(_host, _opts), do: []
  def mod_opt_type(_), do: []
end
