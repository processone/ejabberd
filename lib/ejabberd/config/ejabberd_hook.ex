defmodule Ejabberd.Config.EjabberdHook do
  @moduledoc """
  Module containing functions for manipulating
  ejabberd hooks.
  """

  defstruct hook: nil, opts: [], fun: nil

  alias Ejabberd.Config.EjabberdHook

  @type t :: %EjabberdHook{}

  @doc """
  Register a hook to ejabberd.
  """
  @spec start(EjabberdHook.t) :: none
  def start(%EjabberdHook{hook: hook, opts: opts, fun: fun}) do
    host = Keyword.get(opts, :host, :global)
    priority = Keyword.get(opts, :priority, 50)

    :ejabberd_hooks.add(hook, host, fun, priority)
  end
end
