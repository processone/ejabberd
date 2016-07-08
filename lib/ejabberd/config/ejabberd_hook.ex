defmodule Ejabberd.Config.EjabberdHook do
  defstruct hook: nil, opts: [], fun: nil

  alias Ejabberd.Config.EjabberdHook

  def start(%EjabberdHook{hook: hook, opts: opts, fun: fun}) do
    host = Keyword.get(opts, :host, :global)
    priority = Keyword.get(opts, :priority, 50)

    :ejabberd_hooks.add(hook, host, fun, priority)
  end
end
