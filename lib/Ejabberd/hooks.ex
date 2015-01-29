defmodule Ejabberd.Hooks do

  # Generic hook setting features
  def add(hook_name, host, module, function, priority) do
    :ejabberd_hooks.add(hook_name, host, module, function, priority)
  end

  # Should be named 'removed'
  def delete(hook_name, host, module, function, priority) do
    :ejabberd_hooks.delete(hook_name, host, module, function, priority)
  end

end
