defmodule Ejabberd.Module.Example do

  @moduledoc """
  Example ejabberd module written in Elixir.

  This is an example to demonstrate the usage of Elixir to
  create ejabberd modules.

  Example configuration:
      modules:
        'Ejabberd.Module.Example': {}
  """

  @behaviour :gen_mod
  import Ejabberd.Logger

  def start(host, _opts) do
    info("Starting Ejabberd.Module.Example for host '#{host}'")
    Ejabberd.Hooks.add(:set_presence_hook, host, __MODULE__, :on_presence, 50)
    :ok
  end

  def stop(host) do
    info("Stopping Ejabberd.Module.Example for host '#{host}'")
    Ejabberd.Hooks.delete(:set_presence_hook, host, __MODULE__, :on_presence, 50)
    :ok
  end

  def on_presence(user, _server, _resource, _packet) do
    info("Receive presence for #{user}")
    :none
  end

  def depends(_host, _opts) do
    []
  end

  def mod_options(_host) do
    []
  end

  def mod_doc() do
    %{:desc => "This is just a demonstration."}
  end

end
