defmodule ModPresenceDemo do
  import Ejabberd.Logger # this allow using info, error, etc for logging
  @behaviour :gen_mod

  def start(host, _opts) do
    info('Starting ejabberd module Presence Demo')
    Ejabberd.Hooks.add(:set_presence_hook, host, __ENV__.module, :on_presence, 50)
    :ok
  end
  
  def stop(host) do
    info('Stopping ejabberd module Presence Demo')
    Ejabberd.Hooks.delete(:set_presence_hook, host, __ENV__.module, :on_presence, 50)
    :ok
  end
  
  def on_presence(user, _server, _resource, _packet) do
    info('Receive presence for #{user}')
    :none
  end 
end
