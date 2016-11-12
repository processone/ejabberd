defmodule Ejabberd.ConfigFile do
  use Ejabberd.Config

  def start do
    [loglevel: 4,
     language: "en",
     hosts: ["localhost"],
     shaper: shaper]
  end

  defp shaper do
    [normal: 1000,
      fast: 50000,
      max_fsm_queue: 1000]
  end

  listen :ejabberd_c2s do
    @opts [
      port: 5222,
      max_stanza_size: 65536,
      shaper: :c2s_shaper,
      access: :c2s]
  end

  module :mod_adhoc do
  end

  hook :register_user, [host: "localhost"], fn(user, server) ->
    info("User registered: #{user} on #{server}")
  end
end
