defmodule Ejabberd.ConfigFile do
  use Ejabberd.Config

  def start do
    [loglevel: 4,
     log_rotate_size: 10485760,
     log_rotate_date: "",
     log_rotate_count: 1,
     log_rate_limit: 100,
     auth_method: :internal,
     max_fsm_queue: 1000,
     language: "en",
     allow_contrib_modules: true,
     hosts: ["localhost"],
     shaper: shaper,
     acl: acl,
     access: access]
  end

  defp shaper do
    [normal: 1000,
      fast: 50000,
      max_fsm_queue: 1000]
  end

  defp acl do
    [local:
      [user_regexp: "", loopback: [ip: "127.0.0.0/8"]]]
  end

  defp access do
    [max_user_sessions: [all: 10],
     max_user_offline_messages: [admin: 5000, all: 100],
     local: [local: :allow],
     c2s: [blocked: :deny, all: :allow],
     c2s_shaper: [admin: :none, all: :normal],
     s2s_shaper: [all: :fast],
     announce: [admin: :allow],
     configure: [admin: :allow],
     muc_admin: [admin: :allow],
     muc_create: [local: :allow],
     muc: [all: :allow],
     pubsub_createnode: [local: :allow],
     register: [all: :allow],
     trusted_network: [loopback: :allow]]
  end

  listen :ejabberd_c2s do
    @opts [
      port: 5222,
      max_stanza_size: 65536,
      shaper: :c2s_shaper,
      access: :c2s]
  end

  listen :ejabberd_s2s_in do
    @opts [port: 5269]
  end

  listen :ejabberd_http do
    @opts [
      port: 5280,
      web_admin: true,
      http_poll: true,
      http_bind: true,
      captcha: true]
  end

  module :mod_adhoc do
  end

  module :mod_announce do
    @opts [access: :announce]
  end

  module :mod_blocking do
  end

  module :mod_caps do
  end

  module :mod_carboncopy do
  end

  module :mod_client_state do
    @opts [
      drop_chat_states: true,
      queue_presence: false]
  end

  module :mod_configure do
  end

  module :mod_disco do
  end

  module :mod_irc do
  end

  module :mod_http_bind do
  end

  module :mod_last do
  end

  module :mod_muc do
    @opts [
      access: :muc,
      access_create: :muc_create,
      access_persistent: :muc_create,
      access_admin: :muc_admin]
  end

  module :mod_offline do
    @opts [access_max_user_messages: :max_user_offline_messages]
  end

  module :mod_ping do
  end

  module :mod_privacy do
  end

  module :mod_private do
  end

  module :mod_pubsub do
    @opts [
      access_createnode: :pubsub_createnode,
      ignore_pep_from_offline: true,
      last_item_cache: true,
      plugins: ["flat", "hometree", "pep"]]
  end

  module :mod_register do
    @opts [welcome_message: [
      subject: "Welcome!",
      body: "Hi.\nWelcome to this XMPP Server",
      ip_access: :trusted_network,
      access: :register]]
  end

  module :mod_roster do
  end

  module :mod_shared_roster do
  end

  module :mod_stats do
  end

  module :mod_time do
  end

  module :mod_version do
  end

  # Example of how to define a hook, called when the event
  # specified is triggered.
  #
  # @event: Name of the event
  # @opts: Params are optional. Available: :host and :priority.
  #        If missing, defaults are used. (host: :global | priority: 50)
  # @callback Could be an anonymous function or a callback from a module,
  #           use the &ModuleName.function/arity format for that.
  hook :register_user, [host: "localhost"], fn(user, server) ->
    info("User registered: #{user} on #{server}")
  end
end
