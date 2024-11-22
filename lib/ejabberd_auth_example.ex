defmodule ModAuthExample do
  @moduledoc """

  This is a dummy auth module to demonstrate the usage of Elixir to
  create Ejabberd Auth modules.

  """
  import Ejabberd.Logger
  @behaviour :ejabberd_auth

  @impl true
  def start(host) do
    info("Using mod_auth_example to authenticate #{host} users")
    nil
  end

  @impl true
  def stop(host) do
    info("Stop using mod_auth_example to authenticate #{host} users")
    nil
  end

  @impl true
  def check_password("alice", _authz_id, _host, "secret"), do: {:nocache, true}
  def check_password(_username, _authz_id, _host, _secret), do: {:nocache, false}

  @impl true
  def user_exists("alice", _host), do: {:nocache, true}
  def user_exists(_username, _host), do: {:nocache, false}

  @impl true
  def plain_password_required(_binary), do: true

  @impl true
  def store_type(_host), do: :external

  @impl true
  def use_cache(_host), do: false
end
