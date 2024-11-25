defmodule Ejabberd.Auth.Example do

  @moduledoc """
  Example ejabberd auth method written in Elixir.

  This is an example to demonstrate the usage of Elixir to
  create ejabberd auth methods.

  Example configuration:
      auth_method: 'Ejabberd.Auth.Example'
  """

  @behaviour :ejabberd_auth
  import Ejabberd.Logger

  @impl true
  def start(host) do
    info("Starting Ejabberd.Auth.Example to authenticate '#{host}' users")
    nil
  end

  @impl true
  def stop(host) do
    info("Stopping Ejabberd.Auth.Example to authenticate '#{host}' users")
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
