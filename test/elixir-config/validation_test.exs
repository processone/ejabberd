defmodule Ejabberd.Config.ValidationTest do
  use ExUnit.Case

  alias Ejabberd.Config
  alias Ejabberd.Config.Store
  alias Ejabberd.Config.Validation

  setup_all do
    pid = Process.whereis(Ejabberd.Config.Store)
    unless pid != nil and Process.alive?(pid) do
      Store.start_link

      File.cd("test/elixir-config/shared")
      config_file_path = File.cwd! <> "/ejabberd_for_validation.exs"
      Config.init(config_file_path)
    end

    {:ok, %{}}
  end

  test "validates correctly the modules" do
    [mod_irc, mod_configure, mod_time] = Store.get(:modules)

    [{:error, _mod, errors}] = Validation.validate(mod_configure)
    assert %{dependency: [mod_adhoc: :not_found]} == errors

    [{:error, _mod, errors}] = Validation.validate(mod_time)
    assert %{attribute: [{{:attr_not_supported, true}, :attr_not_supported}]} == errors

    [{:ok, _mod}] = Validation.validate(mod_irc)
  end
end
