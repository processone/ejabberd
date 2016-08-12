defmodule Ejabberd.ConfigTest do
  use ExUnit.Case

  alias Ejabberd.Config
  alias Ejabberd.Config.Store

  setup_all do
    pid = Process.whereis(Ejabberd.Config.Store)
    unless pid != nil and Process.alive?(pid) do
      Store.start_link

      File.cd("test/elixir-config/shared")
      config_file_path = File.cwd! <> "/ejabberd.exs"
      Config.init(config_file_path)
    end

    {:ok, %{}}
  end

  test "extracts successfully the module name from config file" do
    assert [Ejabberd.ConfigFile] == Store.get(:module_name)
  end

  test "extracts successfully general opts from config file" do
    [general] = Store.get(:general)
    shaper = [normal: 1000, fast: 50000, max_fsm_queue: 1000]
    assert [loglevel: 4, language: "en", hosts: ["localhost"], shaper: shaper] == general
  end

  test "extracts successfully listeners from config file" do
    [listen] = Store.get(:listeners)
    assert :ejabberd_c2s == listen.module
    assert [port: 5222, max_stanza_size: 65536, shaper: :c2s_shaper, access: :c2s] == listen.attrs[:opts]
  end

  test "extracts successfully modules from config file" do
    [module] = Store.get(:modules)
    assert :mod_adhoc == module.module
    assert [] == module.attrs[:opts]
  end

  test "extracts successfully hooks from config file" do
    [register_hook] = Store.get(:hooks)

    assert :register_user == register_hook.hook
    assert [host: "localhost"] == register_hook.opts
    assert is_function(register_hook.fun)
  end

  # TODO: When enalbed, this test causes the evaluation of a different config file, so
  # the other tests, that uses the store, are compromised because the data is different.
  # So, until a good way is found, this test should remain disabed.
  #
  # test "init/2 with force:true re-initializes the config store with new data" do
  #   config_file_path = File.cwd! <> "/ejabberd_different_from_default.exs"
  #   Config.init(config_file_path, true)
  #
  #   assert [Ejabberd.ConfigFile] == Store.get(:module_name)
  #   assert [[loglevel: 4, language: "en", hosts: ["localhost"]]] == Store.get(:general)
  #   assert [] == Store.get(:modules)
  #   assert [] == Store.get(:listeners)
  #
  #   Store.stop
  # end
end
