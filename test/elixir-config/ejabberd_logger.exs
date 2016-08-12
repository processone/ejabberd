defmodule Ejabberd.Config.EjabberdLoggerTest do
  use ExUnit.Case

  import ExUnit.CaptureIO

  alias Ejabberd.Config
  alias Ejabberd.Config.Store
  alias Ejabberd.Config.Validation
  alias Ejabberd.Config.EjabberdLogger

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

  test "outputs correctly when attr is not supported" do
    error_msg = "[ WARN ] Annotation @attr_not_supported is not supported.\n"

    [_mod_irc, _mod_configure, mod_time] = Store.get(:modules)
    fun = fn ->
      mod_time
      |> Validation.validate
      |> EjabberdLogger.log_errors
    end

    assert capture_io(fun) == error_msg
  end

  test "outputs correctly when dependency is not found" do
    error_msg = "[ WARN ] Module :mod_adhoc was not found, but is required as a dependency.\n"

    [_mod_irc, mod_configure, _mod_time] = Store.get(:modules)
    fun = fn ->
      mod_configure
      |> Validation.validate
      |> EjabberdLogger.log_errors
    end

    assert capture_io(fun) == error_msg
  end
end
