# ----------------------------------------------------------------------
#
# ejabberd, Copyright (C) 2002-2017   ProcessOne
#
# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License as
# published by the Free Software Foundation; either version 2 of the
# License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
#
# You should have received a copy of the GNU General Public License along
# with this program; if not, write to the Free Software Foundation, Inc.,
# 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
#
# ----------------------------------------------------------------------

# Notes on the tests:
#
# This test suite will print out errors in logs for tests:
#
#   test "Error in run_fold is ignored"
#   test "Throw in run_fold is ignored"
#   test "Exit in run_fold is ignored"
#
# Those tests are not failing and we can safely ignore those errors in
# log as we are exercising hook handler recovery from that situation.

defmodule EjabberdHooksTest do
  use ExUnit.Case, async: false
  
  @author "mremond@process-one.net"
  @host <<"domain.net">>
  @self __MODULE__

  setup_all do
    {:ok, _pid} = :ejabberd_hooks.start_link
    :ok
  end

  setup do
    :meck.unload
    :true = :ejabberd_hooks.delete_all_hooks
    :ok
  end

  test "An anonymous function can be added as a hook" do
    hookname = :test_fun_hook
    :ok = :ejabberd_hooks.add(hookname, @host, fn _ -> :ok end, 50)
    [{50, :undefined, _}] = :ejabberd_hooks.get_handlers(hookname, @host)
  end

  test "A module function can be added as a hook" do
    hookname = :test_mod_hook
    callback = :hook_callback
    :ok = :ejabberd_hooks.add(hookname, @host, @self, callback, 40)
    [{40, @self, _callback}] = :ejabberd_hooks.get_handlers(hookname, @host)
  end

  test "An anonymous function can be removed from hook handlers" do
    hookname = :test_fun_hook
    anon_fun = fn _ -> :ok end
    :ok = :ejabberd_hooks.add(hookname, @host, anon_fun, 50)
    :ok = :ejabberd_hooks.delete(hookname, @host, anon_fun, 50)
    [] = :ejabberd_hooks.get_handlers(hookname, @host)
  end

  test "An module function can be removed from hook handlers" do
    hookname = :test_mod_hook
    callback = :hook_callback
    :ok = :ejabberd_hooks.add(hookname, @host, @self, callback, 40)
    :ok = :ejabberd_hooks.delete(hookname, @host, @self, callback, 40)
    [] = :ejabberd_hooks.get_handlers(hookname, @host)
    # TODO: Check that removed function is not call anymore
  end

  test "'Run hook' call registered handler once" do
    test_result = :hook_result
    run_hook([], fn -> test_result end, test_result)
  end

  test "'Run hook' can call registered handler with parameters" do
    test_result = :hook_result_with_params
    run_hook([:hook_params], fn _ -> test_result end, test_result)
  end

  # TODO test "Several handlers are run in order by hook"

  test "Hook run chain is stopped when handler return 'stop'" do
    # setup test
    hookname = :test_mod_hook
    modulename = :hook_module
    mock(modulename, :hook_callback1, fn _ -> :stop end)
    mock(modulename, :hook_callback2, fn _ -> :end_result end)

    :ok = :ejabberd_hooks.add(hookname, @host, modulename, :hook_callback1, 40)
    :ok = :ejabberd_hooks.add(hookname, @host, modulename, :hook_callback1, 50)

    :ok = :ejabberd_hooks.run(hookname, @host, [:hook_params])
    # callback2 is never run:
    [{_pid, {^modulename, _callback, [:hook_params]}, :stop}] = :meck.history(modulename)
  end

  test "Run fold hooks accumulate state in correct order through handlers" do
    # setup test
    hookname = :test_mod_hook
    modulename = :hook_module
    mock(modulename, :hook_callback1, fn(list, user) -> [user|list] end)
    mock(modulename, :hook_callback2, fn(list, _user) -> ["jid2"|list] end)

    :ok = :ejabberd_hooks.add(hookname, @host, modulename, :hook_callback1, 40)
    :ok = :ejabberd_hooks.add(hookname, @host, modulename, :hook_callback2, 50)

    ["jid2", "jid1"] = :ejabberd_hooks.run_fold(hookname, @host, [], ["jid1"])
  end

  test "Hook run_fold are executed based on priority order, not registration order" do
    # setup test
    hookname = :test_mod_hook
    modulename = :hook_module
    mock(modulename, :hook_callback1, fn(_acc) -> :first end)
    mock(modulename, :hook_callback2, fn(_acc) -> :second end)

    :ok = :ejabberd_hooks.add(hookname, @host, modulename, :hook_callback2, 50)
    :ok = :ejabberd_hooks.add(hookname, @host, modulename, :hook_callback1, 40)

    :second = :ejabberd_hooks.run_fold(hookname, @host, :started, [])
    # Both module have been called:
    2 = length(:meck.history(modulename))
  end

  # TODO: Test with ability to stop and return a value
  test "Hook run_fold chain is stopped when handler return 'stop'" do
    # setup test
    hookname = :test_mod_hook
    modulename = :hook_module
    mock(modulename, :hook_callback1, fn(_acc) -> :stop end)
    mock(modulename, :hook_callback2, fn(_acc) -> :executed end)

    :ok = :ejabberd_hooks.add(hookname, @host, modulename, :hook_callback1, 40)
    :ok = :ejabberd_hooks.add(hookname, @host, modulename, :hook_callback2, 50)

    :stopped = :ejabberd_hooks.run_fold(hookname, @host, :started, [])
    # Only one module has been called
    [{_pid, {^modulename, :hook_callback1, [:started]}, :stop}] = :meck.history(modulename)
  end

  test "Error in run_fold is ignored" do
    run_fold_crash(fn(_acc) -> raise "crashed" end)
  end

  test "Throw in run_fold is ignored" do
    run_fold_crash(fn(_acc) -> throw :crashed end)
  end

  test "Exit in run_fold is ignored" do
    run_fold_crash(fn(_acc) -> exit :crashed end)
  end

  # test for run hook with various number of params
  def run_hook(params, fun, result) do
    # setup test
    hookname = :test_mod_hook
    modulename = :hook_module
    callback = :hook_callback
    mock(modulename, callback, fun)

    # Then check
    :ok = :ejabberd_hooks.add(hookname, @host, modulename, callback, 40)
    :ok = :ejabberd_hooks.run(hookname, @host, params)
    [{_pid, {^modulename, ^callback, ^params}, ^result}] = :meck.history(modulename)
  end

  def run_fold_crash(crash_fun) do
    # setup test
    hookname = :test_mod_hook
    modulename = :hook_module
    mock(modulename, :hook_callback1, crash_fun)
    mock(modulename, :hook_callback2, fn(_acc) -> :final end)

    :ok = :ejabberd_hooks.add(hookname, @host, modulename, :hook_callback1, 40)
    :ok = :ejabberd_hooks.add(hookname, @host, modulename, :hook_callback2, 50)

    :final = :ejabberd_hooks.run_fold(hookname, @host, :started, [])
    # Both handlers were called
    2 = length(:meck.history(modulename))
  end

  # TODO refactor: Move to ejabberd_test_mock
  def mock(module, function, fun) do
    try do
      :meck.new(module, [:non_strict])
    catch
      :error, {:already_started, _pid} -> :ok
    end

    :meck.expect(module, function, fun)
  end

end
