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

defmodule ModHttpApiTest do
  @author "mremond@process-one.net"

  use ExUnit.Case, async: true

  require Record
  Record.defrecord :request, Record.extract(:request, from_lib: "ejabberd/include/ejabberd_http.hrl")
  Record.defrecord :ejabberd_commands, Record.extract(:ejabberd_commands, from_lib: "ejabberd/include/ejabberd_commands.hrl")

  setup_all do
    :ok = :mnesia.start
    :stringprep.start
    :ok = :ejabberd_config.start(["localhost"], [])
    {:ok, _} = :ejabberd_access_permissions.start_link()
    :ok = :ejabberd_commands.init
    :ok = :ejabberd_commands.register_commands(cmds)
    on_exit fn ->
      :meck.unload
      unregister_commands(cmds) end
  end

  test "We can expose several commands to API at a time" do
    setup_mocks()
    :ejabberd_commands.expose_commands([:open_cmd, :user_cmd])
    commands = :ejabberd_commands.get_exposed_commands()
    assert Enum.member?(commands, :open_cmd)
    assert Enum.member?(commands, :user_cmd)
  end

#  test "We can call open commands without authentication" do
#    setup_mocks()
#    :ejabberd_commands.expose_commands([:open_cmd])
#    request = request(method: :POST, ip: {{127,0,0,1},50000}, data: "[]")
#    {200, _, _} = :mod_http_api.process(["open_cmd"], request)
#  end

  # This related to the commands config file option
  test "Attempting to access a command that is not exposed as HTTP API returns 403" do
    setup_mocks()
    :ejabberd_commands.expose_commands([])
    request = request(method: :POST, ip: {{127,0,0,1},50000}, data: "[]")
    {403, _, _} = :mod_http_api.process(["open_cmd"], request)
  end

  test "Call to user, admin or restricted commands without authentication are rejected" do
    setup_mocks()
    :ejabberd_commands.expose_commands([:user_cmd, :admin_cmd, :restricted])
    request = request(method: :POST, ip: {{127,0,0,1},50000}, data: "[]")
    {403, _, _} = :mod_http_api.process(["user_cmd"], request)
    {403, _, _} = :mod_http_api.process(["admin_cmd"], request)
    {403, _, _} = :mod_http_api.process(["restricted_cmd"], request)
  end

  @tag pending: true
  test "If admin_ip_access is enabled, we can call restricted API without authentication from that IP" do
    setup_mocks()
  end

  # Define a set of test commands that we expose through API
  # We define one for each policy type
  defp cmds do
    [:open, :user, :admin, :restricted]
    |> Enum.map(&({&1, String.to_atom(to_string(&1) <> "_cmd")}))
    |> Enum.map(fn({cmd_type, cmd}) ->
      ejabberd_commands(name: cmd, tags: [:test],
                        policy: cmd_type,
                        module: __MODULE__,
                        function: cmd,
                        args: [],
                        result: {:res, :rescode})
    end)
  end

  def open_cmd, do: :ok
  def user_cmd(_, _), do: :ok
  def admin_cmd, do: :ok
  def restricted_cmd, do: :ok

  defp setup_mocks() do
    :meck.unload
    mock(:gen_mod, :get_module_opt,
      fn (_server, :mod_http_api, _admin_ip_access, _, _)  ->
        [{:allow, [{:ip, {{127,0,0,2}, 32}}]}]
      end)
  end

  defp mock(module, function, fun) do
    try do
      :meck.new(module)
    catch
      :error, {:already_started, _pid} -> :ok
    end
    :meck.expect(module, function, fun)
  end

  defp unregister_commands(commands) do
    try do
      :ejabberd_commands.unregister_commands(commands)
    catch
      _,_ -> :ok
    end
  end

end
