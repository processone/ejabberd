# ----------------------------------------------------------------------
#
# ejabberd, Copyright (C) 2002-2016   ProcessOne
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
    :ok = :ejabberd_config.start(["localhost"], [])

    :ok = :ejabberd_commands.init

    :ok = :ejabberd_commands.register_commands(cmds)
    on_exit fn -> unregister_commands(cmds) end
  end

  test "We can call open commands without authentication" do
    :ejabberd_config.add_local_option(:commands, [[{:add_commands, [:open_cmd]}]])
    request = request(method: :POST, data: "[]")
    {200, _, _} = :mod_http_api.process(["open_cmd"], request)
  end

  # This related to the commands config file option
  test "Attempting to access a command that is not exposed as HTTP API returns 401" do
    :ejabberd_config.add_local_option(:commands, [])
    request = request(method: :POST, data: "[]")
    {401, _, _} = :mod_http_api.process(["open_cmd"], request)
  end

  test "Call to user commands without authentication are rejected" do
    :ejabberd_config.add_local_option(:commands, [[{:add_commands, [:user_cmd]}]])
    request = request(method: :POST, data: "[]")
    {401, _, _} = :mod_http_api.process(["user_cmd"], request)
  end

  # Define a set of test commands that we expose through API
  defp cmds do
    # TODO Refactor
    [ejabberd_commands(name: :open_cmd, tags: [:test],
                       policy: :open,
                       module: __MODULE__,
                       function: :open_cmd_fun,
                       args: [],
                       result: {:res, :rescode}),
     ejabberd_commands(name: :user_cmd, tags: [:test],
                       policy: :user,
                       module: __MODULE__,
                       function: :user_cmd_fun,
                       args: [],
                       result: {:res, :rescode})
     ]
  end

  def open_cmd_fun, do: :ok
  def user_cmd_fun, do: :ok

  defp unregister_commands(commands) do
    try do
      :ejabberd_commands.unregister_commands(commands)
    catch
      _,_ -> :ok
    end
  end

end
