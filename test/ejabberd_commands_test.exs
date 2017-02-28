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

defmodule EjabberdCommandsTest do
  @author "mremond@process-one.net"

  use ExUnit.Case, async: true

  require Record
  Record.defrecord :ejabberd_commands, Record.extract(:ejabberd_commands, from_lib: "ejabberd/include/ejabberd_commands.hrl")

  setup_all do
    :mnesia.start
    :stringprep.start
    :ok = :ejabberd_config.start(["localhost"], [])
    {:ok, _} = :ejabberd_access_permissions.start_link()

    :ejabberd_commands.start_link
    :ok
  end

  test "Check that we can register a command" do
    :ok = :ejabberd_commands.register_commands([user_test_command])
    commands = :ejabberd_commands.list_commands
    assert Enum.member?(commands, {:test_user, [], "Test user"})
  end

  test "get_exposed_commands/0 returns registered commands" do
    commands = [open_test_command]
    :ok = :ejabberd_commands.register_commands(commands)
    :ok = :ejabberd_commands.expose_commands(commands)
    exposed_commands = :ejabberd_commands.get_exposed_commands
    assert Enum.member?(exposed_commands, :test_open)
  end

  test "Check that admin commands are rejected with noauth credentials" do
    :ok = :ejabberd_commands.register_commands([admin_test_command])

    assert catch_throw(:ejabberd_commands.execute_command(:undefined, :noauth, :test_admin, [])) == {:error, :account_unprivileged}

    # Command executed from ejabberdctl passes anyway with access commands trick
    # TODO: We should refactor to have explicit call when bypassing auth check for command-line
    :ok = :ejabberd_commands.execute_command([], :noauth, :test_admin, [])
  end

  # TODO Test that we can add command to list of expose commands
  # This can be done with:
  # ejabberd_config:add_local_option(commands, [[{add_commands, [open_cmd]}]]).

#  test "Check that a user can use a user command" do
#    [Command] = ets:lookup(ejabberd_commands, test_user),
#    AccessCommands = ejabberd_commands:get_access_commands(undefined),
#    ejabberd_commands:check_access_commands(AccessCommands, {<<"test">>,<<"localhost">>, {oauth,<<"MyToken">>}, false}, test_user, Command, []).
#  end

  defp user_test_command do
    ejabberd_commands(name: :test_user, tags: [:roster],
                      desc: "Test user",
                      policy: :user,
                      module: __MODULE__,
                      function: :test_user,
                      args: [],
                      result: {:contacts, {:list, {:contact, {:tuple, [
                                                                 {:jid, :string},
                                                                 {:nick, :string}
                                                               ]}}}})
  end

  defp open_test_command do
    ejabberd_commands(name: :test_open, tags: [:test],
                      desc: "Test open",
                      policy: :open,
                      module: __MODULE__,
                      function: :test_open,
                      args: [],
                      result: {:res, :rescode})
  end

  defp admin_test_command do
    ejabberd_commands(name: :test_admin, tags: [:roster],
                      desc: "Test admin",
                      policy: :restricted,
                      module: __MODULE__,
                      function: :test_admin,
                      args: [],
                      result: {:res, :rescode})
  end

  def test_admin, do: :ok
end
