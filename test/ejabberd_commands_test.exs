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

defmodule EjabberdCommandsTest do
  @author "mremond@process-one.net"

  use ExUnit.Case, async: true

  require Record
  Record.defrecord :ejabberd_commands, Record.extract(:ejabberd_commands, from_lib: "ejabberd/include/ejabberd_commands.hrl")

  setup_all do
    :ejabberd_commands.init
  end

  test "Check that we can register a command" do
    assert :ejabberd_commands.register_commands([user_test_command]) == :ok
    commands = :ejabberd_commands.list_commands
    assert Enum.member?(commands, {:test_user, [], "Test user"})
  end

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
end
