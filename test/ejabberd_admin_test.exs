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

defmodule EjabberdAdminTest do
	use ExUnit.Case, async: false

	@author "jsautret@process-one.net"

	setup_all do
		:mnesia.start
		# For some myterious reason, :ejabberd_commands.init mays
		# sometimes fails if module is not loaded before
		{:module, :ejabberd_commands} = Code.ensure_loaded(:ejabberd_commands)
		{:ok, _} = :ejabberd_access_permissions.start_link()
		:ejabberd_commands.init
		:ejabberd_admin.start
		:ok
	end

	setup do
		:ok
	end

	test "Logvel can be set and retrieved" do
		:ejabberd_logger.start()

		assert :lager == :ejabberd_commands.execute_command(:set_loglevel, [1])
		assert {1, :critical, 'Critical'} ==
			:ejabberd_commands.execute_command(:get_loglevel, [])

		assert :lager == :ejabberd_commands.execute_command(:set_loglevel, [2])
		assert {2, :error, 'Error'} ==
			:ejabberd_commands.execute_command(:get_loglevel, [])

		assert :lager == :ejabberd_commands.execute_command(:set_loglevel, [3])
		assert {3, :warning, 'Warning'} ==
			:ejabberd_commands.execute_command(:get_loglevel, [])

		assert {:wrong_loglevel, 6} ==
			catch_throw :ejabberd_commands.execute_command(:set_loglevel, [6])
		assert {3, :warning, 'Warning'} ==
			:ejabberd_commands.execute_command(:get_loglevel, [])

		assert :lager == :ejabberd_commands.execute_command(:set_loglevel, [4])
		assert {4, :info, 'Info'} ==
			:ejabberd_commands.execute_command(:get_loglevel, [])

		assert :lager == :ejabberd_commands.execute_command(:set_loglevel, [5])
		assert {5, :debug, 'Debug'} ==
			:ejabberd_commands.execute_command(:get_loglevel, [])

		assert :lager == :ejabberd_commands.execute_command(:set_loglevel, [0])
		assert {0, :no_log, 'No log'} ==
			:ejabberd_commands.execute_command(:get_loglevel, [])

	end

	test "command status works with ejabberd stopped" do
		assert :ejabberd_not_running ==
			elem(:ejabberd_commands.execute_command(:status, []), 0)
	end

end
