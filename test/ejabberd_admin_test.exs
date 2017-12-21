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
		:ejabberd_mnesia.start
		# For some myterious reason, :ejabberd_commands.init mays
		# sometimes fails if module is not loaded before
		{:module, :ejabberd_commands} = Code.ensure_loaded(:ejabberd_commands)
		:ejabberd_hooks.start_link
		{:ok, _} = :acl.start_link
		{:ok, _} = :ejabberd_access_permissions.start_link()
		:ejabberd_commands.start_link
		:ejabberd_admin.start_link
		:ok
	end

	setup do
		:ok
	end

	test "Logvel can be set and retrieved" do
		:ejabberd_logger.start()

		assert :lager == call_command(:set_loglevel, [1])
		assert {1, :critical, 'Critical'} ==
			call_command(:get_loglevel, [])

		assert :lager == call_command(:set_loglevel, [2])
		assert {2, :error, 'Error'} ==
			call_command(:get_loglevel, [])

		assert :lager == call_command(:set_loglevel, [3])
		assert {3, :warning, 'Warning'} ==
			call_command(:get_loglevel, [])

#		assert {:wrong_loglevel, 6} ==
#			catch_throw call_command(:set_loglevel, [6])
#		assert {3, :warning, 'Warning'} ==
#			call_command(:get_loglevel, [])

		assert :lager == call_command(:set_loglevel, [4])
		assert {4, :info, 'Info'} ==
			call_command(:get_loglevel, [])

		assert :lager == call_command(:set_loglevel, [5])
		assert {5, :debug, 'Debug'} ==
			call_command(:get_loglevel, [])

		assert :lager == call_command(:set_loglevel, [0])
		assert {0, :no_log, 'No log'} ==
			call_command(:get_loglevel, [])

	end

	defp call_command(name, args) do
	  :ejabberd_commands.execute_command2(name, args, %{:caller_module => :ejabberd_ctl})
	end

	test "command status works with ejabberd stopped" do
		assert :ejabberd_not_running ==
			elem(call_command(:status, []), 0)
	end

end
