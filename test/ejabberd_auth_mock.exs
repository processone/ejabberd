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

defmodule EjabberdAuthMock do

	@author "jsautret@process-one.net"
	@agent __MODULE__

	def init do
		try do
			Agent.stop(@agent)
		catch
			:exit, _e -> :ok
		end

		{:ok, _pid} = Agent.start_link(fn -> %{} end, name: @agent)

		mock(:ejabberd_auth, :is_user_exists,
			fn (user, domain)  ->
				Agent.get(@agent, fn users -> Map.get(users, {user, domain}) end) != nil
			end)
		mock(:ejabberd_auth, :get_password_s,
			fn (user, domain)  ->
				Agent.get(@agent, fn users -> Map.get(users, {user, domain}, "") end )
			end)
		mock(:ejabberd_auth, :check_password,
			fn (user, _authzid, domain, password)  ->
				Agent.get(@agent, fn users ->
					Map.get(users, {user, domain}) end) == password
			end)
		mock(:ejabberd_auth, :set_password,
			fn (user, domain, password)  ->
				Agent.update(@agent, fn users ->
					Map.put(users, {user, domain}, password) end)
			end)
	end

	def create_user(user, domain, password) do
		Agent.update(@agent, fn users -> Map.put(users, {user, domain}, password) end)
	end

	####################################################################
	#     Helpers
	####################################################################

	# TODO refactor: Move to ejabberd_test_mock
	def mock(module, function, fun) do
		try do
			:meck.new(module)
		catch
			:error, {:already_started, _pid} -> :ok
		end

		:meck.expect(module, function, fun)
	end

end
