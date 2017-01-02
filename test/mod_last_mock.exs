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

defmodule ModLastMock do

	require Record
	Record.defrecord :session, Record.extract(:session, from_lib: "ejabberd/include/ejabberd_sm.hrl")
	Record.defrecord :jid, Record.extract(:jid, from_lib: "ejabberd/include/jlib.hrl")

  @author "jsautret@process-one.net"
	@agent __MODULE__

	def init do
    try do
			Agent.stop(@agent)
    catch
      :exit, _e -> :ok
    end

		{:ok, _pid} = Agent.start_link(fn -> %{} end, name: @agent)

    mock(:mod_last, :get_last_info,
			fn (user, domain)  ->
				Agent.get(@agent, fn last ->
					case Map.get(last, {user, domain}, :not_found) do
						{ts, status} -> {:ok, ts, status}
						result -> result
					end
				end)
			end)
	end

	def set_last(user, domain, status) do
		set_last(user, domain, status, now)
	end

	def set_last(user, domain, status, timestamp) do
		Agent.update(@agent, fn last ->
			Map.put(last, {user, domain}, {timestamp, status})
		end)
	end

	####################################################################
	#     Helpers
	####################################################################
	def now() do
		{megasecs, secs, _microsecs} = :os.timestamp
		megasecs * 1000000 + secs
	end

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
