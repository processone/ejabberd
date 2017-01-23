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

defmodule EjabberdSmMock do
  @author "jsautret@process-one.net"

	require Record
	Record.defrecord :session, Record.extract(:session, from_lib: "ejabberd/include/ejabberd_sm.hrl")
	Record.defrecord :jid, Record.extract(:jid, from_lib: "ejabberd/include/jlib.hrl")

	@agent __MODULE__

	def init do
		ModLastMock.init

		try do
			Agent.stop(@agent)
    catch
      :exit, _e -> :ok
    end

		{:ok, _pid} = Agent.start_link(fn -> [] end, name: @agent)

    mock(:ejabberd_sm, :get_user_resources,
			fn (user, domain)  -> for s <- get_sessions(user, domain), do: s.resource end)

    mock(:ejabberd_sm, :route,
      fn (to, {:exit, _reason})  ->
        user = jid(to, :user)
        domain = jid(to, :server)
        resource = jid(to, :resource)
        disconnect_resource(user, domain, resource)
        :ok
        (_, _) -> :ok
      end)

	end

	def connect_resource(user, domain, resource,
											 opts \\ [priority: 1, conn: :c2s]) do
		Agent.update(@agent, fn sessions ->
			session = %{user: user, domain: domain, resource: resource,
									timestamp: :os.timestamp, pid: self, node: node,
									auth_module: :ejabberd_auth, ip: :undefined,
									priority: opts[:priority], conn: opts[:conn]}
			[session | sessions]
		end)
	end

	def disconnect_resource(user, domain, resource) do
		disconnect_resource(user, domain, resource, ModLastMock.now)
	end

	def disconnect_resource(user, domain, resource, timestamp) do
		Agent.update(@agent, fn sessions ->
		for s <- sessions,
			s.user != user or s.domain != domain or s.resource != resource, do: s
		end)
		ModLastMock.set_last user, domain, "", timestamp
	end

	def get_sessions() do
		Agent.get(@agent, fn sessions -> sessions end)
	end

	def get_sessions(user, domain) do
		Agent.get(@agent, fn sessions ->
		for s <- sessions, s.user == user, s.domain == domain, do: s
		end)
	end

	def get_session(user, domain, resource) do
		Agent.get(@agent, fn sessions ->
		for s <- sessions,
			s.user == user, s.domain == domain,	s.resource == resource, do: s
		end)
	end

	def to_record(s) do
		session(usr: {s.user, s.domain, s.ressource},
						us: {s.user, s.domain},
						sid: {s.timestamp, s.pid},
						priority: s.priority,
						info: [conn: s.conn, ip: s.ip, node: s.node,
									 oor: false, auth_module: s.auth_module])
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
