	#  ejabberd_auth mock
	######################

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
			fn (user, domain, password)  ->
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
