		#  mod_last mock
	######################
	

defmodule ModLastMock do

	require Record
	Record.defrecord :session, Record.extract(:session,
																						from: "ejabberd_sm.hrl")
	Record.defrecord :jid, Record.extract(:jid,
																				from: "jlib.hrl")
	
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
