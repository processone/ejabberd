	#  mod_roster mock
	######################

defmodule ModRosterMock do
	@author "jsautret@process-one.net"

	require Record
	Record.defrecord :roster, Record.extract(:roster,
																					 from: "mod_roster.hrl")

	@agent __MODULE__

	def init(domain, module) do
		try do
			Agent.stop(@agent)
		catch
			:exit, _e -> :ok
		end

		{:ok, _pid} = Agent.start_link(fn -> %{} end, name: @agent)

		mock_with_moka module

		#:mod_roster.stop(domain)
		:mod_roster.start(domain, [])
	end

	def mock_with_moka(module) do
		try do

			module_mock = :moka.start(module)
			:moka.replace(module_mock, :mod_roster, :invalidate_roster_cache,
				fn (_user, _server)  ->
					:ok
				end)

			:moka.load(module_mock)

			roster_mock = :moka.start(:mod_roster)

			:moka.replace(roster_mock, :gen_mod, :db_type,
				fn (_host, _opts)  ->
					{:none}
				end)

			:moka.replace(roster_mock, :gen_iq_handler, :add_iq_handler,
				fn (_module, _host, _ns, _m, _f, _iqdisc)  ->
					:ok
				end)

			:moka.replace(roster_mock, :gen_iq_handler, :remove_iq_handler,
				fn (_module, _host, _ns)  ->
					:ok
				end)

			:moka.replace(roster_mock, :transaction,
				fn (_server, function)  ->
					{:atomic, function.()}
				end)

			:moka.replace(roster_mock, :get_roster,
				fn (user, domain)  ->
					to_records(get_roster(user, domain))
				end)

			:moka.replace(roster_mock, :update_roster_t,
				fn (user, domain, {u, d, _r}, item)  ->
					add_roster_item(user, domain, u<>"@"<>d,
													roster(item, :name),
													roster(item, :subscription),
													roster(item, :groups),
													roster(item, :ask),
													roster(item, :askmessage))
				end)

			:moka.replace(roster_mock, :del_roster_t,
				fn (user, domain, jid)  ->
					remove_roster_item(user, domain, :jlib.jid_to_string(jid))
				end)

			:moka.load(roster_mock)

		catch
			{:already_started, _pid} -> :ok
		end

	end

	def	mock_with_meck do
#		mock(:gen_mod, :db_type,
#			fn (_server, :mod_roster)  ->
#				:mnesia
#				 end)
#
#		mock(:mnesia, :transaction,
#			fn (_server, function)  ->
#				{:atomic, function.()}
#			end)
#
#		mock(:mnesia, :write,
#			fn (Item)  ->
#				throw Item
#				{:atomic, :ok}
#			end)

		mock(:mod_roster, :transaction,
			fn (_server, function)  ->
				{:atomic, function.()}
			end)

		mock(:mod_roster, :update_roster_t,
				 fn (user, domain, {u, d, _r}, item) ->
					 add_roster_item(user, domain, u<>"@"<>d,
													 roster(item, :name),
													 roster(item, :subscription),
													 roster(item, :groups),
													 roster(item, :ask),
													 roster(item, :askmessage))
				 end)

		mock(:mod_roster, :invalidate_roster_cache,
			fn (_user, _server)  ->
				:ok
			end)

	end

	def add_roster_item(user, domain, jid, nick, subs \\ :none, groups \\ [],
											ask \\ :none, askmessage \\ "")
	when is_binary(user) and byte_size(user) > 0
	and  is_binary(domain) and byte_size(domain) > 0
	and  is_binary(jid) and byte_size(jid) > 0
	and  is_binary(nick)
	and  is_atom(subs)
	and  is_list(groups)
	and  is_atom(ask)
	and  is_binary(askmessage)
		do
		Agent.update(@agent, fn roster ->
			Map.put(roster, {user, domain, jid}, %{nick: nick,
																						 subs: subs, groups: groups,
																						 ask: ask,	askmessage: askmessage})
		end)
	end

	def remove_roster_item(user, domain, jid) do
		Agent.update(@agent, fn roster ->
			Map.delete(roster, {user, domain, jid})
		end)
	end

	def get_rosters() do
		Agent.get(@agent, fn roster -> roster end)
	end

	def get_roster(user, domain) do
		Agent.get(@agent, fn roster ->
		for {u, d, jid} <- Map.keys(roster), u == user, d == domain,
				do: {{u, d, jid}, Map.fetch!(roster, {u, d, jid})}
		end)
	end

	def	to_record({{user, domain, jid}, r}) do
		roster(usj: {user, domain, jid},
					 us: {user, domain},
					 jid: :jlib.string_to_usr(jid),
					 subscription: r.subs,
					 ask: r.ask,
					 groups: r.groups,
					 askmessage: r.askmessage
		)
	end
	def to_records(rosters) do
		for item <- rosters, do: to_record(item)
	end

####################################################################
#     Helpers
####################################################################

	# TODO refactor: Move to ejabberd_test_mock
	def mock(module, function, fun) do
		try do
			:meck.new(module, [:non_strict, :passthrough, :unstick])
		catch
			:error, {:already_started, _pid} -> :ok
		end

		:meck.expect(module, function, fun)
	end

end
