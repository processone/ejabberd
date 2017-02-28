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

## TODO Fix next test error: add admin user ACL

defmodule EjabberdCommandsMockTest do
	use ExUnit.Case, async: false

  require EjabberdOauthMock

	@author "jsautret@process-one.net"

	# mocked callback module
	@module :test_module
	# Admin user
	@admin "admin"
	@adminpass "adminpass"
	# Non admin user
	@user "user"
	@userpass "userpass"
	# XMPP domain
	@domain "domain"

	require Record
	Record.defrecord :ejabberd_commands, Record.extract(:ejabberd_commands,	from_lib: "ejabberd/include/ejabberd_commands.hrl")

	setup_all do
        :ok = :ejabberd.start_app(:lager)
		try do
			:stringprep.start
		rescue
			_ -> :ok
		end
		:mnesia.start
        {:ok, _} = :jid.start
        :ejabberd_hooks.start_link
        :ok = :ejabberd_config.start(["domain1", "domain2"], [])
        {:ok, _} = :ejabberd_access_permissions.start_link()
        {:ok, _} = :acl.start_link
		EjabberdOauthMock.init
        on_exit fn -> :meck.unload end
	end

	setup do
		:meck.unload
		:meck.new(@module, [:non_strict])
		:mnesia.delete_table(:ejabberd_commands)
		:ejabberd_commands.start_link
		:ok
	end

	test "API command can be registered, listed and unregistered" do
		command = ejabberd_commands name: :test, module: @module,
		          function: :test_command

		assert :ok == :ejabberd_commands.register_commands [command]
		commands = :ejabberd_commands.list_commands
		assert Enum.member? commands, {:test, [], ''}

		assert :ok == :ejabberd_commands.unregister_commands [command]
		commands = :ejabberd_commands.list_commands
		refute Enum.member? commands, {:test, [], ''}
	end


	test "API command with versions can be registered, listed and unregistered" do
		command1 = ejabberd_commands name: :test, module: @module,
		function: :test_command, version: 1, desc: 'version1'
		command3 = ejabberd_commands name: :test, module: @module,
		function: :test_command, version: 3, desc: 'version3'
		assert :ejabberd_commands.register_commands [command1, command3]

		version1 = {:test, [], 'version1'}
		version3 = {:test, [], 'version3'}

		# default version is latest one
		commands = :ejabberd_commands.list_commands
		refute Enum.member? commands, version1
		assert Enum.member? commands, version3

		# no such command in APIv0
		commands = :ejabberd_commands.list_commands 0
		refute Enum.member? commands, version1
		refute Enum.member? commands, version3

		commands = :ejabberd_commands.list_commands 1
		assert Enum.member? commands, version1
		refute Enum.member? commands, version3

		commands = :ejabberd_commands.list_commands 2
		assert Enum.member? commands, version1
		refute Enum.member? commands, version3

		commands = :ejabberd_commands.list_commands 3
		refute Enum.member? commands, version1
		assert Enum.member? commands, version3

		commands = :ejabberd_commands.list_commands 4
		refute Enum.member? commands, version1
		assert Enum.member? commands, version3

		assert :ok == :ejabberd_commands.unregister_commands [command1]

		commands = :ejabberd_commands.list_commands 1
		refute Enum.member? commands, version1
		refute Enum.member? commands, version3

		commands = :ejabberd_commands.list_commands 3
		refute Enum.member? commands, version1
		assert Enum.member? commands, version3

		assert :ok == :ejabberd_commands.unregister_commands [command3]

		commands = :ejabberd_commands.list_commands 1
		refute Enum.member? commands, version1
		refute Enum.member? commands, version3

		commands = :ejabberd_commands.list_commands 3
		refute Enum.member? commands, version1
		refute Enum.member? commands, version3
	end


	test "API command can be registered and executed" do
		mock_commands_config

		# Create & register a mocked command test() -> :result
		command_name = :test
		function = :test_command
		command = ejabberd_commands(name: command_name,
																module: @module,
																function: function)
		:meck.expect @module, function, fn -> :result end
		assert :ok == :ejabberd_commands.register_commands [command]

		assert :result == :ejabberd_commands.execute_command(command_name, [])

		assert :meck.validate @module
	end

	test "API command with versions can be registered and executed" do
		mock_commands_config

		command_name = :test

		function1 = :test_command1
		command1 = ejabberd_commands(name: command_name,
																 version: 1,
																 module: @module,
																 function: function1)
		:meck.expect(@module, function1, fn -> :result1 end)

		function3 = :test_command3
		command3 = ejabberd_commands(name: command_name,
																 version: 3,
																 module: @module,
																 function: function3)
		:meck.expect(@module, function3, fn -> :result3 end)

		assert :ok == :ejabberd_commands.register_commands [command1, command3]

		# default version is latest one
		assert :result3 == :ejabberd_commands.execute_command(command_name, [])
		# no such command in APIv0
		assert {:error, :unknown_command} ==
			catch_throw :ejabberd_commands.execute_command(command_name, [], 0)
		assert :result1 == :ejabberd_commands.execute_command(command_name, [], 1)
		assert :result1 == :ejabberd_commands.execute_command(command_name, [], 2)
		assert :result3 == :ejabberd_commands.execute_command(command_name, [], 3)
		assert :result3 == :ejabberd_commands.execute_command(command_name, [], 4)

		assert :meck.validate @module
	end



	test "API command with user policy" do
		mock_commands_config [:user, :admin]

		# Register a command test(user, domain) -> {:versionN, user, domain}
		# with policy=user and versions 1 & 3
		command_name = :test
		command1 = ejabberd_commands(name: command_name,
																 module: @module,
																 function: :test_command1,
																 policy: :user, version: 1)
		command3 = ejabberd_commands(name: command_name,
																 module: @module,
																 function: :test_command3,
																 policy: :user, version: 3)
		:meck.expect(@module, :test_command1,
			fn(user, domain) when is_binary(user) and is_binary(domain) ->
				{:version1, user, domain}
			end)
		:meck.expect(@module, :test_command3,
			fn(user, domain) when is_binary(user) and is_binary(domain) ->
				{:version3, user, domain}
			end)
		assert :ok == :ejabberd_commands.register_commands [command1, command3]

		# A normal user must not pass user info as parameter
		assert {:version1, @user, @domain} ==
			:ejabberd_commands.execute_command(:undefined,
																				 {@user, @domain,
																					@userpass, false},
																				 command_name,
																				 [], 2)
		assert {:version3, @user, @domain} ==
			:ejabberd_commands.execute_command(:undefined,
																				 {@user, @domain,
																					@userpass, false},
																				 command_name,
																				 [], 3)
		token = EjabberdOauthMock.get_token @user, @domain, command_name
		assert {:version3, @user, @domain} ==
			:ejabberd_commands.execute_command(:undefined,
																				 {@user, @domain,
																					{:oauth, token}, false},
																				 command_name,
																				 [], 4)
		# Expired oauth token
		token = EjabberdOauthMock.get_token @user, @domain, command_name, 1
		:timer.sleep 1500
		assert {:error, :invalid_account_data} ==
			catch_throw :ejabberd_commands.execute_command(:undefined,
																										 {@user, @domain,
																											{:oauth, token}, false},
																										 command_name,
																										 [], 4)
		# Wrong oauth scope
		token = EjabberdOauthMock.get_token @user, @domain, :bad_command
		assert {:error, :invalid_account_data} ==
			catch_throw :ejabberd_commands.execute_command(:undefined,
																										 {@user, @domain,
																											{:oauth, token}, false},
																										 command_name,
																										 [], 4)


		assert :function_clause ==
			catch_error :ejabberd_commands.execute_command(:undefined,
																										 {@user, @domain,
																											@userpass, false},
																										 command_name,
																										 [@user, @domain], 2)
		# @user is not admin
		assert {:error, :account_unprivileged} ==
			catch_throw :ejabberd_commands.execute_command(:undefined,
																										 {@user, @domain,
																											@userpass, true},
																										 command_name,
																										 [], 2)
		assert {:error, :account_unprivileged} ==
			catch_throw :ejabberd_commands.execute_command(:undefined,
																										 {@user, @domain,
																											@userpass, true},
																										 command_name,
																										 [@user, @domain], 2)
		assert {:error, :account_unprivileged} ==
			catch_throw :ejabberd_commands.execute_command(:undefined,
																										 {@user, @domain,
																											{:oauth, token}, true},
																										 command_name,
																										 [@user, @domain], 2)


		# An admin must explicitely pass user info
		assert {:version1, @user, @domain} ==
			:ejabberd_commands.execute_command(:undefined, :admin,
																				 command_name, [@user, @domain], 2)
		assert {:version3, @user, @domain} ==
			:ejabberd_commands.execute_command(:undefined, :admin,
																				 command_name, [@user, @domain], 4)
		assert {:version1, @user, @domain} ==
			:ejabberd_commands.execute_command(:undefined,
																				 {@admin, @domain, @adminpass, true},
																				 command_name, [@user, @domain], 1)
		token = EjabberdOauthMock.get_token @admin, @domain, command_name
		assert {:version3, @user, @domain} ==
			:ejabberd_commands.execute_command(:undefined,
																				 {@admin, @domain, {:oauth, token}, true},
																				 command_name, [@user, @domain], 3)
		# Wrong @admin password
		assert {:error, :account_unprivileged} ==
			catch_throw :ejabberd_commands.execute_command(:undefined,
																										 {@admin, @domain,
																											@adminpass<>"bad", true},
																										 command_name,
																										 [@user, @domain], 3)
		# @admin calling as a normal user
		assert {:version3, @admin, @domain} ==
			:ejabberd_commands.execute_command(:undefined,
																				 {@admin, @domain,
																					@adminpass, false},
																				 command_name, [], 5)
		assert {:version3, @admin, @domain} ==
			:ejabberd_commands.execute_command(:undefined,
																				 {@admin, @domain,
																					{:oauth, token}, false},
																				 command_name, [], 6)
		assert :function_clause ==
			catch_error :ejabberd_commands.execute_command(:undefined,
																										 {@admin, @domain,
																											@adminpass, false},
																										 command_name,
																										 [@user, @domain], 5)
		assert :meck.validate @module
	end


	test "API command with admin policy" do
		mock_commands_config [:admin]

		# Register a command test(user, domain) -> {user, domain}
		# with policy=admin
		command_name = :test
		function = :test_command
		command = ejabberd_commands(name: command_name,
																args: [{:user, :binary}, {:host, :binary}],
																module: @module,
																function: function,
																policy: :admin)
		:meck.expect(@module, function,
			fn(user, domain) when is_binary(user) and is_binary(domain) ->
				{user, domain}
			end)
		assert :ok == :ejabberd_commands.register_commands [command]

		# A normal user cannot call the command
		assert {:error, :account_unprivileged} ==
			catch_throw :ejabberd_commands.execute_command(:undefined,
																										 {@user, @domain,
																											@userpass, false},
																										 command_name,
																										 [@user, @domain])

		# An admin can call the command
		assert {@user, @domain} ==
			:ejabberd_commands.execute_command(:undefined,
																				 {@admin, @domain,
																					@adminpass, true},
																				 command_name,
																				 [@user, @domain])

		# An admin can call the command with oauth token
		token = EjabberdOauthMock.get_token @admin, @domain, command_name
		assert {@user, @domain} ==
			:ejabberd_commands.execute_command(:undefined,
																				 {@admin, @domain,
																					{:oauth, token}, true},
																				 command_name,
																				 [@user, @domain])


		# An admin with bad password cannot call the command
		assert {:error, :account_unprivileged} ==
			catch_throw :ejabberd_commands.execute_command(:undefined,
																										 {@admin, @domain,
																											"bad"<>@adminpass, false},
																										 command_name,
																										 [@user, @domain])

		# An admin cannot call the command with bad oauth token
		assert {:error, :account_unprivileged} ==
			catch_throw :ejabberd_commands.execute_command(:undefined,
																				 {@admin, @domain,
																					{:oauth, "bad"<>token}, true},
																				 command_name,
																				 [@user, @domain])

		# An admin as a normal user cannot call the command
		assert {:error, :account_unprivileged} ==
			catch_throw :ejabberd_commands.execute_command(:undefined,
																				 {@admin, @domain,
																					@adminpass, false},
																				 command_name,
																				 [@user, @domain])

		# An admin as a normal user cannot call the command with oauth token
		assert {:error, :account_unprivileged} ==
			catch_throw :ejabberd_commands.execute_command(:undefined,
																				 {@admin, @domain,
																					{:oauth, token}, false},
																				 command_name,
																				 [@user, @domain])

		assert :meck.validate @module
	end

  test "Commands can perform extra check on access" do
    mock_commands_config [:admin, :open]

		command_name = :test
		function = :test_command
		command = ejabberd_commands(name: command_name,
			args: [{:user, :binary}, {:host, :binary}],
      access: [:basic_rule_1],
			module: @module,
			function: function,
			policy: :open)
		:meck.expect(@module, function,
			fn(user, domain) when is_binary(user) and is_binary(domain) ->
				{user, domain}
			end)
		assert :ok == :ejabberd_commands.register_commands [command]

#    :acl.add(:global, :basic_acl_1, {:user, @user, @host})
#    :acl.add_access(:global, :basic_rule_1, [{:allow, [{:acl, :basic_acl_1}]}])

    assert {@user, @domain} ==
			:ejabberd_commands.execute_command(:undefined,
        {@user, @domain,
				 @userpass, false},
				command_name,
				[@user, @domain])
    assert {@user, @domain} ==
			:ejabberd_commands.execute_command(:undefined,
        {@admin, @domain,
				 @adminpass, false},
				command_name,
				[@user, @domain])

  end

	##########################################################
	# Utils

	# Mock a config where only @admin user is allowed to call commands
	# as admin
	def mock_commands_config(commands \\ []) do
		EjabberdAuthMock.init
		EjabberdAuthMock.create_user @user, @domain, @userpass
		EjabberdAuthMock.create_user @admin, @domain, @adminpass

		:meck.new :ejabberd_config
		:meck.expect(:ejabberd_config, :get_option,
			fn(:commands_admin_access, _, _) -> :commands_admin_access
			  (:oauth_access, _, _) -> :all
        (:commands, _, _) -> [{:add_commands, commands}]
				(_, _, default) -> default
			end)
		:meck.expect(:ejabberd_config, :get_myhosts,
			fn() -> [@domain]	end)

		:meck.new :acl
		:meck.expect(:acl, :access_matches,
			fn(:commands_admin_access, info, _scope) ->
        case info do
          %{usr: {@admin, @domain, _}} -> :allow
          _ -> :deny
        end;
			  (:all, _, _scope) ->
					:allow
			end)
	end

end
