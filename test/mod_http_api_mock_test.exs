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

defmodule ModHttpApiMockTest do
	use ExUnit.Case, async: false

	@author "jsautret@process-one.net"

	# Admin user
	@admin "admin"
	@adminpass "adminpass"
	# Non admin user
	@user "user"
	@userpass "userpass"
	# XMPP domain
	@domain "domain"
	# mocked command
	@command "command_test"
	@acommand String.to_atom(@command)
	# default API version
	@version 0

	require Record
	Record.defrecord :request, Record.extract(:request, from_lib: "ejabberd/include/ejabberd_http.hrl")

	setup_all do
		try do
      :jid.start
      :mnesia.start
			:stringprep.start
      :ejabberd_config.start([@domain], [])
      {:ok, _} = :ejabberd_access_permissions.start_link()
      :ejabberd_commands.init
		rescue
			_ -> :ok
		end
		:mod_http_api.start(@domain, [])
		EjabberdOauthMock.init
		:ok
	end

	setup do
		:meck.unload
		:meck.new :ejabberd_commands
		:meck.new(:acl, [:passthrough])  # Need to fake acl to allow oauth
		EjabberdAuthMock.init
		:ok
	end

	test "HTTP GET simple command call with Basic Auth" do
		EjabberdAuthMock.create_user @user, @domain, @userpass

		# Mock a simple command() -> :ok
		:meck.expect(:ejabberd_commands, :get_command_format,
			fn (@acommand, %{usr: {@user, @domain, _}}, @version) ->
				{[], {:res, :rescode}}
			end)
    :meck.expect(:ejabberd_commands, :get_command_policy_and_scope,
			fn (@acommand) -> {:ok, :user, [:erlang.atom_to_binary(@acommand,:utf8)]} end)
		:meck.expect(:ejabberd_commands, :get_exposed_commands,
			fn () -> [@acommand] end)
		:meck.expect(:ejabberd_commands, :execute_command2,
			fn (@acommand, [], %{usr: {@user, @domain, _}}, @version) ->
				:ok
			end)
		:meck.expect(:ejabberd_commands, :execute_command,
			fn (:undefined, {@user, @domain, @userpass, false}, @acommand, [], @version, _) ->
				:ok
			end)

    :ejabberd_config.add_local_option(:commands, [[{:add_commands, [@acommand]}]])

		# Correct Basic Auth call
		req = request(method: :GET,
									path: ["api", @command],
									q: [nokey: ""],
									# Basic auth
									auth: {@user<>"@"<>@domain, @userpass},
									ip: {{127,0,0,1},60000},
									host: @domain)
		result = :mod_http_api.process([@command], req)

    # history = :meck.history(:ejabberd_commands)

		assert 200 == elem(result, 0) # HTTP code
		assert "0" == elem(result, 2) # command result

		# Bad password
		req = request(method: :GET,
									path: ["api", @command],
									q: [nokey: ""],
									# Basic auth
									auth: {@user<>"@"<>@domain, @userpass<>"bad"},
									ip: {{127,0,0,1},60000},
									host: @domain)
		result = :mod_http_api.process([@command], req)
		assert 401 == elem(result, 0) # HTTP code

		# Check that the command was executed only once
		assert 1 ==
			:meck.num_calls(:ejabberd_commands, :execute_command2, :_)

		assert :meck.validate :ejabberd_auth
		assert :meck.validate :ejabberd_commands
	end

	test "HTTP GET simple command call with OAuth" do
		EjabberdAuthMock.create_user @user, @domain, @userpass

		# Mock a simple command() -> :ok
		:meck.expect(:ejabberd_commands, :get_command_format,
			fn (@acommand, %{usr: {@user, @domain, _}}, @version) ->
				{[], {:res, :rescode}}
			end)
    :meck.expect(:ejabberd_commands, :get_command_policy_and_scope,
			fn (@acommand) -> {:ok, :user, [:erlang.atom_to_binary(@acommand,:utf8), "ejabberd:user"]} end)
		:meck.expect(:ejabberd_commands, :get_exposed_commands,
			fn () -> [@acommand] end)
		:meck.expect(:ejabberd_commands, :execute_command2,
			fn (@acommand, [], %{usr: {@user, @domain, _}, oauth_scope: ["ejabberd:user"]}, @version) ->
				:ok
				(@acommand, [], %{usr: {@user, @domain, _}, oauth_scope: [@command]}, @version) ->
					:ok
				(@acommand, [], %{usr: {@user, @domain, _}, oauth_scope: _}, @version) ->
					throw({:error, :access_rules_unauthorized})
			end)
		:meck.expect(:ejabberd_commands, :execute_command,
			fn (:undefined, {@user, @domain, {:oauth, _token}, false},
					@acommand, [], @version, _) ->
					:ok
			end)


		# Correct OAuth call using specific scope
		token = EjabberdOauthMock.get_token @user, @domain, @command
		req = request(method: :GET,
									path: ["api", @command],
									q: [nokey: ""],
									# OAuth
									auth: {:oauth, token, []},
									ip: {{127,0,0,1},60000},
									host: @domain)
		result = :mod_http_api.process([@command], req)
		assert 200 == elem(result, 0) # HTTP code
		assert "0" == elem(result, 2) # command result

		# Correct OAuth call using specific ejabberd:user scope
		token = EjabberdOauthMock.get_token @user, @domain, "ejabberd:user"
		req = request(method: :GET,
									path: ["api", @command],
									q: [nokey: ""],
									# OAuth
									auth: {:oauth, token, []},
									ip: {{127,0,0,1},60000},
									host: @domain)
		result = :mod_http_api.process([@command], req)
		assert 200 == elem(result, 0) # HTTP code
		assert "0" == elem(result, 2) # command result

		# Wrong OAuth token
		req = request(method: :GET,
									path: ["api", @command],
									q: [nokey: ""],
									# OAuth
									auth: {:oauth, "bad"<>token, []},
									ip: {{127,0,0,1},60000},
									host: @domain)
		result = :mod_http_api.process([@command], req)
		assert 401 == elem(result, 0) # HTTP code

		# Expired OAuth token
		token = EjabberdOauthMock.get_token @user, @domain, @command, 1
		:timer.sleep 1500
		req = request(method: :GET,
									path: ["api", @command],
									q: [nokey: ""],
									# OAuth
									auth: {:oauth, token, []},
									ip: {{127,0,0,1},60000},
									host: @domain)
		result = :mod_http_api.process([@command], req)
		assert 401 == elem(result, 0) # HTTP code

		# Wrong OAuth scope
		token = EjabberdOauthMock.get_token @user, @domain, "bad_command"
		:timer.sleep 1500
		req = request(method: :GET,
									path: ["api", @command],
									q: [nokey: ""],
									# OAuth
									auth: {:oauth, token, []},
									ip: {{127,0,0,1},60000},
									host: @domain)
		result = :mod_http_api.process([@command], req)
		assert 403 == elem(result, 0) # HTTP code

		# Check that the command was executed twice
		assert 3 ==
			:meck.num_calls(:ejabberd_commands, :execute_command2, :_)

		assert :meck.validate :ejabberd_auth
		#assert :meck.validate :ejabberd_commands
		#assert :ok = :meck.history(:ejabberd_commands)
	end

	test "Request oauth token, resource owner password credentials" do
		EjabberdAuthMock.create_user @user, @domain, @userpass
    :application.set_env(:oauth2, :backend, :ejabberd_oauth)
    :application.start(:oauth2)

		# Mock a simple command() -> :ok
		:meck.expect(:ejabberd_commands, :get_command_format,
			fn (@acommand, {@user, @domain, {:oauth, _token}, false}, @version) ->
					{[], {:res, :rescode}}
			end)
    :meck.expect(:ejabberd_commands, :get_command_policy_and_scope,
			fn (@acommand) -> {:ok, :user, [:erlang.atom_to_binary(@acommand,:utf8), "ejabberd:user"]} end)
		:meck.expect(:ejabberd_commands, :get_exposed_commands,
			fn () -> [@acommand] end)
		:meck.expect(:ejabberd_commands, :execute_command,
			fn (:undefined, {@user, @domain, {:oauth, _token}, false},
					@acommand, [], @version, _) ->
					:ok
			end)

   #Mock acl  to allow oauth authorizations
   :meck.expect(:acl, :match_rule, fn(_Server, _Access, _Jid) -> :allow end)


    # Correct password
    req = request(method: :POST,
            path: ["oauth", "token"],
            q: [{"grant_type", "password"}, {"scope", @command}, {"username",  @user<>"@"<>@domain}, {"ttl", "4000"}, {"password", @userpass}],
            ip: {{127,0,0,1},60000},
            host: @domain)
    result = :ejabberd_oauth.process([], req)
    assert 200 = elem(result, 0) #http code
    {kv} = :jiffy.decode(elem(result,2))
    assert {_, "bearer"} = List.keyfind(kv, "token_type", 0)
    assert {_, @command} = List.keyfind(kv, "scope", 0)
    assert {_, 4000} = List.keyfind(kv, "expires_in", 0)
    {"access_token", _token} = List.keyfind(kv, "access_token", 0)

    #missing grant_type
    req = request(method: :POST,
            path: ["oauth", "token"],
            q: [{"scope", @command}, {"username",  @user<>"@"<>@domain}, {"password", @userpass}],
            ip: {{127,0,0,1},60000},
            host: @domain)
    result = :ejabberd_oauth.process([], req)
    assert 400 = elem(result, 0) #http code
    {kv} = :jiffy.decode(elem(result,2))
    assert {_, "unsupported_grant_type"} = List.keyfind(kv, "error", 0)


		# incorrect user/pass
    req = request(method: :POST,
            path: ["oauth", "token"],
            q: [{"grant_type", "password"}, {"scope", @command}, {"username",  @user<>"@"<>@domain}, {"password", @userpass<>"aa"}],
            ip: {{127,0,0,1},60000},
            host: @domain)
    result = :ejabberd_oauth.process([], req)
    assert 400 = elem(result, 0) #http code
    {kv} = :jiffy.decode(elem(result,2))
    assert {_, "invalid_grant"} = List.keyfind(kv, "error", 0)

		assert :meck.validate :ejabberd_auth
		assert :meck.validate :ejabberd_commands
	end

end
