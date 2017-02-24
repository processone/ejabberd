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

defmodule EjabberdModAdminExtraTest do
	use ExUnit.Case, async: false

  require EjabberdAuthMock
  require EjabberdSmMock
  require ModLastMock
  require ModRosterMock

	@author "jsautret@process-one.net"

	@user "user"
	@domain "domain"
	@password "password"
	@resource "resource"

	require Record
	Record.defrecord :jid, Record.extract(:jid, from_lib: "ejabberd/include/jlib.hrl")

	setup_all do
		try do
			:jid.start
			:stringprep.start
			:mnesia.start
			:p1_sha.load_nif
		rescue
			_ -> :ok
		end
		{:ok, _} = :ejabberd_access_permissions.start_link()
		:ejabberd_commands.init
                :ok = :ejabberd_config.start([@domain], [])
		:mod_admin_extra.start(@domain, [])
		:sel_application.start_app(:moka)
		{:ok, _pid} = :ejabberd_hooks.start_link
		:ok
	end

	setup do
		:meck.unload
		EjabberdAuthMock.init
		EjabberdSmMock.init
		ModRosterMock.init(@domain, :mod_admin_extra)
		:ok
	end

	###################### Accounts
	test "check_account works" do
		EjabberdAuthMock.create_user @user, @domain, @password

		assert :ejabberd_commands.execute_command(:check_account, [@user, @domain])
		refute :ejabberd_commands.execute_command(:check_account, [@user, "bad_domain"])
		refute :ejabberd_commands.execute_command(:check_account, ["bad_user", @domain])

		assert :meck.validate :ejabberd_auth
	end

	test "check_password works" do

		EjabberdAuthMock.create_user @user, @domain, @password

		assert :ejabberd_commands.execute_command(:check_password,
																							[@user, @domain, @password])
		refute :ejabberd_commands.execute_command(:check_password,
																							[@user, @domain, "bad_password"])
		refute :ejabberd_commands.execute_command(:check_password,
																							[@user, "bad_domain", @password])
		refute :ejabberd_commands.execute_command(:check_password,
																							["bad_user", @domain, @password])

		assert :meck.validate :ejabberd_auth

	end

	test "check_password_hash works" do

		EjabberdAuthMock.create_user @user, @domain, @password
		hash = "5F4DCC3B5AA765D61D8327DEB882CF99" # echo -n password|md5

		assert :ejabberd_commands.execute_command(:check_password_hash,
																							[@user, @domain, hash, "md5"])
		refute :ejabberd_commands.execute_command(:check_password_hash,
																							[@user, @domain, "bad_hash", "md5"])
		refute :ejabberd_commands.execute_command(:check_password_hash,
																							[@user, "bad_domain", hash, "md5"])
		refute :ejabberd_commands.execute_command(:check_password_hash,
																							["bad_user", @domain, hash, "md5"])

		hash = "5BAA61E4C9B93F3F0682250B6CF8331B7EE68FD8" # echo -n password|shasum
		assert :ejabberd_commands.execute_command(:check_password_hash,
																							[@user, @domain, hash, "sha"])

		assert :unkown_hash_method ==
			catch_throw :ejabberd_commands.execute_command(:check_password_hash,
																										 [@user, @domain, hash, "bad_method"])

		assert :meck.validate :ejabberd_auth

	end

	test "set_password works" do
		EjabberdAuthMock.create_user @user, @domain, @password

		assert :ejabberd_commands.execute_command(:change_password,
																							[@user, @domain, "new_password"])
		refute :ejabberd_commands.execute_command(:check_password,
																							[@user, @domain, @password])
		assert :ejabberd_commands.execute_command(:check_password,
																							[@user, @domain, "new_password"])
		assert {:not_found, 'unknown_user'} ==
			catch_throw :ejabberd_commands.execute_command(:change_password,
																										 ["bad_user", @domain,
																											@password])
		assert :meck.validate :ejabberd_auth
	end

	###################### Sessions

	test "num_resources works" do
		assert 0 == :ejabberd_commands.execute_command(:num_resources,
																									 [@user, @domain])

		EjabberdSmMock.connect_resource @user, @domain, @resource
		assert 1 == :ejabberd_commands.execute_command(:num_resources,
																									 [@user, @domain])

		EjabberdSmMock.connect_resource @user, @domain, @resource<>"2"
		assert 2 == :ejabberd_commands.execute_command(:num_resources,
																									 [@user, @domain])

		EjabberdSmMock.connect_resource @user<>"1", @domain, @resource
		assert 2 == :ejabberd_commands.execute_command(:num_resources,
																									 [@user, @domain])

		EjabberdSmMock.disconnect_resource @user, @domain, @resource
		assert 1 == :ejabberd_commands.execute_command(:num_resources,
																									 [@user, @domain])

		assert :meck.validate :ejabberd_sm
	end

	test "resource_num works" do
		EjabberdSmMock.connect_resource @user, @domain, @resource<>"3"
		EjabberdSmMock.connect_resource @user, @domain, @resource<>"2"
		EjabberdSmMock.connect_resource @user, @domain, @resource<>"1"

		assert :bad_argument ==
			elem(catch_throw(:ejabberd_commands.execute_command(:resource_num,
																													[@user, @domain, 0])), 0)
		assert @resource<>"1" ==
			:ejabberd_commands.execute_command(:resource_num, [@user, @domain, 1])
		assert @resource<>"3" ==
			:ejabberd_commands.execute_command(:resource_num, [@user, @domain, 3])
		assert :bad_argument ==
			elem(catch_throw(:ejabberd_commands.execute_command(:resource_num,
																													[@user, @domain, 4])), 0)
		assert :meck.validate :ejabberd_sm
	end

	test "kick_session works" do
		EjabberdSmMock.connect_resource @user, @domain, @resource<>"1"
		EjabberdSmMock.connect_resource @user, @domain, @resource<>"2"
		EjabberdSmMock.connect_resource @user, @domain, @resource<>"3"

		assert 3 == length EjabberdSmMock.get_sessions @user, @domain
		assert 1 == length EjabberdSmMock.get_session @user, @domain, @resource<>"2"

		assert :ok ==
			:ejabberd_commands.execute_command(:kick_session,
																				 [@user, @domain,
																					@resource<>"2", "kick"])

		assert 2 == length EjabberdSmMock.get_sessions @user, @domain
		assert 0 == length EjabberdSmMock.get_session @user, @domain, @resource<>"2"

		assert :meck.validate :ejabberd_sm
	end

	###################### Last

	test "get_last works" do

		assert {_, 'NOT FOUND'} =
			:ejabberd_commands.execute_command(:get_last, [@user, @domain])

		EjabberdSmMock.connect_resource @user, @domain, @resource<>"1"
		EjabberdSmMock.connect_resource @user, @domain, @resource<>"2"

		assert {_, 'ONLINE'} =
			:ejabberd_commands.execute_command(:get_last, [@user, @domain])

		EjabberdSmMock.disconnect_resource @user, @domain, @resource<>"1"

		assert {_, 'ONLINE'} =
			:ejabberd_commands.execute_command(:get_last, [@user, @domain])

		now = {megasecs, secs, _microsecs} = :os.timestamp
		timestamp = megasecs * 1000000 + secs
		EjabberdSmMock.disconnect_resource(@user, @domain, @resource<>"2",
		                                   timestamp)
    {{year, month, day}, {hour, minute, second}} = :calendar.now_to_universal_time now
		result = IO.iodata_to_binary(:io_lib.format(
					"~w-~.2.0w-~.2.0wT~.2.0w:~.2.0w:~.2.0wZ",
					[year, month, day, hour, minute, second]))
		assert {result, ""} ==
			:ejabberd_commands.execute_command(:get_last, [@user, @domain])

		assert :meck.validate :mod_last
	end

	###################### Roster

	test "add_rosteritem and delete_rosteritem work" do
		# Connect user
		# Add user1 & user2 to user's roster
		# Remove user1 & user2 from user's roster

		EjabberdSmMock.connect_resource @user, @domain, @resource

		assert [] == ModRosterMock.get_roster(@user, @domain)

		assert :ok ==
			:ejabberd_commands.execute_command(:add_rosteritem, [@user, @domain,
																													 @user<>"1", @domain,
																													 "nick1",
																													 "group1",
																													 "both"])
		# Check that user1 is the only item of the user's roster
		result = ModRosterMock.get_roster(@user, @domain)
		assert 1 == length result
		[{{@user, @domain, jid}, opts}] = result
		assert @user<>"1@"<>@domain == jid
		assert "nick1" == opts.nick
		assert ["group1"] == opts.groups
		assert :both == opts.subs

		# Check that the item roster user1 was pushed with subscription
		# 'both' to user online ressource
		jid = :jlib.make_jid(@user, @domain, @resource)
		assert 1 ==
			:meck.num_calls(:ejabberd_sm, :route,
                      [jid,
                       {:item, {@user<>"1", @domain, ""}, :both}])

		assert :ok ==
			:ejabberd_commands.execute_command(:add_rosteritem, [@user, @domain,
																													 @user<>"2", @domain,
																													 "nick2",
																													 "group2",
																													 "both"])
		result = ModRosterMock.get_roster(@user, @domain)
		assert 2 == length result


		# Check that the item roster user2 was pushed with subscription
		# 'both' to user online ressource
		assert 1 ==
			:meck.num_calls(:ejabberd_sm, :route,
                      [jid,
                       {:item, {@user<>"2", @domain, ""}, :both}])


		:ejabberd_commands.execute_command(:delete_rosteritem, [@user, @domain,
																														@user<>"1", @domain])
		result = ModRosterMock.get_roster(@user, @domain)
		assert 1 == length result
		[{{@user, @domain, jid}, opts}] = result
		assert @user<>"2@"<>@domain == jid
		assert "nick2" == opts.nick
		assert ["group2"] == opts.groups
		assert :both == opts.subs

		# Check that the item roster user1 was pushed with subscription
		# 'none' to user online ressource
		jid = :jlib.make_jid(@user, @domain, @resource)
		assert 1 ==
			:meck.num_calls(:ejabberd_sm, :route,
                      [jid,
                       {:item, {@user<>"1", @domain, ""}, :none}])

		:ejabberd_commands.execute_command(:delete_rosteritem, [@user, @domain,
																														@user<>"2", @domain])

		# Check that the item roster user2 was pushed with subscription
		# 'none' to user online ressource
		assert 1 ==
			:meck.num_calls(:ejabberd_sm, :route,
                      [jid,
                       {:item, {@user<>"2", @domain, ""}, :none}])

		# Check that nothing else was pushed to user resource
		jid = jid(user: @user, server: @domain, resource: :_,
							luser: @user, lserver: @domain, lresource: :_)
		assert 4 ==
			:meck.num_calls(:ejabberd_sm, :route,
                      [jid,
                       {:item, :_, :_}])

		assert [] == ModRosterMock.get_roster(@user, @domain)
		assert :meck.validate :ejabberd_sm

	end

	test "get_roster works" do
		assert [] == ModRosterMock.get_roster(@user, @domain)
		assert [] == :ejabberd_commands.execute_command(:get_roster, [@user, @domain],
																										:admin)

		assert :ok ==
			:ejabberd_commands.execute_command(:add_rosteritem, [@user, @domain,
																													 @user<>"1", @domain,
																													 "nick1",
																													 "group1",
																													 "both"])
		assert [{@user<>"1@"<>@domain, "", 'both', 'none', "group1"}] ==
			:ejabberd_commands.execute_command(:get_roster, [@user, @domain], :admin)
		assert :ok ==
			:ejabberd_commands.execute_command(:add_rosteritem, [@user, @domain,
																													 @user<>"2", @domain,
																													 "nick2",
																													 "group2",
																													 "none"])
		result = :ejabberd_commands.execute_command(:get_roster, [@user, @domain], :admin)
		assert 2 == length result
		assert Enum.member?(result, {@user<>"1@"<>@domain, "", 'both', 'none', "group1"})
		assert Enum.member?(result, {@user<>"2@"<>@domain, "", 'none', 'none', "group2"})

	end

# kick_user command is defined in ejabberd_sm, move to extra?
#	test "kick_user works" do
#		assert 0 == :ejabberd_commands.execute_command(:num_resources,
#																									 [@user, @domain])
#		EjabberdSmMock.connect_resource(@user, @domain, @resource<>"1")
#		EjabberdSmMock.connect_resource(@user, @domain, @resource<>"2")
#		assert 2 ==
#			:ejabberd_commands.execute_command(:kick_user, [@user, @domain])
#		assert 0 == :ejabberd_commands.execute_command(:num_resources,
#																									 [@user, @domain])
#		assert :meck.validate :ejabberd_sm
#	end

end
