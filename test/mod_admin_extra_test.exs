# ----------------------------------------------------------------------
#
# ejabberd, Copyright (C) 2002-2015   ProcessOne
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

	@author "jsautret@process-one.net"

	@user "user"
	@domain "domain"
	@password "password"
	@resource "resource"

	require Record
	Record.defrecord :jid, Record.extract(:jid,
																				from: "jlib.hrl")

	setup_all do
		try do
			:stringprep.start
			:mnesia.start
			:p1_sha.load_nif
		rescue
			_ -> :ok
		end
		:ejabberd_commands.init
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

	test "change_password works" do
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

	test "check_users_registration works" do
		EjabberdAuthMock.create_user @user<>"1", @domain, @password
		EjabberdAuthMock.create_user @user<>"2", @domain, @password
		EjabberdAuthMock.create_user @user<>"3", @domain, @password

		assert [{@user<>"0", @domain, 0},
						{@user<>"1", @domain, 1},
						{@user<>"2", @domain, 1},
						{@user<>"3", @domain, 1}] ==
			:ejabberd_commands.execute_command(:check_users_registration,
																				 [[{@user<>"0", @domain},
																					 {@user<>"1", @domain},
																					 {@user<>"2", @domain},
																					 {@user<>"3", @domain}]])

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

		assert 'Never' ==
			:ejabberd_commands.execute_command(:get_last, [@user, @domain])

		EjabberdSmMock.connect_resource @user, @domain, @resource<>"1"
		EjabberdSmMock.connect_resource @user, @domain, @resource<>"2"

		assert 'Online' ==
			:ejabberd_commands.execute_command(:get_last, [@user, @domain])

		EjabberdSmMock.disconnect_resource @user, @domain, @resource<>"1"

		assert 'Online' ==
			:ejabberd_commands.execute_command(:get_last, [@user, @domain])

		now = {megasecs, secs, _microsecs} = :os.timestamp
		timestamp = megasecs * 1000000 + secs
		EjabberdSmMock.disconnect_resource(@user, @domain, @resource<>"2",
		                                   timestamp)
		{{year, month, day}, {hour, minute, second}} = :calendar.now_to_local_time now
		result = List.flatten(:io_lib.format(
					"~w-~.2.0w-~.2.0w ~.2.0w:~.2.0w:~.2.0w ",
					[year, month, day, hour, minute, second]))
		assert result ==
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
											[jid, jid,
											 {:broadcast, {:item, {@user<>"1", @domain, ""}, :both}}])

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
											[jid, jid,
											 {:broadcast, {:item, {@user<>"2", @domain, ""}, :both}}])


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
											[jid, jid,
											 {:broadcast, {:item, {@user<>"1", @domain, ""}, :none}}])

		:ejabberd_commands.execute_command(:delete_rosteritem, [@user, @domain,
																														@user<>"2", @domain])

		# Check that the item roster user2 was pushed with subscription
		# 'none' to user online ressource
		assert 1 ==
			:meck.num_calls(:ejabberd_sm, :route,
											[jid, jid,
											 {:broadcast, {:item, {@user<>"2", @domain, ""}, :none}}])

		# Check that nothing else was pushed to user resource
		jid = jid(user: @user, server: @domain, resource: :_,
							luser: @user, lserver: @domain, lresource: :_)
		assert 4 ==
			:meck.num_calls(:ejabberd_sm, :route,
											[jid, jid,
											 {:broadcast, {:item, :_, :_}}])

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


	test "link_contacts & unlink_contacts work" do
		# Create user1 and keep it offline
		EjabberdAuthMock.create_user @user<>"1", @domain, @password

		# fail if one of the users doesn't exist locally
		assert 404 ==
			:ejabberd_commands.execute_command(:link_contacts, [@user<>"1@"<>@domain,
																													"nick1",
																													"group1",
																													@user<>"2@"<>@domain,
																													"nick2",
																													"group2"])

		# Create user2 and connect 2 resources
		EjabberdAuthMock.create_user @user<>"2", @domain, @password

		EjabberdSmMock.connect_resource @user<>"2", @domain, @resource<>"1"
		EjabberdSmMock.connect_resource @user<>"2", @domain, @resource<>"2"

		# Link both user1 & user2 (returns 0 if OK)
		assert 0 ==
			:ejabberd_commands.execute_command(:link_contacts, [@user<>"1@"<>@domain,
																													 "nick1",
																													 "group2",
																													 @user<>"2@"<>@domain,
																													 "nick2",
																													 "group1"])
		assert [{@user<>"2@"<>@domain, "", 'both', 'none', "group2"}] ==
			:ejabberd_commands.execute_command(:get_roster, [@user<>"1", @domain], :admin)

		assert [{@user<>"1@"<>@domain, "", 'both', 'none', "group1"}] ==
			:ejabberd_commands.execute_command(:get_roster, [@user<>"2", @domain], :admin)

		# Check that the item roster user1 was pushed with subscription
		# 'both' to the 2 user2 online ressources
		jid = :jlib.make_jid(@user<>"2", @domain, @resource<>"1")
		assert 1 ==
			:meck.num_calls(:ejabberd_sm, :route,
											[jid, jid,
											 {:broadcast, {:item, {@user<>"1", @domain, ""}, :both}}])
		jid = :jlib.make_jid(@user<>"2", @domain, @resource<>"2")
		assert 1 ==
			:meck.num_calls(:ejabberd_sm, :route,
											[jid, jid,
											 {:broadcast, {:item, {@user<>"1", @domain, ""}, :both}}])


		# Ulink both user1 & user2 (returns 0 if OK)
		assert 0 ==
			:ejabberd_commands.execute_command(:unlink_contacts, [@user<>"1@"<>@domain,
																													@user<>"2@"<>@domain])
		assert [] ==
			:ejabberd_commands.execute_command(:get_roster, [@user<>"1", @domain], :admin)

		assert [] ==
			:ejabberd_commands.execute_command(:get_roster, [@user<>"2", @domain], :admin)

		# Check that the item roster user1 was pushed with subscription
		# 'none' to the 2 user2 online ressources
		jid = :jlib.make_jid(@user<>"2", @domain, @resource<>"1")
		assert 1 ==
			:meck.num_calls(:ejabberd_sm, :route,
											[jid, jid,
											 {:broadcast, {:item, {@user<>"1", @domain, ""}, :none}}])
		jid = :jlib.make_jid(@user<>"2", @domain, @resource<>"2")
		assert 1 ==
			:meck.num_calls(:ejabberd_sm, :route,
											[jid, jid,
											 {:broadcast, {:item, {@user<>"1", @domain, ""}, :none}}])

		# Check that nothing else was pushed to user2 resources
		jid = jid(user: @user<>"2", server: @domain, resource: :_,
							luser: @user<>"2", lserver: @domain, lresource: :_)
		assert 4 ==
			:meck.num_calls(:ejabberd_sm, :route,
											[jid, jid,
											 {:broadcast, {:item, :_, :_}}])

		# Check nothing was pushed to user1
		jid = jid(user: @user<>"1", server: @domain, resource: :_,
							luser: @user<>"1", lserver: @domain, lresource: :_)
		refute :meck.called(:ejabberd_sm, :route,
												[jid, jid,
												 {:broadcast, {:item, :_, :_}}])

		assert :meck.validate :ejabberd_sm
		assert :meck.validate :ejabberd_auth

	end




	test "add_contacts and delete_contacts work" do
		# Create user, user1 & user2
		# Connect user & user1
		# Add user0, user1 & user2 to user's roster
		# Remove user0, user1 & user2 from user's roster

		# user doesn't exists yet, command must fail
		assert 404 ==
			:ejabberd_commands.execute_command(:add_contacts, [@user, @domain,
																												 [{@user<>"1"<>@domain,
																													 "group1",
																													 "nick1"},
																													{@user<>"2"<>@domain,
																													 "group2",
																													 "nick2"}]
																												 ])

		EjabberdAuthMock.create_user @user, @domain, @password
		EjabberdSmMock.connect_resource @user, @domain, @resource
		EjabberdAuthMock.create_user @user<>"1", @domain, @password
		EjabberdSmMock.connect_resource @user<>"1", @domain, @resource
		EjabberdAuthMock.create_user @user<>"2", @domain, @password

		# Add user1 & user2 in user's roster. Try also to add user0 that
		# doesn't exists. Command is supposed to return number of added items.
		assert 2 ==
			:ejabberd_commands.execute_command(:add_contacts, [@user, @domain,
																												 [{@user<>"0@"<>@domain,
																													 "group0",
																													 "nick0"},
																													{@user<>"1@"<>@domain,
																													 "group1",
																													 "nick1"},
																													{@user<>"2@"<>@domain,
																													 "group2",
																													 "nick2"}]
																												])
		# Check that user1 & user2 are the only items in user's roster
		result = ModRosterMock.get_roster(@user, @domain)
		assert 2 == length result
		opts1 = %{nick: "nick1", groups:  ["group1"], subs: :both,
						 ask: :none, askmessage: ""}
		assert Enum.member?(result, {{@user, @domain, @user<>"1@"<>@domain}, opts1})
		opts2 = %{nick: "nick2", groups:  ["group2"], subs: :both,
						 ask: :none, askmessage: ""}
		assert Enum.member?(result, {{@user, @domain, @user<>"2@"<>@domain}, opts2})

		# Check that user is the only item in user1's roster
		assert [{{@user<>"1", @domain, @user<>"@"<>@domain}, %{opts1|:nick => ""}}] ==
			ModRosterMock.get_roster(@user<>"1", @domain)

		# Check that user is the only item in user2's roster
		assert [{{@user<>"2", @domain, @user<>"@"<>@domain}, %{opts2|:nick => ""}}] ==
			ModRosterMock.get_roster(@user<>"2", @domain)


		# Check that the roster items user1 & user2 were pushed with subscription
		# 'both' to the user online ressource
		jid = :jlib.make_jid(@user, @domain, @resource)
		assert 1 ==
			:meck.num_calls(:ejabberd_sm, :route,
											[jid, jid,
											 {:broadcast, {:item, {@user<>"1", @domain, ""}, :both}}])
		assert 1 ==
			:meck.num_calls(:ejabberd_sm, :route,
											[jid, jid,
											 {:broadcast, {:item, {@user<>"2", @domain, ""}, :both}}])

		# Check that the roster item user was pushed with subscription
		# 'both' to the user1 online ressource
		jid = :jlib.make_jid(@user<>"1", @domain, @resource)
		assert 1 ==
			:meck.num_calls(:ejabberd_sm, :route,
											[jid, jid,
											 {:broadcast, {:item, {@user, @domain, ""}, :both}}])

		# Check that nothing else was pushed to online resources
		assert 3 ==
			:meck.num_calls(:ejabberd_sm, :route,
											[:_, :_,
											 {:broadcast, {:item, :_, :_}}])

		# Remove user1 & user2 from user's roster. Try also to remove
		# user0 that doesn't exists. Command is supposed to return number
		# of removed items.
		assert 2 ==
			:ejabberd_commands.execute_command(:remove_contacts, [@user, @domain,
																														[@user<>"0@"<>@domain,
																														 @user<>"1@"<>@domain,
																														 @user<>"2@"<>@domain]
																													 ])
		# Check that roster of user, user1 & user2 are empty
		assert [] == ModRosterMock.get_roster(@user, @domain)
		assert [] == ModRosterMock.get_roster(@user<>"1", @domain)
		assert [] == ModRosterMock.get_roster(@user<>"2", @domain)

		# Check that the roster items user1 & user2 were pushed with subscription
		# 'none' to the user online ressource
		jid = :jlib.make_jid(@user, @domain, @resource)
		assert 1 ==
			:meck.num_calls(:ejabberd_sm, :route,
											[jid, jid,
											 {:broadcast, {:item, {@user<>"1", @domain, ""}, :none}}])
		assert 1 ==
			:meck.num_calls(:ejabberd_sm, :route,
											[jid, jid,
											 {:broadcast, {:item, {@user<>"2", @domain, ""}, :none}}])

		# Check that the roster item user was pushed with subscription
		# 'none' to the user1 online ressource
		jid = :jlib.make_jid(@user<>"1", @domain, @resource)
		assert 1 ==
			:meck.num_calls(:ejabberd_sm, :route,
											[jid, jid,
											 {:broadcast, {:item, {@user, @domain, ""}, :none}}])

		# Check that nothing else was pushed to online resources
		assert 6 ==
			:meck.num_calls(:ejabberd_sm, :route,
											[:_, :_,
											 {:broadcast, {:item, :_, :_}}])

		assert :meck.validate :ejabberd_sm
		assert :meck.validate :ejabberd_auth
	end


	test "update_roster works" do
		# user doesn't exists yet, command must fail
		result =
			:ejabberd_commands.execute_command(:update_roster,
																				 [@user, @domain,
																					[{@user<>"1"<>@domain,
																						"group1",
																						"nick1"},
																					 {@user<>"2"<>@domain,
																						"group2",
																						"nick2"}],
																					[]
																				 ])
		assert :invalid_user == elem(result, 0)

		EjabberdAuthMock.create_user @user, @domain, @password
		EjabberdSmMock.connect_resource @user, @domain, @resource
		EjabberdAuthMock.create_user @user<>"1", @domain, @password
		EjabberdSmMock.connect_resource @user<>"1", @domain, @resource
		EjabberdAuthMock.create_user @user<>"2", @domain, @password
		EjabberdAuthMock.create_user @user<>"3", @domain, @password

		assert :ok ==
			:ejabberd_commands.execute_command(:update_roster,
																				 [@user, @domain,
																					[{[{"username", @user<>"1"},
																						 {"nick",	"nick1"}]},
																					 {[{"username", @user<>"2"},
																						 {"nick",	"nick2"},
																						 {"subscription", "from"}]}],
																					 []])


		# Check that user1 & user2 are the only items in user's roster
		result = ModRosterMock.get_roster(@user, @domain)

		assert 2 == length result
		opts1 = %{nick: "nick1", groups:  [""], subs: :both,
						 ask: :none, askmessage: ""}
		assert Enum.member?(result, {{@user, @domain, @user<>"1@"<>@domain}, opts1})
		opts2 = %{nick: "nick2", groups:  [""], subs: :from,
						 ask: :none, askmessage: ""}
		assert Enum.member?(result, {{@user, @domain, @user<>"2@"<>@domain}, opts2})

		# Check that the roster items user1 & user2 were pushed with subscription
		# 'both' to the user online ressource
		jid = :jlib.make_jid(@user, @domain, @resource)
		assert 1 ==
			:meck.num_calls(:ejabberd_sm, :route,
											[jid, jid,
											 {:broadcast, {:item, {@user<>"1", @domain, ""}, :both}}])
		assert 1 ==
			:meck.num_calls(:ejabberd_sm, :route,
											[jid, jid,
											 {:broadcast, {:item, {@user<>"2", @domain, ""}, :from}}])

		# Check that nothing else was pushed to online resources
		assert 2 ==
			:meck.num_calls(:ejabberd_sm, :route,
											[:_, :_,
											 {:broadcast, {:item, :_, :_}}])

		# Add user3 & remove user1
		assert :ok ==
			:ejabberd_commands.execute_command(:update_roster,
																				 [@user, @domain,
																					[{[{"username", @user<>"3"},
																						 {"nick",	"nick3"},
																						 {"subscription", "to"}]}],
																					[{[{"username", @user<>"1"}]}]
																					])

		# Check that user2 & user3 are the only items in user's roster
		result = ModRosterMock.get_roster(@user, @domain)
		assert 2 == length result
		opts2 = %{nick: "nick2", groups:  [""], subs: :from,
						 ask: :none, askmessage: ""}
		assert Enum.member?(result, {{@user, @domain, @user<>"2@"<>@domain}, opts2})
		opts1 = %{nick: "nick3", groups:  [""], subs: :to,
						 ask: :none, askmessage: ""}
		assert Enum.member?(result, {{@user, @domain, @user<>"3@"<>@domain}, opts1})

		# Check that the roster items user1 & user3 were pushed with subscription
		# 'none' & 'to' to the user online ressource
		jid = :jlib.make_jid(@user, @domain, @resource)
		assert 1 ==
			:meck.num_calls(:ejabberd_sm, :route,
											[jid, jid,
											 {:broadcast, {:item, {@user<>"1", @domain, ""}, :none}}])
		assert 1 ==
			:meck.num_calls(:ejabberd_sm, :route,
											[jid, jid,
											 {:broadcast, {:item, {@user<>"3", @domain, ""}, :to}}])

		# Check that nothing else was pushed to online resources
		assert 4 ==
			:meck.num_calls(:ejabberd_sm, :route,
											[:_, :_,
											 {:broadcast, {:item, :_, :_}}])

		assert :meck.validate :ejabberd_sm
		assert :meck.validate :ejabberd_auth
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
