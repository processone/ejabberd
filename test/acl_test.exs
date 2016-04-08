# ----------------------------------------------------------------------
#
# ejabberd, Copyright (C) 2002-2016   ProcessOne
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

defmodule ACLTest do
  @author "mremond@process-one.net"

  use ExUnit.Case, async: false

  setup_all do
    :ok = :mnesia.start
    :ok = :jid.start
    :ok = :ejabberd_config.start(["domain1", "domain2"], [])
    :ok = :acl.start
  end

  setup do
    :acl.clear
  end

  test "access rule match with user part ACL" do
    :acl.add(:global, :basic_acl_1, {:user, "test1"})
    :acl.add_access(:global, :basic_rule_1, [{:basic_acl_1, :allow}])
    # JID can only be passes as jid record.
    # => TODO: Support passing JID as binary.
    assert :acl.match_rule(:global, :basic_rule_1, :jid.from_string("test1@domain1")) == :allow
    assert :acl.match_rule(:global, :basic_rule_1, :jid.from_string("test1@domain2")) == :allow
    # We match on user part only for local domain. As an implicit rule remote domain are not matched
    assert :acl.match_rule(:global, :basic_rule_1, :jid.from_string("test1@otherdomain")) == :deny
    assert :acl.match_rule(:global, :basic_rule_1, :jid.from_string("test11@domain1")) == :deny

    :acl.add(:global, :basic_acl_2, {:user, {"test2", "domain1"}})
    :acl.add_access(:global, :basic_rule_2, [{:basic_acl_2, :allow}])
    assert :acl.match_rule(:global, :basic_rule_2, :jid.from_string("test2@domain1")) == :allow
    assert :acl.match_rule(:global, :basic_rule_2, :jid.from_string("test2@domain2")) == :deny
    assert :acl.match_rule(:global, :basic_rule_2, :jid.from_string("test2@otherdomain")) == :deny
    assert :acl.match_rule(:global, :basic_rule_2, {127,0,0,1}) == :deny
  end

  test "IP based ACL" do
    :acl.add(:global, :ip_acl_1, {:ip, "127.0.0.0/24"})
    :acl.add_access(:global, :ip_rule_1, [{:ip_acl_1, :allow}])
    # IP must be expressed as a tuple when calling match rule
    assert :acl.match_rule(:global, :ip_rule_1, {127,0,0,1}) == :allow
    assert :acl.match_rule(:global, :ip_rule_1, {127,0,1,1}) == :deny
    assert :acl.match_rule(:global, :ip_rule_1, :jid.from_string("test1@domain1")) == :deny
  end

  test "Access rule are evaluated sequentially" do
    :acl.add(:global, :user_acl_1, {:user, {"test1", "domain2"}})
    :acl.add(:global, :user_acl_2, {:user, "test1"})
    :acl.add_access(:global, :user_rule_1, [{:user_acl_1, :deny}, {:user_acl_2, :allow}])
    assert :acl.match_rule(:global, :user_rule_1, :jid.from_string("test1@domain1")) == :allow
    assert :acl.match_rule(:global, :user_rule_1, :jid.from_string("test1@domain2")) == :deny
  end

  # Access rules are sometimes used to provide values (i.e.: max_s2s_connections, max_user_sessions)
  test "Access rules providing values" do
    :acl.add(:global, :user_acl, {:user_regexp, ""})
    :acl.add(:global, :admin_acl, {:user, "admin"})
    :acl.add_access(:global, :value_rule_1, [{:admin_acl, 10}, {:user_acl, 5}])
    assert :acl.match_rule(:global, :value_rule_1, :jid.from_string("test1@domain1")) == 5
    assert :acl.match_rule(:global, :value_rule_1, :jid.from_string("admin@domain1")) == 10

    # If we have no match, :deny is still the default value
    # => TODO maybe we should have a match rule which allow passing custom default value ?
    assert :acl.match_rule(:global, :value_rule_1, :jid.from_string("user@otherdomain")) == :deny
  end


  # At the moment IP and user rules to no go well together: There is
  # no way to combine IP and user restrictions.
  # => TODO we need to implement access rules that implement both and will deny the access
  # if either IP or user returns deny
  test "mixing IP and user access rules" do
    :acl.add(:global, :user_acl_1, {:user, "test1"})
    :acl.add(:global, :ip_acl_1, {:ip, "127.0.0.0/24"})
    :acl.add_access(:global, :mixed_rule_1, [{:user_acl_1, :allow}, {:ip_acl_1, :allow}])
    assert :acl.match_rule(:global, :mixed_rule_1, :jid.from_string("test1@domain1")) == :allow
    assert :acl.match_rule(:global, :mixed_rule_1, {127,0,0,1}) == :allow

    :acl.add_access(:global, :mixed_rule_2, [{:user_acl_1, :deny}, {:ip_acl_1, :allow}])
    assert :acl.match_rule(:global, :mixed_rule_2, :jid.from_string("test1@domain1")) == :deny
    assert :acl.match_rule(:global, :mixed_rule_2, {127,0,0,1}) == :allow
  end

end
