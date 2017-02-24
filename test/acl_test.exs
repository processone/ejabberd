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

defmodule ACLTest do
  @author "mremond@process-one.net"

  use ExUnit.Case, async: false

  setup_all do
    :ok = :mnesia.start
    {:ok, _} = :jid.start
    :ejabberd_hooks.start_link
    :stringprep.start
    :ok = :ejabberd_config.start(["domain1", "domain2"], [])
    {:ok, _} = :acl.start_link
    :ok
  end

  setup do
    :acl.clear
  end

  test "access rule match with user part ACL" do
    :acl.add(:global, :basic_acl_1, {:user, "test1"})
    :acl.add(:global, :basic_acl_1, {:user, "test2"})
    :acl.add_access(:global, :basic_rule_1, [{:allow, [{:acl, :basic_acl_1}]}])
    # JID can only be passes as jid record.
    # => TODO: Support passing JID as binary.
    assert :acl.match_rule(:global, :basic_rule_1, :jid.from_string("test1@domain1")) == :allow
    assert :acl.match_rule(:global, :basic_rule_1, :jid.from_string("test1@domain2")) == :allow
    assert :acl.match_rule(:global, :basic_rule_1, :jid.from_string("test2@domain1")) == :allow
    assert :acl.match_rule(:global, :basic_rule_1, :jid.from_string("test2@domain2")) == :allow
    # We match on user part only for local domain. As an implicit rule remote domain are not matched
    assert :acl.match_rule(:global, :basic_rule_1, :jid.from_string("test1@otherdomain")) == :deny
    assert :acl.match_rule(:global, :basic_rule_1, :jid.from_string("test2@otherdomain")) == :deny
    assert :acl.match_rule(:global, :basic_rule_1, :jid.from_string("test11@domain1")) == :deny

    :acl.add(:global, :basic_acl_2, {:user, {"test2", "domain1"}})
    :acl.add_access(:global, :basic_rule_2, [{:allow, [{:acl, :basic_acl_2}]}])
    assert :acl.match_rule(:global, :basic_rule_2, :jid.from_string("test2@domain1")) == :allow
    assert :acl.match_rule(:global, :basic_rule_2, :jid.from_string("test2@domain2")) == :deny
    assert :acl.match_rule(:global, :basic_rule_2, :jid.from_string("test2@otherdomain")) == :deny
    assert :acl.match_rule(:global, :basic_rule_2, {127,0,0,1}) == :deny
  end

  test "IP based ACL" do
    :acl.add(:global, :ip_acl_1, {:ip, "127.0.0.0/24"})
    :acl.add_access(:global, :ip_rule_1, [{:allow, [{:acl, :ip_acl_1}]}])
    # IP must be expressed as a tuple when calling match rule
    assert :acl.match_rule(:global, :ip_rule_1, {127,0,0,1}) == :allow
    assert :acl.match_rule(:global, :ip_rule_1, {127,0,1,1}) == :deny
    assert :acl.match_rule(:global, :ip_rule_1, :jid.from_string("test1@domain1")) == :deny
  end

  test "Access rule are evaluated sequentially" do
    :acl.add(:global, :user_acl_1, {:user, {"test1", "domain2"}})
    :acl.add(:global, :user_acl_2, {:user, "test1"})
    :acl.add_access(:global, :user_rule_1, [{:deny, [{:acl, :user_acl_1}]}, {:allow, [{:acl, :user_acl_2}]}])
    assert :acl.match_rule(:global, :user_rule_1, :jid.from_string("test1@domain1")) == :allow
    assert :acl.match_rule(:global, :user_rule_1, :jid.from_string("test1@domain2")) == :deny
  end

  # Access rules are sometimes used to provide values (i.e.: max_s2s_connections, max_user_sessions)
  test "Access rules providing values" do
    :acl.add(:global, :user_acl, {:user_regexp, ""})
    :acl.add(:global, :admin_acl, {:user, "admin"})
    :acl.add_access(:global, :value_rule_1, [{10, [{:acl, :admin_acl}]}, {5, [{:acl, :user_acl}]}])
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
    :acl.add_access(:global, :mixed_rule_1, [{:allow, [{:acl, :user_acl_1}]}, {:allow, [{:acl, :ip_acl_1}]}])
    assert :acl.match_rule(:global, :mixed_rule_1, :jid.from_string("test1@domain1")) == :allow
    assert :acl.match_rule(:global, :mixed_rule_1, {127,0,0,1}) == :allow

    :acl.add_access(:global, :mixed_rule_2, [{:deny, [{:acl, :user_acl_1}]}, {:allow, [{:acl, :ip_acl_1}]}])
    assert :acl.match_rule(:global, :mixed_rule_2, :jid.from_string("test1@domain1")) == :deny
    assert :acl.match_rule(:global, :mixed_rule_2, {127,0,0,1}) == :allow
  end

  test "access_matches works with predefined access rules" do
    :acl.add(:global, :user_acl_2, {:user, "user"})
    :acl.add_access(:global, :user_rule_2, [{:allow, [{:acl, :user_acl_2}]}, {:deny, [:all]}])

    assert :acl.access_matches(:user_rule_2, %{usr: {"user", "domain1", ""}, ip: {127,0,0,1}}, :global) == :allow
    assert :acl.access_matches(:user_rule_2, %{usr: {"user2", "domain1", ""}, ip: {127,0,0,1}}, :global) == :deny
  end

  test "access_matches rule all always matches" do
    assert :acl.access_matches(:all, %{}, :global) == :allow
    assert :acl.access_matches(:all, %{usr: {"user", "domain1", ""}, ip: {127,0,0,1}}, :global) == :allow
  end

  test "access_matches rule none never matches" do
    assert :acl.access_matches(:none, %{}, :global) == :deny
    assert :acl.access_matches(:none, %{usr: {"user", "domain1", ""}, ip: {127,0,0,1}}, :global) == :deny
  end

  test "access_matches with not existing rule never matches" do
    assert :acl.access_matches(:bleble, %{}, :global) == :deny
    assert :acl.access_matches(:bleble, %{usr: {"user", "domain1", ""}, ip: {127,0,0,1}}, :global) == :deny
  end

  test "access_matches works with inlined access rules" do
    :acl.add(:global, :user_acl_3, {:user, "user"})

    assert :acl.access_matches([{:allow, [{:acl, :user_acl_3}]}, {:deny, [:all]}],
                               %{usr: {"user", "domain1", ""}, ip: {127,0,0,1}}, :global) == :allow
    assert :acl.access_matches([{:allow, [{:acl, :user_acl_3}]}, {:deny, [:all]}],
                               %{usr: {"user2", "domain1", ""}, ip: {127,0,0,1}}, :global) == :deny
  end

  test "access_matches allow to have acl rules inlined" do
    assert :acl.access_matches([{:allow, [{:user, "user"}]}, {:deny, [:all]}],
                               %{usr: {"user", "domain1", ""}, ip: {127,0,0,1}}, :global) == :allow
    assert :acl.access_matches([{:allow, [{:user, "user"}]}, {:deny, [:all]}],
                               %{usr: {"user2", "domain1", ""}, ip: {127,0,0,1}}, :global) == :deny
  end

  test "access_matches test have implicit deny at end" do
    assert :acl.access_matches([{:allow, [{:user, "user"}]}],
                               %{usr: {"user", "domain1", ""}, ip: {127,0,0,1}}, :global) == :allow
    assert :acl.access_matches([{:allow, [{:user, "user"}]}],
                               %{usr: {"user2", "domain1", ""}, ip: {127,0,0,1}}, :global) == :deny
  end

  test "access_matches requires that all subrules match" do
    rules = [{:allow, [{:user, "user"}, {:ip, {{127,0,0,1}, 32}}]}]
    assert :acl.access_matches(rules, %{usr: {"user", "domain1", ""}, ip: {127,0,0,1}}, :global) == :allow
    assert :acl.access_matches(rules, %{usr: {"user", "domain1", ""}, ip: {127,0,0,2}}, :global) == :deny
    assert :acl.access_matches(rules, %{usr: {"user2", "domain1", ""}, ip: {127,0,0,1}}, :global) == :deny
  end

  test "access_matches rules are matched in order" do
    rules = [{:allow, [{:user, "user"}]}, {:deny, [{:user, "user2"}]}, {:allow, [{:user_regexp, "user"}]}]
    assert :acl.access_matches(rules, %{usr: {"user", "domain1", ""}, ip: {127,0,0,1}}, :global) == :allow
    assert :acl.access_matches(rules, %{usr: {"user2", "domain1", ""}, ip: {127,0,0,1}}, :global) == :deny
    assert :acl.access_matches(rules, %{usr: {"user22", "domain1", ""}, ip: {127,0,0,1}}, :global) == :allow
  end

  test "access_matches rules that require ip but no one is provided don't crash" do
    rules = [{:allow, [{:ip, {{127,0,0,1}, 32}}]},
             {:allow, [{:user, "user"}]},
             {:allow, [{:user, "user2"}, {:ip, {{127,0,0,1}, 32}}]}]
    assert :acl.access_matches(rules, %{usr: {"user", "domain1", ""}}, :global) == :allow
    assert :acl.access_matches(rules, %{usr: {"user2", "domain1", ""}}, :global) == :deny
  end

  test "access_matches rules that require usr but no one is provided don't crash" do
    rules = [{:allow, [{:ip, {{127,0,0,1}, 32}}]},
             {:allow, [{:user, "user"}]},
             {:allow, [{:user, "user2"}, {:ip, {{127,0,0,2}, 32}}]}]
    assert :acl.access_matches(rules, %{ip: {127,0,0,1}}, :global) == :allow
    assert :acl.access_matches(rules, %{ip: {127,0,0,2}}, :global) == :deny
  end

  test "access_matches rules with all always matches" do
    rules = [{:allow, [:all]}, {:deny, {:user, "user"}}]
    assert :acl.access_matches(rules, %{}, :global) == :allow
    assert :acl.access_matches(rules, %{usr: {"user", "domain1", ""}, ip: {127,0,0,1}}, :global) == :allow
  end

  test "access_matches rules with {acl, all} always matches" do
    rules = [{:allow, [{:acl, :all}]}, {:deny, {:user, "user"}}]
    assert :acl.access_matches(rules, %{}, :global) == :allow
    assert :acl.access_matches(rules, %{usr: {"user", "domain1", ""}, ip: {127,0,0,1}}, :global) == :allow
  end

  test "access_matches rules with none never matches" do
    rules = [{:allow, [:none]}, {:deny, [:all]}]
    assert :acl.access_matches(rules, %{}, :global) == :deny
    assert :acl.access_matches(rules, %{usr: {"user", "domain1", ""}, ip: {127,0,0,1}}, :global) == :deny
  end

  test "access_matches with no rules never matches" do
    assert :acl.access_matches([], %{}, :global) == :deny
    assert :acl.access_matches([], %{usr: {"user", "domain1", ""}, ip: {127,0,0,1}}, :global) == :deny
  end

  test "access_matches ip rule accepts {ip, port}" do
    rules = [{:allow, [{:ip, {{127,0,0,1}, 32}}]}]
    assert :acl.access_matches(rules, %{ip: {{127,0,0,1}, 5000}}, :global) == :allow
    assert :acl.access_matches(rules, %{ip: {{127,0,0,2}, 5000}}, :global) == :deny
  end

  test "access_matches user rule works" do
    rules = [{:allow, [{:user, "user1"}]}]
    assert :acl.access_matches(rules, %{usr: {"user1", "domain1", ""}}, :global) == :allow
    assert :acl.access_matches(rules, %{usr: {"user2", "domain1", ""}}, :global) == :deny
    assert :acl.access_matches(rules, %{usr: {"user1", "domain3", ""}}, :global) == :deny
  end

  test "access_matches 2 arg user rule works" do
    rules = [{:allow, [{:user, {"user1", "server1"}}]}]
    assert :acl.access_matches(rules, %{usr: {"user1", "server1", ""}}, :global) == :allow
    assert :acl.access_matches(rules, %{usr: {"user1", "server2", ""}}, :global) == :deny
    assert :acl.access_matches(rules, %{usr: {"user2", "server1", ""}}, :global) == :deny
    assert :acl.access_matches(rules, %{usr: {"user2", "server2", ""}}, :global) == :deny
  end

  test "access_matches server rule works" do
    rules = [{:allow, [{:server, "server1"}]}]
    assert :acl.access_matches(rules, %{usr: {"user", "server1", ""}}, :global) == :allow
    assert :acl.access_matches(rules, %{usr: {"user", "server2", ""}}, :global) == :deny
  end

  test "access_matches resource rule works" do
    rules = [{:allow, [{:resource, "res1"}]}]
    assert :acl.access_matches(rules, %{usr: {"user", "domain1", "res1"}}, :global) == :allow
    assert :acl.access_matches(rules, %{usr: {"user", "domain1", "res2"}}, :global) == :deny
    assert :acl.access_matches(rules, %{usr: {"user", "domain3", "res1"}}, :global) == :allow
  end

  test "access_matches user_regexp rule works" do
    rules = [{:allow, [{:user_regexp, "user[0-9]"}]}]
    assert :acl.access_matches(rules, %{usr: {"user1", "domain1", "res1"}}, :global) == :allow
    assert :acl.access_matches(rules, %{usr: {"userA", "domain1", "res1"}}, :global) == :deny
    assert :acl.access_matches(rules, %{usr: {"user1", "domain3", "res1"}}, :global) == :deny
  end

  test "access_matches 2 arg user_regexp rule works" do
    rules = [{:allow, [{:user_regexp, {"user[0-9]", "server1"}}]}]
    assert :acl.access_matches(rules, %{usr: {"user1", "server1", "res1"}}, :global) == :allow
    assert :acl.access_matches(rules, %{usr: {"userA", "server1", "res1"}}, :global) == :deny
    assert :acl.access_matches(rules, %{usr: {"user1", "server2", "res1"}}, :global) == :deny
  end

  test "access_matches server_regexp rule works" do
    rules = [{:allow, [{:server_regexp, "server[0-9]"}]}]
    assert :acl.access_matches(rules, %{usr: {"user", "server1", ""}}, :global) == :allow
    assert :acl.access_matches(rules, %{usr: {"user", "serverA", ""}}, :global) == :deny
  end

  test "access_matches resource_regexp rule works" do
    rules = [{:allow, [{:resource_regexp, "res[0-9]"}]}]
    assert :acl.access_matches(rules, %{usr: {"user", "domain1", "res1"}}, :global) == :allow
    assert :acl.access_matches(rules, %{usr: {"user", "domain1", "resA"}}, :global) == :deny
    assert :acl.access_matches(rules, %{usr: {"user", "domain3", "res1"}}, :global) == :allow
  end

  test "access_matches node_regexp rule works" do
    rules = [{:allow, [{:node_regexp, {"user[0-9]", "server[0-9]"}}]}]
    assert :acl.access_matches(rules, %{usr: {"user1", "server1", "res1"}}, :global) == :allow
    assert :acl.access_matches(rules, %{usr: {"userA", "server1", "res1"}}, :global) == :deny
    assert :acl.access_matches(rules, %{usr: {"user1", "serverA", "res1"}}, :global) == :deny
    assert :acl.access_matches(rules, %{usr: {"userA", "serverA", "res1"}}, :global) == :deny
  end

  test "access_matches user_glob rule works" do
    rules = [{:allow, [{:user_glob, "user?"}]}]
    assert :acl.access_matches(rules, %{usr: {"user1", "domain1", "res1"}}, :global) == :allow
    assert :acl.access_matches(rules, %{usr: {"user11", "domain1", "res1"}}, :global) == :deny
    assert :acl.access_matches(rules, %{usr: {"user1", "domain3", "res1"}}, :global) == :deny
  end

  test "access_matches 2 arg user_glob rule works" do
    rules = [{:allow, [{:user_glob, {"user?", "server1"}}]}]
    assert :acl.access_matches(rules, %{usr: {"user1", "server1", "res1"}}, :global) == :allow
    assert :acl.access_matches(rules, %{usr: {"user11", "server1", "res1"}}, :global) == :deny
    assert :acl.access_matches(rules, %{usr: {"user1", "server2", "res1"}}, :global) == :deny
  end

  test "access_matches server_glob rule works" do
    rules = [{:allow, [{:server_glob, "server?"}]}]
    assert :acl.access_matches(rules, %{usr: {"user", "server1", ""}}, :global) == :allow
    assert :acl.access_matches(rules, %{usr: {"user", "server11", ""}}, :global) == :deny
  end

  test "access_matches resource_glob rule works" do
    rules = [{:allow, [{:resource_glob, "res?"}]}]
    assert :acl.access_matches(rules, %{usr: {"user", "domain1", "res1"}}, :global) == :allow
    assert :acl.access_matches(rules, %{usr: {"user", "domain1", "res11"}}, :global) == :deny
    assert :acl.access_matches(rules, %{usr: {"user", "domain3", "res1"}}, :global) == :allow
  end

  test "access_matches node_glob rule works" do
    rules = [{:allow, [{:node_glob, {"user?", "server?"}}]}]
    assert :acl.access_matches(rules, %{usr: {"user1", "server1", "res1"}}, :global) == :allow
    assert :acl.access_matches(rules, %{usr: {"user11", "server1", "res1"}}, :global) == :deny
    assert :acl.access_matches(rules, %{usr: {"user1", "server11", "res1"}}, :global) == :deny
    assert :acl.access_matches(rules, %{usr: {"user11", "server11", "res1"}}, :global) == :deny
  end

  test "transform_access_rules_config expands allow rule" do
    assert :acl.transform_access_rules_config([:allow]) == [{:allow, [:all]}]
  end

  test "transform_access_rules_config expands deny rule" do
    assert :acl.transform_access_rules_config([:deny]) == [{:deny, [:all]}]
  end

  test "transform_access_rules_config expands <integer> rule" do
    assert :acl.transform_access_rules_config([100]) == [{100, [:all]}]
  end

  test "transform_access_rules_config expands <shaper_name> rule" do
    assert :acl.transform_access_rules_config([:fast]) == [{:fast, [:all]}]
  end

  test "transform_access_rules_config expands allow: <acl_name> rule" do
    assert :acl.transform_access_rules_config([{:allow, :test1}]) == [{:allow, [{:acl, :test1}]}]
  end

  test "transform_access_rules_config expands deny: <acl_name> rule" do
    assert :acl.transform_access_rules_config([{:deny, :test1}]) == [{:deny, [{:acl, :test1}]}]
  end

  test "transform_access_rules_config expands integer: <acl_name> rule" do
    assert :acl.transform_access_rules_config([{100, :test1}]) == [{100, [{:acl, :test1}]}]
  end

  test "transform_access_rules_config expands <shaper_name>: <acl_name> rule" do
    assert :acl.transform_access_rules_config([{:fast, :test1}]) == [{:fast, [{:acl, :test1}]}]
  end

  test "transform_access_rules_config expands allow rule (no list)" do
    assert :acl.transform_access_rules_config(:allow) == [{:allow, [:all]}]
  end

  test "transform_access_rules_config expands deny rule (no list)" do
    assert :acl.transform_access_rules_config(:deny) == [{:deny, [:all]}]
  end

  test "transform_access_rules_config expands <integer> rule (no list)" do
    assert :acl.transform_access_rules_config(100) == [{100, [:all]}]
  end

  test "transform_access_rules_config expands <shaper_name> rule (no list)" do
    assert :acl.transform_access_rules_config(:fast) == [{:fast, [:all]}]
  end

  test "access_rules_validator works with <AccessName>" do
    assert :acl.access_rules_validator(:my_access) == :my_access
  end

  test "get_opt with access_rules_validation works with <AccessName>" do
    assert :gen_mod.get_opt(:access, [access: :my_rule], &:acl.access_rules_validator/1)
    == :my_rule
  end

  test "get_opt with access_rules_validation perform normalization for acl rules" do
    assert :gen_mod.get_opt(:access, [access: [[allow: :zed]]], &:acl.access_rules_validator/1)
    == [allow: [acl: :zed]]
  end

  test "get_opt with access_rules_validation perform normalization for user@server rules" do
    assert :gen_mod.get_opt(:access, [access: [allow: [user: "a@b"]]], &:acl.access_rules_validator/1)
    == [allow: [user: {"a", "b"}]]
  end

  test "get_opt with access_rules_validation return default value with number as rule type" do
    assert :gen_mod.get_opt(:access, [access: [{100, [user: "a@b"]}]], &:acl.access_rules_validator/1)
    == :undefined
  end

  test "get_opt with access_rules_validation return default value when invalid rule type is passed" do
    assert :gen_mod.get_opt(:access, [access: [allow2: [user: "a@b"]]], &:acl.access_rules_validator/1)
    == :undefined
  end

  test "get_opt with access_rules_validation return default value when invalid acl is passed" do
    assert :gen_mod.get_opt(:access, [access: [allow: [user2: "a@b"]]], &:acl.access_rules_validator/1)
    == :undefined
  end

  test "shapes_rules_validator works with <AccessName>" do
    assert :acl.shaper_rules_validator(:my_access) == :my_access
  end

  test "get_opt with shaper_rules_validation works with <AccessName>" do
    assert :gen_mod.get_opt(:access, [access: :my_rule], &:acl.shaper_rules_validator/1)
    == :my_rule
  end

  test "get_opt with shaper_rules_validation perform normalization for acl rules" do
    assert :gen_mod.get_opt(:access, [access: [[allow: :zed]]], &:acl.shaper_rules_validator/1)
    == [allow: [acl: :zed]]
  end

  test "get_opt with shaper_rules_validation perform normalization for user@server rules" do
    assert :gen_mod.get_opt(:access, [access: [allow: [user: "a@b"]]], &:acl.shaper_rules_validator/1)
    == [allow: [user: {"a", "b"}]]
  end

  test "get_opt with shaper_rules_validation return accepts number as rule type" do
    assert :gen_mod.get_opt(:access, [access: [{100, [user: "a@b"]}]], &:acl.shaper_rules_validator/1)
    == [{100, [user: {"a", "b"}]}]
  end

  test "get_opt with shaper_rules_validation return accepts any atom as rule type" do
    assert :gen_mod.get_opt(:access, [access: [fast: [user: "a@b"]]], &:acl.shaper_rules_validator/1)
    == [fast: [user: {"a", "b"}]]
  end

  test "get_opt with shaper_rules_validation return default value when invalid acl is passed" do
    assert :gen_mod.get_opt(:access, [access: [allow: [user2: "a@b"]]], &:acl.shaper_rules_validator/1)
    == :undefined
  end

  ## Checking ACL on both user pattern and IP
  ## ========================================

  # Typical example is mod_register

  # Deprecated approach
  test "module can test both IP and user through two independent :acl.match_rule check (deprecated)" do
    :acl.add(:global, :user_acl, {:user, {"test1", "domain1"}})
    :acl.add(:global, :ip_acl, {:ip, "127.0.0.0/24"})
    :acl.add_access(:global, :user_rule, [{:allow, [{:acl, :user_acl}]}])
    :acl.add_access(:global, :ip_rule, [{:allow, [{:acl, :ip_acl}]}])

    # acl module in 16.03 is not able to provide a function for compound result:
    assert :acl.match_rule(:global, :user_rule, :jid.from_string("test1@domain1")) == :allow
    assert :acl.match_rule(:global, :ip_rule, {127,0,0,1}) == :allow
    assert :acl.match_rule(:global, :user_rule, :jid.from_string("test2@domain1")) == :deny
    assert :acl.match_rule(:global, :ip_rule, {127,0,1,1}) == :deny
  end

end
