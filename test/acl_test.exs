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

  test "simple user access rule matches" do
    :acl.add(:global, :basic_acl_1, {:user, "test1"})
    :acl.add_access(:global, :basic_rule_1, [{:basic_acl_1, :allow}])
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
  end

end
