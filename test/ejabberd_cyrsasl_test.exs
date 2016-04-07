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

defmodule EjabberdCyrsaslTest do
  @author "pawel@process-one.net"

  use ExUnit.Case, async: true

  setup_all do
    :mnesia.start
    :ok = start_module(:stringprep)
    :ok = start_module(:jid)
    :ok = :ejabberd_config.start(["domain1", "domain2"], [])
    :ok = :cyrsasl.start
    cyrstate = :cyrsasl.server_new("test", "test", "test", :ok, &get_password/1,
                                   &check_password/3, &check_password_digest/5)
    {:ok, cyrstate: cyrstate}
  end

  test "Plain text (correct user and pass)", context do
    step1 = :cyrsasl.server_start(context[:cyrstate], "PLAIN", <<0,"user1",0,"pass">>)
    assert {:ok, _} = step1
    {:ok, kv} = step1
    assert kv[:authzid] == "user1", "got correct user"
  end

  test "Plain text (correct user wrong pass)", context do
    step1 = :cyrsasl.server_start(context[:cyrstate], "PLAIN", <<0,"user1",0,"badpass">>)
    assert step1 == {:error, "not-authorized", "user1"}, "got error response"
  end

  test "Plain text (wrong user wrong pass)", context do
    step1 = :cyrsasl.server_start(context[:cyrstate], "PLAIN", <<0,"nouser1",0,"badpass">>)
    assert step1 == {:error, "not-authorized", "nouser1"}, "got error response"
  end

  defp start_module(module) do
    case apply(module, :start, []) do
      :ok -> :ok
      {:error, {:already_started, _}} -> :ok
      other -> other
    end
  end

  defp get_password(user) do
    if user == "user1" or user == "user2" do
      {"pass", :internal}
    else
      :false
    end
  end

  defp check_password(user, authzid, pass) do
    case get_password(authzid) do
      {^pass, mod} ->
        {true, mod}
      _ ->
        false
    end
  end

  defp check_password_digest(user, authzid, pass, digest, digest_gen) do
    case get_password(authzid) do
      {spass, mod} ->
        if digest_gen.(spass) == pass do
          {true, mod}
        else
          false
        end
      _ ->
        false
    end
  end
end
