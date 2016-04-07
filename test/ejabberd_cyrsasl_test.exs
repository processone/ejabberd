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
    :p1_sha.load_nif()
    :mnesia.start
    :ok = start_module(:stringprep)
    :ok = start_module(:jid)
    :ok = :ejabberd_config.start(["domain1"], [])
    :ok = :cyrsasl.start
    cyrstate = :cyrsasl.server_new("domain1", "domain1", "domain1", :ok, &get_password/1,
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

  test "Anonymous", context do
    setup_anonymous_mocks()
    step1 = :cyrsasl.server_start(context[:cyrstate], "ANONYMOUS", "domain1")
    assert {:ok, _} = step1
  end

  test "Digest-MD5 (correct user and pass)", context do
    assert {:continue, init_str, state1} = :cyrsasl.server_start(context[:cyrstate], "DIGEST-MD5", "")
    assert [_, nonce] = Regex.run(~r/nonce="(.*?)"/, init_str)
    user = "user1"
    domain = "domain1"
    digest_uri = "xmpp/#{domain}"
    pass = "pass"
    cnonce = "abcd"
    nc = "00000001"
    response_hash = calc_digest_sha(user, domain, pass, nc, nonce, cnonce)
    response = "username=\"#{user}\",realm=\"#{domain}\",nonce=\"#{nonce}\",cnonce=\"#{cnonce}\"," <>
      "nc=\"#{nc}\",qop=auth,digest-uri=\"#{digest_uri}\",response=\"#{response_hash}\"," <>
      "charset=utf-8,algorithm=md5-sess"
    assert {:continue, calc_str, state3} = :cyrsasl.server_step(state1, response)
    assert {:ok, list} = :cyrsasl.server_step(state3, "")
  end

  defp calc_digest_sha(user, domain, pass, nc, nonce, cnonce) do
    digest_uri = "xmpp/#{domain}"
    a0 = "#{user}:#{domain}:#{pass}"
    a1 = "#{str_md5(a0)}:#{nonce}:#{cnonce}"
    a2 = "AUTHENTICATE:#{digest_uri}"
    hex_md5("#{hex_md5(a1)}:#{nonce}:#{nc}:#{cnonce}:auth:#{hex_md5(a2)}")
  end

  defp str_md5(str) do
    :erlang.md5(str)
  end

  defp hex_md5(str) do
    :p1_sha.to_hexlist(:erlang.md5(str))
  end

  defp setup_anonymous_mocks() do
    :meck.unload
    mock(:ejabberd_auth_anonymous, :is_sasl_anonymous_enabled,
      fn (host) ->
        true
      end)
    mock(:ejabberd_auth, :is_user_exists,
      fn (user, domain) ->
        domain == "domain1" and get_password(user) != false
      end)
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
        v = digest_gen.(spass)
        if v == digest do
          {true, mod}
        else
          false
        end
      _ ->
        false
    end
  end

  defp mock(module, function, fun) do
    try do
      :meck.new(module, [:non_strict])
    catch
      :error, {:already_started, _pid} -> :ok
    end
    :meck.expect(module, function, fun)
  end
end
