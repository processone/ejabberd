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

defmodule EjabberdCyrsaslTest do
  @author "pawel@process-one.net"

  use ExUnit.Case, async: true

  setup_all do
    :ok = :ejabberd.start_app(:lager)
    :p1_sha.load_nif()
    :mnesia.start
    :ok = start_module(:stringprep)
    {:ok, _} = start_module(:jid)
    :ok = :ejabberd_config.start(["domain1"], [])
    {:ok, _} = :cyrsasl.start_link
    cyrstate = :cyrsasl.server_new("domain1", "domain1", "domain1", :ok, &get_password/1,
                                   &check_password/3, &check_password_digest/5)
    setup_anonymous_mocks()
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
    assert step1 == {:error, :not_authorized, "user1"}
  end

  test "Plain text (wrong user wrong pass)", context do
    step1 = :cyrsasl.server_start(context[:cyrstate], "PLAIN", <<0,"nouser1",0,"badpass">>)
    assert step1 == {:error, :not_authorized, "nouser1"}
  end

  test "Anonymous", context do
    step1 = :cyrsasl.server_start(context[:cyrstate], "ANONYMOUS", "domain1")
    assert {:ok, _} = step1
  end

  test "Digest-MD5 (correct user and pass)", context do
    assert {:ok, _list} = process_digest_md5(context[:cyrstate], "user1", "domain1", "pass")
  end

  test "Digest-MD5 (correct user wrong pass)", context do
    assert {:error, :not_authorized, "user1"} = process_digest_md5(context[:cyrstate], "user1", "domain1", "badpass")
  end

  test "Digest-MD5 (wrong user correct pass)", context do
    assert {:error, :not_authorized, "baduser"} = process_digest_md5(context[:cyrstate], "baduser", "domain1", "pass")
  end

  test "Digest-MD5 (wrong user and pass)", context do
    assert {:error, :not_authorized, "baduser"} = process_digest_md5(context[:cyrstate], "baduser", "domain1", "badpass")
  end

  defp process_digest_md5(cyrstate, user, domain, pass) do
    assert {:continue, init_str, state1} = :cyrsasl.server_start(cyrstate, "DIGEST-MD5", "")
    assert [_, nonce] = Regex.run(~r/nonce="(.*?)"/, init_str)
    digest_uri = "xmpp/#{domain}"
    cnonce = "abcd"
    nc = "00000001"
    response_hash = calc_digest_md5(user, domain, pass, nc, nonce, cnonce)
    response = "username=\"#{user}\",realm=\"#{domain}\",nonce=\"#{nonce}\",cnonce=\"#{cnonce}\"," <>
      "nc=\"#{nc}\",qop=auth,digest-uri=\"#{digest_uri}\",response=\"#{response_hash}\"," <>
      "charset=utf-8,algorithm=md5-sess"
    case :cyrsasl.server_step(state1, response) do
      {:continue, _calc_str, state2} -> :cyrsasl.server_step(state2, "")
      other -> other
    end
  end

  defp calc_digest_md5(user, domain, pass, nc, nonce, cnonce) do
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
      fn (_host) ->
        true
      end)
    mock(:ejabberd_auth, :is_user_exists,
      fn (user, domain) ->
        domain == "domain1" and get_password(user) != {:false, :internal}
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
      {:false, :internal}
    end
  end

  defp check_password(_user, authzid, pass) do
    case get_password(authzid) do
      {^pass, mod} ->
        {true, mod}
      _ ->
        false
    end
  end

  defp check_password_digest(_user, authzid, _pass, digest, digest_gen) do
    case get_password(authzid) do
      {:false, _} ->
        false
      {spass, mod} ->
        v = digest_gen.(spass)
        if v == digest do
          {true, mod}
        else
          false
        end
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
