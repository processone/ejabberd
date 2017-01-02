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

defmodule EjabberdOauthMock do

	@author "jsautret@process-one.net"

	def init() do
		:mnesia.start
		:mnesia.create_table(:oauth_token,
                         [ram_copies: [node],
                          attributes: [:oauth_token, :us, :scope, :expire]])
    :application.start(:cache_tab)
    :cache_tab.new(:oauth_token,
                   [{:max_size, 1000}, {:life_time, 3600}])
	end

	def get_token(user, domain, command, expiration \\ 3600) do
		now = {megasecs, secs, _} = :os.timestamp
		expire = 1000000 * megasecs + secs + expiration
		:random.seed now
    token = to_string :random.uniform(100000000)

		{:ok, _} = :ejabberd_oauth.associate_access_token(token,
																											[{"resource_owner",
																												{:user, user, domain}},
																											 {"scope", [to_string command]},
																											 {"expiry_time", expire}],
																											[])
		token
	end

end
