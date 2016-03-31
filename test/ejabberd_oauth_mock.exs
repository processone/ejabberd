	#  ejabberd_oauth mock
	######################

defmodule EjabberdOauthMock do

	@author "jsautret@process-one.net"

	def init() do
		:mnesia.start
		:mnesia.create_table(:oauth_token,
                         [ram_copies: [node],
													attributes: [:oauth_token, :us, :scope, :expire]])
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
																											:undefined)
		token
	end

end
