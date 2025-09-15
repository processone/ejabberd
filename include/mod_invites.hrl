-define(INVITE_TOKEN_EXPIRE_SECONDS_DEFAULT, 5*86400).
-define(INVITE_TOKEN_LENGTH_DEFAULT, 24).

-define(NS_INVITE_INVITE, <<"urn:xmpp:invite#invite">>).
-define(NS_INVITE_CREATE_ACCOUNT, <<"urn:xmpp:invite#create-account">>).

-record(invite_token, {token :: binary(),
                       inviter :: {binary(), binary()},
                       %% A non-empty value if `invitee` indicates the invite has been used.
                       invitee = <<>> :: binary(),
                       created_at = calendar:now_to_datetime(erlang:timestamp()) :: calendar:datetime(),
                       expires = calendar:gregorian_seconds_to_datetime(calendar:datetime_to_gregorian_seconds(calendar:now_to_datetime(erlang:timestamp())) + ?INVITE_TOKEN_EXPIRE_SECONDS_DEFAULT) :: calendar:datetime(),
                       type = roster_only :: roster_only | account_only | account_subscription,
                       %% If type is 'roster_only' then we indicate a token has been used to create
                       %% an account (if allowed) by setting `account_name` to the name of the user
                       %% (which should match `invitee`).
                       account_name = <<>> :: binary()
                      }).
