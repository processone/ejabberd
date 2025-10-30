-record(invite_token, {token :: binary(),
                       inviter :: {binary(), binary()},
                       invitee = <<>> :: binary(),
                       created_at = calendar:now_to_datetime(erlang:timestamp()) :: calendar:datetime(),
                       expires :: calendar:datetime(),
                       type = roster_only :: roster_only | account_only | account_subscription,
                       account_name = <<>> :: binary()
                      }).

-define(INVITE_TOKEN_EXPIRE_SECONDS_DEFAULT, 5*86400).
-define(INVITE_TOKEN_LENGTH_DEFAULT, 24).

-define(NS_INVITE_INVITE, <<"urn:xmpp:invite#invite">>).
-define(NS_INVITE_CREATE_ACCOUNT, <<"urn:xmpp:invite#create-account">>).
