%% Created automatically by XML generator (fxml_gen.erl)
%% Source: pubsub_serverinfo_codec.spec

-record(pubsub_serverinfo_remote_domain, {name = <<>> :: binary(),
                                          type = [] :: ['bidi' | 'incoming' | 'outgoing']}).
-type pubsub_serverinfo_remote_domain() :: #pubsub_serverinfo_remote_domain{}.

-record(pubsub_serverinfo_domain, {name = <<>> :: binary(),
                                   remote_domain :: 'undefined' | [#pubsub_serverinfo_remote_domain{}]}).
-type pubsub_serverinfo_domain() :: #pubsub_serverinfo_domain{}.

-record(pubsub_serverinfo, {domain = [] :: [#pubsub_serverinfo_domain{}]}).
-type pubsub_serverinfo() :: #pubsub_serverinfo{}.

-type pubsub_serverinfo_codec() :: pubsub_serverinfo() |
                                   pubsub_serverinfo_domain() |
                                   pubsub_serverinfo_remote_domain().
