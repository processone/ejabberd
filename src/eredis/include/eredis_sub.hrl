%% State in eredis_sub_client
-record(state, {
          host :: string() | undefined,
          port :: integer() | undefined,
          password :: binary() | undefined,
          reconnect_sleep :: integer() | undefined,

          socket :: port() | undefined,
          parser_state :: #pstate{} | undefined,

          %% Channels we should subscribe to
          channels = [] :: [channel()],

          % The process we send pubsub and connection state messages to.
          controlling_process :: undefined | {reference(), pid()},

          % This is the queue of messages to send to the controlling
          % process.
          msg_queue :: queue(),

          %% When the queue reaches this size, either drop all
          %% messages or exit.
          max_queue_size :: integer() | inifinity,
          queue_behaviour :: drop | exit,

          % The msg_state keeps track of whether we are waiting
          % for the controlling process to acknowledge the last
          % message.
          msg_state = need_ack :: ready | need_ack
}).
