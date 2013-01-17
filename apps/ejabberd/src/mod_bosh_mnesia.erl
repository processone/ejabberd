-module(mod_bosh_mnesia).

-behaviour(mod_bosh_backend).

%% mod_bosh_backend callbacks
-export([start/1,
         create_session/1,
         delete_session/1,
         get_session/1]).

-include("mod_bosh.hrl").

start(_Opts) ->
    mnesia:create_table(bosh_session,
                        [{ram_copies, [node()]},
                         {attributes, record_info(fields, bosh_session)}]),
    mnesia:add_table_copy(bosh_session, node(), ram_copies).

create_session(#bosh_session{} = Session) ->
    mnesia:sync_dirty(fun mnesia:write/1, [Session]).

-spec delete_session(bosh_sid()) -> any().
delete_session(Sid) ->
    mnesia:async_dirty(fun mnesia:delete/1, [{bosh_session, Sid}]).

-spec get_session(bosh_sid()) -> [#bosh_session{}] | {error, any()}.
get_session(Sid) ->
    try
        mnesia:dirty_read(bosh_session, Sid)
    catch
        exit:{aborted, _} = Reason ->
            {error, Reason}
    end.
