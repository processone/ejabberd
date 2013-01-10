-module(mod_bosh_mnesia).

-behaviour(mod_bosh_backend).

%% mod_bosh_backend callbacks
-export([start/1,
         create_session/1]).

-include("mod_bosh.hrl").

start(_Opts) ->
    mnesia:create_table(bosh_session,
                        [{ram_copies, [node()]},
                         {attributes, record_info(fields, bosh_session)}]),
    mnesia:add_table_copy(bosh_session, node(), ram_copies).

create_session(#bosh_session{} = Session) ->
    mnesia:sync_dirty(fun mnesia:write/1, [Session]).
