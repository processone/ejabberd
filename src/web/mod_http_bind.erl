%%%----------------------------------------------------------------------
%%% File    : mod_http_bind.erl
%%% Author  : Stefan Strigler <steve@zeank.in-berlin.de>
%%% Purpose : Implementation of XMPP over BOSH (XEP-0206)
%%% Created : Tue Feb 20 13:15:52 CET 2007
%%% Id      : $Id: mod_http_bind.erl 942 2009-04-22 15:25:31Z mremond $
%%%----------------------------------------------------------------------

%%%----------------------------------------------------------------------
%%% this module acts as a bridge to ejabberd_http_bind which implements
%%% the real stuff, this is to handle the new pluggable architecture for
%%% extending ejabberd's http service
%%%----------------------------------------------------------------------

-module(mod_http_bind).
-author('steve@zeank.in-berlin.de').

-define(MOD_HTTP_BIND_VERSION, "1.2").

%%-define(ejabberd_debug, true).

-behaviour(gen_mod).

-export([
         start/2,
         stop/1,
         process/2
	]).

-include("ejabberd.hrl").
-include("jlib.hrl").
-include("ejabberd_http.hrl").

%% Duplicated from ejabberd_http_bind.
%% TODO: move to hrl file.
-record(http_bind, {id, pid, to, hold, wait, version}).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------

process([], #request{method = 'POST',
                     data = []}) ->
    ?DEBUG("Bad Request: no data", []),
    {400, [], {xmlelement, "h1", [],
	       [{xmlcdata, "400 Bad Request"}]}};
process([], #request{method = 'POST',
                     data = Data,
                     ip = IP}) ->
    ?DEBUG("Incoming data: ~s", [Data]),
    ejabberd_http_bind:process_request(Data, IP);
process([], #request{method = 'GET',
                     data = []}) ->
    Heading = "Ejabberd " ++ atom_to_list(?MODULE) ++ " v" ++ ?MOD_HTTP_BIND_VERSION,
    {xmlelement, "html", [{"xmlns", "http://www.w3.org/1999/xhtml"}],
     [{xmlelement, "head", [],
       [{xmlelement, "title", [], [{xmlcdata, Heading}]}]},
      {xmlelement, "body", [],
       [{xmlelement, "h1", [], [{xmlcdata, Heading}]},
        {xmlelement, "p", [],
         [{xmlcdata, "An implementation of "},
          {xmlelement, "a", [{"href", "http://www.xmpp.org/extensions/xep-0206.html"}],
           [{xmlcdata, "XMPP over BOSH (XEP-0206)"}]}]}
       ]}]};
process(_Path, _Request) ->
    ?DEBUG("Bad Request: ~p", [_Request]),
    {400, [], {xmlelement, "h1", [],
	       [{xmlcdata, "400 Bad Request"}]}}.


%%%----------------------------------------------------------------------
%%% BEHAVIOUR CALLBACKS
%%%----------------------------------------------------------------------
start(_Host, _Opts) ->
    setup_database(),
    HTTPBindSupervisor =
        {ejabberd_http_bind_sup,
         {ejabberd_tmp_sup, start_link,
          [ejabberd_http_bind_sup, ejabberd_http_bind]},
         permanent,
         infinity,
         supervisor,
         [ejabberd_tmp_sup]},
    case supervisor:start_child(ejabberd_sup, HTTPBindSupervisor) of
        {ok, _Pid} ->
            ok;
        {ok, _Pid, _Info} ->
            ok;
        {error, {already_started, _PidOther}} ->
            % mod_http_bind is already started so it will not be started again
            ok;
        {error, Error} ->
            {'EXIT', {start_child_error, Error}}
    end.

stop(_Host) ->
    case supervisor:terminate_child(ejabberd_sup, ejabberd_http_bind_sup) of
        ok ->
            ok;
        {error, Error} ->
            {'EXIT', {terminate_child_error, Error}}
    end.

setup_database() ->
    migrate_database(),
    mnesia:create_table(http_bind,
			[{ram_copies, [node()]},
			 {attributes, record_info(fields, http_bind)}]).

migrate_database() ->
    case catch mnesia:table_info(http_bind, attributes) of
        [id, pid, to, hold, wait, version] ->
	    ok;
        _ ->
	    %% Since the stored information is not important, instead
	    %% of actually migrating data, let's just destroy the table
	    mnesia:delete_table(http_bind)
    end.
