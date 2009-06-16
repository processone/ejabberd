%%%----------------------------------------------------------------------
%%% File    : mod_http_bind.erl
%%% Author  : Stefan Strigler <steve@zeank.in-berlin.de>
%%% Purpose : Implementation of XMPP over BOSH (XEP-0206)
%%% Created : Tue Feb 20 13:15:52 CET 2007
%%% Id      : $Id: $
%%%----------------------------------------------------------------------

%%%----------------------------------------------------------------------
%%% this module acts as a bridge to ejabberd_http_bind which implements
%%% the real stuff, this is to handle the new pluggable architecture for
%%% extending ejabberd's http service
%%%----------------------------------------------------------------------

-module(mod_http_bind).
-author('steve@zeank.in-berlin.de').

-define(MOD_HTTP_BIND_VERSION, "1.0").
-vsn(?MOD_HTTP_BIND_VERSION).
-define(ejabberd_debug, true).

-behaviour(gen_mod).

-export([
	start/2,
	stop/1,
	process/2
	]).

-include("ejabberd.hrl").
-include("jlib.hrl").
-include("ejabberd_http.hrl").


%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------

process([], #request{method = 'POST',
                     data = []}) ->
    ?DEBUG("Bad Request: no data", []),
    {400, [], {xmlelement, "h1", [],
	       [{xmlcdata, "400 Bad Request"}]}};
process([], #request{method = 'POST',
                     data = Data}) ->
    ?DEBUG("Data: '~p'", [Data]),
    ejabberd_http_bind:process_request(Data);
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
           [{xmlcdata, "XMPP over BOSH (XEP-0206)"}]}]},
        {xmlelement, "p", [],
         [{xmlcdata, integer_to_list(mnesia:table_info(http_bind, size)) ++ " sessions found."}]},
        {xmlelement, "p", [],
         [{xmlcdata, "Sponsored by "},
          {xmlelement, "a", [{"href", "http://mabber.com"}],
           [{xmlcdata, "mabber"}]},
          {xmlcdata, "."}]}
          ]}]};
process(_Path, _Request) ->
    ?DEBUG("Bad Request: ~p", [_Request]),
    {400, [], {xmlelement, "h1", [],
	       [{xmlcdata, "400 Bad Request"}]}}.


%%%----------------------------------------------------------------------
%%% BEHAVIOUR CALLBACKS
%%%----------------------------------------------------------------------
start(_Host, _Opts) ->
    supervisor:start_child(
      ejabberd_sup,
      {ejabberd_http_bind_sup,
       {ejabberd_tmp_sup, start_link,
        [ejabberd_http_bind_sup, ejabberd_http_bind]},
       permanent,
       infinity,
       supervisor,
       [ejabberd_tmp_sup]}),
    ok.

stop(_Host) ->
    supervisor:terminate_child(ejabberd_sup, ejabberd_http_bind_sup),
    ok.
