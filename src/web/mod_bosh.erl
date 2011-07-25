%%%-------------------------------------------------------------------
%%% @author Evgeniy Khramtsov <ekhramtsov@process-one.net>
%%% @copyright (C) 2011, Evgeniy Khramtsov
%%% @doc
%%% This module acts as a bridge to ejabberd_bosh which implements
%%% the real stuff, this is to handle the new pluggable architecture for
%%% extending ejabberd's http service.
%%% @end
%%% Created : 20 Jul 2011 by Evgeniy Khramtsov <ekhramtsov@process-one.net>
%%%-------------------------------------------------------------------

-module(mod_bosh).
-author('steve@zeank.in-berlin.de').

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
-include("bosh.hrl").

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------

process([], #request{method = 'POST',
                     data = []}) ->
    ?DEBUG("Bad Request: no data", []),
    {400, ?HEADER, {xmlelement, "h1", [],
                    [{xmlcdata, "400 Bad Request"}]}};
process([], #request{method = 'POST',
                     data = Data,
                     ip = IP}) ->
    ?DEBUG("Incoming data: ~s", [Data]),
    ejabberd_bosh:process_request(Data, IP);
process([], #request{method = 'GET',
                     data = []}) ->
    {200, ?HEADER, get_human_html_xmlel()};
process([], #request{method = 'OPTIONS',
                     data = []}) ->
    {200, ?OPTIONS_HEADER, []};
process(_Path, _Request) ->
    ?DEBUG("Bad Request: ~p", [_Request]),
    {400, ?HEADER, {xmlelement, "h1", [],
                    [{xmlcdata, "400 Bad Request"}]}}.

get_human_html_xmlel() ->
    Heading = "ejabberd " ++ atom_to_list(?MODULE),
    {xmlelement, "html", [{"xmlns", "http://www.w3.org/1999/xhtml"}],
     [{xmlelement, "head", [],
       [{xmlelement, "title", [], [{xmlcdata, Heading}]}]},
      {xmlelement, "body", [],
       [{xmlelement, "h1", [], [{xmlcdata, Heading}]},
        {xmlelement, "p", [],
         [{xmlcdata, "An implementation of "},
          {xmlelement, "a",
	   [{"href", "http://xmpp.org/extensions/xep-0206.html"}],
           [{xmlcdata, "XMPP over BOSH (XEP-0206)"}]}]},
        {xmlelement, "p", [],
         [{xmlcdata, "This web page is only informative. "
	   "To use HTTP-Bind you need a Jabber/XMPP client that supports it."}
	 ]}
       ]}]}.

%%%----------------------------------------------------------------------
%%% BEHAVIOUR CALLBACKS
%%%----------------------------------------------------------------------
start(Host, _Opts) ->
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    ChildSpec =
        {Proc,
         {ejabberd_tmp_sup, start_link,
          [Proc, ejabberd_bosh]},
         permanent,
         infinity,
         supervisor,
         [ejabberd_tmp_sup]},
    supervisor:start_child(ejabberd_sup, ChildSpec).

stop(Host) ->
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    supervisor:terminate_child(ejabberd_sup, Proc),
    supervisor:delete_child(ejabberd_sup, Proc).
