%%%-------------------------------------------------------------------
%%% @author Evgeny Khramtsov <ekhramtsov@process-one.net>
%%% @copyright (C) 2015, Evgeny Khramtsov
%%% @doc
%%%
%%% @end
%%% Created : 10 Dec 2015 by Evgeny Khramtsov <ekhramtsov@process-one.net>
%%%-------------------------------------------------------------------
-include("ns.hrl").
-include("jid.hrl").
-include("xmpp_codec.hrl").
-ifdef(NO_EXT_LIB).
-include("fxml.hrl").
-else.
-include_lib("fast_xml/include/fxml.hrl").
-endif.

-type iq_type() :: get | set | result | error.
-type message_type() :: chat | error | groupchat | headline | normal.
-type presence_type() :: available | error | probe | subscribe |
			 subscribed | unavailable | unsubscribe |
			 unsubscribed.

-type stanza() :: iq() | presence() | message().

-define(is_stanza(Pkt),
	(is_record(Pkt, iq) or
	 is_record(Pkt, message) or
	 is_record(Pkt, presence))).
