%%%----------------------------------------------------------------------
%%% File    : adhoc.erl
%%% Author  : Magnus Henoch <henoch@dtek.chalmers.se>
%%% Purpose : Provide helper functions for ad-hoc commands (XEP-0050)
%%% Created : 31 Oct 2005 by Magnus Henoch <henoch@dtek.chalmers.se>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2012   ProcessOne
%%%
%%% This program is free software; you can redistribute it and/or
%%% modify it under the terms of the GNU General Public License as
%%% published by the Free Software Foundation; either version 2 of the
%%% License, or (at your option) any later version.
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%%% General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with this program; if not, write to the Free Software
%%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
%%% 02111-1307 USA
%%%
%%%----------------------------------------------------------------------

-module(adhoc).
-author('henoch@dtek.chalmers.se').

-export([parse_request/1,
	 produce_response/2,
	 produce_response/1]).

-include_lib("exmpp/include/exmpp.hrl").

-include("ejabberd.hrl").
-include("adhoc.hrl").

%% Parse an ad-hoc request.  Return either an adhoc_request record or
%% an {error, ErrorType} tuple.
parse_request(#iq{type = Type, ns = NS, payload = SubEl, lang = Lang}) ->
    try
	case {Type, NS} of
	    {set, ?NS_ADHOC} ->
		?DEBUG("entering parse_request...", []),
		Node = exmpp_xml:get_attribute_as_list(SubEl, <<"node">>, ""),
		SessionID = exmpp_xml:get_attribute_as_list(SubEl, <<"sessionid">>, ""),
		Action = exmpp_xml:get_attribute_as_list(SubEl, <<"action">>, ""),
		XData = find_xdata_el(SubEl),
		AllEls = exmpp_xml:get_child_elements(SubEl),
		Others = case XData of
		    false ->
			AllEls;
		    _ ->
			lists:delete(XData, AllEls)
		end,

		#adhoc_request{lang = Lang,
			       node = Node,
			       sessionid = SessionID,
			       action = Action,
			       xdata = XData,
			       others = Others};
	    _ ->
		{error, 'bad-request'}
	end
    catch
	_ ->
	    {error, 'bad-request'}
    end.

%% Borrowed from mod_vcard.erl
find_xdata_el(#xmlel{children = SubEls}) ->
    find_xdata_el1(SubEls).

find_xdata_el1([]) ->
    false;
find_xdata_el1([#xmlel{ns = ?NS_DATA_FORMS} = El | _Els]) ->
    El;
find_xdata_el1([_ | Els]) ->
    find_xdata_el1(Els).

%% Produce a <command/> node to use as response from an adhoc_response
%% record, filling in values for language, node and session id from
%% the request.
produce_response(#adhoc_request{lang = Lang,
				node = Node,
				sessionid = SessionID},
		 Response) ->
    produce_response(Response#adhoc_response{lang = Lang,
					     node = Node,
					     sessionid = SessionID}).

%% Produce a <command/> node to use as response from an adhoc_response
%% record.
produce_response(#adhoc_response{lang = _Lang,
				 node = Node,
				 sessionid = ProvidedSessionID,
				 status = Status,
				 defaultaction = DefaultAction,
				 actions = Actions,
				 notes = Notes,
				 elements = Elements}) ->
    SessionID = if is_list(ProvidedSessionID), ProvidedSessionID /= "" ->
			ProvidedSessionID;
		   true ->
			jlib:now_to_utc_string(now())
		end,
    case Actions of
	[] ->
	    ActionsEls = [];
	_ ->
	    case DefaultAction of
		"" ->
		    ActionsElAttrs = [];
		_ ->
		    ActionsElAttrs = [?XMLATTR(<<"execute">>, DefaultAction)]
	    end,
	    ActionsEls = [#xmlel{ns = ?NS_ADHOC, name = 'actions', attrs =
			   ActionsElAttrs, children =
			   [#xmlel{ns = ?NS_ADHOC, name = Action} || Action <- Actions]}]
    end,
    NotesEls = lists:map(fun({Type, Text}) ->
				 #xmlel{ns = ?NS_ADHOC, name = 'note', attrs =
				  [?XMLATTR(<<"type">>, Type)],
				  children = [#xmlcdata{cdata = list_to_binary(Text)}]}
			 end, Notes),
    #xmlel{ns = ?NS_ADHOC, name = 'command', attrs =
     [?XMLATTR(<<"sessionid">>, SessionID),
      ?XMLATTR(<<"node">>, Node),
      ?XMLATTR(<<"status">>, Status)], children =
     ActionsEls ++ NotesEls ++ Elements}.
