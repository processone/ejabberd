%%%----------------------------------------------------------------------
%%% File    : adhoc.erl
%%% Author  : Magnus Henoch <henoch@dtek.chalmers.se>
%%% Purpose : Provide helper functions for ad-hoc commands (XEP-0050)
%%% Created : 31 Oct 2005 by Magnus Henoch <henoch@dtek.chalmers.se>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2015   ProcessOne
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
%%% You should have received a copy of the GNU General Public License along
%%% with this program; if not, write to the Free Software Foundation, Inc.,
%%% 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
%%%
%%%----------------------------------------------------------------------

-module(adhoc).

-author('henoch@dtek.chalmers.se').

-export([
    parse_request/1,
    produce_response/2,
    produce_response/1
]).

-include("ejabberd.hrl").
-include("logger.hrl").
-include("jlib.hrl").
-include("adhoc.hrl").

%% Parse an ad-hoc request.  Return either an adhoc_request record or
%% an {error, ErrorType} tuple.
%%
-spec(parse_request/1 ::
(
  IQ :: iq_request())
    -> adhoc_response()
    %%
     | {error, _}
).

parse_request(#iq{type = set, lang = Lang, sub_el = SubEl, xmlns = ?NS_COMMANDS}) ->
    ?DEBUG("entering parse_request...", []),
    Node = xml:get_tag_attr_s(<<"node">>, SubEl),
    SessionID = xml:get_tag_attr_s(<<"sessionid">>, SubEl),
    Action = xml:get_tag_attr_s(<<"action">>, SubEl),
    XData = find_xdata_el(SubEl),
    #xmlel{children = AllEls} = SubEl,
    Others = case XData of
        false -> AllEls;
        _     -> lists:delete(XData, AllEls)
    end,
    #adhoc_request{
        lang      = Lang,
        node      = Node,
        sessionid = SessionID,
        action    = Action,
        xdata     = XData,
        others    = Others
    };
parse_request(_) -> {error, ?ERR_BAD_REQUEST}.

%% Borrowed from mod_vcard.erl
find_xdata_el(#xmlel{children = SubEls}) ->
    find_xdata_el1(SubEls).

find_xdata_el1([]) -> false;
find_xdata_el1([El | Els]) when is_record(El, xmlel) ->
    case xml:get_tag_attr_s(<<"xmlns">>, El) of
        ?NS_XDATA -> El;
        _         -> find_xdata_el1(Els)
    end;
find_xdata_el1([_ | Els]) -> find_xdata_el1(Els).

%% Produce a <command/> node to use as response from an adhoc_response
%% record, filling in values for language, node and session id from
%% the request.
%%
-spec(produce_response/2 ::
(
  Adhoc_Request  :: adhoc_request(),
  Adhoc_Response :: adhoc_response())
    -> Xmlel::xmlel()
).

%% Produce a <command/> node to use as response from an adhoc_response
%% record.
produce_response(#adhoc_request{lang = Lang, node = Node, sessionid = SessionID},
  Adhoc_Response) ->
    produce_response(Adhoc_Response#adhoc_response{
        lang = Lang, node = Node, sessionid = SessionID
    }).

%%
-spec(produce_response/1 ::
(
  Adhoc_Response::adhoc_response())
    -> Xmlel::xmlel()
).

produce_response(
  #adhoc_response{
   %lang          = _Lang,
    node          = Node,
    sessionid     = ProvidedSessionID,
    status        = Status,
    defaultaction = DefaultAction,
    actions       = Actions,
    notes         = Notes,
    elements      = Elements
  }) ->
    SessionID = if is_binary(ProvidedSessionID),
        ProvidedSessionID /= <<"">> -> ProvidedSessionID;
        true                        -> jlib:now_to_utc_string(p1_time_compat:timestamp())
    end,
    case Actions of
        [] ->
            ActionsEls = [];
        _ ->
            case DefaultAction of
                <<"">> -> ActionsElAttrs = [];
                _      -> ActionsElAttrs = [{<<"execute">>, DefaultAction}]
            end,
            ActionsEls = [
                #xmlel{
                    name = <<"actions">>,
                    attrs = ActionsElAttrs,
                    children = [
                        #xmlel{name = Action, attrs = [], children = []}
                            || Action <- Actions]
                }
            ]
    end,
    NotesEls = lists:map(fun({Type, Text}) ->
        #xmlel{
            name     = <<"note">>,
            attrs    = [{<<"type">>, Type}],
            children = [{xmlcdata, Text}]
        }
    end, Notes),
    #xmlel{
        name     = <<"command">>,
        attrs    = [
            {<<"xmlns">>,     ?NS_COMMANDS},
            {<<"sessionid">>, SessionID},
            {<<"node">>,      Node},
            {<<"status">>,    iolist_to_binary(atom_to_list(Status))}
        ],
        children = ActionsEls ++ NotesEls ++ Elements
    }.
