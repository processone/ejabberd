%%%----------------------------------------------------------------------
%%% File    : jlib.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : General XMPP library.
%%% Created : 23 Nov 2002 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2017   ProcessOne
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

-module(jlib).

-author('alexey@process-one.net').

-protocol({xep, 59, '1.0'}).
-protocol({xep, 82, '1.1'}).
-protocol({xep, 203, '2.0'}).

-compile({no_auto_import, [atom_to_binary/2,
                           binary_to_integer/1,
                           integer_to_binary/1]}).

%% The following functions are deprected: use functions from aux.erl
-export([tolower/1, term_to_base64/1, base64_to_term/1,
	 decode_base64/1, encode_base64/1, ip_to_list/1,
	 hex_to_bin/1, hex_to_base64/1, expand_keyword/3,
	 atom_to_binary/1, binary_to_atom/1, tuple_to_binary/1,
	 l2i/1, i2l/1, i2l/2, expr_to_term/1, term_to_expr/1]).

%% The following functions are used by gen_iq_handler.erl for providing backward
%% compatibility and must not be used in other parts of the code
%% Use xmpp:decode() and xmpp:encode() instead
-export([iq_query_info/1, iq_to_xml/1]).

%% The following functions are deprecated and will be removed soon
%% Use functions from xmpp.erl and xmpp_util.erl instead
-export([make_result_iq_reply/1, make_error_reply/3,
	 make_error_reply/2, make_error_element/2,
	 make_correct_from_to_attrs/3, replace_from_to_attrs/3,
	 replace_from_to/3, replace_from_attrs/2, replace_from/2,
	 remove_attr/2, get_iq_namespace/1,
	 iq_query_or_response_info/1, is_iq_request_type/1,
	 parse_xdata_submit/1, unwrap_carbon/1, is_standalone_chat_state/1,
	 add_delay_info/3, add_delay_info/4,
	 timestamp_to_legacy/1, timestamp_to_iso_basic/1, timestamp_to_iso/2,
	 now_to_utc_string/1, now_to_local_string/1,
	 datetime_string_to_timestamp/1,
	 rsm_encode/1, rsm_encode/2, rsm_decode/1,
	 binary_to_integer/1, binary_to_integer/2,
	 integer_to_binary/1, integer_to_binary/2]).

%% The following functions are deprecated and will be removed soon
%% Use corresponding functions from jid.erl instead
-export([make_jid/3, make_jid/1, split_jid/1, string_to_jid/1,
	 jid_to_string/1, is_nodename/1, nodeprep/1,
	 nameprep/1, resourceprep/1, jid_tolower/1,
	 jid_remove_resource/1, jid_replace_resource/2]).

-deprecated([{make_jid, '_'},
	     {split_jid, 1},
	     {string_to_jid, 1},
	     {jid_to_string, 1},
	     {is_nodename, 1},
	     {nodeprep, 1},
	     {nameprep, 1},
	     {resourceprep, 1},
	     {jid_tolower, 1},
	     {jid_remove_resource, 1},
	     {jid_replace_resource, 2},
	     {add_delay_info, 3},
	     {add_delay_info, 4},
	     {make_result_iq_reply, 1},
	     {make_error_reply, 3},
	     {make_error_reply, 2},
	     {make_error_element, 2},
	     {make_correct_from_to_attrs, 3},
	     {replace_from_to_attrs, 3},
	     {replace_from_to, 3},
	     {replace_from_attrs, 2},
	     {replace_from, 2},
	     {remove_attr, 2},
	     {get_iq_namespace, 1},
	     {iq_query_or_response_info, 1},
	     {is_iq_request_type, 1},
	     {parse_xdata_submit, 1},
	     {unwrap_carbon, 1},
	     {is_standalone_chat_state, 1},
	     {timestamp_to_legacy, 1},
	     {timestamp_to_iso_basic, 1},
	     {timestamp_to_iso, 2},
	     {now_to_utc_string, 1},
	     {now_to_local_string, 1},
	     {datetime_string_to_timestamp, 1},
	     {rsm_encode, 1},
	     {rsm_encode, 2},
	     {rsm_decode, 1},
	     {binary_to_integer, 1},
	     {binary_to_integer, 2},
	     {integer_to_binary, 1},
	     {integer_to_binary, 2},
	     {tolower, 1},
	     {term_to_base64, 1},
	     {base64_to_term, 1},
	     {decode_base64, 1},
	     {encode_base64, 1},
	     {ip_to_list, 1},
	     {hex_to_bin, 1},
	     {hex_to_base64, 1},
	     {expand_keyword, 3},
	     {atom_to_binary, 1},
	     {binary_to_atom, 1},
	     {tuple_to_binary, 1},
	     {l2i, 1},
	     {i2l, 1},
	     {i2l, 2},
	     {expr_to_term, 1},
	     {term_to_expr, 1}]).

-include("ejabberd.hrl").
-include("jlib.hrl").

%send_iq(From, To, ID, SubTags) ->
%    ok.

-spec make_result_iq_reply(xmlel()) -> xmlel().

make_result_iq_reply(#xmlel{name = Name, attrs = Attrs,
			    children = SubTags}) ->
    NewAttrs = make_result_iq_reply_attrs(Attrs),
    #xmlel{name = Name, attrs = NewAttrs,
	   children = SubTags}.

-spec make_result_iq_reply_attrs([attr()]) -> [attr()].

make_result_iq_reply_attrs(Attrs) ->
    To = fxml:get_attr(<<"to">>, Attrs),
    From = fxml:get_attr(<<"from">>, Attrs),
    Attrs1 = lists:keydelete(<<"to">>, 1, Attrs),
    Attrs2 = lists:keydelete(<<"from">>, 1, Attrs1),
    Attrs3 = case To of
	       {value, ToVal} -> [{<<"from">>, ToVal} | Attrs2];
	       _ -> Attrs2
	     end,
    Attrs4 = case From of
	       {value, FromVal} -> [{<<"to">>, FromVal} | Attrs3];
	       _ -> Attrs3
	     end,
    Attrs5 = lists:keydelete(<<"type">>, 1, Attrs4),
    Attrs6 = [{<<"type">>, <<"result">>} | Attrs5],
    Attrs6.

-spec make_error_reply(xmlel(), binary(), binary()) -> xmlel().

make_error_reply(#xmlel{name = Name, attrs = Attrs,
			children = SubTags},
		 Code, Desc) ->
    NewAttrs = make_error_reply_attrs(Attrs),
    #xmlel{name = Name, attrs = NewAttrs,
	   children =
	       SubTags ++
		 [#xmlel{name = <<"error">>,
			 attrs = [{<<"code">>, Code}],
			 children = [{xmlcdata, Desc}]}]}.

-spec make_error_reply(xmlel(), xmlel()) -> xmlel().

make_error_reply(#xmlel{name = Name, attrs = Attrs,
			children = SubTags},
		 Error) ->
    NewAttrs = make_error_reply_attrs(Attrs),
    #xmlel{name = Name, attrs = NewAttrs,
	   children = SubTags ++ [Error]}.

-spec make_error_reply_attrs([attr()]) -> [attr()].

make_error_reply_attrs(Attrs) ->
    To = fxml:get_attr(<<"to">>, Attrs),
    From = fxml:get_attr(<<"from">>, Attrs),
    Attrs1 = lists:keydelete(<<"to">>, 1, Attrs),
    Attrs2 = lists:keydelete(<<"from">>, 1, Attrs1),
    Attrs3 = case To of
	       {value, ToVal} -> [{<<"from">>, ToVal} | Attrs2];
	       _ -> Attrs2
	     end,
    Attrs4 = case From of
	       {value, FromVal} -> [{<<"to">>, FromVal} | Attrs3];
	       _ -> Attrs3
	     end,
    Attrs5 = lists:keydelete(<<"type">>, 1, Attrs4),
    Attrs6 = [{<<"type">>, <<"error">>} | Attrs5],
    Attrs6.

-spec make_error_element(binary(), binary()) -> xmlel().

make_error_element(Code, Desc) ->
    #xmlel{name = <<"error">>, attrs = [{<<"code">>, Code}],
	   children = [{xmlcdata, Desc}]}.

-spec make_correct_from_to_attrs(binary(), binary(), [attr()]) -> [attr()].

make_correct_from_to_attrs(From, To, Attrs) ->
    Attrs1 = lists:keydelete(<<"from">>, 1, Attrs),
    Attrs2 = case fxml:get_attr(<<"to">>, Attrs) of
	       {value, _} -> Attrs1;
	       _ -> [{<<"to">>, To} | Attrs1]
	     end,
    Attrs3 = [{<<"from">>, From} | Attrs2],
    Attrs3.

-spec replace_from_to_attrs(binary(), binary(), [attr()]) -> [attr()].

replace_from_to_attrs(From, To, Attrs) ->
    Attrs1 = lists:keydelete(<<"to">>, 1, Attrs),
    Attrs2 = lists:keydelete(<<"from">>, 1, Attrs1),
    Attrs3 = [{<<"to">>, To} | Attrs2],
    Attrs4 = [{<<"from">>, From} | Attrs3],
    Attrs4.

-spec replace_from_to(jid(), jid(), xmlel()) -> xmlel().

replace_from_to(From, To,
		#xmlel{name = Name, attrs = Attrs, children = Els}) ->
    NewAttrs =
	replace_from_to_attrs(jid:encode(From),
			      jid:encode(To), Attrs),
    #xmlel{name = Name, attrs = NewAttrs, children = Els}.

-spec replace_from_attrs(binary(), [attr()]) -> [attr()].

replace_from_attrs(From, Attrs) ->
    Attrs1 = lists:keydelete(<<"from">>, 1, Attrs),
    [{<<"from">>, From} | Attrs1].

-spec replace_from(jid(), xmlel()) -> xmlel().

replace_from(From,
	     #xmlel{name = Name, attrs = Attrs, children = Els}) ->
    NewAttrs = replace_from_attrs(jid:encode(From),
				  Attrs),
    #xmlel{name = Name, attrs = NewAttrs, children = Els}.

-spec remove_attr(binary(), xmlel()) -> xmlel().

remove_attr(Attr,
	    #xmlel{name = Name, attrs = Attrs, children = Els}) ->
    NewAttrs = lists:keydelete(Attr, 1, Attrs),
    #xmlel{name = Name, attrs = NewAttrs, children = Els}.

-spec make_jid(binary(), binary(), binary()) -> jid() | error.

make_jid(User, Server, Resource) ->
    jid:make(User, Server, Resource).

-spec make_jid({binary(), binary(), binary()}) -> jid() | error.

make_jid({User, Server, Resource}) ->
    jid:make({User, Server, Resource}).

%% This is the reverse of make_jid/1
-spec split_jid(jid()) -> {binary(), binary(), binary()} | error.
split_jid(J) ->
    jid:split(J).

-spec string_to_jid(binary()) -> jid() | error.

string_to_jid(S) ->
    try jid:decode(S)
    catch _:_ -> error end.

-spec jid_to_string(jid() | ljid()) -> binary().

jid_to_string(J) ->
    jid:encode(J).

-spec is_nodename(binary()) -> boolean().

is_nodename(Node) ->
    jid:is_nodename(Node).

%tolower_c(C) when C >= $A, C =< $Z ->
%    C + 32;
%tolower_c(C) ->
%    C.

-define(LOWER(Char),
	if Char >= $A, Char =< $Z -> Char + 32;
	   true -> Char
	end).

%tolower(S) ->
%    lists:map(fun tolower_c/1, S).

%tolower(S) ->
%    [?LOWER(Char) || Char <- S].

-spec tolower(binary()) -> binary().

tolower(B) ->
    iolist_to_binary(tolower_s(binary_to_list(B))).

tolower_s([C | Cs]) ->
    if C >= $A, C =< $Z -> [C + 32 | tolower_s(Cs)];
       true -> [C | tolower_s(Cs)]
    end;
tolower_s([]) -> [].

%tolower([C | Cs]) when C >= $A, C =< $Z ->
%    [C + 32 | tolower(Cs)];
%tolower([C | Cs]) ->
%    [C | tolower(Cs)];
%tolower([]) ->
%    [].

-spec nodeprep(binary()) -> binary() | error.

nodeprep(S) -> jid:nodeprep(S).

-spec nameprep(binary()) -> binary() | error.

nameprep(S) -> jid:nameprep(S).

-spec resourceprep(binary()) -> binary() | error.

resourceprep(S) -> jid:resourceprep(S).

-spec jid_tolower(jid() | ljid()) -> error | ljid().

jid_tolower(J) ->
    jid:tolower(J).

-spec jid_remove_resource(jid()) -> jid();
                         (ljid()) -> ljid().

jid_remove_resource(J) -> jid:remove_resource(J).

-spec jid_replace_resource(jid(), binary()) -> error | jid().

jid_replace_resource(JID, Resource) ->
    jid:replace_resource(JID, Resource).

-spec get_iq_namespace(xmlel()) -> binary().

get_iq_namespace(#xmlel{name = <<"iq">>, children = Els}) ->
    case fxml:remove_cdata(Els) of
        [#xmlel{attrs = Attrs}] -> fxml:get_attr_s(<<"xmlns">>, Attrs);
        _                       -> <<"">>
    end;
get_iq_namespace(_) -> <<"">>.

%%
-spec iq_query_info(Xmlel :: xmlel()) ->
			   iq_request() | 'reply' | 'invalid' | 'not_iq'.

%% @spec (xmlelement()) -> iq() | reply | invalid | not_iq
iq_query_info(El) -> iq_info_internal(El, request).

%%
-spec iq_query_or_response_info(Xmlel :: xmlel()) ->
				       iq_request() | iq_reply() |
				       'reply' | 'invalid' | 'not_iq'.

iq_query_or_response_info(El) ->
    iq_info_internal(El, any).

iq_info_internal(#xmlel{name = <<"iq">>, attrs = Attrs, children = Els}, Filter) ->
    ID = fxml:get_attr_s(<<"id">>, Attrs),
    Lang = fxml:get_attr_s(<<"xml:lang">>, Attrs),
    {Type, Class} = case fxml:get_attr_s(<<"type">>, Attrs) of
        <<"set">>    -> {set,     request};
        <<"get">>    -> {get,     request};
        <<"result">> -> {result,  reply};
        <<"error">>  -> {error,   reply};
        _            -> {invalid, invalid}
    end,
    if Type == invalid -> invalid; Class == request; Filter == any ->
        FilteredEls = fxml:remove_cdata(Els),
        {XMLNS, SubEl} = case {Class, FilteredEls} of
            {request, [#xmlel{attrs = Attrs2}]} ->
                {fxml:get_attr_s(<<"xmlns">>, Attrs2), hd(FilteredEls)};
            {reply, _} ->
                NonErrorEls = [El || #xmlel{name = SubName} = El <- FilteredEls,
                    SubName /= <<"error">>],
                {case NonErrorEls of
                     [NonErrorEl] -> fxml:get_tag_attr_s(<<"xmlns">>, NonErrorEl);
                     _            -> <<"">>
                 end,
                 FilteredEls};
            _ ->
                {<<"">>, []}
        end,
        if XMLNS == <<"">>, Class == request ->
            invalid;
        true ->
            #iq{id = ID, type = Type, xmlns = XMLNS, lang = Lang, sub_el = SubEl}
        end;
    Class == reply, Filter /= any ->
        reply
    end;
iq_info_internal(_, _) -> not_iq.

-spec is_iq_request_type(set | get | result | error) -> boolean().

is_iq_request_type(set) -> true;
is_iq_request_type(get) -> true;
is_iq_request_type(_) -> false.

iq_type_to_string(set) -> <<"set">>;
iq_type_to_string(get) -> <<"get">>;
iq_type_to_string(result) -> <<"result">>;
iq_type_to_string(error) -> <<"error">>.

-spec iq_to_xml(IQ :: iq()) -> xmlel().

iq_to_xml(#iq{id = ID, type = Type, sub_el = SubEl}) ->
    Children = 
        if
	    is_list(SubEl) -> SubEl;
	    true -> [SubEl]
        end,
    if ID /= <<"">> ->
	   #xmlel{name = <<"iq">>,
		  attrs =
		      [{<<"id">>, ID}, {<<"type">>, iq_type_to_string(Type)}],
		  children = Children};
       true ->
	   #xmlel{name = <<"iq">>,
		  attrs = [{<<"type">>, iq_type_to_string(Type)}],
		  children = Children}
    end.

-spec parse_xdata_submit(El :: xmlel()) ->
				[{Var::binary(), Values::[binary()]}] | 'invalid'.

parse_xdata_submit(#xmlel{attrs = Attrs, children = Els}) ->
    case fxml:get_attr_s(<<"type">>, Attrs) of
        <<"submit">> ->
            lists:reverse(parse_xdata_fields(Els, []));
        <<"form">> -> %% This is a workaround to accept Psi's wrong forms
            lists:reverse(parse_xdata_fields(Els, []));
        _ ->
            invalid
    end.

-spec parse_xdata_fields(Xmlels :: [xmlel() | cdata()],
			 Res :: [{Var::binary(), Values :: [binary()]}]) ->
				[{Var::binary(), Values::[binary()]}].

parse_xdata_fields([], Res) -> Res;
parse_xdata_fields([#xmlel{name = <<"field">>, attrs = Attrs, children = SubEls}
  | Els], Res) ->
    case fxml:get_attr_s(<<"var">>, Attrs) of
        <<>> ->
            parse_xdata_fields(Els, Res);
        Var ->
            Field = {Var, lists:reverse(parse_xdata_values(SubEls, []))},
            parse_xdata_fields(Els, [Field | Res])
    end;
parse_xdata_fields([_ | Els], Res) ->
    parse_xdata_fields(Els, Res).

-spec parse_xdata_values(Xmlels :: [xmlel() | cdata()],
			 Res :: [binary()]) -> [binary()].

parse_xdata_values([], Res) -> Res;
parse_xdata_values([#xmlel{name = <<"value">>, children = SubEls} | Els], Res) ->
    Val = fxml:get_cdata(SubEls),
    parse_xdata_values(Els, [Val | Res]);
parse_xdata_values([_ | Els], Res) ->
    parse_xdata_values(Els, Res).

-spec rsm_decode(iq() | xmlel()) -> none | rsm_in().

rsm_decode(#iq{sub_el = SubEl}) -> rsm_decode(SubEl);
rsm_decode(#xmlel{} = SubEl) ->
    case fxml:get_subtag(SubEl, <<"set">>) of
      false -> none;
      #xmlel{name = <<"set">>, children = SubEls} ->
	  lists:foldl(fun rsm_parse_element/2, #rsm_in{}, SubEls)
    end.

rsm_parse_element(#xmlel{name = <<"max">>, attrs = []} =
		      Elem,
		  RsmIn) ->
    CountStr = fxml:get_tag_cdata(Elem),
    {Count, _} = str:to_integer(CountStr),
    RsmIn#rsm_in{max = Count};
rsm_parse_element(#xmlel{name = <<"before">>,
			 attrs = []} =
		      Elem,
		  RsmIn) ->
    UID = fxml:get_tag_cdata(Elem),
    RsmIn#rsm_in{direction = before, id = UID};
rsm_parse_element(#xmlel{name = <<"after">>,
			 attrs = []} =
		      Elem,
		  RsmIn) ->
    UID = fxml:get_tag_cdata(Elem),
    RsmIn#rsm_in{direction = aft, id = UID};
rsm_parse_element(#xmlel{name = <<"index">>,
			 attrs = []} =
		      Elem,
		  RsmIn) ->
    IndexStr = fxml:get_tag_cdata(Elem),
    {Index, _} = str:to_integer(IndexStr),
    RsmIn#rsm_in{index = Index};
rsm_parse_element(_, RsmIn) -> RsmIn.

-spec rsm_encode(iq(), rsm_out()) -> iq().

rsm_encode(#iq{sub_el = SubEl} = IQ, RsmOut) ->
    Set = #xmlel{name = <<"set">>,
		 attrs = [{<<"xmlns">>, ?NS_RSM}],
		 children = lists:reverse(rsm_encode_out(RsmOut))},
    #xmlel{name = Name, attrs = Attrs, children = SubEls} =
	SubEl,
    New = #xmlel{name = Name, attrs = Attrs,
		 children = [Set | SubEls]},
    IQ#iq{sub_el = New}.

-spec rsm_encode(none | rsm_out()) -> [xmlel()].

rsm_encode(none) -> [];
rsm_encode(RsmOut) ->
    [#xmlel{name = <<"set">>,
	    attrs = [{<<"xmlns">>, ?NS_RSM}],
	    children = lists:reverse(rsm_encode_out(RsmOut))}].

rsm_encode_out(#rsm_out{count = Count, index = Index,
			first = First, last = Last}) ->
    El = rsm_encode_first(First, Index, []),
    El2 = rsm_encode_last(Last, El),
    rsm_encode_count(Count, El2).

rsm_encode_first(undefined, undefined, Arr) -> Arr;
rsm_encode_first(First, undefined, Arr) ->
    [#xmlel{name = <<"first">>, attrs = [],
	    children = [{xmlcdata, First}]}
     | Arr];
rsm_encode_first(First, Index, Arr) ->
    [#xmlel{name = <<"first">>,
	    attrs = [{<<"index">>, i2l(Index)}],
	    children = [{xmlcdata, First}]}
     | Arr].

rsm_encode_last(undefined, Arr) -> Arr;
rsm_encode_last(Last, Arr) ->
    [#xmlel{name = <<"last">>, attrs = [],
	    children = [{xmlcdata, Last}]}
     | Arr].

rsm_encode_count(undefined, Arr) -> Arr;
rsm_encode_count(Count, Arr) ->
    [#xmlel{name = <<"count">>, attrs = [],
	    children = [{xmlcdata, i2l(Count)}]}
     | Arr].

-spec unwrap_carbon(xmlel()) -> xmlel().

unwrap_carbon(#xmlel{name = <<"message">>} = Stanza) ->
    case unwrap_carbon(Stanza, <<"sent">>) of
      #xmlel{} = Payload ->
	  Payload;
      false ->
	  case unwrap_carbon(Stanza, <<"received">>) of
	    #xmlel{} = Payload ->
		Payload;
	    false ->
		Stanza
	  end
    end;
unwrap_carbon(Stanza) -> Stanza.

-spec unwrap_carbon(xmlel(), binary()) -> xmlel() | false.

unwrap_carbon(Stanza, Direction) ->
    case fxml:get_subtag(Stanza, Direction) of
      #xmlel{name = Direction, attrs = Attrs} = El ->
	  case fxml:get_attr_s(<<"xmlns">>, Attrs) of
	    NS when NS == ?NS_CARBONS_2;
		    NS == ?NS_CARBONS_1 ->
		case fxml:get_subtag_with_xmlns(El, <<"forwarded">>,
						?NS_FORWARD) of
		  #xmlel{children = Els} ->
		      case fxml:remove_cdata(Els) of
			[#xmlel{} = Payload] ->
			    Payload;
			_ ->
			    false
		      end;
		  false ->
		      false
		end;
	    _NS ->
		false
	  end;
      false ->
	  false
    end.

-spec is_standalone_chat_state(xmlel()) -> boolean().

is_standalone_chat_state(Stanza) ->
    case unwrap_carbon(Stanza) of
      #xmlel{name = <<"message">>, children = Els} ->
	  IgnoreNS = [?NS_CHATSTATES, ?NS_DELAY],
	  Stripped = [El || #xmlel{name = Name, attrs = Attrs} = El <- Els,
			    not lists:member(fxml:get_attr_s(<<"xmlns">>,
							     Attrs),
					     IgnoreNS),
			    Name /= <<"thread">>],
	  Stripped == [];
      #xmlel{} ->
	  false
    end.

-spec add_delay_info(xmlel(), jid() | ljid() | binary(), erlang:timestamp())
		     -> xmlel().

add_delay_info(El, From, Time) ->
    add_delay_info(El, From, Time, <<"">>).

-spec add_delay_info(xmlel(), jid() | ljid() | binary(), erlang:timestamp(),
		     binary()) -> xmlel().

add_delay_info(El, From, Time, Desc) ->
	  DelayTag = create_delay_tag(Time, From, Desc),
    fxml:append_subtags(El, [DelayTag]).

-spec create_delay_tag(erlang:timestamp(), jid() | ljid() | binary(), binary())
		       -> xmlel() | error.

create_delay_tag(TimeStamp, FromJID, Desc) when is_tuple(FromJID) ->
    From = jid:encode(FromJID),
    Stamp = now_to_utc_string(TimeStamp, 3),
    Children = case Desc of
		 <<"">> -> [];
		 _ -> [{xmlcdata, Desc}]
	       end,
    #xmlel{name = <<"delay">>,
	   attrs =
	       [{<<"xmlns">>, ?NS_DELAY}, {<<"from">>, From},
		{<<"stamp">>, Stamp}],
	   children = Children};
create_delay_tag(DateTime, Host, Desc) when is_binary(Host) ->
    FromJID = jid:make(Host),
    create_delay_tag(DateTime, FromJID, Desc).

-type tz() :: {binary(), {integer(), integer()}} | {integer(), integer()} | utc.

%% Timezone = utc | {Sign::string(), {Hours, Minutes}} | {Hours, Minutes}
%% Hours = integer()
%% Minutes = integer()
-spec timestamp_to_iso(calendar:datetime(), tz()) -> {binary(), binary()}.

%% This is the XEP-0082 date and time format
%% http://xmpp.org/extensions/xep-0082.html

timestamp_to_iso({{Year, Month, Day},
                  {Hour, Minute, Second}},
                 Timezone) ->
    Timestamp_string =
	lists:flatten(io_lib:format("~4..0B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0B",
				    [Year, Month, Day, Hour, Minute, Second])),
    Timezone_string = case Timezone of
			utc -> "Z";
			{Sign, {TZh, TZm}} ->
			    io_lib:format("~s~2..0B:~2..0B", [Sign, TZh, TZm]);
			{TZh, TZm} ->
			    Sign = case TZh >= 0 of
				     true -> "+";
				     false -> "-"
				   end,
			    io_lib:format("~s~2..0B:~2..0B",
					  [Sign, abs(TZh), TZm])
		      end,
    {iolist_to_binary(Timestamp_string), iolist_to_binary(Timezone_string)}.


-spec timestamp_to_legacy(calendar:datetime()) -> binary().
%% This is the jabber legacy format
%% http://xmpp.org/extensions/xep-0091.html#time
timestamp_to_legacy({{Year, Month, Day},
                  {Hour, Minute, Second}}) ->
    (str:format("~4..0B~2..0B~2..0BT~2..0B:~2..0B:~2..0B",
                                   [Year, Month, Day, Hour, Minute, Second])).

-spec timestamp_to_iso_basic(calendar:datetime()) -> binary().
%% This is the ISO 8601 basic bormat
timestamp_to_iso_basic({{Year, Month, Day},
                  {Hour, Minute, Second}}) ->
    (str:format("~4..0B~2..0B~2..0BT~2..0B~2..0B~2..0B",
                                   [Year, Month, Day, Hour, Minute, Second])).

-spec now_to_utc_string(erlang:timestamp()) -> binary().

now_to_utc_string({MegaSecs, Secs, MicroSecs}) ->
    now_to_utc_string({MegaSecs, Secs, MicroSecs}, 6).

-spec now_to_utc_string(erlang:timestamp(), 1..6) -> binary().

now_to_utc_string({MegaSecs, Secs, MicroSecs}, Precision) ->
    {{Year, Month, Day}, {Hour, Minute, Second}} =
	calendar:now_to_universal_time({MegaSecs, Secs,
					MicroSecs}),
    Max = round(math:pow(10, Precision)),
    case round(MicroSecs / math:pow(10, 6 - Precision)) of
      Max ->
	  now_to_utc_string({MegaSecs, Secs + 1, 0}, Precision);
      FracOfSec ->
	  (str:format("~4..0B-~2..0B-~2..0BT"
				       "~2..0B:~2..0B:~2..0B.~*..0BZ",
				       [Year, Month, Day, Hour, Minute, Second,
					Precision, FracOfSec]))
    end.

-spec now_to_local_string(erlang:timestamp()) -> binary().

now_to_local_string({MegaSecs, Secs, MicroSecs}) ->
    LocalTime = calendar:now_to_local_time({MegaSecs, Secs,
					    MicroSecs}),
    UTCTime = calendar:now_to_universal_time({MegaSecs,
					      Secs, MicroSecs}),
    Seconds =
	calendar:datetime_to_gregorian_seconds(LocalTime) -
	  calendar:datetime_to_gregorian_seconds(UTCTime),
    {{H, M, _}, Sign} = if Seconds < 0 ->
			       {calendar:seconds_to_time(-Seconds), "-"};
			   true -> {calendar:seconds_to_time(Seconds), "+"}
			end,
    {{Year, Month, Day}, {Hour, Minute, Second}} =
	LocalTime,
    (str:format("~4..0B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0B.~6."
                                 ".0B~s~2..0B:~2..0B",
                                 [Year, Month, Day, Hour, Minute, Second,
                                  MicroSecs, Sign, H, M])).

-spec datetime_string_to_timestamp(binary()) -> undefined | erlang:timestamp().

datetime_string_to_timestamp(TimeStr) ->
    case catch parse_datetime(TimeStr) of
      {'EXIT', _Err} -> undefined;
      TimeStamp -> TimeStamp
    end.

parse_datetime(TimeStr) ->
    [Date, Time] = str:tokens(TimeStr, <<"T">>),
    D = parse_date(Date),
    {T, MS, TZH, TZM} = parse_time(Time),
    S = calendar:datetime_to_gregorian_seconds({D, T}),
    S1 = calendar:datetime_to_gregorian_seconds({{1970, 1,
						  1},
						 {0, 0, 0}}),
    Seconds = S - S1 - TZH * 60 * 60 - TZM * 60,
    {Seconds div 1000000, Seconds rem 1000000, MS}.

% yyyy-mm-dd
parse_date(Date) ->
    [Y, M, D] = str:tokens(Date, <<"-">>),
    Date1 = {binary_to_integer(Y), binary_to_integer(M),
	     binary_to_integer(D)},
    case calendar:valid_date(Date1) of
      true -> Date1;
      _ -> false
    end.

% hh:mm:ss[.sss]TZD
parse_time(Time) ->
    case str:str(Time, <<"Z">>) of
      0 -> parse_time_with_timezone(Time);
      _ ->
	  [T | _] = str:tokens(Time, <<"Z">>),
	  {TT, MS} = parse_time1(T),
	  {TT, MS, 0, 0}
    end.

parse_time_with_timezone(Time) ->
    case str:str(Time, <<"+">>) of
      0 ->
	  case str:str(Time, <<"-">>) of
	    0 -> false;
	    _ -> parse_time_with_timezone(Time, <<"-">>)
	  end;
      _ -> parse_time_with_timezone(Time, <<"+">>)
    end.

parse_time_with_timezone(Time, Delim) ->
    [T, TZ] = str:tokens(Time, Delim),
    {TZH, TZM} = parse_timezone(TZ),
    {TT, MS} = parse_time1(T),
    case Delim of
      <<"-">> -> {TT, MS, -TZH, -TZM};
      <<"+">> -> {TT, MS, TZH, TZM}
    end.

parse_timezone(TZ) ->
    [H, M] = str:tokens(TZ, <<":">>),
    {[H1, M1], true} = check_list([{H, 12}, {M, 60}]),
    {H1, M1}.

parse_time1(Time) ->
    [HMS | T] = str:tokens(Time, <<".">>),
    MS = case T of
	   [] -> 0;
	   [Val] -> binary_to_integer(str:left(Val, 6, $0))
	 end,
    [H, M, S] = str:tokens(HMS, <<":">>),
    {[H1, M1, S1], true} = check_list([{H, 24}, {M, 60},
				       {S, 60}]),
    {{H1, M1, S1}, MS}.

check_list(List) ->
    lists:mapfoldl(fun ({L, N}, B) ->
			   V = binary_to_integer(L),
			   if (V >= 0) and (V =< N) -> {V, B};
			      true -> {false, false}
			   end
		   end,
		   true, List).

%
% Base64 stuff (based on httpd_util.erl)
%

-spec term_to_base64(term()) -> binary().

term_to_base64(Term) ->
    encode_base64(term_to_binary(Term)).

-spec base64_to_term(binary()) -> {term, term()} | error.

base64_to_term(Base64) ->
    case catch binary_to_term(decode_base64(Base64), [safe]) of
      {'EXIT', _} ->
	  error;
      Term ->
	  {term, Term}
    end.

-spec decode_base64(binary()) -> binary().

decode_base64(S) ->
    case catch binary:last(S) of
      C when C == $\n; C == $\s ->
	  decode_base64(binary:part(S, 0, byte_size(S) - 1));
      _ ->
	  decode_base64_bin(S, <<>>)
    end.

take_without_spaces(Bin, Count) ->
    take_without_spaces(Bin, Count, <<>>).

take_without_spaces(Bin, 0, Acc) ->
    {Acc, Bin};
take_without_spaces(<<>>, _, Acc) ->
    {Acc, <<>>};
take_without_spaces(<<$\s, Tail/binary>>, Count, Acc) ->
    take_without_spaces(Tail, Count, Acc);
take_without_spaces(<<$\t, Tail/binary>>, Count, Acc) ->
    take_without_spaces(Tail, Count, Acc);
take_without_spaces(<<$\n, Tail/binary>>, Count, Acc) ->
    take_without_spaces(Tail, Count, Acc);
take_without_spaces(<<$\r, Tail/binary>>, Count, Acc) ->
    take_without_spaces(Tail, Count, Acc);
take_without_spaces(<<Char:8, Tail/binary>>, Count, Acc) ->
    take_without_spaces(Tail, Count-1, <<Acc/binary, Char:8>>).

decode_base64_bin(<<>>, Acc) ->
    Acc;
decode_base64_bin(Bin, Acc) ->
    case take_without_spaces(Bin, 4) of
        {<<A, B, $=, $=>>, _} ->
            <<Acc/binary, (d(A)):6, (d(B) bsr 4):2>>;
        {<<A, B, C, $=>>, _} ->
            <<Acc/binary, (d(A)):6, (d(B)):6, (d(C) bsr 2):4>>;
        {<<A, B, C, D>>, Tail} ->
            Acc2 = <<Acc/binary, (d(A)):6, (d(B)):6, (d(C)):6, (d(D)):6>>,
            decode_base64_bin(Tail, Acc2);
        _ ->
            <<"">>
    end.

d(X) when X >= $A, X =< $Z -> X - 65;
d(X) when X >= $a, X =< $z -> X - 71;
d(X) when X >= $0, X =< $9 -> X + 4;
d($+) -> 62;
d($/) -> 63;
d(_) -> 63.


%% Convert Erlang inet IP to list
-spec encode_base64(binary()) -> binary().

encode_base64(Data) ->
    encode_base64_bin(Data, <<>>).

encode_base64_bin(<<A:6, B:6, C:6, D:6, Tail/binary>>, Acc) ->
    encode_base64_bin(Tail, <<Acc/binary, (e(A)):8, (e(B)):8, (e(C)):8, (e(D)):8>>);
encode_base64_bin(<<A:6, B:6, C:4>>, Acc) ->
    <<Acc/binary, (e(A)):8, (e(B)):8, (e(C bsl 2)):8, $=>>;
encode_base64_bin(<<A:6, B:2>>, Acc) ->
    <<Acc/binary, (e(A)):8, (e(B bsl 4)):8, $=, $=>>;
encode_base64_bin(<<>>, Acc) ->
    Acc.

e(X) when X >= 0, X < 26 -> X + 65;
e(X) when X > 25, X < 52 -> X + 71;
e(X) when X > 51, X < 62 -> X - 4;
e(62) -> $+;
e(63) -> $/;
e(X) -> exit({bad_encode_base64_token, X}).

-spec ip_to_list(inet:ip_address() | undefined |
                 {inet:ip_address(), inet:port_number()}) -> binary().

ip_to_list({IP, _Port}) ->
    ip_to_list(IP);
%% This function clause could use inet_parse too:
ip_to_list(undefined) ->
    <<"unknown">>;
ip_to_list(IP) ->
    list_to_binary(inet_parse:ntoa(IP)).

-spec hex_to_bin(binary()) -> binary().

hex_to_bin(Hex) ->
    hex_to_bin(binary_to_list(Hex), []).

-spec hex_to_bin(list(), list()) -> binary().

hex_to_bin([], Acc) ->
    list_to_binary(lists:reverse(Acc));
hex_to_bin([H1, H2 | T], Acc) ->
    {ok, [V], []} = io_lib:fread("~16u", [H1, H2]),
    hex_to_bin(T, [V | Acc]).

-spec hex_to_base64(binary()) -> binary().

hex_to_base64(Hex) -> encode_base64(hex_to_bin(Hex)).

-spec expand_keyword(binary(), binary(), binary()) -> binary().

expand_keyword(Keyword, Input, Replacement) ->
    Parts = binary:split(Input, Keyword, [global]),
    str:join(Parts, Replacement).

binary_to_atom(Bin) ->
    erlang:binary_to_atom(Bin, utf8).

binary_to_integer(Bin) ->
    erlang:binary_to_integer(Bin).

binary_to_integer(Bin, Base) ->
    erlang:binary_to_integer(Bin, Base).

integer_to_binary(I) ->
    erlang:integer_to_binary(I).

integer_to_binary(I, Base) ->
    erlang:integer_to_binary(I, Base).

tuple_to_binary(T) ->
    iolist_to_binary(tuple_to_list(T)).

atom_to_binary(A) ->
    erlang:atom_to_binary(A, utf8).

expr_to_term(Expr) ->
    Str = binary_to_list(<<Expr/binary, ".">>),
    {ok, Tokens, _} = erl_scan:string(Str),
    {ok, Term} = erl_parse:parse_term(Tokens),
    Term.

term_to_expr(Term) ->
    list_to_binary(io_lib:print(Term)).

l2i(I) when is_integer(I) -> I;
l2i(L) when is_binary(L) -> binary_to_integer(L).

i2l(I) when is_integer(I) -> integer_to_binary(I);
i2l(L) when is_binary(L) -> L.

i2l(I, N) when is_integer(I) -> i2l(i2l(I), N);
i2l(L, N) when is_binary(L) ->
    case str:len(L) of
      N -> L;
      C when C > N -> L;
      _ -> i2l(<<$0, L/binary>>, N)
    end.
