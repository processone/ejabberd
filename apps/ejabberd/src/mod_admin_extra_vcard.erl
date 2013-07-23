%%%-------------------------------------------------------------------
%%% File    : mod_admin_extra_vcard.erl
%%% Author  : Badlop <badlop@process-one.net>, Piotr Nosek <piotr.nosek@erlang-solutions.com>
%%% Purpose : Contributed administrative functions and commands
%%% Created : 10 Aug 2008 by Badlop <badlop@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2008   ProcessOne
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
%%%-------------------------------------------------------------------

-module(mod_admin_extra_vcard).
-author('badlop@process-one.net').

-export([
    commands/0,

    set_nickname/3,
    get_vcard/3,
    get_vcard/4,
    get_vcard_multi/4,
    set_vcard/4,
    set_vcard/5
    ]).

-include("ejabberd.hrl").
-include("ejabberd_commands.hrl").
-include("mod_roster.hrl").
-include("jlib.hrl").
-include_lib("exml/include/exml.hrl").

%%%
%%% Register commands
%%%

commands() ->
    Vcard1FieldsString = "Some vcard field names in get/set_vcard are:\n"
                         " FN		- Full Name\n"
                         " NICKNAME	- Nickname\n"
                         " BDAY		- Birthday\n"
                         " TITLE		- Work: Position\n"
                         " ROLE		- Work: Role",

    Vcard2FieldsString = "Some vcard field names and subnames in get/set_vcard2 are:\n"
                         " N FAMILY	- Family name\n"
                         " N GIVEN	- Given name\n"
                         " N MIDDLE	- Middle name\n"
                         " ADR CTRY	- Address: Country\n"
                         " ADR LOCALITY	- Address: City\n"
                         " EMAIL USERID	- E-Mail Address\n"
                         " ORG ORGNAME	- Work: Company\n"
                         " ORG ORGUNIT	- Work: Department",

    VcardXEP = "For a full list of vCard fields check XEP-0054: vcard-temp at "
               "http://www.xmpp.org/extensions/xep-0054.html",

    [
        #ejabberd_commands{name = set_nickname, tags = [vcard],
                           desc = "Set nickname in a user's vCard",
                           module = ?MODULE, function = set_nickname,
                           args = [{user, string}, {host, string}, {nickname, string}],
                           result = {res, rescode}},
        #ejabberd_commands{name = get_vcard, tags = [vcard],
                           desc = "Get content from a vCard field",
                           longdesc = Vcard1FieldsString ++ "\n" ++ Vcard2FieldsString ++ "\n\n" ++ VcardXEP,
                           module = ?MODULE, function = get_vcard,
                           args = [{user, string}, {host, string}, {name, string}],
                           result = {content, string}},
        #ejabberd_commands{name = get_vcard2, tags = [vcard],
                           desc = "Get content from a vCard field",
                           longdesc = Vcard2FieldsString ++ "\n\n" ++ Vcard1FieldsString ++ "\n" ++ VcardXEP,
                           module = ?MODULE, function = get_vcard,
                           args = [{user, string}, {host, string}, {name, string}, {subname, string}],
                           result = {content, string}},
        #ejabberd_commands{name = get_vcard2_multi, tags = [vcard],
                           desc = "Get multiple contents from a vCard field (requires exmpp installed)",
                           longdesc = Vcard2FieldsString ++ "\n\n" ++ Vcard1FieldsString ++ "\n" ++ VcardXEP,
                           module = ?MODULE, function = get_vcard_multi,
                           args = [{user, string}, {host, string}, {name, string}, {subname, string}],
                           result = {contents, {list, string}}},
        #ejabberd_commands{name = set_vcard, tags = [vcard],
                           desc = "Set content in a vCard field",
                           longdesc = Vcard1FieldsString ++ "\n" ++ Vcard2FieldsString ++ "\n\n" ++ VcardXEP,
                           module = ?MODULE, function = set_vcard,
                           args = [{user, string}, {host, string}, {name, string}, {content, string}],
                           result = {res, rescode}},
        #ejabberd_commands{name = set_vcard2, tags = [vcard],
                           desc = "Set content in a vCard subfield",
                           longdesc = Vcard2FieldsString ++ "\n\n" ++ Vcard1FieldsString ++ "\n" ++ VcardXEP,
                           module = ?MODULE, function = set_vcard,
                           args = [{user, string}, {host, string}, {name, string}, {subname, string}, {content, string}],
                           result = {res, rescode}},
        #ejabberd_commands{name = set_vcard2_multi, tags = [vcard],
                           desc = "Set multiple contents in a vCard subfield",
                           longdesc = Vcard2FieldsString ++ "\n\n" ++ Vcard1FieldsString ++ "\n" ++ VcardXEP,
                           module = ?MODULE, function = set_vcard,
                           args = [{user, string}, {host, string}, {name, string}, {subname, string}, {contents, {list, string}}],
                           result = {res, rescode}}
        ].

%%%
%%% Vcard
%%%

set_nickname(User, Host, Nickname) ->
    JID = jlib:make_jid(User, Host, <<>>),
    R = mod_vcard:process_sm_iq(JID, JID,
                                {iq, "", set, "", "en",
                                 #xmlel{ name = <<"vCard">>,
                                        attrs = [{<<"xmlns">>, <<"vcard-temp">>}],
                                        children = [#xmlel{ name = <<"NICKNAME">>, children = [#xmlcdata{content = Nickname}]}]
                                       }}),
    case R of
        {iq, [], result, [], _L, []} ->
            ok;
        _ ->
            error
    end.

get_vcard(User, Host, Name) ->
    [Res | _] = get_vcard_content(User, Host, [Name]),
    Res.

get_vcard(User, Host, Name, Subname) ->
    [Res | _] = get_vcard_content(User, Host, [Name, Subname]),
    Res.

get_vcard_multi(User, Host, Name, Subname) ->
    get_vcard_content(User, Host, [Name, Subname]).

set_vcard(User, Host, Name, SomeContent) ->
    set_vcard_content(User, Host, [Name], SomeContent).

set_vcard(User, Host, Name, Subname, SomeContent) ->
    set_vcard_content(User, Host, [Name, Subname], SomeContent).


%%
%% Internal vcard

get_module_resource(Server) ->
    case gen_mod:get_module_opt(Server, ?MODULE, module_resource, none) of
        none -> atom_to_list(?MODULE);
        R when is_list(R) -> R
    end.

get_vcard_content(User, Server, Data) ->
    [{_, Module, Function, _Opts}] = ets:lookup(sm_iqtable, {?NS_VCARD, Server}),
    JID = jlib:make_jid(User, Server, get_module_resource(Server)),
    IQ = #iq{type = get, xmlns = ?NS_VCARD},
    IQr = Module:Function(JID, JID, IQ),
    case IQr#iq.sub_el of
        [A1] ->
            case get_vcard(Data, A1) of
                [] -> throw(error_no_value_found_in_vcard);
                ElemList -> [xml:get_tag_cdata(Elem) || Elem <- ElemList]
            end;
        [] ->
            throw(error_no_vcard_found)
    end.

get_vcard([Data1, Data2], A1) ->
    case get_subtag(A1, Data1) of
        false -> false;
        A2List -> lists:flatten([get_vcard([Data2], A2) || A2 <- A2List])
    end;

get_vcard([Data], A1) ->
    get_subtag(A1, Data).

get_subtag(Xmlelement, Name) ->
    case code:ensure_loaded(exmpp_xml) of
        {error, _} ->
            [get_subtag_xml(Xmlelement, Name)];
        {module, exmpp_xml} ->
            get_subtag_exmpp(Xmlelement, Name)
    end.

get_subtag_xml(Xmlelement, Name) ->
    xml:get_subtag(Xmlelement, Name).

get_subtag_exmpp(Xmlelement, Name) ->
    Xmlel = exmpp_xml:xmlelement_to_xmlel(Xmlelement),
    XmlelList = exmpp_xml:get_elements(Xmlel, Name),
    [exmpp_xml:xmlel_to_xmlelement(Xmlel2) || Xmlel2 <- XmlelList].

set_vcard_content(User, Server, Data, SomeContent) ->
    ContentList = case SomeContent of
        [Char | _] when not is_list(Char) -> [SomeContent];
        [Char | _] when is_list(Char) -> SomeContent
    end,
    [{_, Module, Function, _Opts}] = ets:lookup(sm_iqtable, {?NS_VCARD, Server}),
    JID = jlib:make_jid(User, Server, get_module_resource(Server)),
    IQ = #iq{type = get, xmlns = ?NS_VCARD},
    IQr = Module:Function(JID, JID, IQ),

    %% Get old vcard
    A4 = case IQr#iq.sub_el of
        [A1] ->
            {_, _, _, A2} = A1,
            update_vcard_els(Data, ContentList, A2);
        [] ->
            update_vcard_els(Data, ContentList, [])
    end,

    %% Build new vcard
    SubEl = #xmlel{ name = <<"vCard">>, attrs = [{<<"xmlns">>,<<"vcard-temp">>}], children = A4},
    IQ2 = #iq{type=set, sub_el = SubEl},

    Module:Function(JID, JID, IQ2),
    ok.

update_vcard_els(Data, ContentList, Els1) ->
    Els2 = lists:keysort(2, Els1),
    [Data1 | Data2] = Data,
    NewEls = case Data2 of
        [] ->
            [#xmlel{ name = Data1, children = [#xmlcdata{content = Content}] } || Content <- ContentList];
        [D2] ->
            OldEl = case lists:keysearch(Data1, 2, Els2) of
                {value, A} -> A;
                false -> #xmlel{ name = Data1 }
            end,
            ContentOld1 = OldEl#xmlel.children,
            Content2 = [#xmlel{ name = D2, children = [#xmlcdata{content=Content}]} || Content <- ContentList],
            ContentOld2 = [A || {_, X, _, _} = A <- ContentOld1, X/=D2],
            ContentOld3 = lists:keysort(2, ContentOld2),
            ContentNew = lists:keymerge(2, Content2, ContentOld3),
            [#xmlel{ name = Data1, children = ContentNew}]
    end,
    Els3 = lists:keydelete(Data1, 2, Els2),
    lists:keymerge(2, NewEls, Els3).

