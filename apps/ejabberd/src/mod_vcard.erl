%%%----------------------------------------------------------------------
%%% File    : mod_vcard.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Vcard management in Mnesia
%%% Created :  2 Jan 2003 by Alexey Shchepin <alexey@process-one.net>
%%%
%%% Store vCards in mnesia to provide "XEP-0054: vcard-temp"
%%% and "XEP-0055: Jabber Search"
%%%
%%% Most of this is now using binaries. The search fields l* in vcard_search
%%% are still stored as lists to allow string prefix search using the match
%%% spec with a trailing element String ++ '_'.
%%%
%%%----------------------------------------------------------------------
%%% ejabberd, Copyright (C) 2002-2011   ProcessOne
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

-module(mod_vcard).
-author('alexey@process-one.net').

-behaviour(gen_mod).

-export([start/2, init/3, stop/1,
	 get_local_features/5,
	 process_local_iq/3,
	 process_sm_iq/3,
	 reindex_vcards/0,
	 remove_user/2]).

-include("ejabberd.hrl").
-include("jlib.hrl").


-define(JUD_MATCHES, 30).

-define(TLFIELD(Type, Label, Var),
	#xmlel{name = <<"field">>,
               attrs = [{<<"type">>, Type},
                             {<<"label">>, translate:translate(Lang, Label)},
                             {<<"var">>, Var}]}).


-define(FORM(JID),
	[#xmlel{name = <<"instructions">>,
	        children = [#xmlcdata{content = translate:translate(
                                                  Lang, <<"You need an x:data capable client to search">>)}]},
	 #xmlel{name = <<"x">>,
	        attrs = [{<<"xmlns">>, ?NS_XDATA}, {<<"type">>, <<"form">>}],
	        children = [#xmlel{name = <<"title">>,
                                   children = [#xmlcdata{content = [translate:translate(Lang, <<"Search users in ">>),
	                                                            jlib:jid_to_binary(JID)]}]},
	                    #xmlel{name = <<"instructions">>,
                                   children = [#xmlcdata{content = translate:translate(Lang, <<"Fill in the form to search "
                                                                                               "for any matching Jabber User "
                                                                                               "(Add * to the end of field to "
	                                                                                       "match substring)">>)}]},
	                    ?TLFIELD(<<"text-single">>, <<"User">>, <<"user">>),
	                    ?TLFIELD(<<"text-single">>, <<"Full Name">>, <<"fn">>),
	                    ?TLFIELD(<<"text-single">>, <<"Name">>, <<"first">>),
	                    ?TLFIELD(<<"text-single">>, <<"Middle Name">>, <<"middle">>),
	                    ?TLFIELD(<<"text-single">>, <<"Family Name">>, <<"last">>),
	                    ?TLFIELD(<<"text-single">>, <<"Nickname">>, <<"nick">>),
	                    ?TLFIELD(<<"text-single">>, <<"Birthday">>, <<"bday">>),
	                    ?TLFIELD(<<"text-single">>, <<"Country">>, <<"ctry">>),
	                    ?TLFIELD(<<"text-single">>, <<"City">>, <<"locality">>),
	                    ?TLFIELD(<<"text-single">>, <<"Email">>, <<"email">>),
	                    ?TLFIELD(<<"text-single">>, <<"Organization Name">>, <<"orgname">>),
	                    ?TLFIELD(<<"text-single">>, <<"Organization Unit">>, <<"orgunit">>)
                           ]}]).

-define(LFIELD(Label, Var),
	#xmlel{name = <<"field">>,
               attrs = [{<<"label">>, translate:translate(Lang, Label)},
                             {<<"var">>, Var}]}).

-define(FIELD(Var, Val),
	#xmlel{name = <<"field">>, attrs = [{<<"var">>, Var}],
	       children = [#xmlel{name = <<"value">>,
	                          children = [#xmlcdata{content = Val}]}]}).

%% no idea what the l* fields were for, but now I'm using them to store
%% binary_to_list versions for string prefix search in match specs.
%% It looks like they are lower case versions for lower case search.
%% These will be UTF-8 bytes so the concept of substring is dodgy here.
%% It would probably be ok but search strings and values should probably get
%% some unicode normalization. This is no worse than it was in 2.1.8 where this
%% fork started.
-record(vcard_search, {us,
		       user,     luser,
		       fn,	 lfn,
		       family,	 lfamily,
		       given,	 lgiven,
		       middle,	 lmiddle,
		       nickname, lnickname,
		       bday,	 lbday,
		       ctry,	 lctry,
		       locality, llocality,
		       email,	 lemail,
		       orgname,	 lorgname,
		       orgunit,	 lorgunit
		      }).
-record(vcard, {us, vcard}).

-define(PROCNAME, ejabberd_mod_vcard).

start(VHost, Opts) ->
    mnesia:create_table(vcard, [{disc_only_copies, [node()]},
				{attributes, record_info(fields, vcard)}]),
    mnesia:create_table(vcard_search,
			[{disc_copies, [node()]},
			 {attributes, record_info(fields, vcard_search)}]),
    update_tables(),
    mnesia:add_table_index(vcard_search, luser),
    mnesia:add_table_index(vcard_search, lfn),
    mnesia:add_table_index(vcard_search, lfamily),
    mnesia:add_table_index(vcard_search, lgiven),
    mnesia:add_table_index(vcard_search, lmiddle),
    mnesia:add_table_index(vcard_search, lnickname),
    mnesia:add_table_index(vcard_search, lbday),
    mnesia:add_table_index(vcard_search, lctry),
    mnesia:add_table_index(vcard_search, llocality),
    mnesia:add_table_index(vcard_search, lemail),
    mnesia:add_table_index(vcard_search, lorgname),
    mnesia:add_table_index(vcard_search, lorgunit),

    ejabberd_hooks:add(remove_user, VHost,
		       ?MODULE, remove_user, 50),
    IQDisc = gen_mod:get_opt(iqdisc, Opts, one_queue),
    gen_iq_handler:add_iq_handler(ejabberd_local, VHost, ?NS_VCARD,
				  ?MODULE, process_local_iq, IQDisc),
    gen_iq_handler:add_iq_handler(ejabberd_sm, VHost, ?NS_VCARD,
				  ?MODULE, process_sm_iq, IQDisc),
    ejabberd_hooks:add(disco_local_features, VHost, ?MODULE, get_local_features, 50),
    DirectoryHost = gen_mod:get_opt_host(VHost, Opts, "vjud.@HOST@"),
    Search = gen_mod:get_opt(search, Opts, true),
    register(gen_mod:get_module_proc(VHost, ?PROCNAME),
	     spawn(?MODULE, init, [DirectoryHost, VHost, Search])).


init(DirectoryHost, VHost, Search) ->
    case Search of
	false ->
	    loop(DirectoryHost, VHost);
	_ ->
	    ejabberd_router:register_route(DirectoryHost),
	    loop(DirectoryHost, VHost)
    end.

loop(DirectoryHost, VHost) ->
    receive
	{route, From, To, Packet} ->
            IQ = jlib:iq_query_info(Packet),
	    case catch do_route(VHost, From, To, Packet, IQ) of
		{'EXIT', Reason} ->
		    ?ERROR_MSG("~p", [Reason]);
		_ ->
		    ok
	    end,
	    loop(DirectoryHost, VHost);
	stop ->
	    ejabberd_router:unregister_route(DirectoryHost),
	    ok;
	_ ->
	    loop(DirectoryHost, VHost)
    end.

stop(VHost) ->
    ejabberd_hooks:delete(remove_user, VHost,
			  ?MODULE, remove_user, 50),
    gen_iq_handler:remove_iq_handler(ejabberd_local, VHost, ?NS_VCARD),
    gen_iq_handler:remove_iq_handler(ejabberd_sm, VHost, ?NS_VCARD),
    ejabberd_hooks:delete(
      disco_local_features, VHost, ?MODULE, get_local_features, 50),
    Proc = gen_mod:get_module_proc(VHost, ?PROCNAME),
    Proc ! stop,
    {wait, Proc}.

get_local_features({error, _Error} = Acc, _From, _To, _Node, _Lang) ->
    Acc;

get_local_features(Acc, _From, _To, Node, _Lang) ->
    case Node of
	<<>> ->
	    case Acc of
		{result, Features} ->
		    {result, [?NS_DISCO_INFO, ?NS_VCARD | Features]};
		empty ->
		    {result, [?NS_DISCO_INFO, ?NS_VCARD]}
	    end;
 	_ ->
	    Acc
     end.

process_local_iq(_From, _To, #iq{ type = set,
                                  sub_el = SubEl } = IQ) ->
    IQ#iq{ type = error,
           sub_el = [SubEl, ?ERR_NOT_ALLOWED] };

process_local_iq(_From, _To, #iq{ type = get,
                                  lang = Lang } = IQ) ->
    IQ#iq{ type = result,
          sub_el = [#xmlel{name = <<"vCard">>, attrs = [{"xmlns", ?NS_VCARD}],
                           children = [#xmlel{name = <<"FN">>,
                                             children = [#xmlcdata{content = <<"ejabberd">>}]},
                                      #xmlel{name = <<"URL">>,
                                             children = [#xmlcdata{content = ?EJABBERD_URI}]},
                                      #xmlel{name = <<"DESC">>,
                                             children = [#xmlcdata{content = [translate:translate(Lang,<<"Erlang Jabber Server">>),
                                                                              <<"\nCopyright (c) 2002-2011 ProcessOne">>]}]},
                                      #xmlel{name = <<"BDAY">>,
                                             children = [#xmlcdata{content = <<"2002-11-16">>}]}
                                     ]}]}.



process_sm_iq(From, To, #iq{ type = set,
                             sub_el = SubEl } = IQ) ->
    #jid{ user = FromUser,
          lserver = FromVHost} = From,
    #jid{ user = ToUser,
          lserver = ToVHost,
          resource = ToResource} = To,
    case lists:member(FromVHost, ?MYHOSTS) of
        true when FromUser == ToUser, FromVHost == ToVHost, ToResource == <<>>;
                  ToUser == <<>>, ToVHost == <<>> ->
            set_vcard(FromUser, FromVHost, SubEl),
            IQ#iq{ type = result,
                   sub_el = [] };
        _ ->
            IQ#iq{ type = error,
                   sub_el = [SubEl, ?ERR_NOT_ALLOWED]}
    end;

process_sm_iq(_From, To, #iq{ type = get,
                              sub_el = SubEl } = IQ) ->
    #jid{ luser = LUser,
          lserver = LServer } = To,
    US = {LUser, LServer},
    F = fun() ->
                mnesia:read({vcard, US})
        end,
    case mnesia:transaction(F) of
        {atomic, []} ->
            IQ#iq{ type = error,
                   sub_el = [SubEl, ?ERR_SERVICE_UNAVAILABLE] };
        {atomic, Rs} ->
            Els = lists:map(fun(R) ->
                                    R#vcard.vcard
                            end, Rs),
            IQ#iq{ type = result,
                   sub_el = Els };
        {aborted, Reason} ->
            ?ERROR_MSG("vCard lookup failed in process_sm_iq: ~p", [Reason]),
            IQ#iq{ type = error,
                   sub_el = [SubEl, ?ERR_INTERNAL_SERVER_ERROR] }
    end.



set_vcard(User, VHost, VCARD) ->
    FN       = xml:get_path_s(VCARD, [{elem, <<"FN">>}, cdata]),
    Family   = xml:get_path_s(VCARD, [{elem, <<"N">>},
                                      {elem, <<"FAMILY">>}, cdata]),
    Given    = xml:get_path_s(VCARD, [{elem, <<"N">>},
                                      {elem, <<"GIVEN">>}, cdata]),
    Middle   = xml:get_path_s(VCARD, [{elem, <<"N">>},
                                      {elem, <<"MIDDLE">>}, cdata]),
    Nickname = xml:get_path_s(VCARD, [{elem, <<"NICKNAME">>}, cdata]),
    BDay     = xml:get_path_s(VCARD, [{elem, <<"BDAY">>}, cdata]),
    CTRY     = xml:get_path_s(VCARD, [{elem, <<"ADR">>},
                                      {elem, <<"CTRY">>}, cdata]),
    Locality = xml:get_path_s(VCARD, [{elem, <<"ADR">>},
                                      {elem, <<"LOCALITY">>}, cdata]),
    EMail1   = xml:get_path_s(VCARD, [{elem, <<"EMAIL">>},
                                      {elem, <<"USERID">>}, cdata]),
    EMail2   = xml:get_path_s(VCARD, [{elem, <<"EMAIL">>}, cdata]),
    OrgName  = xml:get_path_s(VCARD, [{elem, <<"ORG">>},
                                      {elem, <<"ORGNAME">>}, cdata]),
    OrgUnit  = xml:get_path_s(VCARD, [{elem, <<"ORG">>},
                                      {elem, <<"ORGUNIT">>}, cdata]),
    EMail = case EMail1 of
		<<"">> ->
		    EMail2;
		_ ->
		    EMail1
	    end,

    LUser     = jlib:nodeprep(User),
    LFN       = stringprep:tolower(FN),
    LFamily   = stringprep:tolower(Family),
    LGiven    = stringprep:tolower(Given),
    LMiddle   = stringprep:tolower(Middle),
    LNickname = stringprep:tolower(Nickname),
    LBDay     = stringprep:tolower(BDay),
    LCTRY     = stringprep:tolower(CTRY),
    LLocality = stringprep:tolower(Locality),
    LEMail    = stringprep:tolower(EMail),
    LOrgName  = stringprep:tolower(OrgName),
    LOrgUnit  = stringprep:tolower(OrgUnit),

    US = {LUser, VHost},

    if
	(LUser     == error) or
	(LFN       == error) or
	(LFamily   == error) or
	(LGiven    == error) or
	(LMiddle   == error) or
	(LNickname == error) or
	(LBDay     == error) or
	(LCTRY     == error) or
	(LLocality == error) or
	(LEMail    == error) or
	(LOrgName  == error) or
	(LOrgUnit  == error) ->
	    {error, badarg};
	true ->
	    F = fun() ->
		mnesia:write(#vcard{us = US, vcard = VCARD}),
		mnesia:write(
		  #vcard_search{us        = US,
				user      = {User, VHost},
				luser     = b2l(LUser),
				fn        = FN,       lfn        = b2l(LFN),
				family    = Family,   lfamily    = b2l(LFamily),
				given     = Given,    lgiven     = b2l(LGiven),
				middle    = Middle,   lmiddle    = b2l(LMiddle),
				nickname  = Nickname, lnickname  = b2l(LNickname),
				bday      = BDay,     lbday      = b2l(LBDay),
				ctry      = CTRY,     lctry      = b2l(LCTRY),
				locality  = Locality, llocality  = b2l(LLocality),
				email     = EMail,    lemail     = b2l(LEMail),
				orgname   = OrgName,  lorgname   = b2l(LOrgName),
				orgunit   = OrgUnit,  lorgunit   = b2l(LOrgUnit)
			       })
		end,
	    {atomic, _} = mnesia:transaction(F),
	    ejabberd_hooks:run(vcard_set, VHost, [LUser, VHost, VCARD])
    end.

b2l(Binary) ->
    binary_to_list(Binary).

do_route(_VHost, From, #jid{ user = User,
                             resource = Resource } = To, Packet, _IQ)
  when (User /= <<"">>) or (Resource /= <<"">>) ->
    Err = jlib:make_error_reply(Packet, ?ERR_SERVICE_UNAVAILABLE),
    ejabberd_router:route(To, From, Err);

do_route(VHost, From, To, Packet, #iq{ type = set,
                                       xmlns = ?NS_SEARCH,
                                       lang = Lang,
                                       sub_el = SubEl } = IQ) ->
    XDataEl = find_xdata_el(SubEl),
    case XDataEl of
        false ->
            Err = jlib:make_error_reply(Packet, ?ERR_BAD_REQUEST),
            ejabberd_router:route(To, From, Err);
        _ ->
            XData = jlib:parse_xdata_submit(XDataEl),
            case XData of
                invalid ->
                    Err = jlib:make_error_reply(Packet, ?ERR_BAD_REQUEST),
                    ejabberd_router:route(To, From, Err);
                _ ->
                    ResIQ =
                        IQ#iq{
                          type = result,
                          sub_el =
                              [#xmlel{name = <<"query">>,
                                      attrs = [{<<"xmlns">>, ?NS_SEARCH}],
                                      children = [#xmlel{name = <<"x">>,
                                                         attrs = [{<<"xmlns">>, ?NS_XDATA},
                                                                  {<<"type">>, <<"result">>}],
                                                         children = search_result(Lang, To, VHost, XData)}]}]},
                    ejabberd_router:route(To, From, jlib:iq_to_xml(ResIQ))
            end
    end;

do_route(_VHost, From, To, _Packet, #iq{ type = get,
                                         xmlns = ?NS_SEARCH,
                                         lang = Lang } = IQ) ->
    ResIQ = IQ#iq{ type = result,
                  sub_el = [#xmlel{name = <<"query">>,
                                   attrs = [{<<"xmlns">>, ?NS_SEARCH}],
                                   children = ?FORM(To)}]},
    ejabberd_router:route(To, From, jlib:iq_to_xml(ResIQ));

do_route(_VHost, From, To, Packet, #iq{ type = set,
                                        xmlns = ?NS_DISCO_INFO }) ->
    Err = jlib:make_error_reply(Packet, ?ERR_NOT_ALLOWED),
    ejabberd_router:route(To, From, Err);

do_route(VHost, From, To, _Packet, #iq{ type = get,
                                        xmlns = ?NS_DISCO_INFO,
                                        lang = Lang} = IQ) ->
    Info = ejabberd_hooks:run_fold(disco_info, VHost, [], [VHost, ?MODULE, "", ""]),
    ResIQ =
        IQ#iq{type = result,
              sub_el = [#xmlel{name = <<"query">>,
                               attrs = [{<<"xmlns">>, ?NS_DISCO_INFO}],
                               children = [#xmlel{name = <<"identity">>,
                                                  attrs = [{<<"category">>, <<"directory">>},
                                                           {<<"type">>, <<"user">>},
                                                           {<<"name">>,
                                                            translate:translate(Lang, <<"vCard User Search">>)}]},
                                           #xmlel{name = <<"feature">>,
                                                  attrs = [{<<"var">>, ?NS_DISCO_INFO}]},
                                           #xmlel{name = <<"feature">>,
                                                  attrs = [{<<"var">>, ?NS_SEARCH}]},
                                           #xmlel{name = <<"feature">>,
                                                  attrs = [{"var", ?NS_VCARD}]}
                                          ] ++ Info}]},
    ejabberd_router:route(To, From, jlib:iq_to_xml(ResIQ));

do_route(_VHost, From, To, Packet, #iq{ type = set,
                                        xmlns = ?NS_DISCO_ITEMS}) ->
                Err = jlib:make_error_reply(
                        Packet, ?ERR_NOT_ALLOWED),
                ejabberd_router:route(To, From, Err);

do_route(_VHost, From, To, _Packet, #iq{ type = get,
                                         xmlns = ?NS_DISCO_ITEMS} = IQ) ->
    ResIQ =
        IQ#iq{type = result,
              sub_el = [#xmlel{name = <<"query">>,
                               attrs = [{<<"xmlns">>, ?NS_DISCO_ITEMS}]}]},
    ejabberd_router:route(To, From, jlib:iq_to_xml(ResIQ));

do_route(_VHost, From, To, _Packet, #iq{ type = get,
                                         xmlns = ?NS_VCARD,
                                         lang = Lang} = IQ) ->
    ResIQ =
        IQ#iq{type = result,
              sub_el = [#xmlel{name = <<"vCard">>,
                               attrs = [{<<"xmlns">>, ?NS_VCARD}],
                               children = iq_get_vcard(Lang)}]},
    ejabberd_router:route(To, From, jlib:iq_to_xml(ResIQ));

do_route(_VHost, From, To, Packet, _IQ) ->
    Err = jlib:make_error_reply(Packet, ?ERR_SERVICE_UNAVAILABLE),
    ejabberd_router:route(To, From, Err).



iq_get_vcard(Lang) ->
    [#xmlel{name = <<"FN">>,
            children = [#xmlcdata{content = <<"ejabberd/mod_vcard">>}]},
     #xmlel{name = <<"URL">>, children = [#xmlcdata{content = ?EJABBERD_URI}]},
     #xmlel{name = <<"DESC">>,
            children = [#xmlcdata{content = [translate:translate(
                                               Lang,
                                               <<"ejabberd vCard module">>),
                                             <<"\nCopyright (c) 2003-2011 ProcessOne">>]}]}].

find_xdata_el(#xmlel{children = SubEls}) ->
    find_xdata_el1(SubEls).

find_xdata_el1([]) ->
    false;
find_xdata_el1([XE = #xmlel{attrs = Attrs} | Els]) ->
    case xml:get_attr_s(<<"xmlns">>, Attrs) of
	?NS_XDATA ->
	    XE;
	_ ->
	    find_xdata_el1(Els)
    end;
find_xdata_el1([_ | Els]) ->
    find_xdata_el1(Els).

search_result(Lang, JID, VHost, Data) ->
    [#xmlel{name = <<"title">>,
            children = [#xmlcdata{content = [translate:translate(Lang, <<"Search Results for ">>),
                                             jlib:jid_to_binary(JID)]}]},
     #xmlel{name = <<"reported">>,
            children = [?TLFIELD(<<"jid-single">>, <<"Jabber ID">>, <<"jid">>),
                        ?TLFIELD(<<"text-single">>, <<"Full Name">>, <<"fn">>),
                        ?TLFIELD(<<"text-single">>, <<"Name">>, <<"first">>),
                        ?TLFIELD(<<"text-single">>, <<"Middle Name">>, <<"middle">>),
                        ?TLFIELD(<<"text-single">>, <<"Family Name">>, <<"last">>),
                        ?TLFIELD(<<"text-single">>, <<"Nickname">>, <<"nick">>),
                        ?TLFIELD(<<"text-single">>, <<"Birthday">>, <<"bday">>),
                        ?TLFIELD(<<"text-single">>, <<"Country">>, <<"ctry">>),
                        ?TLFIELD(<<"text-single">>, <<"City">>, <<"locality">>),
                        ?TLFIELD(<<"text-single">>, <<"Email">>, <<"email">>),
                        ?TLFIELD(<<"text-single">>, <<"Organization Name">>, <<"orgname">>),
                        ?TLFIELD(<<"text-single">>, <<"Organization Unit">>, <<"orgunit">>)
                       ]}] ++ lists:map(fun record_to_item/1, search(VHost, Data)).


record_to_item(R) ->
    {User, Server} = R#vcard_search.user,
    #xmlel{name = <<"item">>,
           children = [
                        ?FIELD(<<"jid">>, [User, <<"@">>, Server]),
                        ?FIELD(<<"fn">>, (R#vcard_search.fn)),
                        ?FIELD(<<"last">>, (R#vcard_search.family)),
                        ?FIELD(<<"first">>, (R#vcard_search.given)),
                        ?FIELD(<<"middle">>, (R#vcard_search.middle)),
                        ?FIELD(<<"nick">>, (R#vcard_search.nickname)),
                        ?FIELD(<<"bday">>, (R#vcard_search.bday)),
                        ?FIELD(<<"ctry">>, (R#vcard_search.ctry)),
                        ?FIELD(<<"locality">>, (R#vcard_search.locality)),
                        ?FIELD(<<"email">>, (R#vcard_search.email)),
                        ?FIELD(<<"orgname">>, (R#vcard_search.orgname)),
                        ?FIELD(<<"orgunit">>, (R#vcard_search.orgunit))
                       ]}.


search(VHost, Data) ->
    MatchHead = make_matchhead(VHost, Data),
    AllowReturnAll = gen_mod:get_module_opt(VHost, ?MODULE,
					    allow_return_all, false),
    if
	(MatchHead == #vcard_search{_ = '_'}) and (not AllowReturnAll) ->
	    [];
	true ->
	    case catch mnesia:dirty_select(vcard_search,
					   [{MatchHead, [], ['$_']}]) of
		{'EXIT', Reason} ->
		    ?ERROR_MSG("~p", [Reason]),
		    [];
		Rs ->
		    case gen_mod:get_module_opt(VHost, ?MODULE,
						matches, ?JUD_MATCHES) of
			infinity ->
			    Rs;
			Val when is_integer(Val) and (Val > 0) ->
			    lists:sublist(Rs, Val);
			Val ->
			    ?ERROR_MSG("Illegal option value ~p. "
				       "Default value ~p substituted.",
				       [{matches, Val}, ?JUD_MATCHES]),
			    lists:sublist(Rs, ?JUD_MATCHES)
		    end
	    end
    end.


make_matchhead(VHost, Data) ->
    GlobMatch = #vcard_search{_ = '_'},
    Match = filter_fields(Data, GlobMatch, VHost),
    Match.

filter_fields([], Match, _VHost) ->
    Match;
filter_fields([{SVar, [Val]} | Ds], Match, VHost)
  when is_binary(Val) and (Val /= <<"">>) ->
    LVal = stringprep:tolower(Val),
    NewMatch =
        case SVar of
            <<"user">> ->
                case gen_mod:get_module_opt(VHost, ?MODULE,
                                            search_all_hosts, true) of
                    true ->
                        Match#vcard_search{luser = make_val(LVal)};
                    false ->
                        Host = find_my_host(VHost),
                        Match#vcard_search{us = {make_val(LVal), Host}}
                end;
            <<"fn">>       -> Match#vcard_search{lfn       = make_val(LVal)};
            <<"last">>     -> Match#vcard_search{lfamily   = make_val(LVal)};
            <<"first">>    -> Match#vcard_search{lgiven    = make_val(LVal)};
            <<"middle">>   -> Match#vcard_search{lmiddle   = make_val(LVal)};
            <<"nick">>     -> Match#vcard_search{lnickname = make_val(LVal)};
            <<"bday">>     -> Match#vcard_search{lbday     = make_val(LVal)};
            <<"ctry">>     -> Match#vcard_search{lctry     = make_val(LVal)};
            <<"locality">> -> Match#vcard_search{llocality = make_val(LVal)};
            <<"email">>    -> Match#vcard_search{lemail    = make_val(LVal)};
            <<"orgname">>  -> Match#vcard_search{lorgname  = make_val(LVal)};
            <<"orgunit">>  -> Match#vcard_search{lorgunit  = make_val(LVal)};
            _              -> Match
        end,
    filter_fields(Ds, NewMatch, VHost);
filter_fields([_ | Ds], Match, VHost) ->
    filter_fields(Ds, Match, VHost).

%% returns value as list to match substrings using match spec.
%% See vcard_search definition.
make_val(ValBin) ->
    Val = binary_to_list(ValBin),
    case lists:suffix("*", Val) of
	true ->
	    lists:sublist(Val, length(Val) - 1) ++ '_';
	_ ->
	    Val
    end.

find_my_host(VHost) ->
    Parts = binary:matches(VHost, <<".">>),
    find_my_host(Parts, ?MYHOSTS).

find_my_host([], _Hosts) ->
    ?MYNAME;
find_my_host([_ | Tail] = Parts, Hosts) ->
    Domain = parts_to_binstring(Parts),
    case lists:member(Domain, Hosts) of
	true ->
	    Domain;
	false ->
	    find_my_host(Tail, Hosts)
    end.

parts_to_binstring(Parts) ->
    string:strip(lists:flatten(lists:map(fun(S) -> [S, $.] end, Parts)),
		 right, $.).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

set_vcard_t(R, _) ->
    US = R#vcard.us,
    User  = US,
    VCARD = R#vcard.vcard,

    FN       = xml:get_path_s(VCARD, [{elem, <<"FN">>}, cdata]),
    Family   = xml:get_path_s(VCARD, [{elem, <<"N">>},
                                      {elem, <<"FAMILY">>}, cdata]),
    Given    = xml:get_path_s(VCARD, [{elem, <<"N">>},
                                      {elem, <<"GIVEN">>}, cdata]),
    Middle   = xml:get_path_s(VCARD, [{elem, <<"N">>},
                                      {elem, <<"MIDDLE">>}, cdata]),
    Nickname = xml:get_path_s(VCARD, [{elem, <<"NICKNAME">>}, cdata]),
    BDay     = xml:get_path_s(VCARD, [{elem, <<"BDAY">>}, cdata]),
    CTRY     = xml:get_path_s(VCARD, [{elem, <<"ADR">>},
                                      {elem, <<"CTRY">>},    cdata]),
    Locality = xml:get_path_s(VCARD, [{elem, <<"ADR">>},
                                      {elem, <<"LOCALITY">>},cdata]),
    EMail    = xml:get_path_s(VCARD, [{elem, <<"EMAIL">>}, cdata]),
    OrgName  = xml:get_path_s(VCARD, [{elem, <<"ORG">>},
                                      {elem, <<"ORGNAME">>}, cdata]),
    OrgUnit  = xml:get_path_s(VCARD, [{elem, <<"ORG">>},
                                      {elem, <<"ORGUNIT">>}, cdata]),

    {LUser, _LServer} = US,
    LFN       = stringprep:tolower(FN),
    LFamily   = stringprep:tolower(Family),
    LGiven    = stringprep:tolower(Given),
    LMiddle   = stringprep:tolower(Middle),
    LNickname = stringprep:tolower(Nickname),
    LBDay     = stringprep:tolower(BDay),
    LCTRY     = stringprep:tolower(CTRY),
    LLocality = stringprep:tolower(Locality),
    LEMail    = stringprep:tolower(EMail),
    LOrgName  = stringprep:tolower(OrgName),
    LOrgUnit  = stringprep:tolower(OrgUnit),

    if
	(LUser     == error) or
	(LFN       == error) or
	(LFamily   == error) or
	(LGiven    == error) or
	(LMiddle   == error) or
	(LNickname == error) or
	(LBDay     == error) or
	(LCTRY     == error) or
	(LLocality == error) or
	(LEMail    == error) or
	(LOrgName  == error) or
	(LOrgUnit  == error) ->
	    {error, badarg};
	true ->
	    mnesia:write(
	      #vcard_search{us        = US,
			    user      = User,     luser      = LUser,
			    fn        = FN,       lfn        = LFN,
			    family    = Family,   lfamily    = LFamily,
			    given     = Given,    lgiven     = LGiven,
			    middle    = Middle,   lmiddle    = LMiddle,
			    nickname  = Nickname, lnickname  = LNickname,
			    bday      = BDay,     lbday      = LBDay,
			    ctry      = CTRY,     lctry      = LCTRY,
			    locality  = Locality, llocality  = LLocality,
			    email     = EMail,    lemail     = LEMail,
			    orgname   = OrgName,  lorgname   = LOrgName,
			    orgunit   = OrgUnit,  lorgunit   = LOrgUnit
			   })
    end.


reindex_vcards() ->
    F = fun() ->
		mnesia:foldl(fun set_vcard_t/2, [], vcard)
	end,
    mnesia:transaction(F).


remove_user(User, Server) ->
    LUser = jlib:nodeprep(User),
    LServer = jlib:nameprep(Server),
    US = {LUser, LServer},
    F = fun() ->
		mnesia:delete({vcard, US}),
		mnesia:delete({vcard_search, US})
	end,
    mnesia:transaction(F).


update_tables() ->
    update_vcard_table(),
    update_vcard_search_table().

update_vcard_table() ->
    Fields = record_info(fields, vcard),
    case mnesia:table_info(vcard, attributes) of
	Fields ->
	    ok;
	[user, vcard] ->
	    ?INFO_MSG("Converting vcard table from "
		      "{user, vcard} format", []),
	    Host = ?MYNAME,
	    {atomic, ok} = mnesia:create_table(
			     mod_vcard_tmp_table,
			     [{disc_only_copies, [node()]},
			      {type, bag},
			      {local_content, true},
			      {record_name, vcard},
			      {attributes, record_info(fields, vcard)}]),
	    mnesia:transform_table(vcard, ignore, Fields),
	    F1 = fun() ->
			 mnesia:write_lock_table(mod_vcard_tmp_table),
			 mnesia:foldl(
			   fun(#vcard{us = U} = R, _) ->
				   mnesia:dirty_write(
				     mod_vcard_tmp_table,
				     R#vcard{us = {U, Host}})
			   end, ok, vcard)
		 end,
	    mnesia:transaction(F1),
	    mnesia:clear_table(vcard),
	    F2 = fun() ->
			 mnesia:write_lock_table(vcard),
			 mnesia:foldl(
			   fun(R, _) ->
				   mnesia:dirty_write(R)
			   end, ok, mod_vcard_tmp_table)
		 end,
	    mnesia:transaction(F2),
	    mnesia:delete_table(mod_vcard_tmp_table);
	_ ->
	    ?INFO_MSG("Recreating vcard table", []),
	    mnesia:transform_table(vcard, ignore, Fields)
    end.


update_vcard_search_table() ->
    Fields = record_info(fields, vcard_search),
    case mnesia:table_info(vcard_search, attributes) of
	Fields ->
	    ok;
	[user,     luser,
	 fn,       lfn,
	 family,   lfamily,
	 given,    lgiven,
	 middle,   lmiddle,
	 nickname, lnickname,
	 bday,	   lbday,
	 ctry,	   lctry,
	 locality, llocality,
	 email,	   lemail,
	 orgname,  lorgname,
	 orgunit,  lorgunit] ->
	    ?INFO_MSG("Converting vcard_search table from "
		      "{user, luser, fn, lfn, family, lfamily, given, lgiven, middle, lmiddle, nickname, lnickname, bday, lbday, ctry, lctry, locality, llocality, email, lemail, orgname, lorgname, orgunit, lorgunit} format", []),
	    Host = ?MYNAME,
	    {atomic, ok} = mnesia:create_table(
			     mod_vcard_tmp_table,
			     [{disc_only_copies, [node()]},
			      {type, bag},
			      {local_content, true},
			      {record_name, vcard_search},
			      {attributes, record_info(fields, vcard_search)}]),
	    F1 = fun() ->
			 mnesia:write_lock_table(mod_vcard_tmp_table),
			 mnesia:foldl(
			   fun({vcard_search,
				User,     LUser,     
				FN,       LFN,       
				Family,   LFamily,   
				Given,    LGiven,    
				Middle,   LMiddle,   
				Nickname, LNickname, 
				BDay,     LBDay,     
				CTRY,     LCTRY,     
				Locality, LLocality, 
				EMail,    LEMail,    
				OrgName,  LOrgName,  
				OrgUnit,  LOrgUnit   
			       }, _) ->
				   mnesia:dirty_write(
				     mod_vcard_tmp_table,
				     #vcard_search{
				       us        = {LUser, Host},
				       user      = {User, Host},
				       luser     = LUser,
				       fn        = FN,       lfn        = LFN,
				       family    = Family,   lfamily    = LFamily,
				       given     = Given,    lgiven     = LGiven,
				       middle    = Middle,   lmiddle    = LMiddle,
				       nickname  = Nickname, lnickname  = LNickname,
				       bday      = BDay,     lbday      = LBDay,
				       ctry      = CTRY,     lctry      = LCTRY,
				       locality  = Locality, llocality  = LLocality,
				       email     = EMail,    lemail     = LEMail,
				       orgname   = OrgName,  lorgname   = LOrgName,
				       orgunit   = OrgUnit,  lorgunit   = LOrgUnit
				      })
			   end, ok, vcard_search)
		 end,
	    mnesia:transaction(F1),
	    lists:foreach(fun(I) ->
				  mnesia:del_table_index(
				    vcard_search,
				    element(I, {vcard_search,
						user,     luser,
						fn,       lfn,
						family,   lfamily,
						given,    lgiven,
						middle,   lmiddle,
						nickname, lnickname,
						bday,	   lbday,
						ctry,	   lctry,
						locality, llocality,
						email,	   lemail,
						orgname,  lorgname,
						orgunit,  lorgunit}))
			  end, mnesia:table_info(vcard_search, index)),
	    mnesia:clear_table(vcard_search),
	    mnesia:transform_table(vcard_search, ignore, Fields),
	    F2 = fun() ->
	        	 mnesia:write_lock_table(vcard_search),
	        	 mnesia:foldl(
	        	   fun(R, _) ->
	        		   mnesia:dirty_write(R)
	        	   end, ok, mod_vcard_tmp_table)
	         end,
	    mnesia:transaction(F2),
	    mnesia:delete_table(mod_vcard_tmp_table);
	_ ->
	    ?INFO_MSG("Recreating vcard_search table", []),
	    mnesia:transform_table(vcard_search, ignore, Fields)
    end.
