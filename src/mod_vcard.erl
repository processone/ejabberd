%%%----------------------------------------------------------------------
%%% File    : mod_vcard.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Vcard management
%%% Created :  2 Jan 2003 by Alexey Shchepin <alexey@process-one.net>
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

-module(mod_vcard).

-author('alexey@process-one.net').

-protocol({xep, 54, '1.2'}).
-protocol({xep, 55, '1.3'}).

-behaviour(gen_mod).

-export([start/2, init/3, stop/1, get_sm_features/5,
	 process_local_iq/3, process_sm_iq/3, reindex_vcards/0,
	 remove_user/2, export/1, import/1, import/3,
	 mod_opt_type/1]).

-include("ejabberd.hrl").
-include("logger.hrl").

-include("jlib.hrl").

-define(JUD_MATCHES, 30).

-record(vcard_search,
	{us, user, luser, fn, lfn, family, lfamily, given,
	 lgiven, middle, lmiddle, nickname, lnickname, bday,
	 lbday, ctry, lctry, locality, llocality, email, lemail,
	 orgname, lorgname, orgunit, lorgunit}).

-record(vcard, {us = {<<"">>, <<"">>} :: {binary(), binary()} | binary(),
                vcard = #xmlel{} :: xmlel()}).

-define(PROCNAME, ejabberd_mod_vcard).

start(Host, Opts) ->
    case gen_mod:db_type(Host, Opts) of
      mnesia ->
	  mnesia:create_table(vcard,
			      [{disc_only_copies, [node()]},
			       {attributes, record_info(fields, vcard)}]),
	  mnesia:create_table(vcard_search,
			      [{disc_copies, [node()]},
			       {attributes,
				record_info(fields, vcard_search)}]),
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
	  mnesia:add_table_index(vcard_search, lorgunit);
      _ -> ok
    end,
    ejabberd_hooks:add(remove_user, Host, ?MODULE,
		       remove_user, 50),
    IQDisc = gen_mod:get_opt(iqdisc, Opts, fun gen_iq_handler:check_type/1,
                             one_queue),
    gen_iq_handler:add_iq_handler(ejabberd_local, Host,
				  ?NS_VCARD, ?MODULE, process_local_iq, IQDisc),
    gen_iq_handler:add_iq_handler(ejabberd_sm, Host,
				  ?NS_VCARD, ?MODULE, process_sm_iq, IQDisc),
    ejabberd_hooks:add(disco_sm_features, Host, ?MODULE,
		       get_sm_features, 50),
    MyHost = gen_mod:get_opt_host(Host, Opts,
				  <<"vjud.@HOST@">>),
    Search = gen_mod:get_opt(search, Opts,
                             fun(B) when is_boolean(B) -> B end,
                             false),
    register(gen_mod:get_module_proc(Host, ?PROCNAME),
	     spawn(?MODULE, init, [MyHost, Host, Search])).

init(Host, ServerHost, Search) ->
    case Search of
      false -> loop(Host, ServerHost);
      _ ->
	  ejabberd_router:register_route(Host),
	  loop(Host, ServerHost)
    end.

loop(Host, ServerHost) ->
    receive
      {route, From, To, Packet} ->
	  case catch do_route(ServerHost, From, To, Packet) of
	    {'EXIT', Reason} -> ?ERROR_MSG("~p", [Reason]);
	    _ -> ok
	  end,
	  loop(Host, ServerHost);
      stop -> ejabberd_router:unregister_route(Host), ok;
      _ -> loop(Host, ServerHost)
    end.

stop(Host) ->
    ejabberd_hooks:delete(remove_user, Host, ?MODULE,
			  remove_user, 50),
    gen_iq_handler:remove_iq_handler(ejabberd_local, Host,
				     ?NS_VCARD),
    gen_iq_handler:remove_iq_handler(ejabberd_sm, Host,
				     ?NS_VCARD),
    ejabberd_hooks:delete(disco_sm_features, Host, ?MODULE,
			  get_sm_features, 50),
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    Proc ! stop,
    {wait, Proc}.

get_sm_features({error, _Error} = Acc, _From, _To,
		_Node, _Lang) ->
    Acc;
get_sm_features(Acc, _From, _To, Node, _Lang) ->
    case Node of
      <<"">> ->
	  case Acc of
	    {result, Features} ->
		{result, [?NS_DISCO_INFO, ?NS_VCARD | Features]};
	    empty -> {result, [?NS_DISCO_INFO, ?NS_VCARD]}
	  end;
      _ -> Acc
    end.

process_local_iq(_From, _To,
		 #iq{type = Type, lang = Lang, sub_el = SubEl} = IQ) ->
    case Type of
      set ->
	  IQ#iq{type = error, sub_el = [SubEl, ?ERR_NOT_ALLOWED]};
      get ->
	  IQ#iq{type = result,
		sub_el =
		    [#xmlel{name = <<"vCard">>,
			    attrs = [{<<"xmlns">>, ?NS_VCARD}],
			    children =
				[#xmlel{name = <<"FN">>, attrs = [],
					children =
					    [{xmlcdata, <<"ejabberd">>}]},
				 #xmlel{name = <<"URL">>, attrs = [],
					children = [{xmlcdata, ?EJABBERD_URI}]},
				 #xmlel{name = <<"DESC">>, attrs = [],
					children =
					    [{xmlcdata,
					      <<(translate:translate(Lang,
								     <<"Erlang Jabber Server">>))/binary,
						"\nCopyright (c) 2002-2015 ProcessOne">>}]},
				 #xmlel{name = <<"BDAY">>, attrs = [],
					children =
					    [{xmlcdata, <<"2002-11-16">>}]}]}]}
    end.

process_sm_iq(From, To,
	      #iq{type = Type, sub_el = SubEl} = IQ) ->
    case Type of
      set ->
	  #jid{user = User, lserver = LServer} = From,
	  case lists:member(LServer, ?MYHOSTS) of
	    true ->
		set_vcard(User, LServer, SubEl),
		IQ#iq{type = result, sub_el = []};
	    false ->
		IQ#iq{type = error, sub_el = [SubEl, ?ERR_NOT_ALLOWED]}
	  end;
      get ->
	  #jid{luser = LUser, lserver = LServer} = To,
	  case get_vcard(LUser, LServer) of
	    error ->
		IQ#iq{type = error,
		      sub_el = [SubEl, ?ERR_INTERNAL_SERVER_ERROR]};
	    [] ->
		IQ#iq{type = result,
		      sub_el = [#xmlel{name = <<"vCard">>,
			        attrs = [{<<"xmlns">>, ?NS_VCARD}],
			        children = []}]};
	    Els -> IQ#iq{type = result, sub_el = Els}
	  end
    end.

get_vcard(LUser, LServer) ->
    get_vcard(LUser, LServer,
	      gen_mod:db_type(LServer, ?MODULE)).

get_vcard(LUser, LServer, mnesia) ->
    US = {LUser, LServer},
    F = fun () -> mnesia:read({vcard, US}) end,
    case mnesia:transaction(F) of
      {atomic, Rs} ->
	  lists:map(fun (R) -> R#vcard.vcard end, Rs);
      {aborted, _Reason} -> error
    end;
get_vcard(LUser, LServer, odbc) ->
    Username = ejabberd_odbc:escape(LUser),
    case catch odbc_queries:get_vcard(LServer, Username) of
      {selected, [<<"vcard">>], [[SVCARD]]} ->
	  case xml_stream:parse_element(SVCARD) of
	    {error, _Reason} -> error;
	    VCARD -> [VCARD]
	  end;
      {selected, [<<"vcard">>], []} -> [];
      _ -> error
    end;
get_vcard(LUser, LServer, riak) ->
    case ejabberd_riak:get(vcard, vcard_schema(), {LUser, LServer}) of
        {ok, R} ->
            [R#vcard.vcard];
        {error, notfound} ->
            [];
        _ ->
            error
    end.

set_vcard(User, LServer, VCARD) ->
    FN = xml:get_path_s(VCARD, [{elem, <<"FN">>}, cdata]),
    Family = xml:get_path_s(VCARD,
			    [{elem, <<"N">>}, {elem, <<"FAMILY">>}, cdata]),
    Given = xml:get_path_s(VCARD,
			   [{elem, <<"N">>}, {elem, <<"GIVEN">>}, cdata]),
    Middle = xml:get_path_s(VCARD,
			    [{elem, <<"N">>}, {elem, <<"MIDDLE">>}, cdata]),
    Nickname = xml:get_path_s(VCARD,
			      [{elem, <<"NICKNAME">>}, cdata]),
    BDay = xml:get_path_s(VCARD,
			  [{elem, <<"BDAY">>}, cdata]),
    CTRY = xml:get_path_s(VCARD,
			  [{elem, <<"ADR">>}, {elem, <<"CTRY">>}, cdata]),
    Locality = xml:get_path_s(VCARD,
			      [{elem, <<"ADR">>}, {elem, <<"LOCALITY">>},
			       cdata]),
    EMail1 = xml:get_path_s(VCARD,
			    [{elem, <<"EMAIL">>}, {elem, <<"USERID">>}, cdata]),
    EMail2 = xml:get_path_s(VCARD,
			    [{elem, <<"EMAIL">>}, cdata]),
    OrgName = xml:get_path_s(VCARD,
			     [{elem, <<"ORG">>}, {elem, <<"ORGNAME">>}, cdata]),
    OrgUnit = xml:get_path_s(VCARD,
			     [{elem, <<"ORG">>}, {elem, <<"ORGUNIT">>}, cdata]),
    EMail = case EMail1 of
	      <<"">> -> EMail2;
	      _ -> EMail1
	    end,
    LUser = jid:nodeprep(User),
    LFN = string2lower(FN),
    LFamily = string2lower(Family),
    LGiven = string2lower(Given),
    LMiddle = string2lower(Middle),
    LNickname = string2lower(Nickname),
    LBDay = string2lower(BDay),
    LCTRY = string2lower(CTRY),
    LLocality = string2lower(Locality),
    LEMail = string2lower(EMail),
    LOrgName = string2lower(OrgName),
    LOrgUnit = string2lower(OrgUnit),
    if (LUser == error) ->
	   {error, badarg};
       true ->
	   case gen_mod:db_type(LServer, ?MODULE) of
	     mnesia ->
		 US = {LUser, LServer},
		 F = fun () ->
			     mnesia:write(#vcard{us = US, vcard = VCARD}),
			     mnesia:write(#vcard_search{us = US,
							user = {User, LServer},
							luser = LUser, fn = FN,
							lfn = LFN,
							family = Family,
							lfamily = LFamily,
							given = Given,
							lgiven = LGiven,
							middle = Middle,
							lmiddle = LMiddle,
							nickname = Nickname,
							lnickname = LNickname,
							bday = BDay,
							lbday = LBDay,
							ctry = CTRY,
							lctry = LCTRY,
							locality = Locality,
							llocality = LLocality,
							email = EMail,
							lemail = LEMail,
							orgname = OrgName,
							lorgname = LOrgName,
							orgunit = OrgUnit,
							lorgunit = LOrgUnit})
		     end,
		 mnesia:transaction(F);
             riak ->
                 US = {LUser, LServer},
                 ejabberd_riak:put(#vcard{us = US, vcard = VCARD},
				   vcard_schema(),
                                   [{'2i', [{<<"user">>, User},
                                            {<<"luser">>, LUser},
                                            {<<"fn">>, FN},
                                            {<<"lfn">>, LFN},
                                            {<<"family">>, Family},
                                            {<<"lfamily">>, LFamily},
                                            {<<"given">>, Given},
                                            {<<"lgiven">>, LGiven},
                                            {<<"middle">>, Middle},
                                            {<<"lmiddle">>, LMiddle},
                                            {<<"nickname">>, Nickname},
                                            {<<"lnickname">>, LNickname},
                                            {<<"bday">>, BDay},
                                            {<<"lbday">>, LBDay},
                                            {<<"ctry">>, CTRY},
                                            {<<"lctry">>, LCTRY},
                                            {<<"locality">>, Locality},
                                            {<<"llocality">>, LLocality},
                                            {<<"email">>, EMail},
                                            {<<"lemail">>, LEMail},
                                            {<<"orgname">>, OrgName},
                                            {<<"lorgname">>, LOrgName},
                                            {<<"orgunit">>, OrgUnit},
                                            {<<"lorgunit">>, LOrgUnit}]}]);
	     odbc ->
		 Username = ejabberd_odbc:escape(User),
		 LUsername = ejabberd_odbc:escape(LUser),
		 SVCARD =
		     ejabberd_odbc:escape(xml:element_to_binary(VCARD)),
		 SFN = ejabberd_odbc:escape(FN),
		 SLFN = ejabberd_odbc:escape(LFN),
		 SFamily = ejabberd_odbc:escape(Family),
		 SLFamily = ejabberd_odbc:escape(LFamily),
		 SGiven = ejabberd_odbc:escape(Given),
		 SLGiven = ejabberd_odbc:escape(LGiven),
		 SMiddle = ejabberd_odbc:escape(Middle),
		 SLMiddle = ejabberd_odbc:escape(LMiddle),
		 SNickname = ejabberd_odbc:escape(Nickname),
		 SLNickname = ejabberd_odbc:escape(LNickname),
		 SBDay = ejabberd_odbc:escape(BDay),
		 SLBDay = ejabberd_odbc:escape(LBDay),
		 SCTRY = ejabberd_odbc:escape(CTRY),
		 SLCTRY = ejabberd_odbc:escape(LCTRY),
		 SLocality = ejabberd_odbc:escape(Locality),
		 SLLocality = ejabberd_odbc:escape(LLocality),
		 SEMail = ejabberd_odbc:escape(EMail),
		 SLEMail = ejabberd_odbc:escape(LEMail),
		 SOrgName = ejabberd_odbc:escape(OrgName),
		 SLOrgName = ejabberd_odbc:escape(LOrgName),
		 SOrgUnit = ejabberd_odbc:escape(OrgUnit),
		 SLOrgUnit = ejabberd_odbc:escape(LOrgUnit),
		 odbc_queries:set_vcard(LServer, LUsername, SBDay, SCTRY,
					SEMail, SFN, SFamily, SGiven, SLBDay,
					SLCTRY, SLEMail, SLFN, SLFamily,
					SLGiven, SLLocality, SLMiddle,
					SLNickname, SLOrgName, SLOrgUnit,
					SLocality, SMiddle, SNickname, SOrgName,
					SOrgUnit, SVCARD, Username)
	   end,
	   ejabberd_hooks:run(vcard_set, LServer,
			      [LUser, LServer, VCARD])
    end.

string2lower(String) ->
    case stringprep:tolower(String) of
      Lower when is_binary(Lower) -> Lower;
      error -> str:to_lower(String)
    end.

-define(TLFIELD(Type, Label, Var),
	#xmlel{name = <<"field">>,
	       attrs =
		   [{<<"type">>, Type},
		    {<<"label">>, translate:translate(Lang, Label)},
		    {<<"var">>, Var}],
	       children = []}).

-define(FORM(JID),
	[#xmlel{name = <<"instructions">>, attrs = [],
		children =
		    [{xmlcdata,
		      translate:translate(Lang,
					  <<"You need an x:data capable client to "
					    "search">>)}]},
	 #xmlel{name = <<"x">>,
		attrs =
		    [{<<"xmlns">>, ?NS_XDATA}, {<<"type">>, <<"form">>}],
		children =
		    [#xmlel{name = <<"title">>, attrs = [],
			    children =
				[{xmlcdata,
				  <<(translate:translate(Lang,
							 <<"Search users in ">>))/binary,
				    (jid:to_string(JID))/binary>>}]},
		     #xmlel{name = <<"instructions">>, attrs = [],
			    children =
				[{xmlcdata,
				  translate:translate(Lang,
						      <<"Fill in the form to search for any matching "
							"Jabber User (Add * to the end of field "
							"to match substring)">>)}]},
		     ?TLFIELD(<<"text-single">>, <<"User">>, <<"user">>),
		     ?TLFIELD(<<"text-single">>, <<"Full Name">>, <<"fn">>),
		     ?TLFIELD(<<"text-single">>, <<"Name">>, <<"first">>),
		     ?TLFIELD(<<"text-single">>, <<"Middle Name">>,
			      <<"middle">>),
		     ?TLFIELD(<<"text-single">>, <<"Family Name">>,
			      <<"last">>),
		     ?TLFIELD(<<"text-single">>, <<"Nickname">>, <<"nick">>),
		     ?TLFIELD(<<"text-single">>, <<"Birthday">>, <<"bday">>),
		     ?TLFIELD(<<"text-single">>, <<"Country">>, <<"ctry">>),
		     ?TLFIELD(<<"text-single">>, <<"City">>, <<"locality">>),
		     ?TLFIELD(<<"text-single">>, <<"Email">>, <<"email">>),
		     ?TLFIELD(<<"text-single">>, <<"Organization Name">>,
			      <<"orgname">>),
		     ?TLFIELD(<<"text-single">>, <<"Organization Unit">>,
			      <<"orgunit">>)]}]).

do_route(ServerHost, From, To, Packet) ->
    #jid{user = User, resource = Resource} = To,
    if (User /= <<"">>) or (Resource /= <<"">>) ->
	   Err = jlib:make_error_reply(Packet,
				       ?ERR_SERVICE_UNAVAILABLE),
	   ejabberd_router:route(To, From, Err);
       true ->
	   IQ = jlib:iq_query_info(Packet),
	   case IQ of
	     #iq{type = Type, xmlns = ?NS_SEARCH, lang = Lang,
		 sub_el = SubEl} ->
		 case Type of
		   set ->
		       XDataEl = find_xdata_el(SubEl),
		       case XDataEl of
			 false ->
			     Err = jlib:make_error_reply(Packet,
							 ?ERR_BAD_REQUEST),
			     ejabberd_router:route(To, From, Err);
			 _ ->
			     XData = jlib:parse_xdata_submit(XDataEl),
			     case XData of
			       invalid ->
				   Err = jlib:make_error_reply(Packet,
							       ?ERR_BAD_REQUEST),
				   ejabberd_router:route(To, From, Err);
			       _ ->
				   ResIQ = IQ#iq{type = result,
						 sub_el =
						     [#xmlel{name = <<"query">>,
							     attrs =
								 [{<<"xmlns">>,
								   ?NS_SEARCH}],
							     children =
								 [#xmlel{name =
									     <<"x">>,
									 attrs =
									     [{<<"xmlns">>,
									       ?NS_XDATA},
									      {<<"type">>,
									       <<"result">>}],
									 children
									     =
									     search_result(Lang,
											   To,
											   ServerHost,
											   XData)}]}]},
				   ejabberd_router:route(To, From,
							 jlib:iq_to_xml(ResIQ))
			     end
		       end;
		   get ->
		       ResIQ = IQ#iq{type = result,
				     sub_el =
					 [#xmlel{name = <<"query">>,
						 attrs =
						     [{<<"xmlns">>,
						       ?NS_SEARCH}],
						 children = ?FORM(To)}]},
		       ejabberd_router:route(To, From, jlib:iq_to_xml(ResIQ))
		 end;
	     #iq{type = Type, xmlns = ?NS_DISCO_INFO, lang = Lang} ->
		 case Type of
		   set ->
		       Err = jlib:make_error_reply(Packet, ?ERR_NOT_ALLOWED),
		       ejabberd_router:route(To, From, Err);
		   get ->
		       Info = ejabberd_hooks:run_fold(disco_info, ServerHost,
						      [],
						      [ServerHost, ?MODULE,
						       <<"">>, <<"">>]),
		       ResIQ = IQ#iq{type = result,
				     sub_el =
					 [#xmlel{name = <<"query">>,
						 attrs =
						     [{<<"xmlns">>,
						       ?NS_DISCO_INFO}],
						 children =
						     [#xmlel{name =
								 <<"identity">>,
							     attrs =
								 [{<<"category">>,
								   <<"directory">>},
								  {<<"type">>,
								   <<"user">>},
								  {<<"name">>,
								   translate:translate(Lang,
										       <<"vCard User Search">>)}],
							     children = []},
						      #xmlel{name =
								 <<"feature">>,
							     attrs =
								 [{<<"var">>,
								   ?NS_DISCO_INFO}],
							     children = []},
						      #xmlel{name =
								 <<"feature">>,
							     attrs =
								 [{<<"var">>,
								   ?NS_SEARCH}],
							     children = []},
						      #xmlel{name =
								 <<"feature">>,
							     attrs =
								 [{<<"var">>,
								   ?NS_VCARD}],
							     children = []}]
						       ++ Info}]},
		       ejabberd_router:route(To, From, jlib:iq_to_xml(ResIQ))
		 end;
	     #iq{type = Type, xmlns = ?NS_DISCO_ITEMS} ->
		 case Type of
		   set ->
		       Err = jlib:make_error_reply(Packet, ?ERR_NOT_ALLOWED),
		       ejabberd_router:route(To, From, Err);
		   get ->
		       ResIQ = IQ#iq{type = result,
				     sub_el =
					 [#xmlel{name = <<"query">>,
						 attrs =
						     [{<<"xmlns">>,
						       ?NS_DISCO_ITEMS}],
						 children = []}]},
		       ejabberd_router:route(To, From, jlib:iq_to_xml(ResIQ))
		 end;
	     #iq{type = get, xmlns = ?NS_VCARD, lang = Lang} ->
		 ResIQ = IQ#iq{type = result,
			       sub_el =
				   [#xmlel{name = <<"vCard">>,
					   attrs = [{<<"xmlns">>, ?NS_VCARD}],
					   children = iq_get_vcard(Lang)}]},
		 ejabberd_router:route(To, From, jlib:iq_to_xml(ResIQ));
	     _ ->
		 Err = jlib:make_error_reply(Packet,
					     ?ERR_SERVICE_UNAVAILABLE),
		 ejabberd_router:route(To, From, Err)
	   end
    end.

iq_get_vcard(Lang) ->
    [#xmlel{name = <<"FN">>, attrs = [],
	    children = [{xmlcdata, <<"ejabberd/mod_vcard">>}]},
     #xmlel{name = <<"URL">>, attrs = [],
	    children = [{xmlcdata, ?EJABBERD_URI}]},
     #xmlel{name = <<"DESC">>, attrs = [],
	    children =
		[{xmlcdata,
		  <<(translate:translate(Lang,
					 <<"ejabberd vCard module">>))/binary,
		    "\nCopyright (c) 2003-2015 ProcessOne">>}]}].

find_xdata_el(#xmlel{children = SubEls}) ->
    find_xdata_el1(SubEls).

find_xdata_el1([]) -> false;
find_xdata_el1([#xmlel{name = Name, attrs = Attrs,
		       children = SubEls}
		| Els]) ->
    case xml:get_attr_s(<<"xmlns">>, Attrs) of
      ?NS_XDATA ->
	  #xmlel{name = Name, attrs = Attrs, children = SubEls};
      _ -> find_xdata_el1(Els)
    end;
find_xdata_el1([_ | Els]) -> find_xdata_el1(Els).

-define(LFIELD(Label, Var),
	#xmlel{name = <<"field">>,
	       attrs =
		   [{<<"label">>, translate:translate(Lang, Label)},
		    {<<"var">>, Var}],
	       children = []}).

search_result(Lang, JID, ServerHost, Data) ->
    [#xmlel{name = <<"title">>, attrs = [],
	    children =
		[{xmlcdata,
		  <<(translate:translate(Lang,
					 <<"Search Results for ">>))/binary,
		    (jid:to_string(JID))/binary>>}]},
     #xmlel{name = <<"reported">>, attrs = [],
	    children =
		[?TLFIELD(<<"text-single">>, <<"Jabber ID">>,
			  <<"jid">>),
		 ?TLFIELD(<<"text-single">>, <<"Full Name">>, <<"fn">>),
		 ?TLFIELD(<<"text-single">>, <<"Name">>, <<"first">>),
		 ?TLFIELD(<<"text-single">>, <<"Middle Name">>,
			  <<"middle">>),
		 ?TLFIELD(<<"text-single">>, <<"Family Name">>,
			  <<"last">>),
		 ?TLFIELD(<<"text-single">>, <<"Nickname">>, <<"nick">>),
		 ?TLFIELD(<<"text-single">>, <<"Birthday">>, <<"bday">>),
		 ?TLFIELD(<<"text-single">>, <<"Country">>, <<"ctry">>),
		 ?TLFIELD(<<"text-single">>, <<"City">>, <<"locality">>),
		 ?TLFIELD(<<"text-single">>, <<"Email">>, <<"email">>),
		 ?TLFIELD(<<"text-single">>, <<"Organization Name">>,
			  <<"orgname">>),
		 ?TLFIELD(<<"text-single">>, <<"Organization Unit">>,
			  <<"orgunit">>)]}]
      ++
      lists:map(fun (R) -> record_to_item(ServerHost, R) end,
		search(ServerHost, Data)).

-define(FIELD(Var, Val),
	#xmlel{name = <<"field">>, attrs = [{<<"var">>, Var}],
	       children =
		   [#xmlel{name = <<"value">>, attrs = [],
			   children = [{xmlcdata, Val}]}]}).

record_to_item(LServer,
	       [Username, FN, Family, Given, Middle, Nickname, BDay,
		CTRY, Locality, EMail, OrgName, OrgUnit]) ->
    #xmlel{name = <<"item">>, attrs = [],
	   children =
	       [?FIELD(<<"jid">>,
		       <<Username/binary, "@", LServer/binary>>),
		?FIELD(<<"fn">>, FN), ?FIELD(<<"last">>, Family),
		?FIELD(<<"first">>, Given),
		?FIELD(<<"middle">>, Middle),
		?FIELD(<<"nick">>, Nickname), ?FIELD(<<"bday">>, BDay),
		?FIELD(<<"ctry">>, CTRY),
		?FIELD(<<"locality">>, Locality),
		?FIELD(<<"email">>, EMail),
		?FIELD(<<"orgname">>, OrgName),
		?FIELD(<<"orgunit">>, OrgUnit)]};
record_to_item(_LServer, #vcard_search{} = R) ->
    {User, Server} = R#vcard_search.user,
    #xmlel{name = <<"item">>, attrs = [],
	   children =
	       [?FIELD(<<"jid">>, <<User/binary, "@", Server/binary>>),
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
		?FIELD(<<"orgunit">>, (R#vcard_search.orgunit))]}.

search(LServer, Data) ->
    DBType = gen_mod:db_type(LServer, ?MODULE),
    MatchSpec = make_matchspec(LServer, Data, DBType),
    AllowReturnAll = gen_mod:get_module_opt(LServer, ?MODULE, allow_return_all,
                                            fun(B) when is_boolean(B) -> B end,
                                            false),
    search(LServer, MatchSpec, AllowReturnAll, DBType).

search(LServer, MatchSpec, AllowReturnAll, mnesia) ->
    if (MatchSpec == #vcard_search{_ = '_'}) and
	 not AllowReturnAll ->
	   [];
       true ->
	   case catch mnesia:dirty_select(vcard_search,
					  [{MatchSpec, [], ['$_']}])
	       of
	     {'EXIT', Reason} -> ?ERROR_MSG("~p", [Reason]), [];
	     Rs ->
		 case gen_mod:get_module_opt(LServer, ?MODULE, matches,
                                             fun(infinity) -> infinity;
                                                (I) when is_integer(I),
                                                         I>0 ->
                                                     I
                                             end, ?JUD_MATCHES) of
                     infinity ->
                         Rs;
                     Val ->
                         lists:sublist(Rs, Val)
                 end
	   end
    end;
search(LServer, MatchSpec, AllowReturnAll, odbc) ->
    if (MatchSpec == <<"">>) and not AllowReturnAll -> [];
       true ->
	   Limit = case gen_mod:get_module_opt(LServer, ?MODULE, matches,
                                               fun(infinity) -> infinity;
                                                  (I) when is_integer(I),
                                                           I>0 ->
                                                       I
                                               end, ?JUD_MATCHES) of
                       infinity ->
                           <<"">>;
                       Val ->
                           [<<" LIMIT ">>,
                            jlib:integer_to_binary(Val)]
                   end,
	   case catch ejabberd_odbc:sql_query(LServer,
					      [<<"select username, fn, family, given, "
						 "middle,        nickname, bday, ctry, "
						 "locality,        email, orgname, orgunit "
						 "from vcard_search ">>,
					       MatchSpec, Limit, <<";">>])
	       of
	     {selected,
	      [<<"username">>, <<"fn">>, <<"family">>, <<"given">>,
	       <<"middle">>, <<"nickname">>, <<"bday">>, <<"ctry">>,
	       <<"locality">>, <<"email">>, <<"orgname">>,
	       <<"orgunit">>],
	      Rs}
		 when is_list(Rs) ->
		 Rs;
	     Error -> ?ERROR_MSG("~p", [Error]), []
	   end
    end;
search(_LServer, _MatchSpec, _AllowReturnAll, riak) ->
    [].

make_matchspec(LServer, Data, mnesia) ->
    GlobMatch = #vcard_search{_ = '_'},
    Match = filter_fields(Data, GlobMatch, LServer, mnesia),
    Match;
make_matchspec(LServer, Data, odbc) ->
    filter_fields(Data, <<"">>, LServer, odbc);
make_matchspec(_LServer, _Data, riak) ->
    [].

filter_fields([], Match, _LServer, mnesia) -> Match;
filter_fields([], Match, _LServer, odbc) ->
    case Match of
      <<"">> -> <<"">>;
      _ -> [<<" where ">>, Match]
    end;
filter_fields([{SVar, [Val]} | Ds], Match, LServer,
	      mnesia)
    when is_binary(Val) and (Val /= <<"">>) ->
    LVal = string2lower(Val),
    NewMatch = case SVar of
		 <<"user">> ->
		     case gen_mod:get_module_opt(LServer, ?MODULE,
                                                 search_all_hosts,
                                                 fun(B) when is_boolean(B) ->
                                                         B
                                                 end, true)
			 of
		       true -> Match#vcard_search{luser = make_val(LVal)};
		       false ->
			   Host = find_my_host(LServer),
			   Match#vcard_search{us = {make_val(LVal), Host}}
		     end;
		 <<"fn">> -> Match#vcard_search{lfn = make_val(LVal)};
		 <<"last">> ->
		     Match#vcard_search{lfamily = make_val(LVal)};
		 <<"first">> ->
		     Match#vcard_search{lgiven = make_val(LVal)};
		 <<"middle">> ->
		     Match#vcard_search{lmiddle = make_val(LVal)};
		 <<"nick">> ->
		     Match#vcard_search{lnickname = make_val(LVal)};
		 <<"bday">> ->
		     Match#vcard_search{lbday = make_val(LVal)};
		 <<"ctry">> ->
		     Match#vcard_search{lctry = make_val(LVal)};
		 <<"locality">> ->
		     Match#vcard_search{llocality = make_val(LVal)};
		 <<"email">> ->
		     Match#vcard_search{lemail = make_val(LVal)};
		 <<"orgname">> ->
		     Match#vcard_search{lorgname = make_val(LVal)};
		 <<"orgunit">> ->
		     Match#vcard_search{lorgunit = make_val(LVal)};
		 _ -> Match
	       end,
    filter_fields(Ds, NewMatch, LServer, mnesia);
filter_fields([{SVar, [Val]} | Ds], Match, LServer,
	      odbc)
    when is_binary(Val) and (Val /= <<"">>) ->
    LVal = string2lower(Val),
    NewMatch = case SVar of
		 <<"user">> -> make_val(Match, <<"lusername">>, LVal);
		 <<"fn">> -> make_val(Match, <<"lfn">>, LVal);
		 <<"last">> -> make_val(Match, <<"lfamily">>, LVal);
		 <<"first">> -> make_val(Match, <<"lgiven">>, LVal);
		 <<"middle">> -> make_val(Match, <<"lmiddle">>, LVal);
		 <<"nick">> -> make_val(Match, <<"lnickname">>, LVal);
		 <<"bday">> -> make_val(Match, <<"lbday">>, LVal);
		 <<"ctry">> -> make_val(Match, <<"lctry">>, LVal);
		 <<"locality">> ->
		     make_val(Match, <<"llocality">>, LVal);
		 <<"email">> -> make_val(Match, <<"lemail">>, LVal);
		 <<"orgname">> -> make_val(Match, <<"lorgname">>, LVal);
		 <<"orgunit">> -> make_val(Match, <<"lorgunit">>, LVal);
		 _ -> Match
	       end,
    filter_fields(Ds, NewMatch, LServer, odbc);
filter_fields([_ | Ds], Match, LServer, DBType) ->
    filter_fields(Ds, Match, LServer, DBType).

make_val(Match, Field, Val) ->
    Condition = case str:suffix(<<"*">>, Val) of
		  true ->
		      Val1 = str:substr(Val, 1, byte_size(Val) - 1),
		      SVal = <<(ejabberd_odbc:escape_like(Val1))/binary,
			       "%">>,
		      [Field, <<" LIKE '">>, SVal, <<"'">>];
		  _ ->
		      SVal = ejabberd_odbc:escape(Val),
		      [Field, <<" = '">>, SVal, <<"'">>]
		end,
    case Match of
      <<"">> -> Condition;
      _ -> [Match, <<" and ">>, Condition]
    end.

make_val(Val) ->
    case str:suffix(<<"*">>, Val) of
      true -> [str:substr(Val, 1, byte_size(Val) - 1)] ++ '_';
      _ -> Val
    end.

find_my_host(LServer) ->
    Parts = str:tokens(LServer, <<".">>),
    find_my_host(Parts, ?MYHOSTS).

find_my_host([], _Hosts) -> ?MYNAME;
find_my_host([_ | Tail] = Parts, Hosts) ->
    Domain = parts_to_string(Parts),
    case lists:member(Domain, Hosts) of
      true -> Domain;
      false -> find_my_host(Tail, Hosts)
    end.

parts_to_string(Parts) ->
    str:strip(list_to_binary(
                lists:map(fun (S) -> <<S/binary, $.>> end, Parts)),
              right, $.).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

set_vcard_t(R, _) ->
    US = R#vcard.us,
    User = US,
    VCARD = R#vcard.vcard,
    FN = xml:get_path_s(VCARD, [{elem, <<"FN">>}, cdata]),
    Family = xml:get_path_s(VCARD,
			    [{elem, <<"N">>}, {elem, <<"FAMILY">>}, cdata]),
    Given = xml:get_path_s(VCARD,
			   [{elem, <<"N">>}, {elem, <<"GIVEN">>}, cdata]),
    Middle = xml:get_path_s(VCARD,
			    [{elem, <<"N">>}, {elem, <<"MIDDLE">>}, cdata]),
    Nickname = xml:get_path_s(VCARD,
			      [{elem, <<"NICKNAME">>}, cdata]),
    BDay = xml:get_path_s(VCARD,
			  [{elem, <<"BDAY">>}, cdata]),
    CTRY = xml:get_path_s(VCARD,
			  [{elem, <<"ADR">>}, {elem, <<"CTRY">>}, cdata]),
    Locality = xml:get_path_s(VCARD,
			      [{elem, <<"ADR">>}, {elem, <<"LOCALITY">>},
			       cdata]),
    EMail = xml:get_path_s(VCARD,
			   [{elem, <<"EMAIL">>}, cdata]),
    OrgName = xml:get_path_s(VCARD,
			     [{elem, <<"ORG">>}, {elem, <<"ORGNAME">>}, cdata]),
    OrgUnit = xml:get_path_s(VCARD,
			     [{elem, <<"ORG">>}, {elem, <<"ORGUNIT">>}, cdata]),
    {LUser, _LServer} = US,
    LFN = string2lower(FN),
    LFamily = string2lower(Family),
    LGiven = string2lower(Given),
    LMiddle = string2lower(Middle),
    LNickname = string2lower(Nickname),
    LBDay = string2lower(BDay),
    LCTRY = string2lower(CTRY),
    LLocality = string2lower(Locality),
    LEMail = string2lower(EMail),
    LOrgName = string2lower(OrgName),
    LOrgUnit = string2lower(OrgUnit),
    mnesia:write(#vcard_search{us = US, user = User,
                               luser = LUser, fn = FN, lfn = LFN,
                               family = Family, lfamily = LFamily,
                               given = Given, lgiven = LGiven,
                               middle = Middle, lmiddle = LMiddle,
                               nickname = Nickname,
                               lnickname = LNickname, bday = BDay,
                               lbday = LBDay, ctry = CTRY, lctry = LCTRY,
                               locality = Locality,
                               llocality = LLocality, email = EMail,
                               lemail = LEMail, orgname = OrgName,
                               lorgname = LOrgName, orgunit = OrgUnit,
                               lorgunit = LOrgUnit}).

reindex_vcards() ->
    F = fun () -> mnesia:foldl(fun set_vcard_t/2, [], vcard)
	end,
    mnesia:transaction(F).

remove_user(User, Server) ->
    LUser = jid:nodeprep(User),
    LServer = jid:nameprep(Server),
    remove_user(LUser, LServer,
		gen_mod:db_type(LServer, ?MODULE)).

remove_user(LUser, LServer, mnesia) ->
    US = {LUser, LServer},
    F = fun () ->
		mnesia:delete({vcard, US}),
		mnesia:delete({vcard_search, US})
	end,
    mnesia:transaction(F);
remove_user(LUser, LServer, odbc) ->
    Username = ejabberd_odbc:escape(LUser),
    ejabberd_odbc:sql_transaction(LServer,
				  [[<<"delete from vcard where username='">>,
				    Username, <<"';">>],
				   [<<"delete from vcard_search where lusername='">>,
				    Username, <<"';">>]]);
remove_user(LUser, LServer, riak) ->
    {atomic, ejabberd_riak:delete(vcard, {LUser, LServer})}.

update_tables() ->
    update_vcard_table(),
    update_vcard_search_table().

update_vcard_table() ->
    Fields = record_info(fields, vcard),
    case mnesia:table_info(vcard, attributes) of
      Fields ->
          ejabberd_config:convert_table_to_binary(
            vcard, Fields, set,
            fun(#vcard{us = {U, _}}) -> U end,
            fun(#vcard{us = {U, S}, vcard = El} = R) ->
                    R#vcard{us = {iolist_to_binary(U),
                                  iolist_to_binary(S)},
                            vcard = xml:to_xmlel(El)}
            end);
      _ ->
	  ?INFO_MSG("Recreating vcard table", []),
	  mnesia:transform_table(vcard, ignore, Fields)
    end.

update_vcard_search_table() ->
    Fields = record_info(fields, vcard_search),
    case mnesia:table_info(vcard_search, attributes) of
      Fields ->
          ejabberd_config:convert_table_to_binary(
            vcard_search, Fields, set,
            fun(#vcard_search{us = {U, _}}) -> U end,
            fun(#vcard_search{} = VS) ->
                    [vcard_search | L] = tuple_to_list(VS),
                    NewL = lists:map(
                             fun({U, S}) ->
                                     {iolist_to_binary(U),
                                      iolist_to_binary(S)};
                                (Str) ->
                                     iolist_to_binary(Str)
                             end, L),
                    list_to_tuple([vcard_search | NewL])
            end);
      _ ->
	  ?INFO_MSG("Recreating vcard_search table", []),
	  mnesia:transform_table(vcard_search, ignore, Fields)
    end.

vcard_schema() ->
    {record_info(fields, vcard), #vcard{}}.

export(_Server) ->   
    [{vcard,
      fun(Host, #vcard{us = {LUser, LServer}, vcard = VCARD})
            when LServer == Host ->
              Username = ejabberd_odbc:escape(LUser),
              SVCARD =
                  ejabberd_odbc:escape(xml:element_to_binary(VCARD)),
              [[<<"delete from vcard where username='">>, Username, <<"';">>],
               [<<"insert into vcard(username, vcard) values ('">>,
                Username, <<"', '">>, SVCARD, <<"');">>]];
         (_Host, _R) ->
              []
      end},
     {vcard_search,
      fun(Host, #vcard_search{user = {User, LServer}, luser = LUser,
                              fn = FN, lfn = LFN, family = Family,
                              lfamily = LFamily, given = Given,
                              lgiven = LGiven, middle = Middle,
                              lmiddle = LMiddle, nickname = Nickname,
                              lnickname = LNickname, bday = BDay,
                              lbday = LBDay, ctry = CTRY, lctry = LCTRY,
                              locality = Locality, llocality = LLocality,
                              email = EMail, lemail = LEMail,
                              orgname = OrgName, lorgname = LOrgName,
                              orgunit = OrgUnit, lorgunit = LOrgUnit})
            when LServer == Host ->
              Username = ejabberd_odbc:escape(User),
              LUsername = ejabberd_odbc:escape(LUser),
              SFN = ejabberd_odbc:escape(FN),
              SLFN = ejabberd_odbc:escape(LFN),
              SFamily = ejabberd_odbc:escape(Family),
              SLFamily = ejabberd_odbc:escape(LFamily),
              SGiven = ejabberd_odbc:escape(Given),
              SLGiven = ejabberd_odbc:escape(LGiven),
              SMiddle = ejabberd_odbc:escape(Middle),
              SLMiddle = ejabberd_odbc:escape(LMiddle),
              SNickname = ejabberd_odbc:escape(Nickname),
              SLNickname = ejabberd_odbc:escape(LNickname),
              SBDay = ejabberd_odbc:escape(BDay),
              SLBDay = ejabberd_odbc:escape(LBDay),
              SCTRY = ejabberd_odbc:escape(CTRY),
              SLCTRY = ejabberd_odbc:escape(LCTRY),
              SLocality = ejabberd_odbc:escape(Locality),
              SLLocality = ejabberd_odbc:escape(LLocality),
              SEMail = ejabberd_odbc:escape(EMail),
              SLEMail = ejabberd_odbc:escape(LEMail),
              SOrgName = ejabberd_odbc:escape(OrgName),
              SLOrgName = ejabberd_odbc:escape(LOrgName),
              SOrgUnit = ejabberd_odbc:escape(OrgUnit),
              SLOrgUnit = ejabberd_odbc:escape(LOrgUnit),
              [[<<"delete from vcard_search where lusername='">>,
                LUsername, <<"';">>],
               [<<"insert into vcard_search(        username, "
                  "lusername, fn, lfn, family, lfamily, "
                  "       given, lgiven, middle, lmiddle, "
                  "nickname, lnickname,        bday, lbday, "
                  "ctry, lctry, locality, llocality,   "
                  "     email, lemail, orgname, lorgname, "
                  "orgunit, lorgunit)values (">>,
                <<"        '">>, Username, <<"', '">>, LUsername,
                <<"',        '">>, SFN, <<"', '">>, SLFN,
                <<"',        '">>, SFamily, <<"', '">>, SLFamily,
                <<"',        '">>, SGiven, <<"', '">>, SLGiven,
                <<"',        '">>, SMiddle, <<"', '">>, SLMiddle,
                <<"',        '">>, SNickname, <<"', '">>, SLNickname,
                <<"',        '">>, SBDay, <<"', '">>, SLBDay,
                <<"',        '">>, SCTRY, <<"', '">>, SLCTRY,
                <<"',        '">>, SLocality, <<"', '">>, SLLocality,
                <<"',        '">>, SEMail, <<"', '">>, SLEMail,
                <<"',        '">>, SOrgName, <<"', '">>, SLOrgName,
                <<"',        '">>, SOrgUnit, <<"', '">>, SLOrgUnit,
                <<"');">>]];
         (_Host, _R) ->
              []
      end}].

import(LServer) ->
    [{<<"select username, vcard from vcard;">>,
      fun([LUser, SVCard]) ->
              #xmlel{} = VCARD = xml_stream:parse_element(SVCard),
              #vcard{us = {LUser, LServer}, vcard = VCARD}
      end},
     {<<"select username, lusername, fn, lfn, family, lfamily, "
        "given, lgiven, middle, lmiddle, nickname, lnickname, "
        "bday, lbday, ctry, lctry, locality, llocality, email, "
        "lemail, orgname, lorgname, orgunit, lorgunit from vcard_search;">>,
      fun([User, LUser, FN, LFN,
           Family, LFamily, Given, LGiven,
           Middle, LMiddle, Nickname, LNickname,
           BDay, LBDay, CTRY, LCTRY, Locality, LLocality,
           EMail, LEMail, OrgName, LOrgName, OrgUnit, LOrgUnit]) ->
              #vcard_search{us = {LUser, LServer},
                            user = {User, LServer}, luser = LUser,
                            fn = FN, lfn = LFN, family = Family,
                            lfamily = LFamily, given = Given,
                            lgiven = LGiven, middle = Middle,
                            lmiddle = LMiddle, nickname = Nickname,
                            lnickname = LNickname, bday = BDay,
                            lbday = LBDay, ctry = CTRY, lctry = LCTRY,
                            locality = Locality, llocality = LLocality,
                            email = EMail, lemail = LEMail,
                            orgname = OrgName, lorgname = LOrgName,
                            orgunit = OrgUnit, lorgunit = LOrgUnit}
      end}].

import(_LServer, mnesia, #vcard{} = VCard) ->
    mnesia:dirty_write(VCard);
import(_LServer, mnesia, #vcard_search{} = S) ->
    mnesia:dirty_write(S);
import(_LServer, riak, #vcard{us = {LUser, _}, vcard = El} = VCard) ->
    FN = xml:get_path_s(El, [{elem, <<"FN">>}, cdata]),
    Family = xml:get_path_s(El,
			    [{elem, <<"N">>}, {elem, <<"FAMILY">>}, cdata]),
    Given = xml:get_path_s(El,
			   [{elem, <<"N">>}, {elem, <<"GIVEN">>}, cdata]),
    Middle = xml:get_path_s(El,
			    [{elem, <<"N">>}, {elem, <<"MIDDLE">>}, cdata]),
    Nickname = xml:get_path_s(El,
			      [{elem, <<"NICKNAME">>}, cdata]),
    BDay = xml:get_path_s(El,
			  [{elem, <<"BDAY">>}, cdata]),
    CTRY = xml:get_path_s(El,
			  [{elem, <<"ADR">>}, {elem, <<"CTRY">>}, cdata]),
    Locality = xml:get_path_s(El,
			      [{elem, <<"ADR">>}, {elem, <<"LOCALITY">>},
			       cdata]),
    EMail1 = xml:get_path_s(El,
			    [{elem, <<"EMAIL">>}, {elem, <<"USERID">>}, cdata]),
    EMail2 = xml:get_path_s(El,
			    [{elem, <<"EMAIL">>}, cdata]),
    OrgName = xml:get_path_s(El,
			     [{elem, <<"ORG">>}, {elem, <<"ORGNAME">>}, cdata]),
    OrgUnit = xml:get_path_s(El,
			     [{elem, <<"ORG">>}, {elem, <<"ORGUNIT">>}, cdata]),
    EMail = case EMail1 of
	      <<"">> -> EMail2;
	      _ -> EMail1
	    end,
    LFN = string2lower(FN),
    LFamily = string2lower(Family),
    LGiven = string2lower(Given),
    LMiddle = string2lower(Middle),
    LNickname = string2lower(Nickname),
    LBDay = string2lower(BDay),
    LCTRY = string2lower(CTRY),
    LLocality = string2lower(Locality),
    LEMail = string2lower(EMail),
    LOrgName = string2lower(OrgName),
    LOrgUnit = string2lower(OrgUnit),
    ejabberd_riak:put(VCard, vcard_schema(),
                      [{'2i', [{<<"user">>, LUser},
                               {<<"luser">>, LUser},
                               {<<"fn">>, FN},
                               {<<"lfn">>, LFN},
                               {<<"family">>, Family},
                               {<<"lfamily">>, LFamily},
                               {<<"given">>, Given},
                               {<<"lgiven">>, LGiven},
                               {<<"middle">>, Middle},
                               {<<"lmiddle">>, LMiddle},
                               {<<"nickname">>, Nickname},
                               {<<"lnickname">>, LNickname},
                               {<<"bday">>, BDay},
                               {<<"lbday">>, LBDay},
                               {<<"ctry">>, CTRY},
                               {<<"lctry">>, LCTRY},
                               {<<"locality">>, Locality},
                               {<<"llocality">>, LLocality},
                               {<<"email">>, EMail},
                               {<<"lemail">>, LEMail},
                               {<<"orgname">>, OrgName},
                               {<<"lorgname">>, LOrgName},
                               {<<"orgunit">>, OrgUnit},
                               {<<"lorgunit">>, LOrgUnit}]}]);
import(_LServer, riak, #vcard_search{}) ->
    ok;
import(_, _, _) ->
    pass.

mod_opt_type(allow_return_all) ->
    fun (B) when is_boolean(B) -> B end;
mod_opt_type(db_type) -> fun gen_mod:v_db/1;
mod_opt_type(host) -> fun iolist_to_binary/1;
mod_opt_type(iqdisc) -> fun gen_iq_handler:check_type/1;
mod_opt_type(matches) ->
    fun (infinity) -> infinity;
	(I) when is_integer(I), I > 0 -> I
    end;
mod_opt_type(search) ->
    fun (B) when is_boolean(B) -> B end;
mod_opt_type(search_all_hosts) ->
    fun (B) when is_boolean(B) -> B end;
mod_opt_type(_) ->
    [allow_return_all, db_type, host, iqdisc, matches,
     search, search_all_hosts].
