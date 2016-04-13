%%%----------------------------------------------------------------------
%%% File    : mod_vcard.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Vcard management
%%% Created :  2 Jan 2003 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2016   ProcessOne
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
	 process_local_iq/3, process_sm_iq/3, string2lower/1,
	 remove_user/2, export/1, import/1, import/3,
	 mod_opt_type/1, set_vcard/3, make_vcard_search/4]).

-include("ejabberd.hrl").
-include("logger.hrl").
-include("jlib.hrl").
-include("mod_vcard.hrl").

-define(JUD_MATCHES, 30).

-define(PROCNAME, ejabberd_mod_vcard).

-callback init(binary(), gen_mod:opts()) -> any().
-callback import(binary(), #vcard{} | #vcard_search{}) -> ok | pass.
-callback get_vcard(binary(), binary()) -> [xmlel()] | error.
-callback set_vcard(binary(), binary(),
		    xmlel(), #vcard_search{}) -> {atomic, any()}.
-callback search(binary(), [{binary(), [binary()]}], boolean(),
		 infinity | pos_integer()) -> [binary()].
-callback remove_user(binary(), binary()) -> {atomic, any()}.

start(Host, Opts) ->
    Mod = gen_mod:db_mod(Host, Opts, ?MODULE),
    Mod:init(Host, Opts),
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
	  ejabberd_router:register_route(Host, ServerHost),
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
	  Txt = <<"Value 'set' of 'type' attribute is not allowed">>,
	  IQ#iq{type = error, sub_el = [SubEl, ?ERRT_NOT_ALLOWED(Lang, Txt)]};
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
						"\nCopyright (c) 2002-2016 ProcessOne">>}]},
				 #xmlel{name = <<"BDAY">>, attrs = [],
					children =
					    [{xmlcdata, <<"2002-11-16">>}]}]}]}
    end.

process_sm_iq(From, To,
	      #iq{type = Type, lang = Lang, sub_el = SubEl} = IQ) ->
    case Type of
      set ->
	  #jid{user = User, lserver = LServer} = From,
	  case lists:member(LServer, ?MYHOSTS) of
	    true ->
		set_vcard(User, LServer, SubEl),
		IQ#iq{type = result, sub_el = []};
	    false ->
		Txt = <<"The query is only allowed from local users">>,
		IQ#iq{type = error, sub_el = [SubEl, ?ERRT_NOT_ALLOWED(Lang, Txt)]}
	  end;
      get ->
	  #jid{luser = LUser, lserver = LServer} = To,
	  case get_vcard(LUser, LServer) of
	    error ->
		Txt = <<"Database failure">>,
		IQ#iq{type = error,
		      sub_el = [SubEl, ?ERRT_INTERNAL_SERVER_ERROR(Lang, Txt)]};
	    [] ->
		IQ#iq{type = result,
		      sub_el = [#xmlel{name = <<"vCard">>,
			        attrs = [{<<"xmlns">>, ?NS_VCARD}],
			        children = []}]};
	    Els -> IQ#iq{type = result, sub_el = Els}
	  end
    end.

get_vcard(LUser, LServer) ->
    Mod = gen_mod:db_mod(LServer, ?MODULE),
    Mod:get_vcard(LUser, LServer).

make_vcard_search(User, LUser, LServer, VCARD) ->
    FN = fxml:get_path_s(VCARD, [{elem, <<"FN">>}, cdata]),
    Family = fxml:get_path_s(VCARD,
			    [{elem, <<"N">>}, {elem, <<"FAMILY">>}, cdata]),
    Given = fxml:get_path_s(VCARD,
			   [{elem, <<"N">>}, {elem, <<"GIVEN">>}, cdata]),
    Middle = fxml:get_path_s(VCARD,
			    [{elem, <<"N">>}, {elem, <<"MIDDLE">>}, cdata]),
    Nickname = fxml:get_path_s(VCARD,
			      [{elem, <<"NICKNAME">>}, cdata]),
    BDay = fxml:get_path_s(VCARD,
			  [{elem, <<"BDAY">>}, cdata]),
    CTRY = fxml:get_path_s(VCARD,
			  [{elem, <<"ADR">>}, {elem, <<"CTRY">>}, cdata]),
    Locality = fxml:get_path_s(VCARD,
			      [{elem, <<"ADR">>}, {elem, <<"LOCALITY">>},
			       cdata]),
    EMail1 = fxml:get_path_s(VCARD,
			    [{elem, <<"EMAIL">>}, {elem, <<"USERID">>}, cdata]),
    EMail2 = fxml:get_path_s(VCARD,
			    [{elem, <<"EMAIL">>}, cdata]),
    OrgName = fxml:get_path_s(VCARD,
			     [{elem, <<"ORG">>}, {elem, <<"ORGNAME">>}, cdata]),
    OrgUnit = fxml:get_path_s(VCARD,
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
    US = {LUser, LServer},
    #vcard_search{us = US,
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
		  lorgunit = LOrgUnit}.

set_vcard(User, LServer, VCARD) ->
    case jid:nodeprep(User) of
	error ->
	    {error, badarg};
	LUser ->
	    VCardSearch = make_vcard_search(User, LUser, LServer, VCARD),
	    Mod = gen_mod:db_mod(LServer, ?MODULE),
	    Mod:set_vcard(LUser, LServer, VCARD, VCardSearch),
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
			     Txt = <<"Data form not found">>,
			     Err = jlib:make_error_reply(
				     Packet, ?ERRT_BAD_REQUEST(Lang, Txt)),
			     ejabberd_router:route(To, From, Err);
			 _ ->
			     XData = jlib:parse_xdata_submit(XDataEl),
			     case XData of
			       invalid ->
				   Txt = <<"Incorrect data form">>,
				   Err = jlib:make_error_reply(
					   Packet, ?ERRT_BAD_REQUEST(Lang, Txt)),
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
		       Txt = <<"Value 'set' of 'type' attribute is not allowed">>,
		       Err = jlib:make_error_reply(Packet, ?ERRT_NOT_ALLOWED(Lang, Txt)),
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
	     #iq{type = Type, lang = Lang, xmlns = ?NS_DISCO_ITEMS} ->
		 case Type of
		   set ->
		       Txt = <<"Value 'set' of 'type' attribute is not allowed">>,
		       Err = jlib:make_error_reply(Packet, ?ERRT_NOT_ALLOWED(Lang, Txt)),
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
		    "\nCopyright (c) 2003-2016 ProcessOne">>}]}].

find_xdata_el(#xmlel{children = SubEls}) ->
    find_xdata_el1(SubEls).

find_xdata_el1([]) -> false;
find_xdata_el1([#xmlel{name = Name, attrs = Attrs,
		       children = SubEls}
		| Els]) ->
    case fxml:get_attr_s(<<"xmlns">>, Attrs) of
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
    Mod = gen_mod:db_mod(LServer, ?MODULE),
    AllowReturnAll = gen_mod:get_module_opt(LServer, ?MODULE, allow_return_all,
                                            fun(B) when is_boolean(B) -> B end,
                                            false),
    MaxMatch = gen_mod:get_module_opt(LServer, ?MODULE, matches,
				      fun(infinity) -> infinity;
					 (I) when is_integer(I),
						  I>0 ->
					      I
				      end, ?JUD_MATCHES),
    Mod:search(LServer, Data, AllowReturnAll, MaxMatch).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
remove_user(User, Server) ->
    LUser = jid:nodeprep(User),
    LServer = jid:nameprep(Server),
    Mod = gen_mod:db_mod(LServer, ?MODULE),
    Mod:remove_user(LUser, LServer).

export(LServer) ->
    Mod = gen_mod:db_mod(LServer, ?MODULE),
    Mod:export(LServer).

import(LServer) ->
    Mod = gen_mod:db_mod(LServer, ?MODULE),
    Mod:import(LServer).

import(LServer, DBType, VCard) ->
    Mod = gen_mod:db_mod(DBType, ?MODULE),
    Mod:import(LServer, VCard).

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
