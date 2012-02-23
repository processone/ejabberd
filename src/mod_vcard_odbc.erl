%%%----------------------------------------------------------------------
%%% File    : mod_vcard.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : vCard support via ODBC
%%% Created :  2 Jan 2003 by Alexey Shchepin <alexey@process-one.net>
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

-module(mod_vcard_odbc).
-author('alexey@process-one.net').

-behaviour(gen_mod).

-export([start/2, init/3, stop/1,
	 get_sm_features/5,
	 process_local_iq/3,
	 process_sm_iq/3,
	 %reindex_vcards/0,
	 remove_user/2]).

-include("ejabberd.hrl").
-include("jlib.hrl").


-define(JUD_MATCHES, 30).
-define(PROCNAME, ejabberd_mod_vcard).

start(Host, Opts) ->
    ejabberd_hooks:add(remove_user, Host,
		       ?MODULE, remove_user, 50),
    IQDisc = gen_mod:get_opt(iqdisc, Opts, one_queue),
    gen_iq_handler:add_iq_handler(ejabberd_local, Host, ?NS_VCARD,
				  ?MODULE, process_local_iq, IQDisc),
    gen_iq_handler:add_iq_handler(ejabberd_sm, Host, ?NS_VCARD,
				  ?MODULE, process_sm_iq, IQDisc),
    ejabberd_hooks:add(disco_sm_features, Host, ?MODULE, get_sm_features, 50),
    MyHost = gen_mod:get_opt_host(Host, Opts, "vjud.@HOST@"),
    Search = gen_mod:get_opt(search, Opts, true),
    register(gen_mod:get_module_proc(Host, ?PROCNAME),
	     spawn(?MODULE, init, [MyHost, Host, Search])).


init(Host, ServerHost, Search) ->
    case Search of
	false ->
	    loop(Host, ServerHost);
	_ ->
	    ejabberd_router:register_route(Host),
	    loop(Host, ServerHost)
    end.

loop(Host, ServerHost) ->
    receive
	{route, From, To, Packet} ->
	    case catch do_route(ServerHost, From, To, Packet) of
		{'EXIT', Reason} ->
		    ?ERROR_MSG("~p", [Reason]);
		_ ->
		    ok
	    end,
	    loop(Host, ServerHost);
	stop ->
	    ejabberd_router:unregister_route(Host),
	    ok;
	_ ->
	    loop(Host, ServerHost)
    end.

stop(Host) ->
    ejabberd_hooks:delete(remove_user, Host,
			  ?MODULE, remove_user, 50),
    gen_iq_handler:remove_iq_handler(ejabberd_local, Host, ?NS_VCARD),
    gen_iq_handler:remove_iq_handler(ejabberd_sm, Host, ?NS_VCARD),
    ejabberd_hooks:delete(disco_sm_features, Host, ?MODULE, get_sm_features, 50),
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    Proc ! stop,
    {wait, Proc}.

get_sm_features({error, _Error} = Acc, _From, _To, _Node, _Lang) ->
    Acc;

get_sm_features(Acc, _From, _To, Node, _Lang) ->
    case Node of
	[] ->
	    case Acc of
		{result, Features} ->
		    {result, [?NS_VCARD | Features]};
		empty ->
		    {result, [?NS_VCARD]}
	    end;
 	_ ->
	    Acc
     end.

process_local_iq(_From, _To, #iq{type = Type, lang = Lang, sub_el = SubEl} = IQ) ->
    case Type of
	set ->
	    IQ#iq{type = error, sub_el = [SubEl, ?ERR_NOT_ALLOWED]};
	get ->
	    IQ#iq{type = result,
		  sub_el = [{xmlelement, "vCard",
			     [{"xmlns", ?NS_VCARD}],
			     [{xmlelement, "FN", [],
			       [{xmlcdata, "ejabberd"}]},
			      {xmlelement, "URL", [],
			       [{xmlcdata, ?EJABBERD_URI}]},
			      {xmlelement, "DESC", [],
			       [{xmlcdata,
				 translate:translate(
				   Lang,
				   "Erlang Jabber Server") ++
				   "\nCopyright (c) 2002-2012 ProcessOne"}]},
			      {xmlelement, "BDAY", [],
			       [{xmlcdata, "2002-11-16"}]}
			     ]}]}
    end.


process_sm_iq(From, To, #iq{type = Type, sub_el = SubEl} = IQ) ->
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
	    Username = ejabberd_odbc:escape(LUser),
	    case catch odbc_queries:get_vcard(LServer, Username) of
		{selected, ["vcard"], [{SVCARD}]} ->
		    case xml_stream:parse_element(SVCARD) of
			{error, _Reason} ->
			    IQ#iq{type = error,
				  sub_el = [SubEl, ?ERR_SERVICE_UNAVAILABLE]};
			VCARD ->
			    IQ#iq{type = result, sub_el = [VCARD]}
		    end;
		{selected, ["vcard"], []} ->
		    IQ#iq{type = result, sub_el = []};
		_ ->
		    IQ#iq{type = error,
			  sub_el = [SubEl, ?ERR_INTERNAL_SERVER_ERROR]}
	    end
    end.

set_vcard(User, LServer, VCARD) ->
    FN       = xml:get_path_s(VCARD, [{elem, "FN"},                     cdata]),
    Family   = xml:get_path_s(VCARD, [{elem, "N"}, {elem, "FAMILY"},    cdata]),
    Given    = xml:get_path_s(VCARD, [{elem, "N"}, {elem, "GIVEN"},     cdata]),
    Middle   = xml:get_path_s(VCARD, [{elem, "N"}, {elem, "MIDDLE"},    cdata]),
    Nickname = xml:get_path_s(VCARD, [{elem, "NICKNAME"},               cdata]),
    BDay     = xml:get_path_s(VCARD, [{elem, "BDAY"},                   cdata]),
    CTRY     = xml:get_path_s(VCARD, [{elem, "ADR"}, {elem, "CTRY"},    cdata]),
    Locality = xml:get_path_s(VCARD, [{elem, "ADR"}, {elem, "LOCALITY"},cdata]),
    EMail1   = xml:get_path_s(VCARD, [{elem, "EMAIL"}, {elem, "USERID"},cdata]),
    EMail2   = xml:get_path_s(VCARD, [{elem, "EMAIL"},                  cdata]),
    OrgName  = xml:get_path_s(VCARD, [{elem, "ORG"}, {elem, "ORGNAME"}, cdata]),
    OrgUnit  = xml:get_path_s(VCARD, [{elem, "ORG"}, {elem, "ORGUNIT"}, cdata]),
    EMail = case EMail1 of
		"" ->
		    EMail2;
		_ ->
		    EMail1
	    end,

    LUser     = jlib:nodeprep(User),
    LFN       = string2lower(FN),
    LFamily   = string2lower(Family),
    LGiven    = string2lower(Given),
    LMiddle   = string2lower(Middle),
    LNickname = string2lower(Nickname),
    LBDay     = string2lower(BDay),
    LCTRY     = string2lower(CTRY),
    LLocality = string2lower(Locality),
    LEMail    = string2lower(EMail),
    LOrgName  = string2lower(OrgName),
    LOrgUnit  = string2lower(OrgUnit),

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
	    Username = ejabberd_odbc:escape(User),
	    LUsername = ejabberd_odbc:escape(LUser),
	    SVCARD = ejabberd_odbc:escape(
		       xml:element_to_binary(VCARD)),

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

	    odbc_queries:set_vcard(LServer, LUsername, SBDay, SCTRY, SEMail,
				   SFN, SFamily, SGiven, SLBDay, SLCTRY,
				   SLEMail, SLFN, SLFamily, SLGiven,
				   SLLocality, SLMiddle, SLNickname,
				   SLOrgName, SLOrgUnit, SLocality,
				   SMiddle, SNickname, SOrgName,
				   SOrgUnit, SVCARD, Username),

	    ejabberd_hooks:run(vcard_set, LServer, [LUser, LServer, VCARD])
    end.

string2lower(String) ->
    case stringprep:tolower(String) of
	Lower when is_list(Lower) -> Lower;
	error -> string:to_lower(String)
    end.

-define(TLFIELD(Type, Label, Var),
	{xmlelement, "field", [{"type", Type},
			       {"label", translate:translate(Lang, Label)},
			       {"var", Var}], []}).


-define(FORM(JID),
	[{xmlelement, "instructions", [],
	  [{xmlcdata, translate:translate(Lang, "You need an x:data capable client to search")}]},
	 {xmlelement, "x", [{"xmlns", ?NS_XDATA}, {"type", "form"}],
	  [{xmlelement, "title", [],
	    [{xmlcdata, translate:translate(Lang, "Search users in ") ++
	      jlib:jid_to_string(JID)}]},
	   {xmlelement, "instructions", [],
	    [{xmlcdata, translate:translate(Lang, "Fill in the form to search "
					    "for any matching Jabber User "
					    "(Add * to the end of field to "
					    "match substring)")}]},
	   ?TLFIELD("text-single", "User", "user"),
	   ?TLFIELD("text-single", "Full Name", "fn"),
	   ?TLFIELD("text-single", "Name", "first"),
	   ?TLFIELD("text-single", "Middle Name", "middle"),
	   ?TLFIELD("text-single", "Family Name", "last"),
	   ?TLFIELD("text-single", "Nickname", "nick"),
	   ?TLFIELD("text-single", "Birthday", "bday"),
	   ?TLFIELD("text-single", "Country", "ctry"),
	   ?TLFIELD("text-single", "City", "locality"),
	   ?TLFIELD("text-single", "Email", "email"),
	   ?TLFIELD("text-single", "Organization Name", "orgname"),
	   ?TLFIELD("text-single", "Organization Unit", "orgunit")
	  ]}]).

do_route(ServerHost, From, To, Packet) ->
    #jid{user = User, resource = Resource} = To,
    if
	(User /= "") or (Resource /= "") ->
	    Err = jlib:make_error_reply(Packet, ?ERR_SERVICE_UNAVAILABLE),
	    ejabberd_router:route(To, From, Err);
	true ->
	    IQ = jlib:iq_query_info(Packet),
	    case IQ of
		#iq{type = Type, xmlns = ?NS_SEARCH, lang = Lang, sub_el = SubEl} ->
		    case Type of
			set ->
			    XDataEl = find_xdata_el(SubEl),
			    case XDataEl of
				false ->
				    Err = jlib:make_error_reply(
					    Packet, ?ERR_BAD_REQUEST),
				    ejabberd_router:route(To, From, Err);
				_ ->
				    XData = jlib:parse_xdata_submit(XDataEl),
				    case XData of
					invalid ->
					    Err = jlib:make_error_reply(
						    Packet,
						    ?ERR_BAD_REQUEST),
					    ejabberd_router:route(To, From,
								  Err);
					_ ->
					    ResIQ =
						IQ#iq{
						  type = result,
						  sub_el =
						  [{xmlelement,
						    "query",
						    [{"xmlns", ?NS_SEARCH}],
						    [{xmlelement, "x",
						      [{"xmlns", ?NS_XDATA},
						       {"type", "result"}],
						      search_result(Lang, To, ServerHost, XData)
						     }]}]},
					    ejabberd_router:route(
					      To, From, jlib:iq_to_xml(ResIQ))
				    end
			    end;
			get ->
			    ResIQ = IQ#iq{type = result,
					  sub_el = [{xmlelement,
						     "query",
						     [{"xmlns", ?NS_SEARCH}],
						     ?FORM(To)
						    }]},
			    ejabberd_router:route(To,
						  From,
						  jlib:iq_to_xml(ResIQ))
		    end;
		#iq{type = Type, xmlns = ?NS_DISCO_INFO, lang = Lang} ->
		    case Type of
			set ->
			    Err = jlib:make_error_reply(
				    Packet, ?ERR_NOT_ALLOWED),
			    ejabberd_router:route(To, From, Err);
			get ->
			    Info = ejabberd_hooks:run_fold(
				     disco_info, ServerHost, [],
				     [ServerHost, ?MODULE, "", ""]),
			    ResIQ =
				IQ#iq{type = result,
				      sub_el = [{xmlelement,
						 "query",
						 [{"xmlns", ?NS_DISCO_INFO}],
						 [{xmlelement, "identity",
						   [{"category", "directory"},
						    {"type", "user"},
						    {"name",
						     translate:translate(Lang, "vCard User Search")}],
						   []},
						  {xmlelement, "feature",
						   [{"var", ?NS_SEARCH}], []},
						  {xmlelement, "feature",
						   [{"var", ?NS_VCARD}], []}
						 ] ++ Info
						}]},
			    ejabberd_router:route(To,
						  From,
						  jlib:iq_to_xml(ResIQ))
		    end;
		#iq{type = Type, xmlns = ?NS_DISCO_ITEMS} ->
		    case Type of
			set ->
			    Err = jlib:make_error_reply(
				    Packet, ?ERR_NOT_ALLOWED),
			    ejabberd_router:route(To, From, Err);
			get ->
			    ResIQ =
				IQ#iq{type = result,
				      sub_el = [{xmlelement,
						 "query",
						 [{"xmlns", ?NS_DISCO_ITEMS}],
						 []}]},
			    ejabberd_router:route(To,
						  From,
						  jlib:iq_to_xml(ResIQ))
		    end;
		#iq{type = get, xmlns = ?NS_VCARD, lang = Lang} ->
		    ResIQ =
			IQ#iq{type = result,
			      sub_el = [{xmlelement,
					 "vCard",
					 [{"xmlns", ?NS_VCARD}],
					 iq_get_vcard(Lang)}]},
		    ejabberd_router:route(To,
					  From,
					  jlib:iq_to_xml(ResIQ));
		_ ->
		    Err = jlib:make_error_reply(Packet,
						?ERR_SERVICE_UNAVAILABLE),
		    ejabberd_router:route(To, From, Err)
	    end
    end.

iq_get_vcard(Lang) ->
    [{xmlelement, "FN", [],
      [{xmlcdata, "ejabberd/mod_vcard"}]},
     {xmlelement, "URL", [],
      [{xmlcdata, ?EJABBERD_URI}]},
     {xmlelement, "DESC", [],
      [{xmlcdata, translate:translate(
		    Lang,
		    "ejabberd vCard module") ++
		    "\nCopyright (c) 2003-2012 ProcessOne"}]}].

find_xdata_el({xmlelement, _Name, _Attrs, SubEls}) ->
    find_xdata_el1(SubEls).

find_xdata_el1([]) ->
    false;
find_xdata_el1([{xmlelement, Name, Attrs, SubEls} | Els]) ->
    case xml:get_attr_s("xmlns", Attrs) of
	?NS_XDATA ->
	    {xmlelement, Name, Attrs, SubEls};
	_ ->
	    find_xdata_el1(Els)
    end;
find_xdata_el1([_ | Els]) ->
    find_xdata_el1(Els).

-define(LFIELD(Label, Var),
	{xmlelement, "field", [{"label", translate:translate(Lang, Label)},
			       {"var", Var}], []}).

search_result(Lang, JID, ServerHost, Data) ->
    [{xmlelement, "title", [],
      [{xmlcdata, translate:translate(Lang, "Search Results for ") ++
	jlib:jid_to_string(JID)}]},
     {xmlelement, "reported", [],
      [?TLFIELD("text-single", "Jabber ID", "jid"),
       ?TLFIELD("text-single", "Full Name", "fn"),
       ?TLFIELD("text-single", "Name", "first"),
       ?TLFIELD("text-single", "Middle Name", "middle"),
       ?TLFIELD("text-single", "Family Name", "last"),
       ?TLFIELD("text-single", "Nickname", "nick"),
       ?TLFIELD("text-single", "Birthday", "bday"),
       ?TLFIELD("text-single", "Country", "ctry"),
       ?TLFIELD("text-single", "City", "locality"),
       ?TLFIELD("text-single", "Email", "email"),
       ?TLFIELD("text-single", "Organization Name", "orgname"),
       ?TLFIELD("text-single", "Organization Unit", "orgunit")
      ]}] ++ lists:map(fun(R) -> record_to_item(ServerHost, R) end,
		       search(ServerHost, Data)).

-define(FIELD(Var, Val),
	{xmlelement, "field", [{"var", Var}],
	 [{xmlelement, "value", [],
	   [{xmlcdata, Val}]}]}).


record_to_item(LServer, {Username, FN, Family, Given, Middle,
			 Nickname, BDay, CTRY, Locality,
			 EMail, OrgName, OrgUnit}) ->
    {xmlelement, "item", [],
     [
       ?FIELD("jid",      Username ++ "@" ++ LServer),
       ?FIELD("fn",       FN),
       ?FIELD("last",     Family),
       ?FIELD("first",    Given),
       ?FIELD("middle",   Middle),
       ?FIELD("nick",     Nickname),
       ?FIELD("bday",     BDay),
       ?FIELD("ctry",     CTRY),
       ?FIELD("locality", Locality),
       ?FIELD("email",    EMail),
       ?FIELD("orgname",  OrgName),
       ?FIELD("orgunit",  OrgUnit)
      ]
     }.


search(LServer, Data) ->
    MatchSpec = make_matchspec(LServer, Data),
    AllowReturnAll = gen_mod:get_module_opt(LServer, ?MODULE,
					    allow_return_all, false),
    if
	(MatchSpec == "") and (not AllowReturnAll) ->
	    [];
	true ->
	    Limit = case gen_mod:get_module_opt(LServer, ?MODULE,
						matches, ?JUD_MATCHES) of
			infinity ->
			    "";
			Val when is_integer(Val) and (Val > 0) ->
			    [" LIMIT ", integer_to_list(Val)];
			Val ->
			    ?ERROR_MSG("Illegal option value ~p. "
				       "Default value ~p substituted.",
				       [{matches, Val}, ?JUD_MATCHES]),
			    [" LIMIT ", integer_to_list(?JUD_MATCHES)]
		    end,
	    case catch ejabberd_odbc:sql_query(
			 LServer,
			 ["select username, fn, family, given, middle, "
			  "       nickname, bday, ctry, locality, "
			  "       email, orgname, orgunit from vcard_search ",
			  MatchSpec, Limit, ";"]) of
		{selected, ["username", "fn", "family", "given", "middle",
			    "nickname", "bday", "ctry", "locality",
			    "email", "orgname", "orgunit"],
		 Rs} when is_list(Rs) ->
		    Rs;
		Error ->
		    ?ERROR_MSG("~p", [Error]),
		    []
	    end
    end.


make_matchspec(LServer, Data) ->
    filter_fields(Data, "", LServer).

filter_fields([], Match, _LServer) ->
    case Match of
	"" ->
	    "";
	_ ->
	    [" where ", Match]
    end;
filter_fields([{SVar, [Val]} | Ds], Match, LServer)
  when is_list(Val) and (Val /= "") ->
    LVal = string2lower(Val),
    NewMatch = case SVar of
                   "user"     -> make_val(Match, "lusername", LVal);
                   "fn"       -> make_val(Match, "lfn",       LVal);
                   "last"     -> make_val(Match, "lfamily",   LVal);
                   "first"    -> make_val(Match, "lgiven",    LVal);
                   "middle"   -> make_val(Match, "lmiddle",   LVal);
                   "nick"     -> make_val(Match, "lnickname", LVal);
                   "bday"     -> make_val(Match, "lbday",     LVal);
                   "ctry"     -> make_val(Match, "lctry",     LVal);
                   "locality" -> make_val(Match, "llocality", LVal);
                   "email"    -> make_val(Match, "lemail",    LVal);
                   "orgname"  -> make_val(Match, "lorgname",  LVal);
                   "orgunit"  -> make_val(Match, "lorgunit",  LVal);
		   _          -> Match
	       end,
    filter_fields(Ds, NewMatch, LServer);
filter_fields([_ | Ds], Match, LServer) ->
    filter_fields(Ds, Match, LServer).

make_val(Match, Field, Val) ->
    Condition =
	case lists:suffix("*", Val) of
	    true ->
		Val1 = lists:sublist(Val, length(Val) - 1),
		SVal = ejabberd_odbc:escape_like(Val1) ++ "%",
		[Field, " LIKE '", SVal, "'"];
	    _ ->
		SVal = ejabberd_odbc:escape(Val),
		[Field, " = '", SVal, "'"]
	end,
    case Match of
	"" ->
	    Condition;
	_ ->
	    [Match, " and ", Condition]
    end.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%set_vcard_t(R, _) ->
%    US = R#vcard.us,
%    User  = US,
%    VCARD = R#vcard.vcard,
%
%    FN       = xml:get_path_s(VCARD, [{elem, "FN"},                     cdata]),
%    Family   = xml:get_path_s(VCARD, [{elem, "N"}, {elem, "FAMILY"},    cdata]),
%    Given    = xml:get_path_s(VCARD, [{elem, "N"}, {elem, "GIVEN"},     cdata]),
%    Middle   = xml:get_path_s(VCARD, [{elem, "N"}, {elem, "MIDDLE"},    cdata]),
%    Nickname = xml:get_path_s(VCARD, [{elem, "NICKNAME"},               cdata]),
%    BDay     = xml:get_path_s(VCARD, [{elem, "BDAY"},                   cdata]),
%    CTRY     = xml:get_path_s(VCARD, [{elem, "ADR"}, {elem, "CTRY"},    cdata]),
%    Locality = xml:get_path_s(VCARD, [{elem, "ADR"}, {elem, "LOCALITY"},cdata]),
%    EMail    = xml:get_path_s(VCARD, [{elem, "EMAIL"},                  cdata]),
%    OrgName  = xml:get_path_s(VCARD, [{elem, "ORG"}, {elem, "ORGNAME"}, cdata]),
%    OrgUnit  = xml:get_path_s(VCARD, [{elem, "ORG"}, {elem, "ORGUNIT"}, cdata]),
%
%    {LUser, _LServer} = US,
%    LFN       = stringprep:tolower(FN),
%    LFamily   = stringprep:tolower(Family),
%    LGiven    = stringprep:tolower(Given),
%    LMiddle   = stringprep:tolower(Middle),
%    LNickname = stringprep:tolower(Nickname),
%    LBDay     = stringprep:tolower(BDay),
%    LCTRY     = stringprep:tolower(CTRY),
%    LLocality = stringprep:tolower(Locality),
%    LEMail    = stringprep:tolower(EMail),
%    LOrgName  = stringprep:tolower(OrgName),
%    LOrgUnit  = stringprep:tolower(OrgUnit),
%
%    if
%	(LUser     == error) or
%	(LFN       == error) or
%	(LFamily   == error) or
%	(LGiven    == error) or
%	(LMiddle   == error) or
%	(LNickname == error) or
%	(LBDay     == error) or
%	(LCTRY     == error) or
%	(LLocality == error) or
%	(LEMail    == error) or
%	(LOrgName  == error) or
%	(LOrgUnit  == error) ->
%	    {error, badarg};
%	true ->
%	    mnesia:write(
%	      #vcard_search{us        = US,
%			    user      = User,     luser      = LUser,
%			    fn        = FN,       lfn        = LFN,
%			    family    = Family,   lfamily    = LFamily,
%			    given     = Given,    lgiven     = LGiven,
%			    middle    = Middle,   lmiddle    = LMiddle,
%			    nickname  = Nickname, lnickname  = LNickname,
%			    bday      = BDay,     lbday      = LBDay,
%			    ctry      = CTRY,     lctry      = LCTRY,
%			    locality  = Locality, llocality  = LLocality,
%			    email     = EMail,    lemail     = LEMail,
%			    orgname   = OrgName,  lorgname   = LOrgName,
%			    orgunit   = OrgUnit,  lorgunit   = LOrgUnit
%			   })
%    end.
%
%
%reindex_vcards() ->
%    F = fun() ->
%		mnesia:foldl(fun set_vcard_t/2, [], vcard)
%	end,
%    mnesia:transaction(F).


remove_user(User, Server) ->
    LUser = jlib:nodeprep(User),
    LServer = jlib:nameprep(Server),
    Username = ejabberd_odbc:escape(LUser),
    ejabberd_odbc:sql_transaction(
      LServer,
      [["delete from vcard where username='", Username, "';"],
       ["delete from vcard_search where lusername='", Username, "';"]]).
