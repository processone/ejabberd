%%%----------------------------------------------------------------------
%%% File    : mod_vcard.erl
%%% Author  : Alexey Shchepin <alexey@sevcom.net>
%%% Purpose : 
%%% Created :  2 Jan 2003 by Alexey Shchepin <alexey@sevcom.net>
%%% Id      : $Id$
%%%----------------------------------------------------------------------

-module(mod_vcard).
-author('alexey@sevcom.net').
-vsn('$Revision$ ').

-behaviour(gen_mod).

-export([start/1, init/1,
	 process_local_iq/3,
	 process_sm_iq/3,
	 reindex_vcards/0,
	 remove_user/1]).

-include("ejabberd.hrl").
-include("jlib.hrl").


-record(vcard_search, {user,     luser,
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
-record(vcard, {user, vcard}).


start(Opts) ->
    mnesia:create_table(vcard, [{disc_only_copies, [node()]},
				{attributes, record_info(fields, vcard)}]),
    mnesia:create_table(vcard_search,
			[{disc_copies, [node()]},
			 {attributes, record_info(fields, vcard_search)}]),
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

    IQDisc = gen_mod:get_opt(iqdisc, Opts, one_queue),
    gen_iq_handler:add_iq_handler(ejabberd_local, ?NS_VCARD,
				  ?MODULE, process_local_iq, IQDisc),
    gen_iq_handler:add_iq_handler(ejabberd_sm, ?NS_VCARD,
				  ?MODULE, process_sm_iq, IQDisc),
    Host = gen_mod:get_opt(host, Opts, "vjud." ++ ?MYNAME),
    spawn(?MODULE, init, [Host]).


init(Host) ->
    ejabberd_router:register_local_route(Host),
    loop().

loop() ->
    receive
	{route, From, To, Packet} ->
	    do_route(From, To, Packet),
	    loop();
	_ ->
	    loop()
    end.


process_local_iq(From, To, {iq, ID, Type, XMLNS, SubEl}) ->
    case Type of
	set ->
	    {User, Server, _} = From,
	    LUser = jlib:tolower(User),
	    LServer = jlib:tolower(Server),
	    case ?MYNAME of
		LServer ->
		    set_vcard(User, SubEl),
		    {iq, ID, result, XMLNS, []};
		_ ->
		    {iq, ID, error, XMLNS,
		     [SubEl, {xmlelement, "error",
			      [{"code", "405"}],
			      [{xmlcdata, "Not Allowed"}]}]}
	    end;
	get ->
	    {iq, ID, result, XMLNS,
	     [{xmlelement, "vCard",
	       [{"xmlns", ?NS_VCARD}],
	       [{xmlelement, "FN", [],
		 [{xmlcdata, "ejabberd"}]},
		{xmlelement, "URL", [],
		 [{xmlcdata,
		   "http://ejabberd.jabberstudio.org/"}]},
		{xmlelement, "DESC", [],
		 [{xmlcdata, "Erlang Jabber Server\n"
		   "Copyright (c) 2002, 2003 Alexey Shchepin"}]},
		{xmlelement, "BDAY", [],
		 [{xmlcdata, "2002-11-16"}]}
	       ]}]}
    end.


process_sm_iq(From, To, {iq, ID, Type, XMLNS, SubEl}) ->
    case Type of
	set ->
	    {iq, ID, error, XMLNS, [SubEl, {xmlelement, "error",
					    [{"code", "405"}],
					    [{xmlcdata, "Not Allowed"}]}]};
	get ->
	    {User, _, _} = To,
	    LUser = jlib:tolower(User),
	    F = fun() ->
			mnesia:read({vcard, LUser})
		end,
	    Els = case mnesia:transaction(F) of
		      {atomic, Rs} ->
			  lists:map(fun(R) ->
					    R#vcard.vcard
				    end, Rs);
		      {aborted, Reason} ->
			  []
		  end,
	    {iq, ID, result, XMLNS, Els}
    end.

set_vcard(User, VCARD) ->
    FN       = xml:get_path_s(VCARD, [{elem, "FN"},                     cdata]),
    Family   = xml:get_path_s(VCARD, [{elem, "N"}, {elem, "FAMILY"},    cdata]),
    Given    = xml:get_path_s(VCARD, [{elem, "N"}, {elem, "GIVEN"},     cdata]),
    Middle   = xml:get_path_s(VCARD, [{elem, "N"}, {elem, "MIDDLE"},    cdata]),
    Nickname = xml:get_path_s(VCARD, [{elem, "NICKNAME"},               cdata]),
    BDay     = xml:get_path_s(VCARD, [{elem, "BDAY"},                   cdata]),
    CTRY     = xml:get_path_s(VCARD, [{elem, "ADR"}, {elem, "CTRY"},    cdata]),
    Locality = xml:get_path_s(VCARD, [{elem, "ADR"}, {elem, "LOCALITY"},cdata]),
    EMail    = xml:get_path_s(VCARD, [{elem, "EMAIL"},                  cdata]),
    OrgName  = xml:get_path_s(VCARD, [{elem, "ORG"}, {elem, "ORGNAME"}, cdata]),
    OrgUnit  = xml:get_path_s(VCARD, [{elem, "ORG"}, {elem, "ORGUNIT"}, cdata]),

    LUser     = jlib:tolower(User),
    LFN       = jlib:tolower(FN),
    LFamily   = jlib:tolower(Family),
    LGiven    = jlib:tolower(Given),
    LMiddle   = jlib:tolower(Middle),
    LNickname = jlib:tolower(Nickname),
    LBDay     = jlib:tolower(BDay),
    LCTRY     = jlib:tolower(CTRY),
    LLocality = jlib:tolower(Locality),
    LEMail    = jlib:tolower(EMail),
    LOrgName  = jlib:tolower(OrgName),
    LOrgUnit  = jlib:tolower(OrgUnit),

    F = fun() ->
		mnesia:write(#vcard{user = LUser, vcard = VCARD}),
		mnesia:write(
		  #vcard_search{user      = User,     luser      = LUser,     
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
	end,
    mnesia:transaction(F).

-define(TLFIELD(Type, Label, Var),
	{xmlelement, "field", [{"type", Type},
			       {"label", translate:translate(Lang, Label)},
			       {"var", Var}], []}).


-define(FORM,
	[{xmlelement, "instructions", [],
	  [{xmlcdata, translate:translate(Lang, "You need a x:data capable client to search")}]},
	 {xmlelement, "x", [{"xmlns", ?NS_XDATA}, {"type", "form"}],
	  [{xmlelement, "title", [],
	    [{xmlcdata, translate:translate(Lang, "Users Search")}]},
	   {xmlelement, "instructions", [],
	    [{xmlcdata, translate:translate(Lang, "Fill in fields to search "
					    "for any matching Jabber User")}]},
	   ?TLFIELD("text-single", "User", "user"),
	   ?TLFIELD("text-single", "Full Name", "fn"),
	   ?TLFIELD("text-single", "Name", "given"),
	   ?TLFIELD("text-single", "Middle Name", "middle"),
	   ?TLFIELD("text-single", "Family Name", "family"),
	   ?TLFIELD("text-single", "Nickname", "nickname"),
	   ?TLFIELD("text-single", "Birthday", "bday"),
	   ?TLFIELD("text-single", "Country", "ctry"),
	   ?TLFIELD("text-single", "City", "locality"),
	   ?TLFIELD("text-single", "email", "email"),
	   ?TLFIELD("text-single", "Organization Name", "orgname"),
	   ?TLFIELD("text-single", "Organization Unit", "orgunit")
	  ]}]).




do_route(From, To, Packet) ->
    {User, Server, Resource} = To,
    if
	(User /= "") or (Resource /= "") ->
	    Err = jlib:make_error_reply(Packet, "503", "Service Unavailable"),
	    ejabberd_router ! {route, To, From, Err};
	true ->
	    IQ = jlib:iq_query_info(Packet),
	    case IQ of
		{iq, ID, Type, ?NS_SEARCH, SubEl} ->
		    Lang = xml:get_tag_attr_s("xml:lang", SubEl),
		    case Type of
			set ->
			    XDataEl = find_xdata_el(SubEl),
			    case XDataEl of
				false ->
				    Err = jlib:make_error_reply(
					    Packet, "400", "Bad Request"),
				    ejabberd_router:route(To, From, Err);
				_ ->
				    XData = jlib:parse_xdata_submit(XDataEl),
				    case XData of
					invalid ->
					    Err = jlib:make_error_reply(
						    Packet,
						    "400", "Bad Request"),
					    ejabberd_router:route(To, From,
								  Err);
					_ ->
					    ResIQ =
						{iq, ID, result, ?NS_SEARCH,
						 [{xmlelement,
						   "query",
						   [{"xmlns", ?NS_SEARCH}],
						   [{xmlelement, "x",
						     [{"xmlns", ?NS_XDATA},
						      {"type", "result"}],
						     search_result(Lang, XData)
						    }]}]},
					    ejabberd_router:route(
					      To, From, jlib:iq_to_xml(ResIQ))
				    end
			    end;
			get ->
			    ResIQ = {iq, ID, result, ?NS_SEARCH,
				     [{xmlelement,
				       "query",
				       [{"xmlns", ?NS_SEARCH}],
				       ?FORM
				      }]},
			    ejabberd_router:route(To,
						  From,
						  jlib:iq_to_xml(ResIQ))
		    end;
		{iq, ID, Type, ?NS_DISCO_INFO, SubEl} ->
		    case Type of
			set ->
			    Err = jlib:make_error_reply(
				    Packet, "405", "Not Allowed"),
			    ejabberd_router:route(To, From, Err);
			get ->
			    ResIQ = {iq, ID, result, ?NS_DISCO_INFO,
				     [{xmlelement,
				       "query",
				       [{"xmlns", ?NS_DISCO_INFO}],
				       [{xmlelement, "identity",
					 [{"category", "directory"},
					  {"type", "user"},
					  {"name", "vCard User Search"}], []},
					{xmlelement, "feature",
					 [{"var", ?NS_SEARCH}], []}
				       ]
				      }]},
			    ejabberd_router:route(To,
						  From,
						  jlib:iq_to_xml(ResIQ))
		    end;
		{iq, ID, Type, ?NS_DISCO_ITEMS, SubEl} ->
		    case Type of
			set ->
			    Err = jlib:make_error_reply(
				    Packet, "405", "Not Allowed"),
			    ejabberd_router:route(To, From, Err);
			get ->
			    ResIQ = {iq, ID, result, ?NS_DISCO_INFO,
				     [{xmlelement,
				       "query",
				       [{"xmlns", ?NS_DISCO_INFO}], []}]},
			    ejabberd_router:route(To,
						  From,
						  jlib:iq_to_xml(ResIQ))
		    end;
		_ ->
		    Err = jlib:make_error_reply(Packet,
						"503", "Service Unavailable"),
		    ejabberd_router:route(To, From, Err)
	    end
    end.

find_xdata_el({xmlelement, Name, Attrs, SubEls}) ->
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

search_result(Lang, Data) ->
    [{xmlelement, "title", [],
      [{xmlcdata, translate:translate(Lang, "Users Search Results")}]},
     {xmlelement, "reported", [],
      [?LFIELD("JID", "jid"),
       ?LFIELD("Full Name", "fn"),
       ?LFIELD("Name", "given"),
       ?LFIELD("Middle Name", "middle"),
       ?LFIELD("Family Name", "family"),
       ?LFIELD("Nickname", "nickname"),
       ?LFIELD("Birthday", "bday"),
       ?LFIELD("Country", "ctry"),
       ?LFIELD("City", "locality"),
       ?LFIELD("email", "email"),
       ?LFIELD("Organization Name", "orgname"),
       ?LFIELD("Organization Unit", "orgunit")
      ]}] ++ lists:map(fun record_to_item/1, search(Data)).

-define(FIELD(Var, Val),
	{xmlelement, "field", [{"var", Var}],
	 [{xmlelement, "value", [],
	   [{xmlcdata, Val}]}]}).

record_to_item(R) ->
     {xmlelement, "item", [],
      [
       ?FIELD("jid",      R#vcard_search.user ++ "@" ++ ?MYNAME),
       ?FIELD("fn",       R#vcard_search.fn),
       ?FIELD("family",   R#vcard_search.family),
       ?FIELD("given",    R#vcard_search.given),
       ?FIELD("middle",   R#vcard_search.middle),
       ?FIELD("nickname", R#vcard_search.nickname),
       ?FIELD("bday",     R#vcard_search.bday),
       ?FIELD("ctry",     R#vcard_search.ctry),
       ?FIELD("locality", R#vcard_search.locality),
       ?FIELD("email",    R#vcard_search.email),
       ?FIELD("orgname",  R#vcard_search.orgname),
       ?FIELD("orgunit",  R#vcard_search.orgunit)
      ]
     }.


search(Data) ->
    MatchSpec = make_matchspec(Data),
    case catch mnesia:dirty_select(vcard_search, [{MatchSpec, [], ['$_']}]) of
	{'EXIT', Reason} ->
	    ?ERROR_MSG("~p", [Reason]),
	    [];
	Rs ->
	    Rs
    end.

% TODO: remove
%    F = fun() ->
%		mnesia:select(vcard_search, [{MatchSpec, [], ['$_']}])
%	end,
%    case mnesia:transaction(F) of
%	{atomic, Rs} ->
%	    Rs;
%	_ ->
%	    []
%    end.


make_matchspec(Data) ->
    GlobMatch = #vcard_search{_ = '_'},
    Match = filter_fields(Data, GlobMatch),
    Match.

filter_fields([], Match) ->
    Match;
filter_fields([{SVar, [Val]} | Ds], Match)
  when is_list(Val) and (Val /= "") ->
    LVal = jlib:tolower(Val),
    NewMatch = case SVar of
                   "user"     -> Match#vcard_search{luser     = LVal};
                   "fn"       -> Match#vcard_search{lfn       = LVal};
                   "family"   -> Match#vcard_search{lfamily   = LVal};
                   "given"    -> Match#vcard_search{lgiven    = LVal};
                   "middle"   -> Match#vcard_search{lmiddle   = LVal};
                   "nickname" -> Match#vcard_search{lnickname = LVal};
                   "bday"     -> Match#vcard_search{lbday     = LVal};
                   "ctry"     -> Match#vcard_search{lctry     = LVal};
                   "locality" -> Match#vcard_search{llocality = LVal};
                   "email"    -> Match#vcard_search{lemail    = LVal};
                   "orgname"  -> Match#vcard_search{lorgname  = LVal};
                   "orgunit"  -> Match#vcard_search{lorgunit  = LVal};
		   _          -> Match
	       end,
    filter_fields(Ds, NewMatch);
filter_fields([_ | Ds], Match) ->
    filter_fields(Ds, Match).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

set_vcard_t(R, _) ->
    User  = R#vcard.user,
    VCARD = R#vcard.vcard,

    FN       = xml:get_path_s(VCARD, [{elem, "FN"},                     cdata]),
    Family   = xml:get_path_s(VCARD, [{elem, "N"}, {elem, "FAMILY"},    cdata]),
    Given    = xml:get_path_s(VCARD, [{elem, "N"}, {elem, "GIVEN"},     cdata]),
    Middle   = xml:get_path_s(VCARD, [{elem, "N"}, {elem, "MIDDLE"},    cdata]),
    Nickname = xml:get_path_s(VCARD, [{elem, "NICKNAME"},               cdata]),
    BDay     = xml:get_path_s(VCARD, [{elem, "BDAY"},                   cdata]),
    CTRY     = xml:get_path_s(VCARD, [{elem, "ADR"}, {elem, "CTRY"},    cdata]),
    Locality = xml:get_path_s(VCARD, [{elem, "ADR"}, {elem, "LOCALITY"},cdata]),
    EMail    = xml:get_path_s(VCARD, [{elem, "EMAIL"},                  cdata]),
    OrgName  = xml:get_path_s(VCARD, [{elem, "ORG"}, {elem, "ORGNAME"}, cdata]),
    OrgUnit  = xml:get_path_s(VCARD, [{elem, "ORG"}, {elem, "ORGUNIT"}, cdata]),

    LUser     = jlib:tolower(User),
    LFN       = jlib:tolower(FN),
    LFamily   = jlib:tolower(Family),
    LGiven    = jlib:tolower(Given),
    LMiddle   = jlib:tolower(Middle),
    LNickname = jlib:tolower(Nickname),
    LBDay     = jlib:tolower(BDay),
    LCTRY     = jlib:tolower(CTRY),
    LLocality = jlib:tolower(Locality),
    LEMail    = jlib:tolower(EMail),
    LOrgName  = jlib:tolower(OrgName),
    LOrgUnit  = jlib:tolower(OrgUnit),

    mnesia:write(
      #vcard_search{user      = User,     luser      = LUser,     
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
		   }).


reindex_vcards() ->
    F = fun() ->
		mnesia:foldl(fun set_vcard_t/2, [], vcard)
	end,
    mnesia:transaction(F).


remove_user(User) ->
    LUser = jlib:tolower(User),
    F = fun() ->
		mnesia:delete({vcard, LUser}),
		lists:foreach(fun(R) ->
				      mnesia:delete_object(R)
			      end,
			      mnesia:index_read(vcard_search,
						LUser,
						#vcard_search.luser))
	end,
    mnesia:transaction(F).

