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

-export([start/0, init/0,
	 process_local_iq/3,
	 process_sm_iq/3]).

-include("ejabberd.hrl").
-include("namespaces.hrl").


-record(vcard_search, {user, fn, family, given, middle, nickname,
		       bday, ctry, locality, email,
		       orgname, orgunit}).
-record(vcard, {user, vcard}).


start() ->
    mnesia:create_table(vcard, [{disc_only_copies, [node()]},
				{attributes, record_info(fields, vcard)}]),
    mnesia:create_table(vcard_search,
			[{disc_copies, [node()]},
			 {attributes, record_info(fields, vcard_search)}]),
    %mnesia:add_table_index(vcard_search, fn),
    %mnesia:add_table_index(vcard_search, n),
    %mnesia:add_table_index(vcard_search, nickname),
    %mnesia:add_table_index(vcard_search, bday),
    %mnesia:add_table_index(vcard_search, ctry),
    %mnesia:add_table_index(vcard_search, locality),
    %mnesia:add_table_index(vcard_search, email),
    %mnesia:add_table_index(vcard_search, orgname),
    %mnesia:add_table_index(vcard_search, orgunit),


    ejabberd_local:register_iq_handler(?NS_VCARD,
				       ?MODULE, process_local_iq),
    ejabberd_sm:register_iq_handler(?NS_VCARD,
        			    ?MODULE, process_sm_iq),
    spawn(?MODULE, init, []).

init() ->
    ejabberd_router:register_local_route("ejud." ++ ?MYNAME),
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
		    set_vcard(LUser, SubEl),
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
		   "http://www.jabber.ru/projects/ejabberd/"}]},
		{xmlelement, "DESC", [],
		 [{xmlcdata, "Erlang Jabber Server\n"
		   "Copyright (c) 2002, 2003 Alexey Shchepin"}]},
		{xmlelement, "BDAY", [],
		 [{xmlcdata, "20021116"}]}
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


set_vcard(LUser, VCARD) ->
    FN       = xml:get_path_s(VCARD, [{elem, "FN"},                     cdata]),
    Family   = xml:get_path_s(VCARD, [{elem, "N"}, {elem, "FAMILY"},    cdata]),
    Given    = xml:get_path_s(VCARD, [{elem, "N"}, {elem, "GIVEN"},     cdata]),
    Middle   = xml:get_path_s(VCARD, [{elem, "N"}, {elem, "MIDDLE"},    cdata]),
    Nickname = xml:get_path_s(VCARD, [{elem, "NICKNAME"},               cdata]),
    BDay     = xml:get_path_s(VCARD, [{elem, "BDAY"},                   cdata]),
    %ctry     = xml:get_path_s(VCARD, [{elem, "CTRY"}, cdata]),
    %locality = xml:get_path_s(VCARD, [{elem, "FN"}, cdata]),
    EMail    = xml:get_path_s(VCARD, [{elem, "EMAIL"},                  cdata]),
    OrgName  = xml:get_path_s(VCARD, [{elem, "ORG"}, {elem, "ORGNAME"}, cdata]),
    OrgUnit  = xml:get_path_s(VCARD, [{elem, "ORG"}, {elem, "ORGUNIT"}, cdata]),
    F = fun() ->
		mnesia:write(#vcard{user = LUser, vcard = VCARD}),
		mnesia:write(#vcard_search{user = LUser,
					   fn = FN,
					   family = Family,
					   given = Given,
					   middle = Middle,
					   nickname = Nickname,
					   bday = BDay,
					   %ctry = CTRY,
					   %locality = Locality,
					   email = EMail,
					   orgname = OrgName,
					   orgunit = OrgUnit
					  })
	end,
    mnesia:transaction(F).

-define(FORM,
	[{xmlelement, "instructions", [],
	  [{xmlcdata, "You need a x:data capable client to search"}]},
	 {xmlelement, "x", [{"xmlns", ?NS_XDATA}, {"type", "form"}],
	  [{xmlelement, "title", [], [{xmlcdata, "Users Search"}]},
	   {xmlelement, "instructions", [],
	    [{xmlcdata, "Fill in fields to search "
	      "for any matching Jabber User"}]},
	   {xmlelement, "field", [{"type", "text-single"},
				  {"label", "Full Name"},
				  {"var", "fn"}], []}
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
		    case Type of
			set ->
			    % TODO
			    Err = jlib:make_error_reply(
				    Packet, "501", "Not Implemented"),
			    ejabberd_router:route(To, From, Err);
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
					  {"name", "EJUD"}], []},
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

