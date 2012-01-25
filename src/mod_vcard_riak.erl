%%%----------------------------------------------------------------------
%%% File    : mod_vcard_riak.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : vCard support via Riak
%%% Created :  6 Jan 2012 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
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

-module(mod_vcard_riak).
-author('alexey@process-one.net').

-behaviour(gen_mod).

-export([start/2, stop/1,
	 get_sm_features/5,
	 process_local_iq/3,
	 process_sm_iq/3,
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
    ok.

stop(Host) ->
    ejabberd_hooks:delete(remove_user, Host,
			  ?MODULE, remove_user, 50),
    gen_iq_handler:remove_iq_handler(ejabberd_local, Host, ?NS_VCARD),
    gen_iq_handler:remove_iq_handler(ejabberd_sm, Host, ?NS_VCARD),
    ejabberd_hooks:delete(disco_sm_features, Host, ?MODULE, get_sm_features, 50),
    ok.

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
				   "\nCopyright (c) 2002-2011 ProcessOne"}]},
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
            Username = list_to_binary(LUser),
	    case catch ejabberd_riak:get(LServer, <<"vcard">>, Username) of
		{ok, SVCARD} ->
		    case xml_stream:parse_element(SVCARD) of
			{error, _Reason} ->
			    IQ#iq{type = error,
				  sub_el = [SubEl, ?ERR_SERVICE_UNAVAILABLE]};
			VCARD ->
			    IQ#iq{type = result, sub_el = [VCARD]}
		    end;
		{error, notfound} ->
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
            Username = list_to_binary(LUser),
	    SVCARD = xml:element_to_binary(VCARD),

            ejabberd_riak:put(
              LServer, <<"vcard">>, Username, SVCARD,
              [{<<"bday_bin">>, list_to_binary(LBDay)},
               {<<"ctry_bin">>, list_to_binary(LCTRY)},
               {<<"email_bin">>, list_to_binary(LEMail)},
               {<<"fn_bin">>, list_to_binary(LFN)},
               {<<"family_bin">>, list_to_binary(LFamily)},
               {<<"given_bin">>, list_to_binary(LGiven)},
               {<<"locality_bin">>, list_to_binary(LLocality)},
               {<<"middle_bin">>, list_to_binary(LMiddle)},
               {<<"nickname_bin">>, list_to_binary(LNickname)},
               {<<"orgname_bin">>, list_to_binary(LOrgName)},
               {<<"orgunit_bin">>, list_to_binary(LOrgUnit)},
               {<<"user_bin">>, Username}]),

	    ejabberd_hooks:run(vcard_set, LServer, [LUser, LServer, VCARD])
    end.

remove_user(User, Server) ->
    LUser = jlib:nodeprep(User),
    LServer = jlib:nameprep(Server),
    Username = list_to_binary(LUser),
    ejabberd_riak:delete(LServer, <<"vcard">>, Username),
    ok.
