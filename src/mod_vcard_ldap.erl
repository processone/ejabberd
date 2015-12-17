%%%----------------------------------------------------------------------
%%% File    : mod_vcard_ldap.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Support for VCards from LDAP storage.
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

-module(mod_vcard_ldap).

-behaviour(ejabberd_config).

-author('alexey@process-one.net').

-behaviour(gen_server).

-behaviour(gen_mod).

%% gen_server callbacks.
-export([init/1, handle_info/2, handle_call/3,
	 handle_cast/2, terminate/2, code_change/3]).

-export([start/2, start_link/2, stop/1,
	 get_sm_features/5, process_local_iq/3, process_sm_iq/3,
	 remove_user/1, route/4, transform_module_options/1,
	 mod_opt_type/1, opt_type/1]).

-include("ejabberd.hrl").
-include("logger.hrl").

-include("eldap.hrl").

-include("jlib.hrl").

-define(PROCNAME, ejabberd_mod_vcard_ldap).

-record(state,
	{serverhost = <<"">>        :: binary(),
         myhost = <<"">>            :: binary(),
         eldap_id = <<"">>          :: binary(),
         search = false             :: boolean(),
         servers = []               :: [binary()],
         backups = []               :: [binary()],
	 port = ?LDAP_PORT          :: inet:port_number(),
         tls_options = []           :: list(),
         dn = <<"">>                :: binary(),
         base = <<"">>              :: binary(),
         password = <<"">>          :: binary(),
         uids = []                  :: [{binary()} | {binary(), binary()}],
         vcard_map = []             :: [{binary(), binary(), [binary()]}],
	 vcard_map_attrs = []       :: [binary()],
         user_filter = <<"">>       :: binary(),
         search_filter              :: eldap:filter(),
	 search_fields = []         :: [{binary(), binary()}],
         search_reported = []       :: [{binary(), binary()}],
         search_reported_attrs = [] :: [binary()],
	 deref_aliases = never      :: never | searching | finding | always,
         matches = 0                :: non_neg_integer()}).

-define(VCARD_MAP,
	[{<<"NICKNAME">>, <<"%u">>, []},
	 {<<"FN">>, <<"%s">>, [<<"displayName">>]},
	 {<<"FAMILY">>, <<"%s">>, [<<"sn">>]},
	 {<<"GIVEN">>, <<"%s">>, [<<"givenName">>]},
	 {<<"MIDDLE">>, <<"%s">>, [<<"initials">>]},
	 {<<"ORGNAME">>, <<"%s">>, [<<"o">>]},
	 {<<"ORGUNIT">>, <<"%s">>, [<<"ou">>]},
	 {<<"CTRY">>, <<"%s">>, [<<"c">>]},
	 {<<"LOCALITY">>, <<"%s">>, [<<"l">>]},
	 {<<"STREET">>, <<"%s">>, [<<"street">>]},
	 {<<"REGION">>, <<"%s">>, [<<"st">>]},
	 {<<"PCODE">>, <<"%s">>, [<<"postalCode">>]},
	 {<<"TITLE">>, <<"%s">>, [<<"title">>]},
	 {<<"URL">>, <<"%s">>, [<<"labeleduri">>]},
	 {<<"DESC">>, <<"%s">>, [<<"description">>]},
	 {<<"TEL">>, <<"%s">>, [<<"telephoneNumber">>]},
	 {<<"EMAIL">>, <<"%s">>, [<<"mail">>]},
	 {<<"BDAY">>, <<"%s">>, [<<"birthDay">>]},
	 {<<"ROLE">>, <<"%s">>, [<<"employeeType">>]},
	 {<<"PHOTO">>, <<"%s">>, [<<"jpegPhoto">>]}]).

-define(SEARCH_FIELDS,
	[{<<"User">>, <<"%u">>},
	 {<<"Full Name">>, <<"displayName">>},
	 {<<"Given Name">>, <<"givenName">>},
	 {<<"Middle Name">>, <<"initials">>},
	 {<<"Family Name">>, <<"sn">>},
	 {<<"Nickname">>, <<"%u">>},
	 {<<"Birthday">>, <<"birthDay">>},
	 {<<"Country">>, <<"c">>}, {<<"City">>, <<"l">>},
	 {<<"Email">>, <<"mail">>},
	 {<<"Organization Name">>, <<"o">>},
	 {<<"Organization Unit">>, <<"ou">>}]).

-define(SEARCH_REPORTED,
	[{<<"Full Name">>, <<"FN">>},
	 {<<"Given Name">>, <<"FIRST">>},
	 {<<"Middle Name">>, <<"MIDDLE">>},
	 {<<"Family Name">>, <<"LAST">>},
	 {<<"Nickname">>, <<"NICK">>},
	 {<<"Birthday">>, <<"BDAY">>},
	 {<<"Country">>, <<"CTRY">>},
	 {<<"City">>, <<"LOCALITY">>},
	 {<<"Email">>, <<"EMAIL">>},
	 {<<"Organization Name">>, <<"ORGNAME">>},
	 {<<"Organization Unit">>, <<"ORGUNIT">>}]).

handle_cast(_Request, State) -> {noreply, State}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

start(Host, Opts) ->
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    ChildSpec = {Proc, {?MODULE, start_link, [Host, Opts]},
		 transient, 1000, worker, [?MODULE]},
    supervisor:start_child(ejabberd_sup, ChildSpec).

stop(Host) ->
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    gen_server:call(Proc, stop),
    supervisor:terminate_child(ejabberd_sup, Proc),
    supervisor:delete_child(ejabberd_sup, Proc).

terminate(_Reason, State) ->
    Host = State#state.serverhost,
    gen_iq_handler:remove_iq_handler(ejabberd_local, Host,
				     ?NS_VCARD),
    gen_iq_handler:remove_iq_handler(ejabberd_sm, Host,
				     ?NS_VCARD),
    ejabberd_hooks:delete(disco_sm_features, Host, ?MODULE,
			  get_sm_features, 50),
    case State#state.search of
      true ->
	  ejabberd_router:unregister_route(State#state.myhost);
      _ -> ok
    end.

start_link(Host, Opts) ->
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    gen_server:start_link({local, Proc}, ?MODULE,
			  [Host, Opts], []).

init([Host, Opts]) ->
    State = parse_options(Host, Opts),
    IQDisc = gen_mod:get_opt(iqdisc, Opts, fun gen_iq_handler:check_type/1,
                             one_queue),
    gen_iq_handler:add_iq_handler(ejabberd_local, Host,
				  ?NS_VCARD, ?MODULE, process_local_iq, IQDisc),
    gen_iq_handler:add_iq_handler(ejabberd_sm, Host,
				  ?NS_VCARD, ?MODULE, process_sm_iq, IQDisc),
    ejabberd_hooks:add(disco_sm_features, Host, ?MODULE,
		       get_sm_features, 50),
    eldap_pool:start_link(State#state.eldap_id,
			  State#state.servers, State#state.backups,
			  State#state.port, State#state.dn,
			  State#state.password, State#state.tls_options),
    case State#state.search of
      true ->
	  ejabberd_router:register_route(State#state.myhost);
      _ -> ok
    end,
    {ok, State}.

handle_info({route, From, To, Packet}, State) ->
    case catch do_route(State, From, To, Packet) of
      Pid when is_pid(Pid) -> ok;
      _ ->
	  Err = jlib:make_error_reply(Packet,
				      ?ERR_INTERNAL_SERVER_ERROR),
	  ejabberd_router:route(To, From, Err)
    end,
    {noreply, State};
handle_info(_Info, State) -> {noreply, State}.

get_sm_features({error, _Error} = Acc, _From, _To,
		_Node, _Lang) ->
    Acc;
get_sm_features(Acc, _From, _To, Node, _Lang) ->
    case Node of
      <<"">> ->
	  case Acc of
	    {result, Features} -> {result, [?NS_VCARD | Features]};
	    empty -> {result, [?NS_VCARD]}
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

process_sm_iq(_From, #jid{lserver = LServer} = To,
	      #iq{sub_el = SubEl} = IQ) ->
    case catch process_vcard_ldap(To, IQ, LServer) of
      {'EXIT', _} ->
	  IQ#iq{type = error,
		sub_el = [SubEl, ?ERR_INTERNAL_SERVER_ERROR]};
      Other -> Other
    end.

process_vcard_ldap(To, IQ, Server) ->
    {ok, State} = eldap_utils:get_state(Server, ?PROCNAME),
    #iq{type = Type, sub_el = SubEl} = IQ,
    case Type of
      set ->
	  IQ#iq{type = error, sub_el = [SubEl, ?ERR_NOT_ALLOWED]};
      get ->
	  #jid{luser = LUser} = To,
	  LServer = State#state.serverhost,
	  case ejabberd_auth:is_user_exists(LUser, LServer) of
	    true ->
		VCardMap = State#state.vcard_map,
		case find_ldap_user(LUser, State) of
		  #eldap_entry{attributes = Attributes} ->
		      Vcard = ldap_attributes_to_vcard(Attributes, VCardMap,
						       {LUser, LServer}),
		      IQ#iq{type = result, sub_el = Vcard};
		  _ -> IQ#iq{type = result, sub_el = []}
		end;
	    _ -> IQ#iq{type = result, sub_el = []}
	  end
    end.

handle_call(get_state, _From, State) ->
    {reply, {ok, State}, State};
handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(_Request, _From, State) ->
    {reply, bad_request, State}.

find_ldap_user(User, State) ->
    Base = State#state.base,
    RFC2254_Filter = State#state.user_filter,
    Eldap_ID = State#state.eldap_id,
    VCardAttrs = State#state.vcard_map_attrs,
    case eldap_filter:parse(RFC2254_Filter,
			    [{<<"%u">>, User}])
	of
      {ok, EldapFilter} ->
	  case eldap_pool:search(Eldap_ID,
				 [{base, Base}, {filter, EldapFilter},
				  {deref_aliases, State#state.deref_aliases},
				  {attributes, VCardAttrs}])
	      of
	    #eldap_search_result{entries = [E | _]} -> E;
	    _ -> false
	  end;
      _ -> false
    end.

ldap_attributes_to_vcard(Attributes, VCardMap, UD) ->
    Attrs = lists:map(fun ({VCardName, _, _}) ->
			      {stringprep:tolower(VCardName),
			       map_vcard_attr(VCardName, Attributes, VCardMap,
					      UD)}
		      end,
		      VCardMap),
    Elts = [ldap_attribute_to_vcard(vCard, Attr)
	    || Attr <- Attrs],
    NElts = [ldap_attribute_to_vcard(vCardN, Attr)
	     || Attr <- Attrs],
    OElts = [ldap_attribute_to_vcard(vCardO, Attr)
	     || Attr <- Attrs],
    AElts = [ldap_attribute_to_vcard(vCardA, Attr)
	     || Attr <- Attrs],
    [#xmlel{name = <<"vCard">>,
	    attrs = [{<<"xmlns">>, ?NS_VCARD}],
	    children =
		lists:append([X || X <- Elts, X /= none],
			     [#xmlel{name = <<"N">>, attrs = [],
				     children = [X || X <- NElts, X /= none]},
			      #xmlel{name = <<"ORG">>, attrs = [],
				     children = [X || X <- OElts, X /= none]},
			      #xmlel{name = <<"ADR">>, attrs = [],
				     children =
					 [X || X <- AElts, X /= none]}])}].

ldap_attribute_to_vcard(vCard, {<<"fn">>, Value}) ->
    #xmlel{name = <<"FN">>, attrs = [],
	   children = [{xmlcdata, Value}]};
ldap_attribute_to_vcard(vCard,
			{<<"nickname">>, Value}) ->
    #xmlel{name = <<"NICKNAME">>, attrs = [],
	   children = [{xmlcdata, Value}]};
ldap_attribute_to_vcard(vCard, {<<"title">>, Value}) ->
    #xmlel{name = <<"TITLE">>, attrs = [],
	   children = [{xmlcdata, Value}]};
ldap_attribute_to_vcard(vCard, {<<"bday">>, Value}) ->
    #xmlel{name = <<"BDAY">>, attrs = [],
	   children = [{xmlcdata, Value}]};
ldap_attribute_to_vcard(vCard, {<<"url">>, Value}) ->
    #xmlel{name = <<"URL">>, attrs = [],
	   children = [{xmlcdata, Value}]};
ldap_attribute_to_vcard(vCard, {<<"desc">>, Value}) ->
    #xmlel{name = <<"DESC">>, attrs = [],
	   children = [{xmlcdata, Value}]};
ldap_attribute_to_vcard(vCard, {<<"role">>, Value}) ->
    #xmlel{name = <<"ROLE">>, attrs = [],
	   children = [{xmlcdata, Value}]};
ldap_attribute_to_vcard(vCard, {<<"tel">>, Value}) ->
    #xmlel{name = <<"TEL">>, attrs = [],
	   children =
	       [#xmlel{name = <<"VOICE">>, attrs = [], children = []},
		#xmlel{name = <<"WORK">>, attrs = [], children = []},
		#xmlel{name = <<"NUMBER">>, attrs = [],
		       children = [{xmlcdata, Value}]}]};
ldap_attribute_to_vcard(vCard, {<<"email">>, Value}) ->
    #xmlel{name = <<"EMAIL">>, attrs = [],
	   children =
	       [#xmlel{name = <<"INTERNET">>, attrs = [],
		       children = []},
		#xmlel{name = <<"PREF">>, attrs = [], children = []},
		#xmlel{name = <<"USERID">>, attrs = [],
		       children = [{xmlcdata, Value}]}]};
ldap_attribute_to_vcard(vCard, {<<"photo">>, Value}) ->
    #xmlel{name = <<"PHOTO">>, attrs = [],
	   children =
	       [#xmlel{name = <<"TYPE">>, attrs = [],
		       children = [{xmlcdata, <<"image/jpeg">>}]},
		#xmlel{name = <<"BINVAL">>, attrs = [],
		       children = [{xmlcdata, jlib:encode_base64(Value)}]}]};
ldap_attribute_to_vcard(vCardN,
			{<<"family">>, Value}) ->
    #xmlel{name = <<"FAMILY">>, attrs = [],
	   children = [{xmlcdata, Value}]};
ldap_attribute_to_vcard(vCardN, {<<"given">>, Value}) ->
    #xmlel{name = <<"GIVEN">>, attrs = [],
	   children = [{xmlcdata, Value}]};
ldap_attribute_to_vcard(vCardN,
			{<<"middle">>, Value}) ->
    #xmlel{name = <<"MIDDLE">>, attrs = [],
	   children = [{xmlcdata, Value}]};
ldap_attribute_to_vcard(vCardO,
			{<<"orgname">>, Value}) ->
    #xmlel{name = <<"ORGNAME">>, attrs = [],
	   children = [{xmlcdata, Value}]};
ldap_attribute_to_vcard(vCardO,
			{<<"orgunit">>, Value}) ->
    #xmlel{name = <<"ORGUNIT">>, attrs = [],
	   children = [{xmlcdata, Value}]};
ldap_attribute_to_vcard(vCardA,
			{<<"locality">>, Value}) ->
    #xmlel{name = <<"LOCALITY">>, attrs = [],
	   children = [{xmlcdata, Value}]};
ldap_attribute_to_vcard(vCardA,
			{<<"street">>, Value}) ->
    #xmlel{name = <<"STREET">>, attrs = [],
	   children = [{xmlcdata, Value}]};
ldap_attribute_to_vcard(vCardA, {<<"ctry">>, Value}) ->
    #xmlel{name = <<"CTRY">>, attrs = [],
	   children = [{xmlcdata, Value}]};
ldap_attribute_to_vcard(vCardA,
			{<<"region">>, Value}) ->
    #xmlel{name = <<"REGION">>, attrs = [],
	   children = [{xmlcdata, Value}]};
ldap_attribute_to_vcard(vCardA, {<<"pcode">>, Value}) ->
    #xmlel{name = <<"PCODE">>, attrs = [],
	   children = [{xmlcdata, Value}]};
ldap_attribute_to_vcard(_, _) -> none.

-define(TLFIELD(Type, Label, Var),
	#xmlel{name = <<"field">>,
	       attrs =
		   [{<<"type">>, Type},
		    {<<"label">>, translate:translate(Lang, Label)},
		    {<<"var">>, Var}],
	       children = []}).

-define(FORM(JID, SearchFields),
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
						      <<"Fill in fields to search for any matching "
							"Jabber User">>)}]}]
		      ++
		      lists:map(fun ({X, Y}) ->
					?TLFIELD(<<"text-single">>, X, Y)
				end,
				SearchFields)}]).

do_route(State, From, To, Packet) ->
    spawn(?MODULE, route, [State, From, To, Packet]).

route(State, From, To, Packet) ->
    #jid{user = User, resource = Resource} = To,
    ServerHost = State#state.serverhost,
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
											   State,
											   XData)}]}]},
				   ejabberd_router:route(To, From,
							 jlib:iq_to_xml(ResIQ))
			     end
		       end;
		   get ->
		       SearchFields = State#state.search_fields,
		       ResIQ = IQ#iq{type = result,
				     sub_el =
					 [#xmlel{name = <<"query">>,
						 attrs =
						     [{<<"xmlns">>,
						       ?NS_SEARCH}],
						 children =
						     ?FORM(To, SearchFields)}]},
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

-define(LFIELD(Label, Var),
	#xmlel{name = <<"field">>,
	       attrs =
		   [{<<"label">>, translate:translate(Lang, Label)},
		    {<<"var">>, Var}],
	       children = []}).

search_result(Lang, JID, State, Data) ->
    SearchReported = State#state.search_reported,
    Header = [#xmlel{name = <<"title">>, attrs = [],
		     children =
			 [{xmlcdata,
			   <<(translate:translate(Lang,
						  <<"Search Results for ">>))/binary,
			     (jid:to_string(JID))/binary>>}]},
	      #xmlel{name = <<"reported">>, attrs = [],
		     children =
			 [?TLFIELD(<<"text-single">>, <<"Jabber ID">>,
				   <<"jid">>)]
			   ++
			   lists:map(fun ({Name, Value}) ->
					     ?TLFIELD(<<"text-single">>, Name,
						      Value)
				     end,
				     SearchReported)}],
    case search(State, Data) of
      error -> Header;
      Result -> Header ++ Result
    end.

-define(FIELD(Var, Val),
	#xmlel{name = <<"field">>, attrs = [{<<"var">>, Var}],
	       children =
		   [#xmlel{name = <<"value">>, attrs = [],
			   children = [{xmlcdata, Val}]}]}).

search(State, Data) ->
    Base = State#state.base,
    SearchFilter = State#state.search_filter,
    Eldap_ID = State#state.eldap_id,
    UIDs = State#state.uids,
    Limit = State#state.matches,
    ReportedAttrs = State#state.search_reported_attrs,
    Filter = eldap:'and'([SearchFilter,
			  eldap_utils:make_filter(Data, UIDs)]),
    case eldap_pool:search(Eldap_ID,
			   [{base, Base}, {filter, Filter}, {limit, Limit},
			    {deref_aliases, State#state.deref_aliases},
			    {attributes, ReportedAttrs}])
	of
      #eldap_search_result{entries = E} ->
	  search_items(E, State);
      _ -> error
    end.

search_items(Entries, State) ->
    LServer = State#state.serverhost,
    SearchReported = State#state.search_reported,
    VCardMap = State#state.vcard_map,
    UIDs = State#state.uids,
    Attributes = lists:map(fun (E) ->
				   #eldap_entry{attributes = Attrs} = E, Attrs
			   end,
			   Entries),
    lists:flatmap(fun (Attrs) ->
			  case eldap_utils:find_ldap_attrs(UIDs, Attrs) of
			    {U, UIDAttrFormat} ->
				case eldap_utils:get_user_part(U, UIDAttrFormat)
				    of
				  {ok, Username} ->
				      case
					ejabberd_auth:is_user_exists(Username,
								     LServer)
					  of
					true ->
					    RFields = lists:map(fun ({_,
								      VCardName}) ->
									{VCardName,
									 map_vcard_attr(VCardName,
											Attrs,
											VCardMap,
											{Username,
											 ?MYNAME})}
								end,
								SearchReported),
					    Result = [?FIELD(<<"jid">>,
							     <<Username/binary,
							       "@",
							       LServer/binary>>)]
						       ++
						       [?FIELD(Name, Value)
							|| {Name, Value}
							       <- RFields],
					    [#xmlel{name = <<"item">>,
						    attrs = [],
						    children = Result}];
					_ -> []
				      end;
				  _ -> []
				end;
			    <<"">> -> []
			  end
		  end,
		  Attributes).

remove_user(_User) -> true.

%%%-----------------------
%%% Auxiliary functions.
%%%-----------------------

map_vcard_attr(VCardName, Attributes, Pattern, UD) ->
    Res = lists:filter(fun ({Name, _, _}) ->
			       eldap_utils:case_insensitive_match(Name,
								  VCardName)
		       end,
		       Pattern),
    case Res of
      [{_, Str, Attrs}] ->
	  process_pattern(Str, UD,
			  [eldap_utils:get_ldap_attr(X, Attributes)
			   || X <- Attrs]);
      _ -> <<"">>
    end.

process_pattern(Str, {User, Domain}, AttrValues) ->
    eldap_filter:do_sub(Str,
			[{<<"%u">>, User}, {<<"%d">>, Domain}] ++
			  [{<<"%s">>, V, 1} || V <- AttrValues]).

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

parse_options(Host, Opts) ->
    MyHost = gen_mod:get_opt_host(Host, Opts,
				  <<"vjud.@HOST@">>),
    Search = gen_mod:get_opt(search, Opts,
                             fun(B) when is_boolean(B) -> B end,
                             false),
    Matches = gen_mod:get_opt(matches, Opts,
                              fun(infinity) -> 0;
                                 (I) when is_integer(I), I>0 -> I
                              end, 30),
    Eldap_ID = jlib:atom_to_binary(gen_mod:get_module_proc(Host, ?PROCNAME)),
    Cfg = eldap_utils:get_config(Host, Opts),
    UIDsTemp = gen_mod:get_opt(
                 {ldap_uids, Host}, Opts,
                 fun(Us) ->
                         lists:map(
                           fun({U, P}) ->
                                   {iolist_to_binary(U),
                                    iolist_to_binary(P)};
                              ({U}) ->
                                   {iolist_to_binary(U)}
                           end, Us)
                 end, [{<<"uid">>, <<"%u">>}]),
    UIDs = eldap_utils:uids_domain_subst(Host, UIDsTemp),
    SubFilter = eldap_utils:generate_subfilter(UIDs),
    UserFilter = case gen_mod:get_opt(
                        {ldap_filter, Host}, Opts,
                        fun check_filter/1, <<"">>) of
                     <<"">> ->
			 SubFilter;
                     F ->
                         <<"(&", SubFilter/binary, F/binary, ")">>
                 end,
    {ok, SearchFilter} =
	eldap_filter:parse(eldap_filter:do_sub(UserFilter,
					       [{<<"%u">>, <<"*">>}])),
    VCardMap = gen_mod:get_opt(ldap_vcard_map, Opts,
                               fun(Ls) ->
                                       lists:map(
                                         fun({S, [{P, L}]}) ->
                                                 {iolist_to_binary(S),
                                                  iolist_to_binary(P),
                                                  [iolist_to_binary(E)
                                                   || E <- L]}
                                         end, Ls)
                               end, ?VCARD_MAP),
    SearchFields = gen_mod:get_opt(ldap_search_fields, Opts,
                                   fun(Ls) ->
                                           [{iolist_to_binary(S),
                                             iolist_to_binary(P)}
                                            || {S, P} <- Ls]
                                   end, ?SEARCH_FIELDS),
    SearchReported = gen_mod:get_opt(ldap_search_reported, Opts,
                                     fun(Ls) ->
                                             [{iolist_to_binary(S),
                                               iolist_to_binary(P)}
                                              || {S, P} <- Ls]
                                     end, ?SEARCH_REPORTED),
    UIDAttrs = [UAttr || {UAttr, _} <- UIDs],
    VCardMapAttrs = lists:usort(lists:append([A
					      || {_, _, A} <- VCardMap])
				  ++ UIDAttrs),
    SearchReportedAttrs = lists:usort(lists:flatmap(fun ({_,
							  N}) ->
							    case
							      lists:keysearch(N,
									      1,
									      VCardMap)
								of
							      {value,
							       {_, _, L}} ->
								  L;
							      _ -> []
							    end
						    end,
						    SearchReported)
					++ UIDAttrs),
    #state{serverhost = Host, myhost = MyHost,
	   eldap_id = Eldap_ID, search = Search,
	   servers = Cfg#eldap_config.servers,
	   backups = Cfg#eldap_config.backups,
           port = Cfg#eldap_config.port,
	   tls_options = Cfg#eldap_config.tls_options,
	   dn = Cfg#eldap_config.dn,
           password = Cfg#eldap_config.password,
           base = Cfg#eldap_config.base,
           deref_aliases = Cfg#eldap_config.deref_aliases,
	   uids = UIDs, vcard_map = VCardMap,
	   vcard_map_attrs = VCardMapAttrs,
	   user_filter = UserFilter, search_filter = SearchFilter,
	   search_fields = SearchFields,
	   search_reported = SearchReported,
	   search_reported_attrs = SearchReportedAttrs,
	   matches = Matches}.

transform_module_options(Opts) ->
    lists:map(
      fun({ldap_vcard_map, Map}) ->
              NewMap = lists:map(
                         fun({Field, Pattern, Attrs}) ->
                                 {Field, [{Pattern, Attrs}]};
                            (Opt) ->
                                 Opt
                         end, Map),
              {ldap_vcard_map, NewMap};
         (Opt) ->
              Opt
      end, Opts).

check_filter(F) ->
    NewF = iolist_to_binary(F),
    {ok, _} = eldap_filter:parse(NewF),
    NewF.

mod_opt_type(host) -> fun iolist_to_binary/1;
mod_opt_type(iqdisc) -> fun gen_iq_handler:check_type/1;
mod_opt_type(ldap_filter) -> fun check_filter/1;
mod_opt_type(ldap_search_fields) ->
    fun (Ls) ->
	    [{iolist_to_binary(S), iolist_to_binary(P)}
	     || {S, P} <- Ls]
    end;
mod_opt_type(ldap_search_reported) ->
    fun (Ls) ->
	    [{iolist_to_binary(S), iolist_to_binary(P)}
	     || {S, P} <- Ls]
    end;
mod_opt_type(ldap_uids) ->
    fun (Us) ->
	    lists:map(fun ({U, P}) ->
			      {iolist_to_binary(U), iolist_to_binary(P)};
			  ({U}) -> {iolist_to_binary(U)}
		      end,
		      Us)
    end;
mod_opt_type(ldap_vcard_map) ->
    fun (Ls) ->
	    lists:map(fun ({S, [{P, L}]}) ->
			      {iolist_to_binary(S), iolist_to_binary(P),
			       [iolist_to_binary(E) || E <- L]}
		      end,
		      Ls)
    end;
mod_opt_type(matches) ->
    fun (infinity) -> 0;
	(I) when is_integer(I), I > 0 -> I
    end;
mod_opt_type(search) ->
    fun (B) when is_boolean(B) -> B end;
mod_opt_type(deref_aliases) ->
    fun (never) -> never;
	(searching) -> searching;
	(finding) -> finding;
	(always) -> always
    end;
mod_opt_type(ldap_backups) ->
    fun (L) -> [iolist_to_binary(H) || H <- L] end;
mod_opt_type(ldap_base) -> fun iolist_to_binary/1;
mod_opt_type(ldap_deref_aliases) ->
    fun (never) -> never;
	(searching) -> searching;
	(finding) -> finding;
	(always) -> always
    end;
mod_opt_type(ldap_encrypt) ->
    fun (tls) -> tls;
	(starttls) -> starttls;
	(none) -> none
    end;
mod_opt_type(ldap_password) -> fun iolist_to_binary/1;
mod_opt_type(ldap_port) ->
    fun (I) when is_integer(I), I > 0 -> I end;
mod_opt_type(ldap_rootdn) -> fun iolist_to_binary/1;
mod_opt_type(ldap_servers) ->
    fun (L) -> [iolist_to_binary(H) || H <- L] end;
mod_opt_type(ldap_tls_cacertfile) ->
    fun iolist_to_binary/1;
mod_opt_type(ldap_tls_certfile) ->
    fun iolist_to_binary/1;
mod_opt_type(ldap_tls_depth) ->
    fun (I) when is_integer(I), I >= 0 -> I end;
mod_opt_type(ldap_tls_verify) ->
    fun (hard) -> hard;
	(soft) -> soft;
	(false) -> false
    end;
mod_opt_type(_) ->
    [host, iqdisc, ldap_filter, ldap_search_fields,
     ldap_search_reported, ldap_uids, ldap_vcard_map,
     matches, search, deref_aliases, ldap_backups, ldap_base,
     ldap_deref_aliases, ldap_encrypt, ldap_password,
     ldap_port, ldap_rootdn, ldap_servers,
     ldap_tls_cacertfile, ldap_tls_certfile, ldap_tls_depth,
     ldap_tls_verify].

opt_type(ldap_filter) -> fun check_filter/1;
opt_type(ldap_uids) ->
    fun (Us) ->
	    lists:map(fun ({U, P}) ->
			      {iolist_to_binary(U), iolist_to_binary(P)};
			  ({U}) -> {iolist_to_binary(U)}
		      end,
		      Us)
    end;
opt_type(_) ->
    [ldap_filter, ldap_uids, deref_aliases, ldap_backups, ldap_base,
     ldap_deref_aliases, ldap_encrypt, ldap_password,
     ldap_port, ldap_rootdn, ldap_servers,
     ldap_tls_cacertfile, ldap_tls_certfile, ldap_tls_depth,
     ldap_tls_verify].
