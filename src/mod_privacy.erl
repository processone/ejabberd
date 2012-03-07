%%%----------------------------------------------------------------------
%%% File    : mod_privacy.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : XEP-0016: Privacy Lists
%%% Created : 21 Jul 2003 by Alexey Shchepin <alexey@process-one.net>
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

%%% Database schema (version / storage / table)
%%%
%%% 2.1.x / mnesia / privacy
%%%  us = {Username::string(), Host::string()}
%%%  default = none | ListName::string()
%%%  lists = [ {ListName::string(), [listitem()]} ]
%%%
%%% 2.1.x / odbc / privacy_default_list
%%%  username = varchar250
%%%  name = varchar250
%%% 2.1.x / odbc / privacy_list
%%%  username = varchar250
%%%  name = varchar250
%%%  id = bigint-unsigned
%%% 2.1.x / odbc / privacy_list_data
%%%  id = bigint
%%%  t = character(1)
%%%  value = text
%%%  action = character(1)
%%%  ord = NUMERIC
%%%  match_all = boolean
%%%  match_iq = boolean
%%%  match_message = boolean
%%%  match_presence_in = boolean
%%%  match_presence_out = boolean
%%%
%%% 3.0.0-prealpha / mnesia / privacy
%%%  us = {Username::string(), Host::string()}
%%%  default = none | ListName::string()
%%%  lists = [ {ListName::string(), [listitem()]} ]
%%%
%%% 3.0.0-prealpha / odbc / privacy
%%%  Same as 2.1.x
%%%
%%% 3.0.0-alpha / mnesia / privacy_default_list
%%%  user_host = {Username::binary(), Server::binary()}
%%%  name = binary()
%%% 3.0.0-alpha / mnesia / privacy_list
%%%  user_host = {Username::binary(), Server::binary()}
%%%  name = binary()
%%% 3.0.0-alpha / mnesia / privacy_list_data
%%%  user_host = {Username::binary(), Server::binary()}
%%%  name = binary()
%%%  type = jid | group | subscription | none
%%%  value = JID::binary() | Group::binary() | <<"none">> | <<"both">> | <<"from">> | <<"to">> | none
%%%  action = allow | deny
%%%  order = integer()
%%%  match_all = boolean()
%%%  match_iq = boolean()
%%%  match_message = boolean()
%%%  match_presence_in = boolean()
%%%  match_presence_out = boolean()
%%%
%%% 3.0.0-alpha / odbc / privacy_default_list
%%%  user =  varchar150
%%%  host =  varchar150
%%%  name = text
%%% 3.0.0-alpha / odbc / privacy_list
%%%  user =  varchar150
%%%  host =  varchar150
%%%  name =  varchar150
%%% 3.0.0-alpha / odbc / privacy_list_data
%%%  user =  varchar150
%%%  host =  varchar150
%%%  name =  varchar150
%%%  type = varchar150
%%%  value = varchar150
%%%  action = varchar150
%%%  order = int 11
%%%  match_all = varchar150
%%%  match_iq = varchar150
%%%  match_message = varchar150
%%%  match_presence_in = varchar150
%%%  match_presence_out = varchar150

-module(mod_privacy).
-author('alexey@process-one.net').

-behaviour(gen_mod).

-export([start/2, stop/1,
	 process_iq/3,
	 process_iq_set/4,
	 process_iq_get/5,
	 get_user_list/3,
	 remove_user/2,
	 check_packet/6,
	 updated_list/3]).

-include_lib("exmpp/include/exmpp.hrl").
-include_lib("exmpp/include/exmpp_jid.hrl").

-include("ejabberd.hrl").
-include("mod_privacy.hrl").

start(Host, Opts) when is_list(Host) ->
    start(list_to_binary(Host), Opts);
start(HostB, Opts) ->
    IQDisc = gen_mod:get_opt(iqdisc, Opts, one_queue),
    Backend = gen_mod:get_opt(backend, Opts, mnesia),
    gen_storage:create_table(Backend, HostB, privacy_list,
			     [{disc_copies, [node()]},
			      {odbc_host, HostB},
			      {type, bag},
			      {attributes, record_info(fields, privacy_list)},
			      {types, [{user_host, {text, text}}]}]),
    gen_storage:create_table(Backend, HostB, privacy_default_list,
			     [{disc_copies, [node()]},
			      {odbc_host, HostB},
			      {attributes, record_info(fields, privacy_default_list)},
			      {types, [{user_host, {text, text}}]}]),
    gen_storage:create_table(Backend, HostB, privacy_list_data,
			     [{disc_copies, [node()]},
			      {odbc_host, HostB},
			      {type, bag},
			      {attributes, record_info(fields, privacy_list_data)},
			      {types, [{user_host, {text, text}},
				       {type, atom},
				       {value, binary},
				       {action, atom},
				       {order, int},
				       {match_all, atom},
				       {match_iq, atom},
				       {match_message, atom},
				       {match_presence_in, atom},
				       {match_presence_out, atom}
				      ]}]),
    update_tables(HostB, Backend),
    gen_storage:add_table_index(HostB, privacy_list, name),
    gen_storage:add_table_index(HostB, privacy_list_data, name),
    ejabberd_hooks:add(privacy_iq_get, HostB,
		       ?MODULE, process_iq_get, 50),
    ejabberd_hooks:add(privacy_iq_set, HostB,
		       ?MODULE, process_iq_set, 50),
    ejabberd_hooks:add(privacy_get_user_list, HostB,
		       ?MODULE, get_user_list, 50),
    ejabberd_hooks:add(privacy_check_packet, HostB,
		       ?MODULE, check_packet, 50),
    ejabberd_hooks:add(privacy_updated_list, HostB,
		       ?MODULE, updated_list, 50),
    ejabberd_hooks:add(remove_user, HostB,
		       ?MODULE, remove_user, 50),
    gen_iq_handler:add_iq_handler(ejabberd_sm, HostB, ?NS_PRIVACY,
				  ?MODULE, process_iq, IQDisc).

stop(Host) ->
    HostB = list_to_binary(Host),
    ejabberd_hooks:delete(privacy_iq_get, HostB,
			  ?MODULE, process_iq_get, 50),
    ejabberd_hooks:delete(privacy_iq_set, HostB,
			  ?MODULE, process_iq_set, 50),
    ejabberd_hooks:delete(privacy_get_user_list, HostB,
			  ?MODULE, get_user_list, 50),
    ejabberd_hooks:delete(privacy_check_packet, HostB,
			  ?MODULE, check_packet, 50),
    ejabberd_hooks:delete(privacy_updated_list, HostB,
			  ?MODULE, updated_list, 50),
    ejabberd_hooks:delete(remove_user, HostB,
			  ?MODULE, remove_user, 50),
    gen_iq_handler:remove_iq_handler(ejabberd_sm, HostB, ?NS_PRIVACY).

process_iq(_From, _To, IQ_Rec) ->
    exmpp_iq:error(IQ_Rec, 'not-allowed').


process_iq_get(_, From, _To, #iq{ns = ?NS_PRIVACY, payload = SubEl},
	       #userlist{name = Active}) ->
    LUser = exmpp_jid:prep_node(From),
    LServer = exmpp_jid:prep_domain(From),
    case exmpp_xml:get_child_elements(SubEl) of
	[] ->
	    process_lists_get(LUser, LServer, Active);
	[#xmlel{name = Name} = Child] ->
	    case Name of
		list ->
		    ListName = exmpp_xml:get_attribute_as_binary(Child, <<"name">>, false),
		    process_list_get(LUser, LServer, ListName);
		_ ->
		    {error, 'bad-request'}
	    end;
	_ ->
	    {error, 'bad-request'}
    end;

process_iq_get(Acc, _, _, _, _) ->
    Acc.

process_lists_get(LUser, LServer, Active) ->
    F = fun() ->
		Default =
		    case gen_storage:read(LServer, {privacy_default_list, {LUser, LServer}}) of
			[#privacy_default_list{name = Name}] ->
			    Name;
			_ ->
			    none
		    end,
		Lists = [List#privacy_list.name
			 || List <- gen_storage:read(LServer, {privacy_list, {LUser, LServer}})],
		{Default, Lists}
	end,
    case gen_storage:transaction(LServer, privacy_list, F) of
	{aborted, _Reason} ->
	    {error, 'internal-server-error'};
	{atomic, {Default, Lists}} ->
	    case Lists of
		[] ->
		    {result, #xmlel{ns = ?NS_PRIVACY, name = 'query'}};
		_ ->
		    LItems = [exmpp_xml:set_attribute(#xmlel{ns = ?NS_PRIVACY, name = list}, <<"name">>, N) || N <- Lists],
		    DItems =
			case Default of
			    none ->
				LItems;
			    _ ->
				[exmpp_xml:set_attribute(#xmlel{ns = ?NS_PRIVACY, name = default}, <<"name">>, Default) | LItems]
			end,
		    ADItems =
			case Active of
			    none ->
				DItems;
			    _ ->
				[exmpp_xml:set_attribute(#xmlel{ns = ?NS_PRIVACY, name = active}, <<"name">>, Active) | DItems]
			end,
			{result, #xmlel{ns = ?NS_PRIVACY, name = 'query', children = ADItems}}
	    end
    end.

process_list_get(_LUser, _LServer, false) ->
    {error, 'bad-request'};

process_list_get(LUser, LServer, Name) ->
    F = fun() ->
		case gen_storage:select(LServer, privacy_list,
					[{'=', user_host, {LUser, LServer}},
					 {'=', name, Name}]) of
		    [] ->
			none;
		    [#privacy_list{}] ->
			gen_storage:select(LServer, privacy_list_data,
					   [{'=', user_host, {LUser, LServer}},
					    {'=', name, Name}])
		end
	end,
    case gen_storage:transaction(LServer, privacy_list, F) of
	{aborted, _Reason} ->
	    {error, 'internal-server-error'};
	{atomic, none} ->
	    {error, 'item-not-found'};
	{atomic, List} ->
	    LItems = lists:map(fun item_to_xml/1, List),
	    ListEl = exmpp_xml:set_attribute(#xmlel{ns = ?NS_PRIVACY, name = list, children = LItems}, <<"name">>, Name),
	    {result,#xmlel{ns = ?NS_PRIVACY, name = 'query', children = [ListEl]}}
    end.


item_to_xml(Item) ->
    Attrs1 = [?XMLATTR(<<"action">>, action_to_binary(Item#privacy_list_data.action)),
	      ?XMLATTR(<<"order">>, order_to_binary(Item#privacy_list_data.order))],
    Attrs2 = case Item#privacy_list_data.type of
		 none ->
		     Attrs1;
		 Type ->
		     [?XMLATTR(<<"type">>, type_to_binary(Type)),
		      ?XMLATTR(<<"value">>, Item#privacy_list_data.value) |
		      Attrs1]
	     end,
    SubEls = case Item#privacy_list_data.match_all of
		 true ->
		     [];
		 false ->
		     SE1 = case Item#privacy_list_data.match_iq of
			       true ->
				   [#xmlel{ns = ?NS_PRIVACY, name = iq}];
			       false ->
				   []
			   end,
		     SE2 = case Item#privacy_list_data.match_message of
			       true ->
				   [#xmlel{ns = ?NS_PRIVACY, name = message} | SE1];
			       false ->
				   SE1
			   end,
		     SE3 = case Item#privacy_list_data.match_presence_in of
			       true ->
				   [#xmlel{ns = ?NS_PRIVACY, name = 'presence-in'} | SE2];
			       false ->
				   SE2
			   end,
		     SE4 = case Item#privacy_list_data.match_presence_out of
			       true ->
				   [#xmlel{ns = ?NS_PRIVACY, name = 'presence-out'} | SE3];
			       false ->
				   SE3
			   end,
		     SE4
	     end,
    exmpp_xml:set_attributes(#xmlel{ns = ?NS_PRIVACY, name = item, children = SubEls}, Attrs2).


action_to_binary(Action) ->
    case Action of
	allow -> <<"allow">>;
	deny -> <<"deny">>
    end.

order_to_binary(Order) ->
    list_to_binary(integer_to_list(Order)).

type_to_binary(Type) ->
    case Type of
	jid -> <<"jid">>;
	group -> <<"group">>;
	subscription -> <<"subscription">>
    end.



list_to_action(S) ->
    case S of
	"allow" -> allow;
	"deny" -> deny
    end.



process_iq_set(_, From, _To, #iq{ns = ?NS_PRIVACY, payload = SubEl}) ->
    LUser = exmpp_jid:prep_node(From),
    LServer = exmpp_jid:prep_domain(From),
    case exmpp_xml:get_child_elements(SubEl) of
	[#xmlel{name = Name} = Child] ->
	    ListName = exmpp_xml:get_attribute_as_binary(Child, <<"name">>, false),
	    case Name of
		list ->
		    process_list_set(LUser, LServer, ListName,
				     exmpp_xml:get_child_elements(Child));
		active ->
		    process_active_set(LUser, LServer, ListName);
		default ->
		    process_default_set(LUser, LServer, ListName);
		_ ->
		    {error, 'bad-request'}
	    end;
	_ ->
	    {error, 'bad-request'}
    end;

process_iq_set(Acc, _, _, _) ->
    Acc.


process_default_set(LUser, LServer, false) ->
    F = fun() ->
		gen_storage:delete(LServer, {privacy_default_list, {LUser, LServer}})
	end,
    case gen_storage:transaction(LServer, privacy_default_list, F) of
	{atomic, _} ->
	    {result, []};
	_ ->
	    {error, 'internal-server-error'}
    end;

process_default_set(LUser, LServer, Name) ->
    F = fun() ->
		case gen_storage:select(LServer, privacy_list,
					[{'=', user_host, {LUser, LServer}},
					 {'=', name, Name}]) of
		    [] ->
			{error, 'item-not-found'};
		    [#privacy_list{}] ->
			gen_storage:write(LServer,
					  #privacy_default_list{user_host = {LUser, LServer},
								name = Name}),
			{result, []}
		end
	end,
    case gen_storage:transaction(LServer, privacy_list, F) of
	{atomic, {error, _} = Error} ->
	    Error;
	{atomic, {result, _} = Res} ->
	    Res;
	_ ->
	    {error, 'internal-server-error'}
    end.


process_active_set(_LUser, _LServer, false) ->
    {result, [], #userlist{}};

process_active_set(LUser, LServer, Name) ->
    F = fun() ->
		case gen_storage:select(LServer, privacy_list,
					[{'=', user_host, {LUser, LServer}},
					 {'=', name, Name}]) of
		    [#privacy_list{}] ->
			Data = gen_storage:select(LServer, privacy_list_data,
						  [{'=', user_host, {LUser, LServer}},
						   {'=', name, Name}]),
		        List = list_data_to_items(Data),
			NeedDb = is_list_needdb(List),
			{result, [], #userlist{name = Name,
					       needdb = NeedDb,
					       list = List}};
		    [] ->
			{error, 'item-not-found'}
		end
	end,
    case gen_storage:transaction(LServer, privacy_list, F) of
	{atomic, Res} ->
	    Res;
	_ ->
	    {error, 'internal-server-error'}
    end.



process_list_set(_LUser, _LServer, false, _Els) ->
    {error, 'bad-request'};

process_list_set(LUser, LServer, Name, Els) ->
    case parse_items(Els) of
	false ->
	    {error, 'bad-request'};
	remove ->
	    F = fun() ->
			case gen_storage:read(LServer,
					      {privacy_default_list, {LUser, LServer}}) of
			    [#privacy_default_list{name = Default}] when Name == Default ->
				{error, 'conflict'};
			    _ ->
				gen_storage:delete_where(LServer, privacy_list,
							 [{'=', user_host, {LUser, LServer}},
							  {'=', name, Name}]),
				gen_storage:delete_where(LServer, privacy_list_data,
							 [{'=', user_host, {LUser, LServer}},
							  {'=', name, Name}]),
				{result, []}
			end
		end,
	    case gen_storage:transaction(LServer, privacy_list, F) of
		{atomic, {error, _} = Error} ->
		    Error;
		{atomic, {result, _} = Res} ->
		    ListString = lists:flatten(io_lib:format("~p.", [#userlist{name = Name, list = []}])),
		    ejabberd_router:route(
		      exmpp_jid:make(LUser, LServer),
		      exmpp_jid:make(LUser, LServer),
		      #xmlel{name = 'broadcast', ns = privacy_list,
			attrs = [?XMLATTR(<<"list_name">>, Name)],
			children = [exmpp_xml:cdata(ListString)]}),
		    Res;
		_ ->
		    {error, 'internal-server-error'}
	    end;
	List ->
	    F = fun() ->
			OldData =
			    gen_storage:select(LServer, privacy_list_data,
					       [{'=', user_host, {LUser, LServer}},
						{'=', name, Name}]),
			lists:foreach(
			  fun(Data1) ->
				  gen_storage:delete_object(LServer, Data1)
			  end, OldData),

			gen_storage:write(LServer, #privacy_list{user_host = {LUser, LServer},
								 name = Name}),
			NewData = list_items_to_data(LUser, LServer, Name, List),
			lists:foreach(
			  fun(Data1) ->
				  gen_storage:write(LServer, Data1)
			  end, NewData),
			{result, []}
		end,
	    case gen_storage:transaction(LServer, privacy_list, F) of
		{atomic, {error, _} = Error} ->
		    Error;
		{atomic, {result, _} = Res} ->
		    ListString = lists:flatten(io_lib:format("~p.", [#userlist{name = Name, list = List}])),
		    ejabberd_router:route(
		      exmpp_jid:make(LUser, LServer),
		      exmpp_jid:make(LUser, LServer),
		      #xmlel{name = 'broadcast', ns = privacy_list,
			attrs = [?XMLATTR(<<"list_name">>, Name)],
			children = [exmpp_xml:cdata(ListString)]}),
		    Res;
		_ ->
		    {error, 'internal_server_error'}
	    end
    end.



parse_items([]) ->
    remove;
parse_items(Els) ->
    parse_items(Els, []).

parse_items([], Res) ->
    %% Sort the items by their 'order' attribute
    %% lists:keysort(#listitem.order, Res);
    lists:reverse(Res);
parse_items([El = #xmlel{name = item} | Els], Res) ->
    Type   = exmpp_xml:get_attribute_as_list(El, <<"type">>, false),
    Value  = exmpp_xml:get_attribute_as_binary(El, <<"value">>, false),
    SAction =exmpp_xml:get_attribute_as_list(El, <<"action">>, false),
    SOrder = exmpp_xml:get_attribute_as_list(El, <<"order">>, false),
    Action = case catch list_to_action(SAction) of
		 {'EXIT', _} -> false;
		 Val -> Val
	     end,
    Order = case catch list_to_integer(SOrder) of
		{'EXIT', _} ->
		    false;
		IntVal ->
		    if
			IntVal >= 0 ->
			    IntVal;
			true ->
			    false
		    end
	    end,
    if
	(Action /= false) and (Order /= false) ->
	    I1 = #listitem{action = Action, order = Order},
	    I2 = case {Type, Value} of
		     {T,  V} when is_list(T), is_binary(V) ->
			 case T of
			     "jid" ->
				 I1#listitem{type = jid,
					     value = V};
			     "group" ->
				 I1#listitem{type = group,
					     value = V};
			     "subscription" ->
				 I1#listitem{type = subscription,
					     value = V}
			 end;
		     {T, false} when is_list(T) ->
			 false;
		     _ ->
			 I1
		 end,
	    case I2 of
		false ->
		    false;
		_ ->
		    case parse_matches(I2, exmpp_xml:get_child_elements(El)) of
			false ->
			    false;
			I3 ->
			    parse_items(Els, [I3 | Res])
		    end
	    end;
	true ->
	    false
    end;

parse_items(_, _Res) ->
    false.


parse_matches(Item, []) ->
    Item#listitem{match_all = true};
parse_matches(Item, Els) ->
    parse_matches1(Item, Els).

parse_matches1(Item, []) ->
    Item;
parse_matches1(Item, [#xmlel{name = message} | Els]) ->
    parse_matches1(Item#listitem{match_message = true}, Els);
parse_matches1(Item, [#xmlel{name = iq} | Els]) ->
    parse_matches1(Item#listitem{match_iq = true}, Els);
parse_matches1(Item, [#xmlel{name = 'presence-in'} | Els]) ->
    parse_matches1(Item#listitem{match_presence_in = true}, Els);
parse_matches1(Item, [#xmlel{name = 'presence-out'} | Els]) ->
    parse_matches1(Item#listitem{match_presence_out = true}, Els);
parse_matches1(_Item, [#xmlel{} | _Els]) ->
    false.



%% storage representation to ejabberd representation
list_data_to_items(Data) ->
    List =
	lists:map(
	  fun(Data1) ->
		  #listitem{type = Data1#privacy_list_data.type,
			    value = Data1#privacy_list_data.value,
			    action = Data1#privacy_list_data.action,
			    order = Data1#privacy_list_data.order,
			    match_all = Data1#privacy_list_data.match_all,
			    match_iq = Data1#privacy_list_data.match_iq,
			    match_message = Data1#privacy_list_data.match_message,
			    match_presence_in = Data1#privacy_list_data.match_presence_in,
			    match_presence_out = Data1#privacy_list_data.match_presence_out}
	  end, Data),
    SortedList = lists:keysort(#listitem.order, List),
    SortedList.

%% ejabberd representation to storage representation
list_items_to_data(LUser, LServer, Name, List) ->
    lists:map(
      fun(Item) ->
	      #privacy_list_data{user_host = {LUser, LServer},
				 name = Name,
				 type = Item#listitem.type,
				 value = Item#listitem.value,
				 action = Item#listitem.action,
				 order = Item#listitem.order,
				 match_all = Item#listitem.match_all,
				 match_iq = Item#listitem.match_iq,
				 match_message = Item#listitem.match_message,
				 match_presence_in = Item#listitem.match_presence_in,
				 match_presence_out = Item#listitem.match_presence_out}
      end, List).

is_list_needdb(Items) ->
    lists:any(
      fun(X) ->
	      case X#listitem.type of
		  subscription -> true;
		  group -> true;
		  _ -> false
	      end
      end, Items).

get_user_list(_, LUser, LServer)
        when is_binary(LUser), is_binary(LServer) ->
    F = fun() ->
		case gen_storage:read(LServer,
				      {privacy_default_list, {LUser, LServer}}) of
		    [] ->
			#userlist{};
		    [#privacy_default_list{name = Default}] ->
			Data = gen_storage:select(LServer, privacy_list_data,
						  [{'=', user_host, {LUser, LServer}},
						   {'=', name, Default}]),
			List = list_data_to_items(Data),
			NeedDb = is_list_needdb(List),
			#userlist{name = Default,
			          needdb = NeedDb,
				  list = List}
		end
	end,
    {atomic, Res} = gen_storage:transaction(LServer, privacy_default_list, F),
    Res.


%% From is the sender, To is the destination.
%% If Dir = out, User@Server is the sender account (From).
%% If Dir = in, User@Server is the destination account (To).
check_packet(_, _User, _Server,
	     _UserList,
	     {#jid{node = "", domain = Server} = _From,
              #jid{domain = Server} = _To,
              _},
	     in) ->
    allow;
check_packet(_, _User, _Server,
	     _UserList,
	     {#jid{domain = Server} = _From,
              #jid{node = "", domain = Server} = _To,
              _},
	     out) ->
    allow;
check_packet(_, _User, _Server,
	     _UserList,
	     {#jid{node = User, domain = Server} = _From,
              #jid{node = User, domain = Server} = _To,
              _},
	     _Dir) ->
    allow;
check_packet(_, User, Server,
	     #userlist{list = List, needdb = NeedDb},
	     {From, To, #xmlel{name = PName} = El},
	     Dir) when 
               PName =:= message ; 
		       PName =:= iq ;
		       PName =:= presence ->
    case List of
	[] ->
	    allow;
	_ ->
	    PType = case PName of
			'message' -> message;
			'iq' -> iq;
			'presence' ->
			    case exmpp_xml:get_attribute(El, <<"type">>, '') of
				%% notification
				'' -> presence;
				'unavailable' -> presence;
				%% subscribe, subscribed, unsubscribe,
				%% unsubscribed, error, probe, or other
				_ -> other
			    end
		    end,
	    PType2 = case {PType, Dir} of
			 {message, in} -> message;
			 {iq, in} -> iq;
			 {presence, in} -> presence_in;
			 {presence, out} -> presence_out;
			 {_, _} -> other
		     end,
	    LJID = case Dir of
		       in -> From;
		       out -> To
		   end,
	    {Subscription, Groups} =
		case NeedDb of
		    true -> ejabberd_hooks:run_fold(roster_get_jid_info,
						    exmpp_stringprep:nameprep(Server),
						    {none, []},
						    [User, Server, LJID]);
		    false -> {[], []}
		end,
	    check_packet_aux(List, PType2, LJID, Subscription, Groups)
    end.

%% Ptype = mesage | iq | presence_in | presence_out | other
check_packet_aux([], _PType, _JID, _Subscription, _Groups) ->
    allow;
check_packet_aux([Item | List], PType, JID, Subscription, Groups) ->
    #listitem{type = Type, value = Value, action = Action} = Item,
    case is_ptype_match(Item, PType) of
	true ->
	    case Type of
		none ->
		    Action;
		_ ->
		    case is_type_match(Type, Value,
				       JID, Subscription, Groups) of
			true ->
			    Action;
			false ->
			    check_packet_aux(List, PType,
					     JID, Subscription, Groups)
		    end
	    end;
	false ->
	    check_packet_aux(List, PType, JID, Subscription, Groups)
    end.


is_ptype_match(Item, PType) ->
    case Item#listitem.match_all of
	true ->
	    true;
	false ->
	    case PType of
		message ->
		    Item#listitem.match_message;
		iq ->
		    Item#listitem.match_iq;
		presence_in ->
		    Item#listitem.match_presence_in;
		presence_out ->
		    Item#listitem.match_presence_out;
		other ->
		    false
	    end
    end.


%% TODO: Investigate this: sometimes Value has binaries, other times has strings
is_type_match(jid, Value, JID, _Subscription, _Groups) ->
    {User, Server, Resource} = exmpp_jid:to_lower(exmpp_jid:parse(Value)),
    ((User == undefined) orelse (User == exmpp_jid:prep_node(JID)))
	andalso ((Server == undefined) orelse (Server == exmpp_jid:prep_domain(JID)))
        andalso ((Resource == undefined) orelse (Resource == exmpp_jid:prep_resource(JID)));
is_type_match(subscription, Value, _JID, Subscription, _Groups) ->
    Value == Subscription;
is_type_match(group, Value, _JID, _Subscription, Groups) ->
    lists:member(Value, Groups).


%% The ejabberd hook provides the arguments as binaries, 
%% but the mod_privacy internal functions provide them as strings.
%% Once this module stores information as binaries, this incoherence will be solved.
remove_user(User, Server) when is_list(User) and is_list(Server) ->
	remove_user(list_to_binary(User), list_to_binary(Server));
remove_user(User, Server) when is_binary(User) and is_binary(Server) ->
    LUser = exmpp_stringprep:nodeprep(User),
    LServer = exmpp_stringprep:nameprep(Server),
    F = fun() ->
		gen_storage:delete(LServer, {privacy_list, {LUser, LServer}})
	end,
    case gen_storage:transaction(LServer, privacy_list, F) of
	{atomic, _} ->
	    {result, []};
	_ ->
	    {error, 'internal-server-error'}
    end.


updated_list(_,
	     #userlist{name = OldName} = Old,
	     #userlist{name = NewName} = New) ->
    if
	OldName == NewName ->
	    New;
	true ->
	    Old
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

update_tables(global, Storage) ->
    [update_tables(HostB, Storage) || HostB <- ejabberd_hosts:get_hosts(ejabberd)];

update_tables(HostB, mnesia) ->
    gen_storage_migration:migrate_mnesia(
      HostB, privacy_default_list,
      [{privacy, [us, default, lists],
	fun({privacy, {U, S}, Default, Lists}) ->
		US = {list_to_binary(U), list_to_binary(S)},
		lists:foreach(
		  fun({Name, List}) ->
			  NameBin = list_to_binary(Name),
			  lists:foreach(
			    fun(#listitem{type = Type,
					  value = Value,
					  action = Action,
					  order = Order,
					  match_all = MatchAll,
					  match_iq = MatchIq,
					  match_message = MatchMessage,
					  match_presence_in = MatchPresenceIn,
					  match_presence_out = MatchPresenceOut}) ->
				    ValueBin = convert_value_to_binary(Value),
				    gen_storage:write(HostB,
						      #privacy_list_data{user_host = US,
									 name = NameBin,
									 type = Type,
									 value = ValueBin,
									 action = Action,
									 order = Order,
									 match_all = MatchAll,
									 match_iq = MatchIq,
									 match_message = MatchMessage,
									 match_presence_in = MatchPresenceIn,
									 match_presence_out = MatchPresenceOut})
			    end, List),
			  gen_storage:write(HostB,
					    #privacy_list{user_host = US,
							  name = NameBin})
		  end, Lists),
		if
		    is_list(Default) ->
		        DefaultBin = list_to_binary(Default),
			#privacy_default_list{user_host = US,
					      name = DefaultBin};
		    true -> null
		end
	end}]);

update_tables(Host, odbc) ->
    gen_storage_migration:migrate_odbc(
      Host, [privacy_default_list, privacy_list, privacy_list_data],
      [{[{"privacy_list", ["username", "name", "id"]},
	 {"privacy_list_data", ["id","t","value","action","ord","match_all","match_iq","match_message",
     "match_presence_in","match_presence_out"]}],
	fun(SELECT, Username, Name, Id) ->
		US = {Username, Host},
		DefaultLists = [#privacy_default_list{user_host = US,
						      name = Name}
				|| [_, _] <- SELECT(["username", "name"],
						    "privacy_default_list",
						    [{"username", Username},
						     {"name", Name}])],
		ListData = lists:map(
			     fun([SType, SValue, SAction, SOrder, SMatchAll,
				  SMatchIQ, SMatchMessage, SMatchPresenceIn,
				  SMatchPresenceOut]) ->
				     {Type, Value} =
					 case SType of
					     "n" ->
						 {none, none};
					     "j" ->
						 case exmpp_jid:parse(SValue) of
						     JID ->
							 {jid, jlib:short_prepd_jid(JID)}
						 end;
					     "g" ->
						 {group, SValue};
					     "s" ->
						 case SValue of
						     "none" ->
							 {subscription, none};
						     "both" ->
							 {subscription, both};
						     "from" ->
							 {subscription, from};
						     "to" ->
							 {subscription, to}
						 end
					 end,
				     ValueBin = convert_value_to_binary(Value),
				     Action =
					 case SAction of
					     "a" -> allow;
					     "d" -> deny
					 end,
				     Order = list_to_integer(SOrder),
				     MatchAll = ejabberd_odbc:to_bool(SMatchAll),
				     MatchIQ = ejabberd_odbc:to_bool(SMatchIQ),
				     MatchMessage = ejabberd_odbc:to_bool(SMatchMessage),
				     MatchPresenceIn = ejabberd_odbc:to_bool(SMatchPresenceIn),
				     MatchPresenceOut = ejabberd_odbc:to_bool(SMatchPresenceOut),
				     #privacy_list_data{user_host = US,
							name = Name,
							type = Type,
							value = ValueBin,
							action = Action,
							order = Order,
							match_all = MatchAll,
							match_iq = MatchIQ,
							match_message = MatchMessage,
							match_presence_in = MatchPresenceIn,
							match_presence_out = MatchPresenceOut}
			     end, SELECT(["t", "value", "action", "ord", "match_all",
					  "match_iq", "match_message", "match_presence_in",
					  "match_presence_out"],
					 "privacy_list_data",
					 [{"id", Id}])),
		[#privacy_list{user_host = US,
			       name = Name} | DefaultLists ++ ListData]
       end},
       {"privacy_default_list", ["username", "name"],
       fun(_, Username, Name) ->
               US = {Username, Host},
               [#privacy_default_list{user_host = US,
                                      name = Name}]
	end}
      ]).

convert_value_to_binary({U, H, R}) ->
    exmpp_jid:to_binary(U, H, R);
convert_value_to_binary(Value) when is_list(Value) ->
    list_to_binary(Value);
convert_value_to_binary(none) ->
    <<"none">>;
convert_value_to_binary(both) ->
    <<"both">>;
convert_value_to_binary(from) ->
    <<"from">>;
convert_value_to_binary(to) ->
    <<"to">>.
