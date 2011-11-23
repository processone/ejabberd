-module(mod_pubsub_minimal).

%% One global mnesia Disc-copies table holding topic names and who can publish
%%     This way each ejabberd node have the same view of the existing topics
%% One process per node listening mnesia events, so if a topic is removed, we can
%% cleanup locally. 
%% One mnesia ram, local_copies table, holding all (allways temporarly) subscribers. 
%% One mnesia ram, local_copies table, holding last published item.
%%
%% on publish  -> 
%%		1) check publisher has right
%%              2) publish on all ejabberd nodes
%%
%% on subscribe ->
%%		1) check if node exists
%%		2) add entry to mnesia
%%		3) monitor pid
%%		4) retrieve last published item and send
%%
%% on create node ->
%%		1) check if node already exists
%%		2) create
%%
%% on delete node ->
%%		1) check rights
%%		2) delete from mnesia distributed table


-include("ejabberd.hrl").
-include("jlib.hrl").

-export([start/2, stop/1]).

%% gen_server callbacks
-export([start_link/2, init/1, handle_info/2, handle_cast/2, handle_call/3, terminate/2]).

%% routing
-export([route/3]).


%% rpc api
-export([do_local_publish/3]).




-record(topic, {name, admins}).
-record(topic_subscribers, {key, pid, jid, topic}).

-record(state, {host, pubsub_host, create_node_acl}).

start(Host, Opts) ->
    Proc = gen_mod:get_module_proc(Host, ?MODULE),
    ChildSpec = {Proc,
		 {?MODULE, start_link, [Host, Opts]},
		 transient, 1000, worker, [?MODULE]},
    supervisor:start_child(ejabberd_sup, ChildSpec).

stop(Host) ->
    Proc = gen_mod:get_module_proc(Host, ?MODULE),
    gen_server:call(Proc, stop),
    supervisor:delete_child(ejabberd_sup, Proc).


start_link(Host, Opts) ->
	Proc = gen_mod:get_module_proc(Host, ?MODULE),
	gen_server:start_link({local, Proc}, ?MODULE, [Host, Opts], []).


init([Domain, Opts]) ->
	PubsubHost = gen_mod:get_opt_host(Domain, Opts, "pubsub.@HOST@"),
	Access = gen_mod:get_opt(access_createnode, Opts, all),
	mnesia:create_table(topic, [
			{disc_copies,[node()]}, 
			{type,set}, 
			{attributes, record_info(fields, topic)}
		]),
	mnesia:subscribe({table, topic, simple}),
	mnesia:add_table_copy(topic, node(), disc_copies),
	mnesia:create_table(topic_subscribers, [
			{ram_copies, [node()]}, 
			{local_content, true}, 
			{type, set}, 
			{attributes, record_info(fields, topic_subscribers)}
		]),
	mnesia:add_table_index(topic_subscribers, topic),
	mnesia:add_table_index(topic_subscribers, pid),
	ejabberd_router:register_route(PubsubHost, {apply, ?MODULE, route}),

	%% in case the module is restarted, monitor again any subscriber, so we can remove them at logout.
	catch [erlang:monitor(process, Pid) || Pid <-
		mnesia:dirty_select(topic_subscribers,[{#topic_subscribers{jid='_', topic='_', key='_', pid='$1'}, [], ['$1']}])],
	{ok, #state{create_node_acl = Access, host = Domain, pubsub_host = PubsubHost}}.

handle_cast({monitor, Pid}, State) ->
	_Ref = erlang:monitor(process, Pid),
	{noreply, State}.

handle_call({create_node, From, Topic}, _,  State = #state{create_node_acl = Access, host = Host}) ->
	case acl:match_rule(Host, Access, From) of
		allow ->
			{reply, create_node(From, Topic), State};
		deny ->
			{reply, {error, ?ERR_NOT_ALLOWED}, State}
	end;
handle_call(stop, _From,  State) ->
	{stop, normal, ok, State}.

%% a subscriber process die
handle_info({'DOWN', _MonitorRef, process, Pid, _Info}, State) ->
	Objs = mnesia:dirty_index_read(topic_subscribers, Pid, #topic_subscribers.pid),
	[mnesia:dirty_delete({topic_subscribers, Key}) || #topic_subscribers{key = Key} <- Objs],
	{noreply, State};

%% TODO
handle_info({mnesia_table_event, {delete_object, #topic{name=Topic}, _ActivityId}}, State) ->
	Subs = mnesia:dirty_index_read(topic_subscribers, Topic, #topic_subscribers.topic),
	[mnesia:dirty_delete_object(S) || S <- Subs],
	{noreply, State};
handle_info({mnesia_table_event, {delete, {topic,Topic}, _ActivityId}}, State) ->
	Subs = mnesia:dirty_index_read(topic_subscribers, Topic, #topic_subscribers.topic),
	[mnesia:dirty_delete_object(S) || S <- Subs],
	{noreply, State};
handle_info({mnesia_table_event, _}, State) ->
	{noreply, State};
handle_info(Info, State) ->
	?ERROR_MSG("Unknown info received ~p", [Info]),
	{noreply, State}.


terminate(_Reason, #state{pubsub_host = PubSubHost}) ->
	ejabberd_router:unregister_route(PubSubHost),
	ok.



%% route
%%    four cases: 
%%		create node
%%		subscribe
%%		publish
%%		delete node

route(From, #jid{server = PubSubHost} = To, {xmlelement, "iq", _Attrs, _Children} = Packet) ->
	IQReply = case jlib:iq_query_info(Packet) of
		#iq{xmlns = ?NS_PUBSUB} = IQ ->
			case iq_pubsub(From, PubSubHost, IQ) of
				{ok, Reply} ->  jlib:iq_to_xml(IQ#iq{type=result, sub_el = Reply});
				{error, Error} -> jlib:make_error_reply(Packet, Error)
			end;
		_ ->
			?INFO_MSG("IQ not implemented ~p", [Packet]),
			jlib:iq_to_xml(jlib:make_error_reply(Packet,?ERR_FEATURE_NOT_IMPLEMENTED))
	end,
	ejabberd_router:route(To, From, IQReply);

route(_, _ , Packet) ->
	?INFO_MSG("Message to pubsub service (discarded) ~p", [Packet]).


%% create node | publish | subscribe
iq_pubsub(From =#jid{server = S}, PubSubHost, #iq{type = 'set', sub_el = SubEl}) ->
	{xmlelement, _, _, SubEls} = SubEl,
	Action = xml:remove_cdata(SubEls),
	case Action of
		[{xmlelement, "publish", Attrs, Children}] ->
			Node = xml:get_attr_s("node", Attrs),
			[Item] = xml:remove_cdata(Children),
			publish(From, PubSubHost, Node, Item);
		[{xmlelement, "subscribe", Attrs, _}] ->
			Node = xml:get_attr_s("node", Attrs),
			subscribe(From, Node);
		[{xmlelement, "create", Attrs, _}, {xmlelement, "configure", _, _}] ->
			Node = xml:get_attr_s("node", Attrs),
			%% TODO: only local.
			gen_server:call(gen_mod:get_module_proc(S, ?MODULE),  {create_node, From, Node});
		_ ->
		    ?INFO_MSG("Invalid pubsub action: ~p", [Action]),
		    {error, ?ERR_BAD_REQUEST}
	end.



%%%%%%%           Publication  functions  %%%%%%%
build_publish_reply(Node, ItemId) ->
	[{xmlelement, "pubsub", [{"xmlns", ?NS_PUBSUB}], 
		    [{xmlelement, "publish", [{"node", Node}], 
				    [{xmlelement, "item", [{"id", ItemId}], []}]}]}].

complete_id({xmlelement, Name, Attrs, Children}=Item) ->
	case xml:get_attr("id", Attrs) of
		{value, ID} -> 
			{Item, ID};
		false ->
			ID = uniqid(),
			{{xmlelement, Name, [{"id", ID} | Attrs], Children}, ID}
	end.

%% publish to subscribers on all nodes
publish(From, PubSubHost, Topic, Item) ->
	{NewItem, ItemID} = complete_id(Item),
	case mnesia:dirty_read({topic, Topic}) of
		[#topic{admins = Admins}] ->
			case lists:member(From, Admins) of
				true ->
					%%TODO: item tengo que ponerle el id
					[rpc:cast(N, ?MODULE, do_publish, [PubSubHost, Topic, NewItem]) || N <- nodes(), N /= node()],
					do_local_publish(PubSubHost, Topic, NewItem),
					{ok, build_publish_reply(Topic, ItemID)};
				false ->
					{error, ?ERR_NOT_AUTHORIZED}
			end;
		 [] ->
			 {error, ?ERR_ITEM_NOT_FOUND}
	end.

%% publish to local subscribers
do_local_publish(PubsubHost, Topic, Item) ->
	Message = {xmlelement, "message", [], 
			[{xmlelement, "event", [{"xmlns", ?NS_PUBSUB_EVENT}], 
				[{xmlelement,"items", [{"node", Topic}],[Item]}]}]},

	Subs = mnesia:dirty_index_read(topic_subscribers, Topic, #topic_subscribers.topic),

	%% don't let the vm do context switch to the reciving process until we multicasted to all.
	OldPriority = process_flag(priority, high),
	[ Pid ! {route, jlib:make_jid("",PubsubHost,""), JID, Message}   || #topic_subscribers{pid = Pid, jid=JID} <- Subs],
	process_flag(priority, OldPriority).



%%%%%%%           Subscription  functions  %%%%%%%
build_subscribe_reply(JID, Topic) ->
	Fields = [{"jid", jlib:jid_to_string(JID)},{"node", Topic}],
	[{xmlelement, "pubsub", [{"xmlns", ?NS_PUBSUB}], 
			[{xmlelement, "subscription", Fields, []}]}].

subscribe(#jid{user=U, server=S, resource=R}=From, Topic) ->
	case mnesia:dirty_read({topic, Topic}) of
		[] ->
			{error, ?ERR_ITEM_NOT_FOUND};
		[#topic{}] ->
			case ejabberd_sm:get_session_pid(U, S, R) of
				none ->
					%%the client went offline, forget it.
					ok;
				Pid ->
					mnesia:dirty_write(
						#topic_subscribers{key =  {Pid, Topic},
								  topic = Topic, 
								  jid = From,
								  pid = Pid}),

	    			  	%% TODO: this will only work on local users, and with only 1 domain.
					%%       to fix it we need to know the host at which the subscribe
					%%       packet is directed.
					gen_server:cast(gen_mod:get_module_proc(S, ?MODULE), {monitor, Pid})
			end,
			{ok, build_subscribe_reply(From, Topic)}
	end.
	

%%%%%%%           Topic creation  functions  %%%%%%%
create_node(From, Node) ->
	case mnesia:dirty_read({topic, Node}) of
		[] ->
			%% this is a full transaction, we don't want to have conflict on cluster,
			%% and we want to be sure than when returning IQ to owner, the node is
			%% really created
			case mnesia:transaction(fun() ->
						mnesia:write(#topic{name = Node, admins = [From]}),
						ok
					end) of
				{atomic,ok} ->
					{ok, []};
				Error ->
					?ERROR_MSG("Error creating topic: ~p", [Error]),
					{error, ?ERR_INTERNAL_SERVER_ERROR}
			end;
		[_|_] ->
			{error, ?ERR_CONFLICT}
	end.


uniqid() ->
    {T1, T2, T3} = now(),
    lists:flatten(io_lib:fwrite("~.16B~.16B~.16B", [T1, T2, T3])).
