%%% ====================================================================
%%% This software is copyright 2006-2010, ProcessOne.
%%%
%%% mod_filter
%%% allow message filtering using regexp on message body
%%% THIS MODULE NEEDS A PATCHED ERLANG VM AGAINST
%%% THE PCRE PATCH AND NEEDS LIBPCRE INSTALLED
%%% ejabberd MUST USE THAT PATCHED ERLANG VM
%%% BUT, if patch is not available, mod_filter uses re.beam module
%%% instead, with speed degradation.
%%%
%%% @copyright 2006-2010 ProcessOne
%%% @author Christophe Romain <christophe.romain@process-one.net>
%%%   [http://www.process-one.net/]
%%% @version {@vsn}, {@date} {@time}
%%% @end
%%% ====================================================================

-module(mod_filter).
-author('christophe.romain@process-one.net').
-vsn('$Id: mod_filter.erl 972 2010-10-14 21:53:56Z jpcarlino $').

-behaviour(gen_mod).

% module functions
-export([start/2,stop/1,init/2,update/2,is_loaded/0,loop/5]).
-export([add_regexp/4,add_regexp/3,del_regexp/3,del_regexp/2]).
-export([purge_logs/0,purge_regexps/1,reload/1]).
-export([logged/0,logged/1,rules/0]).
-export([process_local_iq/3]).

% handled ejabberd hooks
-export([filter_packet/1]).

-include("ejabberd.hrl").
-include("jlib.hrl").
-include("licence.hrl").

-record(filter_rule, {id, type="filter", regexp, binre}).
-record(filter_log, {date, from, to, message}).

-define(TIMEOUT, 5000). % deliver message anyway if filter does not respond after 5s
-define(PROCNAME(VH), list_to_atom(VH++"_message_filter")).
-define(NS_FILTER, "p1:iq:filter").

-define(ALLHOSTS, "all hosts"). %% must be sync with filter.erl

start(Host, Opts) ->
    case ?IS_VALID of
    true ->
	mnesia:create_table(filter_rule, [
					{disc_copies, [node()]}, {type, set},
					{attributes, record_info(fields, filter_rule)} ]),
	mnesia:create_table(filter_log, [
					{disc_only_copies, [node()]}, {type, bag},
					{attributes, record_info(fields, filter_log)} ]),
	%% this force the last code to be used
	case whereis(?PROCNAME(Host)) of 
	    undefined -> 
		ok;
	    _ ->
		ejabberd_hooks:delete(filter_packet, ?MODULE, filter_packet, 10),
		gen_iq_handler:remove_iq_handler(ejabberd_local, Host, ?NS_FILTER),
		?PROCNAME(Host) ! quit
	end,
	ejabberd_hooks:add(filter_packet, ?MODULE, filter_packet, 10),
	gen_iq_handler:add_iq_handler(ejabberd_local, Host, ?NS_FILTER,
				    ?MODULE, process_local_iq, one_queue),
	case whereis(?PROCNAME(Host)) of
	    undefined -> register(?PROCNAME(Host), spawn(?MODULE, init, [Host, Opts]));
	    _ -> ok
	end,
	%% start the all_alias handler
	case whereis(?PROCNAME(?ALLHOSTS)) of
	    undefined -> init_all_hosts_handler();
	    _ -> ok
	end,
	start;
    false ->
	not_started
    end.

stop(Host) ->
	ejabberd_hooks:delete(filter_packet, ?MODULE, filter_packet, 10),
	gen_iq_handler:remove_iq_handler(ejabberd_local, Host, ?NS_FILTER),
	exit(whereis(?PROCNAME(Host)), kill),
	{wait, ?PROCNAME(Host)}.

% this is used by team_leader to check code presence
is_loaded() ->
	ok.

% this loads rules and return {Types, BinRegExps}
% RegExps are text regexp list
% Types are regexp type list
% both list share the same ordered
load_rules(Host) ->
	Rules = mnesia:dirty_match_object(#filter_rule{id={'_', Host}, _ = '_'})
		 ++ mnesia:dirty_match_object(#filter_rule{id={'_', ?ALLHOSTS}, _ = '_'}),
	lists:map(fun({filter_rule, _, Type, _, BinRegExp}) -> {Type, BinRegExp} end, Rules).
	%lists:unzip(Config).

% this call init or reset local rules reading database
init(Host, Opts) ->
	Rules = load_rules(Host),
	Scope = gen_mod:get_opt(scope, Opts, message),
	Pattern = gen_mod:get_opt(pattern, Opts, ""),
	?MODULE:loop(Host, Opts, Rules, Scope, Pattern).

init_all_hosts_handler() ->
	register(?PROCNAME(?ALLHOSTS), spawn(?MODULE, loop, [?ALLHOSTS, [], [], none, []])).

% this call reset local rules reading database
% and tell other nodes to reset rules as well
update(?ALLHOSTS, _Opts) ->
	lists:foreach(fun(Host) ->
		lists:foreach(fun(Node) ->
			catch rpc:call(Node,mod_filter,reload,[Host])
		end, mnesia:system_info(running_db_nodes))
	end, ejabberd_config:get_global_option(hosts)),
	?MODULE:loop(?ALLHOSTS, [], [], none, []);
update(Host, Opts) ->
	% tell other nodes to update filter
	lists:foreach(fun(Node) ->
		catch rpc:call(Node,mod_filter,reload,[Host])
	end, mnesia:system_info(running_db_nodes)--[node()]),
	% update rules
	init(Host, Opts).

loop(Host, Opts, Rules, Scope, Pattern) ->
	receive
	{add, Id, RegExp} ->
		try
		    [BinRegExp] = tlre:compile([RegExp]),
		    ?INFO_MSG("Adding new filter rule with regexp=~p", [RegExp]),
		    mnesia:dirty_write(#filter_rule{id={Id, Host}, regexp=RegExp, binre=BinRegExp}),
		    ?MODULE:update(Host, Opts)
		catch
		    Class:Reason ->
			?INFO_MSG("~p can't add filter rule with regexp=~p for id=~p. Reason: ~p", [Class, RegExp, Id, Reason]),
			loop(Host, Opts, Rules, Scope, Pattern)
		end;
	{add, Id, RegExp, Type} ->
		try
		    [BinRegExp] = tlre:compile([RegExp]),
		    ?INFO_MSG("Adding new filter rule with regexp=~p", [RegExp]),
		    mnesia:dirty_write(#filter_rule{id={Id, Host}, regexp=RegExp, binre=BinRegExp, type=Type}),
		    ?MODULE:update(Host, Opts)
		catch
		    Class:Reason ->
			?INFO_MSG("~p can't add filter rule with regexp=~p for id=~p with type=~p. Reason: ~p", [Class, RegExp, Id, Type, Reason]),
			loop(Host, Opts, Rules, Scope, Pattern)
		end;
	{del, Id} ->
		RulesToRemove = mnesia:dirty_match_object(#filter_rule{id={Id, Host}, _='_'}),
		lists:foreach(fun(Rule) ->
                      mnesia:dirty_delete_object(Rule)
		end, RulesToRemove),
		?MODULE:update(Host, Opts);
	{del, Id, RegExp} ->
		RulesToRemove = mnesia:dirty_match_object(#filter_rule{id={Id, Host}, regexp=RegExp, _='_'}),
		lists:foreach(fun(Rule) ->
                      mnesia:dirty_delete_object(Rule)
		end, RulesToRemove),
		?MODULE:update(Host, Opts);
	{match, From, String} ->
		From ! {match, string_filter(String, Rules, Scope, Pattern)},
		?MODULE:loop(Host, Opts, Rules, Scope, Pattern);
	reload ->
		?MODULE:init(Host, Opts);
	quit ->
		unregister(?PROCNAME(Host)),
		ok
	end.

string_filter(String, Rules, Scope, Pattern) ->
    lists:foldl(fun
    (_, {Pass, []}) -> {Pass, []};
    ({Type, RegExp}, {Pass, NewString}) -> string_filter(NewString, Pass, RegExp, Type, Scope, Pattern)
    end, {"pass", String}, Rules).
string_filter(String, Pass, RegExp, Type, Scope, Pattern) ->
    case tlre:grep(String, [RegExp]) of
    [no_match] ->
	{Pass, String};
    [{S1, S2, _}] ->
	case Scope of
	word ->
	    Start = string:sub_string(String, 1, S1),
	    StringTail = string:sub_string(String, S2+1, length(String)),
	    NewPass = pass_rule(Pass, Type),
	    {LastPass, End} = string_filter(StringTail, NewPass, RegExp, Type, Scope, Pattern),
	    NewString = case Type of
	    "log" -> lists:append([string:sub_string(String, 1, S2), End]);
	    _ -> lists:append([Start, Pattern, End])
	    end,
	    {LastPass, NewString};
	_ ->
	    NewString = case Type of
	    "log" -> String;
	    _ -> []
	    end,
	    {pass_rule(Pass, Type), NewString}
	end
    end.

pass_rule("pass", New) -> New;
pass_rule("log", "log") -> "log";
pass_rule("log", "log and filter") -> "log and filter";
pass_rule("log", "filter") -> "log and filter";
pass_rule("filter", "log") -> "log and filter";
pass_rule("filter", "log and filter") -> "log and filter";
pass_rule("filter", "filter") -> "filter";
pass_rule("log and filter", _) -> "log and filter".

add_regexp(VH, Id, RegExp) ->
	?PROCNAME(VH) ! {add, Id, RegExp},
	ok.

add_regexp(VH, Id, RegExp, Type) ->
	?PROCNAME(VH) ! {add, Id, RegExp, Type},
	ok.

del_regexp(VH, Id) ->
	?PROCNAME(VH) ! {del, Id},
	ok.

del_regexp(VH, Id, RegExp) ->
	?PROCNAME(VH) ! {del, Id, RegExp},
	ok.

reload(VH) ->
	?PROCNAME(VH) ! reload,
	ok.

purge_logs() ->
	mnesia:dirty_delete_object(#filter_log{_='_'}).

%purge_regexps() ->
%	mnesia:dirty_delete_object(#filter_rule{_='_'}),
%   reload().

purge_regexps(VH) ->
	mnesia:dirty_delete_object(#filter_rule{id={'_', VH}, _='_'}),
	reload(VH).

rules() ->
	lists:map(fun(#filter_rule{id={Label, VH}, type=Type, regexp=Regexp}) ->
		{VH, Label, Type, Regexp}
	end, mnesia:dirty_match_object(#filter_rule{_='_'})).
 
logged() ->
	lists:reverse(lists:map(fun(#filter_log{date=Date, from=From, to=To, message=Msg}) ->
		{Date, jlib:jid_to_string(From), jlib:jid_to_string(To), Msg}
	end, mnesia:dirty_match_object(#filter_log{_='_'}))).

logged(Limit) ->
	List = mnesia:dirty_match_object(#filter_log{_='_'}),
	Len = length(List),
	FinalList = if 
		Len < Limit -> List;
		true -> lists:nthtail(Len-Limit, List)
	end,
	lists:reverse(lists:map(fun(#filter_log{date=Date, from=From, to=To, message=Msg}) ->
		{Date, jlib:jid_to_string(From), jlib:jid_to_string(To), Msg}
	end, FinalList)).

%% filter_packet can receive drop if a previous filter already dropped
%% the packet
filter_packet(drop) -> drop;
filter_packet({From, To, Packet}) ->
	case Packet of
	{xmlelement, "message", MsgAttrs, Els} ->
		case lists:keysearch("body", 2, Els) of
		{value, {xmlelement, "body", BodyAttrs, Data}} ->
			NewData = lists:foldl(fun
			({xmlcdata, CData}, DataAcc) when is_binary(CData) ->
				#jid{lserver = Host} = To,
				case lists:member(Host, ejabberd_config:get_global_option(hosts)) of
				true ->
					Msg = binary_to_list(CData),
					?PROCNAME(Host) ! {match, self(), Msg},
					receive
					{match, {"pass", _}} ->
						[{xmlcdata, CData}|DataAcc];
					{match, {"log", _}} ->
						mnesia:dirty_write(#filter_log{
							date=erlang:localtime(),
							from=From, to=To, message=Msg}),
						[{xmlcdata, CData}|DataAcc];
					{match, {"log and filter", FinalString}} ->
						mnesia:dirty_write(#filter_log{
							date=erlang:localtime(),
							from=From, to=To, message=Msg}),
						case FinalString of
						[] -> % entire message is dropped
							DataAcc;
						S -> % message must be regenerated
							[{xmlcdata, list_to_binary(S)}|DataAcc]
						end;
					{match, {"filter", FinalString}} ->
						case FinalString of
						[] -> % entire message is dropped
							DataAcc;
						S -> % message must be regenerated
							[{xmlcdata, list_to_binary(S)}|DataAcc]
						end
					after ?TIMEOUT -> 
						[{xmlcdata, CData}|DataAcc]
					end;
				false ->
					[{xmlcdata, CData}|DataAcc]
				end;
			(Item, DataAcc) -> %% to not filter internal messages
			    [Item|DataAcc]
			end, [], Data),
			case NewData of
			    [] -> 
				drop;
			    D -> 
				NewEls = lists:keyreplace("body", 2, Els, {xmlelement, "body", BodyAttrs, lists:reverse(D)}),
				{From, To, {xmlelement, "message", MsgAttrs, NewEls}}
			end;
		    _ ->
			{From, To, Packet}
		end;
	    _ ->
		{From, To, Packet}
	end.

process_local_iq(From, #jid{lserver=VH} = _To, #iq{type = Type, sub_el = SubEl} = IQ) ->
	case Type of
	get ->
		IQ#iq{type = error, sub_el = [SubEl, ?ERR_NOT_ALLOWED]};
	set ->
		#jid{luser = User, lserver = Server, lresource = Resource} = From,
		case acl:match_rule(global, configure, {User, Server, Resource}) of
		allow ->
			case xml:get_subtag(SubEl, "add") of
			{xmlelement, "add", AddAttrs, _} ->
				AID = xml:get_attr_s("id", AddAttrs),
				ARE = xml:get_attr_s("re", AddAttrs),
				case xml:get_attr_s("type", AddAttrs) of
				"" -> add_regexp(VH, AID, ARE);
				ATP -> add_regexp(VH, AID, ARE, ATP)
				end;
			_ -> ok
			end,
			case xml:get_subtag(SubEl, "del") of
			{xmlelement, "del", DelAttrs, _} ->
				DID = xml:get_attr_s("id", DelAttrs),
				case xml:get_attr_s("re", DelAttrs) of
				"" -> del_regexp(VH, DID);
				DRE -> del_regexp(VH, DID, DRE)
				end;
			_ -> ok
			end,
			case xml:get_subtag(SubEl, "dellogs") of
			{xmlelement, "dellogs", _, _} -> purge_logs();
			_ -> ok
			end,
			case xml:get_subtag(SubEl, "delrules") of
			{xmlelement, "delrules", _, _} -> purge_regexps(VH);
			_ -> ok
			end,
			IQ#iq{type = result, sub_el = []};
		_ ->
			IQ#iq{type = error, sub_el = [SubEl, ?ERR_NOT_ALLOWED]}
		end
	end.

