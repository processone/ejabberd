-module(eldap).
%%% --------------------------------------------------------------------
%%% Created:  12 Oct 2000 by Tobbe <tnt@home.se>
%%% Function: Erlang client LDAP implementation according RFC 2251.
%%%           The interface is based on RFC 1823, and
%%%           draft-ietf-asid-ldap-c-api-00.txt
%%%
%%% Copyright (C) 2000  Torbjorn Tornkvist, tnt@home.se
%%% 
%%%
%%% This program is free software; you can redistribute it and/or modify
%%% it under the terms of the GNU General Public License as published by
%%% the Free Software Foundation; either version 2 of the License, or
%%% (at your option) any later version.
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License along
%%% with this program; if not, write to the Free Software Foundation, Inc.,
%%% 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.

%%% Modified by Sean Hinde <shinde@iee.org> 7th Dec 2000
%%% Turned into gen_fsm, made non-blocking, added timers etc to support this.
%%% Now has the concept of a name (string() or atom()) per instance which allows
%%% multiple users to call by name if so desired.
%%%
%%% Can be configured with start_link parameters or use a config file to get
%%% host to connect to, dn, password, log function etc.

%%% Modified by Alexey Shchepin <alexey@sevcom.net>

%%% Modified by Evgeniy Khramtsov <ekhramtsov@process-one.net>
%%% Implemented queue for bind() requests to prevent pending binds.
%%% Implemented extensibleMatch/2 function.
%%% Implemented LDAP Extended Operations (currently only Password Modify
%%%   is supported - RFC 3062).

%%% Modified by Christophe Romain <christophe.romain@process-one.net>
%%% Improve error case handling

%%% Modified by Mickael Remond <mremond@process-one.net>
%%% Now use ejabberd log mechanism

%%% Modified by:
%%%   Thomas Baden <roo@ham9.net> 2008 April 6th
%%%   Andy Harb <Ahmad.N.Abou-Harb@jpl.nasa.gov> 2008 April 28th
%%%   Anton Podavalov <a.podavalov@gmail.com> 2009 February 22th
%%% Added LDAPS support, modeled off jungerl eldap.erl version.
%%% NOTICE: STARTTLS is not supported.

%%% --------------------------------------------------------------------
-vc('$Id$ ').

%%%----------------------------------------------------------------------
%%% LDAP Client state machine.
%%% Possible states are:
%%%     connecting - actually disconnected, but retrying periodically
%%%     wait_bind_response  - connected and sent bind request
%%%     active - bound to LDAP Server and ready to handle commands
%%%     active_bind - sent bind() request and waiting for response
%%%----------------------------------------------------------------------

-behaviour(gen_fsm).

-include("ejabberd.hrl").
-include("logger.hrl").

%% External exports
-export([start_link/1, start_link/6]).

-export([baseObject/0, singleLevel/0, wholeSubtree/0,
	 close/1, equalityMatch/2, greaterOrEqual/2,
	 lessOrEqual/2, approxMatch/2, search/2, substrings/2,
	 present/1, extensibleMatch/2, 'and'/1, 'or'/1, 'not'/1,
	 modify/3, mod_add/2, mod_delete/2, mod_replace/2, add/3,
	 delete/2, modify_dn/5, modify_passwd/3, bind/3]).

-export([get_status/1]).

%% gen_fsm callbacks
-export([init/1, connecting/2, connecting/3,
	 wait_bind_response/3, active/3, active_bind/3,
	 handle_event/3, handle_sync_event/4, handle_info/3,
	 terminate/3, code_change/4]).

-export_type([filter/0]).

-include("ELDAPv3.hrl").

-include("eldap.hrl").

-define(LDAP_VERSION, 3).

-define(RETRY_TIMEOUT, 500).

-define(BIND_TIMEOUT, 10000).

-define(CMD_TIMEOUT, 100000).
%% Used in gen_fsm sync calls.
%% Used as a timeout for gen_tcp:send/2

-define(CALL_TIMEOUT,
	(?CMD_TIMEOUT) + (?BIND_TIMEOUT) + (?RETRY_TIMEOUT)).

-define(SEND_TIMEOUT, 30000).

-define(MAX_TRANSACTION_ID, 65535).

-define(MIN_TRANSACTION_ID, 0).
%% Grace period after "soft" LDAP bind errors:

-define(GRACEFUL_RETRY_TIMEOUT, 5000).

-define(SUPPORTEDEXTENSION,
	<<"1.3.6.1.4.1.1466.101.120.7">>).

-define(SUPPORTEDEXTENSIONSYNTAX,
	<<"1.3.6.1.4.1.1466.115.121.1.38">>).

-define(STARTTLS, <<"1.3.6.1.4.1.1466.20037">>).

-type handle() :: pid() | atom() | binary().

-record(eldap,
	{version = ?LDAP_VERSION :: non_neg_integer(),
         hosts = []              :: [binary()],
         host                    :: binary(),
	 port = 389              :: inet:port_number(),
         sockmod = gen_tcp       :: ssl | gen_tcp,
         tls = none              :: none | tls,
         tls_options = []        :: [{cacertfile, string()} |
                                     {depth, non_neg_integer()} |
                                     {verify, non_neg_integer()}],
	 fd,
         rootdn = <<"">>         :: binary(),
         passwd = <<"">>         :: binary(),
         id = 0                  :: non_neg_integer(),
         bind_timer = make_ref() :: reference(),
	 dict = dict:new()       :: dict(),
         req_q = queue:new()     :: queue()}).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start_link(Name) ->
    Reg_name = jlib:binary_to_atom(<<"eldap_",
				       Name/binary>>),
    gen_fsm:start_link({local, Reg_name}, ?MODULE, [], []).

-spec start_link(binary(), [binary()], inet:port_number(), binary(),
                 binary(), tlsopts()) -> any().

start_link(Name, Hosts, Port, Rootdn, Passwd, Opts) ->
    Reg_name = jlib:binary_to_atom(<<"eldap_",
				       Name/binary>>),
    gen_fsm:start_link({local, Reg_name}, ?MODULE,
		       [Hosts, Port, Rootdn, Passwd, Opts], []).

-spec get_status(handle()) -> any().

%%% --------------------------------------------------------------------
%%% Get status of connection.
%%% --------------------------------------------------------------------
get_status(Handle) ->
    Handle1 = get_handle(Handle),
    gen_fsm:sync_send_all_state_event(Handle1, get_status).

%%% --------------------------------------------------------------------
%%% Shutdown connection (and process) asynchronous.
%%% --------------------------------------------------------------------
-spec close(handle()) -> any().

close(Handle) ->
    Handle1 = get_handle(Handle),
    gen_fsm:send_all_state_event(Handle1, close).

%%% --------------------------------------------------------------------
%%% Add an entry. The entry field MUST NOT exist for the AddRequest
%%% to succeed. The parent of the entry MUST exist.
%%% Example:
%%%
%%%  add(Handle, 
%%%         "cn=Bill Valentine, ou=people, o=Bluetail AB, dc=bluetail, dc=com",
%%%         [{"objectclass", ["person"]},
%%%          {"cn", ["Bill Valentine"]},
%%%          {"sn", ["Valentine"]},
%%%          {"telephoneNumber", ["545 555 00"]}]
%%%     )
%%% --------------------------------------------------------------------
add(Handle, Entry, Attributes) ->
    Handle1 = get_handle(Handle),
    gen_fsm:sync_send_event(Handle1,
			    {add, Entry, add_attrs(Attributes)}, ?CALL_TIMEOUT).

%%% Do sanity check !
add_attrs(Attrs) ->
    F = fun ({Type, Vals}) ->
		{'AddRequest_attributes', Type, Vals}
	end,
    case catch lists:map(F, Attrs) of
      {'EXIT', _} -> throw({error, attribute_values});
      Else -> Else
    end.

%%% --------------------------------------------------------------------
%%% Delete an entry. The entry consists of the DN of 
%%% the entry to be deleted.
%%% Example:
%%%
%%%  delete(Handle, 
%%%         "cn=Bill Valentine, ou=people, o=Bluetail AB, dc=bluetail, dc=com"
%%%        )
%%% --------------------------------------------------------------------
delete(Handle, Entry) ->
    Handle1 = get_handle(Handle),
    gen_fsm:sync_send_event(Handle1, {delete, Entry},
			    ?CALL_TIMEOUT).

%%% --------------------------------------------------------------------
%%% Modify an entry. Given an entry a number of modification
%%% operations can be performed as one atomic operation.
%%% Example:
%%%
%%%  modify(Handle, 
%%%         "cn=Torbjorn Tornkvist, ou=people, o=Bluetail AB, dc=bluetail, dc=com",
%%%         [replace("telephoneNumber", ["555 555 00"]),
%%%          add("description", ["LDAP hacker"])] 
%%%        )
%%% --------------------------------------------------------------------
-spec modify(handle(), any(), [add | delete | replace]) -> any().

modify(Handle, Object, Mods) ->
    Handle1 = get_handle(Handle),
    gen_fsm:sync_send_event(Handle1, {modify, Object, Mods},
			    ?CALL_TIMEOUT).

%%%
%%% Modification operations. 
%%% Example:
%%%            replace("telephoneNumber", ["555 555 00"])
%%%
mod_add(Type, Values) ->
    m(add, Type, Values).

mod_delete(Type, Values) ->
    m(delete, Type, Values).

%%% --------------------------------------------------------------------
%%% Modify an entry. Given an entry a number of modification
%%% operations can be performed as one atomic operation.
%%% Example:
%%%
%%%  modify_dn(Handle, 
%%%    "cn=Bill Valentine, ou=people, o=Bluetail AB, dc=bluetail, dc=com",
%%%    "cn=Ben Emerson",
%%%    true,
%%%    ""
%%%        )
%%% --------------------------------------------------------------------
mod_replace(Type, Values) ->
    m(replace, Type, Values).

m(Operation, Type, Values) ->
    #'ModifyRequest_modification_SEQOF'{operation =
					    Operation,
					modification =
					    #'AttributeTypeAndValues'{type =
									  Type,
								      vals =
									  Values}}.

modify_dn(Handle, Entry, NewRDN, DelOldRDN, NewSup) ->
    Handle1 = get_handle(Handle),
    gen_fsm:sync_send_event(Handle1,
			    {modify_dn, Entry, NewRDN, bool_p(DelOldRDN),
			     optional(NewSup)},
			    ?CALL_TIMEOUT).

-spec modify_passwd(handle(), binary(), binary()) -> any().

modify_passwd(Handle, DN, Passwd) ->
    Handle1 = get_handle(Handle),
    gen_fsm:sync_send_event(Handle1,
			    {modify_passwd, DN, Passwd}, ?CALL_TIMEOUT).

%%% --------------------------------------------------------------------
%%% Bind.
%%% Example:
%%%
%%%  bind(Handle, 
%%%    "cn=Bill Valentine, ou=people, o=Bluetail AB, dc=bluetail, dc=com",
%%%    "secret")
%%% --------------------------------------------------------------------
-spec bind(handle(), binary(), binary()) -> any().
 
bind(Handle, RootDN, Passwd) ->
    Handle1 = get_handle(Handle),
    gen_fsm:sync_send_event(Handle1, {bind, RootDN, Passwd},
			    ?CALL_TIMEOUT).

%%% Sanity checks !

bool_p(Bool) when Bool == true; Bool == false -> Bool.

optional([]) -> asn1_NOVALUE;
optional(Value) -> Value.

%%% --------------------------------------------------------------------
%%% Synchronous search of the Directory returning a 
%%% requested set of attributes.
%%%
%%%  Example:
%%%
%%%	Filter = eldap:substrings("sn", [{any,"o"}]),
%%%	eldap:search(S, [{base, "dc=bluetail, dc=com"},
%%%	                 {filter, Filter},
%%%			 {attributes,["cn"]}])),
%%%
%%% Returned result:  {ok, #eldap_search_result{}}
%%%
%%% Example:
%%%
%%%  {ok,{eldap_search_result,
%%%        [{eldap_entry,
%%%           "cn=Magnus Froberg, dc=bluetail, dc=com",
%%%           [{"cn",["Magnus Froberg"]}]},
%%%         {eldap_entry,
%%%           "cn=Torbjorn Tornkvist, dc=bluetail, dc=com",
%%%           [{"cn",["Torbjorn Tornkvist"]}]}],
%%%        []}}
%%%
%%% --------------------------------------------------------------------
-type search_args() :: [{base, binary()} |
                        {filter, filter()} |
                        {scope, scope()} |
                        {attributes, [binary()]} |
                        {types_only, boolean()} |
                        {timeout, non_neg_integer()} |
                        {limit, non_neg_integer()} |
                        {deref_aliases, never | searching | finding | always}].

-spec search(handle(), eldap_search() | search_args()) -> any().

search(Handle, A) when is_record(A, eldap_search) ->
    call_search(Handle, A);
search(Handle, L) when is_list(L) ->
    case catch parse_search_args(L) of
      {error, Emsg} -> {error, Emsg};
      {'EXIT', Emsg} -> {error, Emsg};
      A when is_record(A, eldap_search) ->
	  call_search(Handle, A)
    end.

call_search(Handle, A) ->
    Handle1 = get_handle(Handle),
    gen_fsm:sync_send_event(Handle1, {search, A},
			    ?CALL_TIMEOUT).

-spec parse_search_args(search_args()) -> eldap_search().

parse_search_args(Args) ->
    parse_search_args(Args,
		      #eldap_search{scope = wholeSubtree}).

parse_search_args([{base, Base} | T], A) ->
    parse_search_args(T, A#eldap_search{base = Base});
parse_search_args([{filter, Filter} | T], A) ->
    parse_search_args(T, A#eldap_search{filter = Filter});
parse_search_args([{scope, Scope} | T], A) ->
    parse_search_args(T, A#eldap_search{scope = Scope});
parse_search_args([{attributes, Attrs} | T], A) ->
    parse_search_args(T,
		      A#eldap_search{attributes = Attrs});
parse_search_args([{types_only, TypesOnly} | T], A) ->
    parse_search_args(T,
		      A#eldap_search{types_only = TypesOnly});
parse_search_args([{timeout, Timeout} | T], A)
    when is_integer(Timeout) ->
    parse_search_args(T, A#eldap_search{timeout = Timeout});
parse_search_args([{limit, Limit} | T], A)
    when is_integer(Limit) ->
    parse_search_args(T, A#eldap_search{limit = Limit});
parse_search_args([{deref_aliases, never} | T], A) ->
    parse_search_args(T,
		      A#eldap_search{deref_aliases = neverDerefAliases});
parse_search_args([{deref_aliases, searching} | T],
		  A) ->
    parse_search_args(T,
		      A#eldap_search{deref_aliases = derefInSearching});
parse_search_args([{deref_aliases, finding} | T], A) ->
    parse_search_args(T,
		      A#eldap_search{deref_aliases = derefFindingBaseObj});
parse_search_args([{deref_aliases, always} | T], A) ->
    parse_search_args(T,
		      A#eldap_search{deref_aliases = derefAlways});
parse_search_args([H | _], _) ->
    throw({error, {unknown_arg, H}});
parse_search_args([], A) -> A.

baseObject() -> baseObject.

singleLevel() -> singleLevel.

%%%
%%% The Scope parameter
%%%
wholeSubtree() -> wholeSubtree.

%%%
%%% Boolean filter operations
%%%
-type filter() :: 'and'() | 'or'() | 'not'() | equalityMatch() |
                  greaterOrEqual() | lessOrEqual() | approxMatch() |
                  present() | substrings() | extensibleMatch().

%%%
%%% The following Filter parameters consist of an attribute
%%% and an attribute value. Example: F("uid","tobbe")
%%%
-type 'and'() :: {'and', [filter()]}.
-spec 'and'([filter()]) -> 'and'().

'and'(ListOfFilters) when is_list(ListOfFilters) ->
    {'and', ListOfFilters}.

-type 'or'() :: {'or', [filter()]}.
-spec 'or'([filter()]) -> 'or'().

'or'(ListOfFilters) when is_list(ListOfFilters) ->
    {'or', ListOfFilters}.

-type 'not'() :: {'not', filter()}.
-spec 'not'(filter()) -> 'not'().

'not'(Filter) when is_tuple(Filter) -> {'not', Filter}.

-type equalityMatch() :: {equalityMatch, 'AttributeValueAssertion'()}.
-spec equalityMatch(binary(), binary()) -> equalityMatch().

equalityMatch(Desc, Value) ->
    {equalityMatch, av_assert(Desc, Value)}.

-type greaterOrEqual() :: {greaterOrEqual, 'AttributeValueAssertion'()}.
-spec greaterOrEqual(binary(), binary()) -> greaterOrEqual().

greaterOrEqual(Desc, Value) ->
    {greaterOrEqual, av_assert(Desc, Value)}.

-type lessOrEqual() :: {lessOrEqual, 'AttributeValueAssertion'()}.
-spec lessOrEqual(binary(), binary()) -> lessOrEqual().

lessOrEqual(Desc, Value) ->
    {lessOrEqual, av_assert(Desc, Value)}.

-type approxMatch() :: {approxMatch, 'AttributeValueAssertion'()}.
-spec approxMatch(binary(), binary()) -> approxMatch().

approxMatch(Desc, Value) ->
    {approxMatch, av_assert(Desc, Value)}.

-type 'AttributeValueAssertion'() ::
        #'AttributeValueAssertion'{attributeDesc :: binary(),
                                   assertionValue :: binary()}.

-spec av_assert(binary(), binary()) -> 'AttributeValueAssertion'().

av_assert(Desc, Value) ->
    #'AttributeValueAssertion'{attributeDesc = Desc,
			       assertionValue = Value}.

%%%
%%% Filter to check for the presence of an attribute
%%%
-type present() :: {present, binary()}.
-spec present(binary()) -> present().

%%%
%%% A substring filter seem to be based on a pattern:
%%%
%%%   InitValue*AnyValue*FinalValue
%%%
%%% where all three parts seem to be optional (at least when
%%% talking with an OpenLDAP server). Thus, the arguments
%%% to substrings/2 looks like this:
%%%
%%% Type   ::= string( <attribute> )
%%% SubStr ::= listof( {initial,Value} | {any,Value}, {final,Value})
%%%
%%% Example: substrings("sn",[{initial,"To"},{any,"kv"},{final,"st"}])
%%% will match entries containing:  'sn: Tornkvist'
%%%
present(Attribute) ->
    {present, Attribute}.

%%%
%%% extensibleMatch filter.
%%% FIXME: Describe the purpose of this filter.
%%%
%%% Value   ::= string( <attribute> )
%%% Opts    ::= listof( {matchingRule, Str} | {type, Str} | {dnAttributes, true} )
%%%
%%% Example: extensibleMatch("Fred", [{matchingRule, "1.2.3.4.5"}, {type, "cn"}]).
%%%
-type substr() :: [{initial | any | final, binary()}].
-type 'SubstringFilter'() ::
        #'SubstringFilter'{type :: binary(),
                           substrings :: {'SubstringFilter_substrings',
                                          substr()}}.

-type substrings() :: {substrings, 'SubstringFilter'()}.
-spec substrings(binary(), substr()) -> substrings().

substrings(Type, SubStr) ->
    Ss = {'SubstringFilter_substrings', SubStr},
    {substrings,
     #'SubstringFilter'{type = Type, substrings = Ss}}.

-type match_opts() :: [{matchingRule | type, binary()} |
                       {dnAttributes, boolean()}].

-type 'MatchingRuleAssertion'() ::
        #'MatchingRuleAssertion'{matchValue :: binary(),
                                 type :: asn1_NOVALUE | binary(),
                                 matchingRule :: asn1_NOVALUE | binary(),
                                 dnAttributes :: asn1_DEFAULT | true}.

-type extensibleMatch() :: {extensibleMatch, 'MatchingRuleAssertion'()}.
-spec extensibleMatch(binary(), match_opts()) -> extensibleMatch().

extensibleMatch(Value, Opts) ->
    MRA = #'MatchingRuleAssertion'{matchValue = Value},
    {extensibleMatch, extensibleMatch_opts(Opts, MRA)}.

extensibleMatch_opts([{matchingRule, Rule} | Opts], MRA) ->
    extensibleMatch_opts(Opts,
			 MRA#'MatchingRuleAssertion'{matchingRule = Rule});
extensibleMatch_opts([{type, Desc} | Opts], MRA) ->
    extensibleMatch_opts(Opts,
			 MRA#'MatchingRuleAssertion'{type = Desc});
extensibleMatch_opts([{dnAttributes, true} | Opts],
		     MRA) ->
    extensibleMatch_opts(Opts,
			 MRA#'MatchingRuleAssertion'{dnAttributes = true});
extensibleMatch_opts([_ | Opts], MRA) ->
    extensibleMatch_opts(Opts, MRA);
extensibleMatch_opts([], MRA) -> MRA.

get_handle(Pid) when is_pid(Pid) -> Pid;
get_handle(Atom) when is_atom(Atom) -> Atom;
get_handle(Name) when is_binary(Name) ->
    jlib:binary_to_atom(<<"eldap_",
			    Name/binary>>).

%%%----------------------------------------------------------------------
%%% Callback functions from gen_fsm
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, StateName, StateData}          |
%%          {ok, StateName, StateData, Timeout} |
%%          ignore                              |
%%          {stop, StopReason}             
%% I use the trick of setting a timeout of 0 to pass control into the
%% process.      
%%----------------------------------------------------------------------
init([Hosts, Port, Rootdn, Passwd, Opts]) ->
    Encrypt = case gen_mod:get_opt(encrypt, Opts,
                                   fun(tls) -> tls;
                                      (starttls) -> starttls;
                                      (none) -> none
                                   end) of
                  tls -> tls;
                  _ -> none
	      end,
    PortTemp = case Port of
		 undefined ->
		     case Encrypt of
		       tls -> ?LDAPS_PORT;
		       _ -> ?LDAP_PORT
		     end;
		 PT -> PT
	       end,
    CacertOpts = case gen_mod:get_opt(
                        tls_cacertfile, Opts,
                        fun(S) when is_binary(S) ->
                                binary_to_list(S);
                           (undefined) ->
                                undefined
                        end) of
                     undefined ->
                         [];
                     Path ->
                         [{cacertfile, Path}]
                 end,
    DepthOpts = case gen_mod:get_opt(
                       tls_depth, Opts,
                       fun(I) when is_integer(I), I>=0 ->
                               I;
                          (undefined) ->
                               undefined
                       end) of
                    undefined ->
                        [];
                    Depth ->
                        [{depth, Depth}]
                end,
    Verify = gen_mod:get_opt(tls_verify, Opts,
                             fun(hard) -> hard;
                                (soft) -> soft;
                                (false) -> false
                             end, false),
    TLSOpts = if (Verify == hard orelse Verify == soft)
		   andalso CacertOpts == [] ->
		     ?WARNING_MSG("TLS verification is enabled but no CA "
				  "certfiles configured, so verification "
				  "is disabled.",
				  []),
		     [];
		 Verify == soft ->
		     [{verify, 1}] ++ CacertOpts ++ DepthOpts;
		 Verify == hard ->
		     [{verify, 2}] ++ CacertOpts ++ DepthOpts;
		 true -> []
	      end,
    {ok, connecting,
     #eldap{hosts = Hosts, port = PortTemp, rootdn = Rootdn,
	    passwd = Passwd, tls = Encrypt, tls_options = TLSOpts,
	    id = 0, dict = dict:new(), req_q = queue:new()},
     0}.

%%----------------------------------------------------------------------
%% Func: StateName/2
%% Called when gen_fsm:send_event/2,3 is invoked (async)
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}                         
%%----------------------------------------------------------------------
connecting(timeout, S) ->
    {ok, NextState, NewS} = connect_bind(S),
    {next_state, NextState, NewS}.

%%----------------------------------------------------------------------
%% Func: StateName/3
%% Called when gen_fsm:sync_send_event/2,3 is invoked.
%% Returns: {next_state, NextStateName, NextStateData}            |
%%          {next_state, NextStateName, NextStateData, Timeout}   |
%%          {reply, Reply, NextStateName, NextStateData}          |
%%          {reply, Reply, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}                          |
%%          {stop, Reason, Reply, NewStateData}                    
%%----------------------------------------------------------------------
connecting(Event, From, S) ->
    Q = queue:in({Event, From}, S#eldap.req_q),
    {next_state, connecting, S#eldap{req_q = Q}}.

wait_bind_response(Event, From, S) ->
    Q = queue:in({Event, From}, S#eldap.req_q),
    {next_state, wait_bind_response, S#eldap{req_q = Q}}.

active_bind(Event, From, S) ->
    Q = queue:in({Event, From}, S#eldap.req_q),
    {next_state, active_bind, S#eldap{req_q = Q}}.

active(Event, From, S) ->
    process_command(S, Event, From).

%%----------------------------------------------------------------------
%% Func: handle_event/3
%% Called when gen_fsm:send_all_state_event/2 is invoked.
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}                         
%%----------------------------------------------------------------------
handle_event(close, _StateName, S) ->
    catch (S#eldap.sockmod):close(S#eldap.fd),
    {stop, normal, S};
handle_event(_Event, StateName, S) ->
    {next_state, StateName, S}.

%%----------------------------------------------------------------------
%% Func: handle_sync_event/4
%% Called when gen_fsm:sync_send_all_state_event/2,3 is invoked
%% Returns: {next_state, NextStateName, NextStateData}            |
%%          {next_state, NextStateName, NextStateData, Timeout}   |
%%          {reply, Reply, NextStateName, NextStateData}          |
%%          {reply, Reply, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}                          |
%%          {stop, Reason, Reply, NewStateData}                    
%%----------------------------------------------------------------------
handle_sync_event(_Event, _From, StateName, S) ->
    {reply, {StateName, S}, StateName, S}.

%%----------------------------------------------------------------------
%% Func: handle_info/3
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}                         
%%          {stop, Reason, NewStateData}
%%----------------------------------------------------------------------

%%
%% Packets arriving in various states
%%
handle_info({Tag, _Socket, Data}, connecting, S)
    when Tag == tcp; Tag == ssl ->
    ?DEBUG("tcp packet received when disconnected!~n~p",
	   [Data]),
    {next_state, connecting, S};
handle_info({Tag, _Socket, Data}, wait_bind_response, S)
    when Tag == tcp; Tag == ssl ->
    cancel_timer(S#eldap.bind_timer),
    case catch recvd_wait_bind_response(Data, S) of
      bound -> dequeue_commands(S);
      {fail_bind, Reason} ->
	  report_bind_failure(S#eldap.host, S#eldap.port, Reason),
	  {next_state, connecting,
	   close_and_retry(S, ?GRACEFUL_RETRY_TIMEOUT)};
      {'EXIT', Reason} ->
	  report_bind_failure(S#eldap.host, S#eldap.port, Reason),
	  {next_state, connecting, close_and_retry(S)};
      {error, Reason} ->
	  report_bind_failure(S#eldap.host, S#eldap.port, Reason),
	  {next_state, connecting, close_and_retry(S)}
    end;
handle_info({Tag, _Socket, Data}, StateName, S)
    when (StateName == active orelse
	    StateName == active_bind)
	   andalso (Tag == tcp orelse Tag == ssl) ->
    case catch recvd_packet(Data, S) of
      {response, Response, RequestType} ->
	  NewS = case Response of
		   {reply, Reply, To, S1} -> gen_fsm:reply(To, Reply), S1;
		   {ok, S1} -> S1
		 end,
	  if StateName == active_bind andalso
	       RequestType == bindRequest
	       orelse StateName == active ->
		 dequeue_commands(NewS);
	     true -> {next_state, StateName, NewS}
	  end;
      _ -> {next_state, StateName, S}
    end;
handle_info({Tag, _Socket}, Fsm_state, S)
    when Tag == tcp_closed; Tag == ssl_closed ->
    ?WARNING_MSG("LDAP server closed the connection: ~s:~p~nIn "
		 "State: ~p",
		 [S#eldap.host, S#eldap.port, Fsm_state]),
    {next_state, connecting, close_and_retry(S)};
handle_info({Tag, _Socket, Reason}, Fsm_state, S)
    when Tag == tcp_error; Tag == ssl_error ->
    ?DEBUG("eldap received tcp_error: ~p~nIn State: ~p",
	   [Reason, Fsm_state]),
    {next_state, connecting, close_and_retry(S)};
%%
%% Timers
%%
handle_info({timeout, Timer, {cmd_timeout, Id}},
	    StateName, S) ->
    case cmd_timeout(Timer, Id, S) of
      {reply, To, Reason, NewS} ->
	  gen_fsm:reply(To, Reason),
	  {next_state, StateName, NewS};
      {error, _Reason} -> {next_state, StateName, S}
    end;
handle_info({timeout, retry_connect}, connecting, S) ->
    {ok, NextState, NewS} = connect_bind(S),
    {next_state, NextState, NewS};
handle_info({timeout, _Timer, bind_timeout},
	    wait_bind_response, S) ->
    {next_state, connecting, close_and_retry(S)};
%%
%% Make sure we don't fill the message queue with rubbish
%%
handle_info(Info, StateName, S) ->
    ?DEBUG("eldap. Unexpected Info: ~p~nIn state: "
	   "~p~n when StateData is: ~p",
	   [Info, StateName, S]),
    {next_state, StateName, S}.

%%----------------------------------------------------------------------
%% Func: terminate/3
%% Purpose: Shutdown the fsm
%% Returns: any
%%----------------------------------------------------------------------
terminate(_Reason, _StateName, _StatData) -> ok.

%%----------------------------------------------------------------------
%% Func: code_change/4
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState, NewStateData}
%%----------------------------------------------------------------------
code_change(_OldVsn, StateName, S, _Extra) ->
    {ok, StateName, S}.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------
dequeue_commands(S) ->
    case queue:out(S#eldap.req_q) of
      {{value, {Event, From}}, Q} ->
	  case process_command(S#eldap{req_q = Q}, Event, From) of
	    {_, active, NewS} -> dequeue_commands(NewS);
	    Res -> Res
	  end;
      {empty, _} -> {next_state, active, S}
    end.

process_command(S, Event, From) ->
    case send_command(Event, From, S) of
      {ok, NewS} ->
	  case Event of
	    {bind, _, _} -> {next_state, active_bind, NewS};
	    _ -> {next_state, active, NewS}
	  end;
      {error, _Reason} ->
	  Q = queue:in_r({Event, From}, S#eldap.req_q),
	  NewS = close_and_retry(S#eldap{req_q = Q}),
	  {next_state, connecting, NewS}
    end.

send_command(Command, From, S) ->
    Id = bump_id(S),
    {Name, Request} = gen_req(Command),
    Message = #'LDAPMessage'{messageID = Id,
			     protocolOp = {Name, Request}},
    ?DEBUG("~p~n", [{Name, Request}]),
    {ok, Bytes} = asn1rt:encode('ELDAPv3', 'LDAPMessage',
				Message),
    case (S#eldap.sockmod):send(S#eldap.fd, Bytes) of
      ok ->
	  Timer = erlang:start_timer(?CMD_TIMEOUT, self(),
				     {cmd_timeout, Id}),
	  New_dict = dict:store(Id,
				[{Timer, Command, From, Name}], S#eldap.dict),
	  {ok, S#eldap{id = Id, dict = New_dict}};
      Error -> Error
    end.

gen_req({search, A}) ->
    {searchRequest,
     #'SearchRequest'{baseObject = A#eldap_search.base,
		      scope = A#eldap_search.scope,
		      derefAliases = A#eldap_search.deref_aliases,
		      sizeLimit = A#eldap_search.limit,
		      timeLimit = A#eldap_search.timeout,
		      typesOnly = A#eldap_search.types_only,
		      filter = A#eldap_search.filter,
		      attributes = A#eldap_search.attributes}};
gen_req({add, Entry, Attrs}) ->
    {addRequest,
     #'AddRequest'{entry = Entry, attributes = Attrs}};
gen_req({delete, Entry}) -> {delRequest, Entry};
gen_req({modify, Obj, Mod}) ->
    {modifyRequest,
     #'ModifyRequest'{object = Obj, modification = Mod}};
gen_req({modify_dn, Entry, NewRDN, DelOldRDN,
	 NewSup}) ->
    {modDNRequest,
     #'ModifyDNRequest'{entry = Entry, newrdn = NewRDN,
			deleteoldrdn = DelOldRDN, newSuperior = NewSup}};
gen_req({modify_passwd, DN, Passwd}) ->
    {ok, ReqVal} = asn1rt:encode('ELDAPv3',
				 'PasswdModifyRequestValue',
				 #'PasswdModifyRequestValue'{userIdentity = DN,
							     newPasswd =
								 Passwd}),
    {extendedReq,
     #'ExtendedRequest'{requestName = ?passwdModifyOID,
			requestValue = iolist_to_binary(ReqVal)}};
gen_req({bind, RootDN, Passwd}) ->
    {bindRequest,
     #'BindRequest'{version = ?LDAP_VERSION, name = RootDN,
		    authentication = {simple, Passwd}}}.

%%-----------------------------------------------------------------------
%% recvd_packet
%% Deals with incoming packets in the active state
%% Will return one of:
%%  {ok, NewS} - Don't reply to client yet as this is part of a search 
%%               result and we haven't got all the answers yet.
%%  {reply, Result, From, NewS} - Reply with result to client From
%%  {error, Reason}
%%  {'EXIT', Reason} - Broke
%%-----------------------------------------------------------------------
recvd_packet(Pkt, S) ->
    case asn1rt:decode('ELDAPv3', 'LDAPMessage', Pkt) of
      {ok, Msg} ->
	  Op = Msg#'LDAPMessage'.protocolOp,
	  ?DEBUG("~p", [Op]),
	  Dict = S#eldap.dict,
	  Id = Msg#'LDAPMessage'.messageID,
	  {Timer, From, Name, Result_so_far} = get_op_rec(Id,
							  Dict),
	  Answer = case {Name, Op} of
		     {searchRequest, {searchResEntry, R}}
			 when is_record(R, 'SearchResultEntry') ->
			 New_dict = dict:append(Id, R, Dict),
			 {ok, S#eldap{dict = New_dict}};
		     {searchRequest, {searchResDone, Result}} ->
			 Reason = Result#'LDAPResult'.resultCode,
			 if Reason == success; Reason == sizeLimitExceeded ->
				{Res, Ref} = polish(Result_so_far),
				New_dict = dict:erase(Id, Dict),
				cancel_timer(Timer),
				{reply,
				 #eldap_search_result{entries = Res,
						      referrals = Ref},
				 From, S#eldap{dict = New_dict}};
			    true ->
				New_dict = dict:erase(Id, Dict),
				cancel_timer(Timer),
				{reply, {error, Reason}, From,
				 S#eldap{dict = New_dict}}
			 end;
		     {searchRequest, {searchResRef, R}} ->
			 New_dict = dict:append(Id, R, Dict),
			 {ok, S#eldap{dict = New_dict}};
		     {addRequest, {addResponse, Result}} ->
			 New_dict = dict:erase(Id, Dict),
			 cancel_timer(Timer),
			 Reply = check_reply(Result, From),
			 {reply, Reply, From, S#eldap{dict = New_dict}};
		     {delRequest, {delResponse, Result}} ->
			 New_dict = dict:erase(Id, Dict),
			 cancel_timer(Timer),
			 Reply = check_reply(Result, From),
			 {reply, Reply, From, S#eldap{dict = New_dict}};
		     {modifyRequest, {modifyResponse, Result}} ->
			 New_dict = dict:erase(Id, Dict),
			 cancel_timer(Timer),
			 Reply = check_reply(Result, From),
			 {reply, Reply, From, S#eldap{dict = New_dict}};
		     {modDNRequest, {modDNResponse, Result}} ->
			 New_dict = dict:erase(Id, Dict),
			 cancel_timer(Timer),
			 Reply = check_reply(Result, From),
			 {reply, Reply, From, S#eldap{dict = New_dict}};
		     {bindRequest, {bindResponse, Result}} ->
			 New_dict = dict:erase(Id, Dict),
			 cancel_timer(Timer),
			 Reply = check_bind_reply(Result, From),
			 {reply, Reply, From, S#eldap{dict = New_dict}};
		     {extendedReq, {extendedResp, Result}} ->
			 New_dict = dict:erase(Id, Dict),
			 cancel_timer(Timer),
			 Reply = check_extended_reply(Result, From),
			 {reply, Reply, From, S#eldap{dict = New_dict}};
		     {OtherName, OtherResult} ->
			 New_dict = dict:erase(Id, Dict),
			 cancel_timer(Timer),
			 {reply,
			  {error, {invalid_result, OtherName, OtherResult}},
			  From, S#eldap{dict = New_dict}}
		   end,
	  {response, Answer, Name};
      Error -> Error
    end.

check_reply(#'LDAPResult'{resultCode = success},
	    _From) ->
    ok;
check_reply(#'LDAPResult'{resultCode = Reason},
	    _From) ->
    {error, Reason};
check_reply(Other, _From) -> {error, Other}.

check_bind_reply(#'BindResponse'{resultCode = success},
		 _From) ->
    ok;
check_bind_reply(#'BindResponse'{resultCode = Reason},
		 _From) ->
    {error, Reason};
check_bind_reply(Other, _From) -> {error, Other}.

%% TODO: process reply depending on requestName:
%% this requires BER-decoding of #'ExtendedResponse'.response
check_extended_reply(#'ExtendedResponse'{resultCode =
					     success},
		     _From) ->
    ok;
check_extended_reply(#'ExtendedResponse'{resultCode =
					     Reason},
		     _From) ->
    {error, Reason};
check_extended_reply(Other, _From) -> {error, Other}.

get_op_rec(Id, Dict) ->
    case dict:find(Id, Dict) of
      {ok, [{Timer, _Command, From, Name} | Res]} ->
	  {Timer, From, Name, Res};
      error -> throw({error, unkown_id})
    end.

%%-----------------------------------------------------------------------
%% recvd_wait_bind_response packet
%% Deals with incoming packets in the wait_bind_response state
%% Will return one of:
%%  bound - Success - move to active state
%%  {fail_bind, Reason} - Failed
%%  {error, Reason}
%%  {'EXIT', Reason} - Broken packet
%%-----------------------------------------------------------------------
recvd_wait_bind_response(Pkt, S) ->
    case asn1rt:decode('ELDAPv3', 'LDAPMessage', Pkt) of
      {ok, Msg} ->
	  ?DEBUG("~p", [Msg]),
	  check_id(S#eldap.id, Msg#'LDAPMessage'.messageID),
	  case Msg#'LDAPMessage'.protocolOp of
	    {bindResponse, Result} ->
		case Result#'BindResponse'.resultCode of
		  success -> bound;
		  Error -> {fail_bind, Error}
		end
	  end;
      Else -> {fail_bind, Else}
    end.

check_id(Id, Id) -> ok;
check_id(_, _) -> throw({error, wrong_bind_id}).

%%-----------------------------------------------------------------------
%% General Helpers
%%-----------------------------------------------------------------------

cancel_timer(Timer) ->
    erlang:cancel_timer(Timer),
    receive {timeout, Timer, _} -> ok after 0 -> ok end.


close_and_retry(S, Timeout) ->
    catch (S#eldap.sockmod):close(S#eldap.fd),
    Queue = dict:fold(fun (_Id,
			   [{Timer, Command, From, _Name} | _], Q) ->
			      cancel_timer(Timer),
			      queue:in_r({Command, From}, Q);
			  (_, _, Q) -> Q
		      end,
		      S#eldap.req_q, S#eldap.dict),
    erlang:send_after(Timeout, self(),
		      {timeout, retry_connect}),
    S#eldap{fd = undefined, req_q = Queue, dict = dict:new()}.

close_and_retry(S) ->
    close_and_retry(S, ?RETRY_TIMEOUT).

report_bind_failure(Host, Port, Reason) ->
    ?WARNING_MSG("LDAP bind failed on ~s:~p~nReason: ~p",
		 [Host, Port, Reason]).

%%-----------------------------------------------------------------------
%% Sort out timed out commands
%%-----------------------------------------------------------------------
cmd_timeout(Timer, Id, S) ->
    Dict = S#eldap.dict,
    case dict:find(Id, Dict) of
      {ok, [{Timer, _Command, From, Name} | Res]} ->
	  case Name of
	    searchRequest ->
		{Res1, Ref1} = polish(Res),
		New_dict = dict:erase(Id, Dict),
		{reply, From,
		 {timeout,
		  #eldap_search_result{entries = Res1, referrals = Ref1}},
		 S#eldap{dict = New_dict}};
	    _ ->
		New_dict = dict:erase(Id, Dict),
		{reply, From, {error, timeout},
		 S#eldap{dict = New_dict}}
	  end;
      error -> {error, timed_out_cmd_not_in_dict}
    end.

%%-----------------------------------------------------------------------
%% Common stuff for results
%%-----------------------------------------------------------------------
%%%
%%% Polish the returned search result
%%%

polish(Entries) -> polish(Entries, [], []).

polish([H | T], Res, Ref)
    when is_record(H, 'SearchResultEntry') ->
    ObjectName = H#'SearchResultEntry'.objectName,
    F = fun ({_, A, V}) -> {A, V} end,
    Attrs = lists:map(F, H#'SearchResultEntry'.attributes),
    polish(T,
	   [#eldap_entry{object_name = ObjectName,
			 attributes = Attrs}
	    | Res],
	   Ref);
polish([H | T], Res,
       Ref) ->     % No special treatment of referrals at the moment.
    polish(T, Res, [H | Ref]);
polish([], Res, Ref) -> {Res, Ref}.

%%-----------------------------------------------------------------------
%% Connect to next server in list and attempt to bind to it.
%%-----------------------------------------------------------------------
connect_bind(S) ->
    Host = next_host(S#eldap.host, S#eldap.hosts),
    ?INFO_MSG("LDAP connection on ~s:~p",
	      [Host, S#eldap.port]),
    Opts = if S#eldap.tls == tls ->
		  [{packet, asn1}, {active, true}, {keepalive, true},
		   binary
		   | S#eldap.tls_options];
	      true ->
		  [{packet, asn1}, {active, true}, {keepalive, true},
		   {send_timeout, ?SEND_TIMEOUT}, binary]
	   end,
    HostS = binary_to_list(Host),
    SocketData = case S#eldap.tls of
		   tls ->
		       SockMod = ssl, ssl:connect(HostS, S#eldap.port, Opts);
		   %% starttls -> %% TODO: Implement STARTTLS;
		   _ ->
		       SockMod = gen_tcp,
		       gen_tcp:connect(HostS, S#eldap.port, Opts)
		 end,
    case SocketData of
      {ok, Socket} ->
	  case bind_request(Socket, S#eldap{sockmod = SockMod}) of
	    {ok, NewS} ->
		Timer = erlang:start_timer(?BIND_TIMEOUT, self(),
					   {timeout, bind_timeout}),
		{ok, wait_bind_response,
		 NewS#eldap{fd = Socket, sockmod = SockMod, host = Host,
			    bind_timer = Timer}};
	    {error, Reason} ->
		report_bind_failure(Host, S#eldap.port, Reason),
		NewS = close_and_retry(S),
		{ok, connecting, NewS#eldap{host = Host}}
	  end;
      {error, Reason} ->
	  ?ERROR_MSG("LDAP connection failed:~n** Server: "
		     "~s:~p~n** Reason: ~p~n** Socket options: ~p",
		     [Host, S#eldap.port, Reason, Opts]),
	  NewS = close_and_retry(S),
	  {ok, connecting, NewS#eldap{host = Host}}
    end.

bind_request(Socket, S) ->
    Id = bump_id(S),
    Req = #'BindRequest'{version = S#eldap.version,
			 name = S#eldap.rootdn,
			 authentication = {simple, S#eldap.passwd}},
    Message = #'LDAPMessage'{messageID = Id,
			     protocolOp = {bindRequest, Req}},
    ?DEBUG("Bind Request Message:~p~n", [Message]),
    {ok, Bytes} = asn1rt:encode('ELDAPv3', 'LDAPMessage',
				Message),
    case (S#eldap.sockmod):send(Socket, Bytes) of
      ok -> {ok, S#eldap{id = Id}};
      Error -> Error
    end.

%% Given last tried Server, find next one to try
next_host(undefined, [H | _]) ->
    H;                    % First time, take first
next_host(Host,
	  Hosts) ->                       % Find next in turn
    next_host(Host, Hosts, Hosts).

%%% --------------------------------------------------------------------
%%% Verify the input data
%%% --------------------------------------------------------------------
%%% --------------------------------------------------------------------
%%% Get and Validate the initial configuration
%%% --------------------------------------------------------------------
%% get_atom(Key, List) ->
%%     case lists:keysearch(Key, 1, List) of
%% 	{value, {Key, Value}} when is_atom(Value) ->
%% 	    Value;
%% 	{value, {Key, _Value}} ->
%% 	    throw({error, "Bad Value in Config for " ++ atom_to_list(Key)});
%% 	false ->
%% 	    throw({error, "No Entry in Config for " ++ atom_to_list(Key)})
%%     end.
%%% --------------------------------------------------------------------
%%% Other Stuff
%%% --------------------------------------------------------------------
next_host(Host, [Host], Hosts) ->
    hd(Hosts);    % Wrap back to first
next_host(Host, [Host | Tail], _Hosts) ->
    hd(Tail);       % Take next
next_host(_Host, [], Hosts) ->
    hd(Hosts);       % Never connected before? (shouldn't happen)
next_host(Host, [_ | T], Hosts) ->
    next_host(Host, T, Hosts).

bump_id(#eldap{id = Id})
    when Id > (?MAX_TRANSACTION_ID) ->
    ?MIN_TRANSACTION_ID;
bump_id(#eldap{id = Id}) -> Id + 1.
