%%%----------------------------------------------------------------------
%%% File    : acl.erl
%%% Author  : Alexey Shchepin <alexey@sevcom.net>
%%% Purpose : ACL support
%%% Created : 18 Jan 2003 by Alexey Shchepin <alexey@sevcom.net>
%%% Id      : $Id$
%%%----------------------------------------------------------------------

-module(acl).
-author('alexey@sevcom.net').
-vsn('$Revision$ ').

-export([start/0,
	 to_record/2,
	 add/2,
	 add_list/2,
	 match_rule/2,
	 % for debugging only
	 match_acl/2]).

-include("ejabberd.hrl").

-record(acl, {aclname, aclspec}).

start() ->
    mnesia:create_table(acl,
			[{disc_copies, [node()]},
			 {type, bag},
			 {attributes, record_info(fields, acl)}]),
    mnesia:add_table_copy(acl, node(), ram_copies),
    ok.

to_record(ACLName, ACLSpec) ->
    #acl{aclname = ACLName, aclspec = ACLSpec}.

add(ACLName, ACLSpec) ->
    F = fun() ->
		mnesia:write(#acl{aclname = ACLName, aclspec = ACLSpec})
	end,
    mnesia:transaction(F).

add_list(ACLs, Clear) ->
    F = fun() ->
		if
		    Clear ->
			Ks = mnesia:all_keys(acl),
			lists:foreach(fun(K) ->
					      mnesia:delete({acl, K})
				      end, Ks);
		    true ->
			ok
		end,
		lists:foreach(fun(ACL) ->
				      case ACL of
					  #acl{} ->
					      mnesia:write(ACL)
				      end
			      end, ACLs)
	end,
    case mnesia:transaction(F) of
	{atomic, _} ->
	    ok;
	_ ->
	    false
    end.



match_rule(Rule, JID) ->
    case Rule of
	all -> allow;
	none -> deny;
	_ ->
	    case ejabberd_config:get_global_option({access, Rule}) of
		undefined ->
		    deny;
		ACLs ->
		    match_acls(ACLs, JID)
	    end
    end.

match_acls([], _) ->
    deny;
match_acls([{Access, ACL} | ACLs], JID) ->
    case match_acl(ACL, JID) of
	true ->
	    Access;
	_ ->
	    match_acls(ACLs, JID)
    end.

match_acl(ACL, JID) ->
    case ACL of
	all -> true;
	none -> false;
	_ ->
	    {User, Server, Resource} = jlib:jid_tolower(JID),
	    lists:any(fun(#acl{aclspec = Spec}) ->
			      case Spec of
				  all ->
				      true;
				  {user, U} ->
				      (U == User) andalso (?MYNAME == Server);
				  {user, U, S} ->
				      (U == User) andalso (S == Server);
				  {server, S} ->
				      S == Server;
				  {user_regexp, UR} ->
				      (?MYNAME == Server) andalso
					  is_regexp_match(User, UR);
				  {user_regexp, UR, S} ->
				      (S == Server) andalso
					  is_regexp_match(User, UR);
				  {server_regexp, SR} ->
				      is_regexp_match(Server, SR);
				  {node_regexp, UR, SR} ->
				      is_regexp_match(Server, SR) andalso
					  is_regexp_match(User, UR);
				  {user_glob, UR} ->
				      (?MYNAME == Server) andalso
					  is_glob_match(User, UR);
				  {user_glob, UR, S} ->
				      (S == Server) andalso
					  is_glob_match(User, UR);
				  {server_glob, SR} ->
				      is_glob_match(Server, SR);
				  {node_glob, UR, SR} ->
				      is_glob_match(Server, SR) andalso
					  is_glob_match(User, UR)
			      end
		      end, ets:lookup(acl, ACL))
    end.

is_regexp_match(String, RegExp) ->
    case regexp:first_match(String, RegExp) of
	nomatch ->
	    false;
	{match, _, _} ->
	    true;
	{error, ErrDesc} ->
	    ?ERROR_MSG(
	       "Wrong regexp ~p in ACL: ~p",
	       [RegExp, lists:flatten(regexp:format_error(ErrDesc))]),
	    false
    end.

is_glob_match(String, Glob) ->
    is_regexp_match(String, regexp:sh_to_awk(Glob)).


