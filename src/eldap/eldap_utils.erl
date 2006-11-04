%%%----------------------------------------------------------------------
%%% File    : eldap_utils.erl
%%% Author  : Mickael Remond <mickael.remond@process-one.net>
%%% Purpose : ejabberd LDAP helper functions
%%% Created : 12 Oct 2006 by Mickael Remond <mickael.remond@process-one.net>
%%% Id      : $Id: ejabberd_auth_ldap.erl 623 2006-09-23 09:52:53Z mremond $
%%%----------------------------------------------------------------------

-module(eldap_utils).
-author('mickael.remond@process-one.net').
-svn('$Revision: $ ').

-export([generate_subfilter/1,
	 find_ldap_attrs/2,
	 get_ldap_attr/2,
	 usort_attrs/1,
	 get_user_part/2,
	 make_filter/2,
	 case_insensitive_match/2]).

%% Generate an 'or' LDAP query on one or several attributes
%% If there is only one attribute
generate_subfilter([UID]) ->
    subfilter(UID);
%% If there is several attributes
generate_subfilter(UIDs) ->
    "(|" ++ [subfilter(UID) || UID <- UIDs] ++ ")".
%% Subfilter for a single attribute
subfilter({UIDAttr, UIDAttrFormat}) ->
    "(" ++ UIDAttr ++ "=" ++ UIDAttrFormat ++ ")";
%% The default UiDAttrFormat is %u
subfilter({UIDAttr}) ->
    "(" ++ UIDAttr ++ "=" ++ "%u)".

%% Not tail-recursive, but it is not very terribly.
%% It stops finding on the first not empty value.
find_ldap_attrs([{Attr, Format} | Rest], Attributes) ->
	case get_ldap_attr(Attr, Attributes) of
	Value when is_list(Value), Value /= "" ->
		{Value, Format};
	_ ->
		find_ldap_attrs(Rest, Attributes)
	end;
find_ldap_attrs([], _) ->
	"".

get_ldap_attr(LDAPAttr, Attributes) ->
    Res = lists:filter(
	    fun({Name, _}) ->
		    case_insensitive_match(Name, LDAPAttr)
	    end, Attributes),
    case Res of
	[{_, [Value|_]}] -> Value;
	_ -> ""
    end.


usort_attrs(Attrs) when is_list(Attrs) ->
    lists:usort(Attrs);
usort_attrs(_) ->
    [].

get_user_part(String, Pattern) ->
    F = fun(S, P) ->
		First = string:str(P, "%u"),
		TailLength = length(P) - (First+1),
		string:sub_string(S, First, length(S) - TailLength)
	end,
    case catch F(String, Pattern) of
	{'EXIT', _} ->
	    {error, badmatch};
	Result ->
	    case regexp:sub(Pattern, "%u", Result) of
		{ok, String, _} -> {ok, Result};
		_ -> {error, badmatch}
	    end
    end.

make_filter(Data, UIDs) ->
    NewUIDs = [{U, eldap_filter:do_sub(UF, [{"%u", "*%u*", 1}])} || {U, UF} <- UIDs],
    Filter = lists:flatmap(
	       fun({Name, [Value | _]}) ->
		       case Name of
			   "%u" when Value /= "" ->
			       case eldap_filter:parse(
				      lists:flatten(generate_subfilter(NewUIDs)),
				               [{"%u", Value}]) of
				   {ok, F} -> [F];
				   _ -> []
			       end;
			   _ when Value /= "" ->
			       [eldap:substrings(Name, [{any, Value}])];
			   _ ->
			       []
		       end
	       end, Data),
    case Filter of
	[F] ->
	    F;
	_ ->
	    eldap:'and'(Filter)
    end.

case_insensitive_match(X, Y) ->
    X1 = stringprep:tolower(X),
    Y1 = stringprep:tolower(Y),
    if
	X1 == Y1 -> true;
	true -> false
    end.

