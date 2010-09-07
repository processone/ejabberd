%%%----------------------------------------------------------------------
%%% File    : pshb_http.erl
%%% Author  : Eric Cestari <ecestari@process-one.net>
%%% Purpose :
%%% Created :01-09-2010
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2010   ProcessOne
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
%%%
%%%  {5280, ejabberd_http, [
%%%			 http_poll, 
%%%			 web_admin,
%%%			 {request_handlers, [{["pshb"], pshb_http}]} % this should be added
%%%			]}
%%%
%%% To post to a node the content of the file "sam.atom" on the "foo", on the localhost virtual host, using cstar@localhost
%%%  curl -u cstar@localhost:encore  -i -X POST  http://localhost:5280/pshb/localhost/foo -d @sam.atom
%%%


-module (pshb_http).
-author('ecestari@process-one.net').

-include("ejabberd.hrl").
-include("jlib.hrl").
-include("ejabberd_http.hrl").

-include("mod_pubsub/pubsub.hrl").

-export([process/2]).  
 
process(LocalPath,  #request{auth = Auth} = Request)->
  ?DEBUG("LocalPath = ~p", [LocalPath]),
	case get_auth(Auth) of
	%%make sure user belongs to pubsub domain
	{User, Domain} ->
	    out(Request, Request#request.method, LocalPath,{User, Domain});
	_ ->
	    out(Request, Request#request.method, LocalPath, undefined)
    end.  
	
get_auth(Auth) ->
    case Auth of
        {SJID, P} ->
            case jlib:string_to_jid(SJID) of
                error ->
                    unauthorized;
                #jid{user = U, server = S} ->
                    case ejabberd_auth:check_password(U, S, P) of
                        true ->
                            {U, S};
                        false ->
                            unauthorized
                    end
            end;
         _ ->
            unauthorized
    end.

out(Args, 'GET', [Domain,Node]=Uri, _User) -> 
    case mod_pubsub:tree_action(get_host(Uri), get_node, [get_host(Uri),get_collection(Uri)]) of
	{error, _} -> error(404);
	_ ->
		Items = lists:sort(fun(X,Y)->
				{DateX, _} = X#pubsub_item.modification,
				{DateY, _} = Y#pubsub_item.modification,
				DateX > DateY
			end, mod_pubsub:get_items(
					get_host(Uri),
					get_collection(Uri))),
		case Items of
			[] -> ?DEBUG("Items : ~p ~n", [collection(get_collection(Uri), 
				collection_uri(Args,Domain,Node), calendar:now_to_universal_time(erlang:now()), "", [])]),
				{200, [{"Content-Type", "application/atom+xml"}],
					collection(get_collection(Uri), 
						collection_uri(Args,Domain,Node), calendar:now_to_universal_time(erlang:now()), "", [])};
			_ ->
				#pubsub_item{modification = {LastDate, _JID}} = LastItem = hd(Items),
				Etag =generate_etag(LastItem),
				IfNoneMatch=proplists:get_value('If-None-Match', Args#request.headers),
				if IfNoneMatch==Etag
					-> 
						success(304);
					true ->
						XMLEntries= [item_to_entry(Args,Domain, Node,Entry)||Entry <-  Items], 
						{200, [{"Content-Type", "application/atom+xml"},{"Etag", Etag}], 
						"<?xml version=\"1.0\" encoding=\"utf-8\"?>" 
						++	xml:element_to_string(
						collection(get_collection(Uri), collection_uri(Args,Domain,Node),
							calendar:now_to_universal_time(LastDate), "", XMLEntries))}
			end
		end
	end;

out(Args, 'POST', [Domain, Node]=Uri, {User, _Domain}) -> 
	Slug =  uniqid(false),
	Payload = xml_stream:parse_element(Args#request.data),
	[FilteredPayload]=xml:remove_cdata([Payload]),

	%FilteredPayload2 = case xml:get_subtag(FilteredPayload, "app:edited") ->
	%	{xmlelement, Name, Attrs, [{cdata, }]}
	case mod_pubsub:publish_item(get_host(Uri),
								 Domain,
								 get_collection(Uri),
								 jlib:make_jid(User,Domain, ""), 
								 Slug, 
								 [FilteredPayload]) of
		{result, [_]} ->
				?DEBUG("Publishing to ~p~n",[entry_uri(Args, Domain, Node,Slug)]),
			{201, [{"location", entry_uri(Args, Domain,Node,Slug)}], Payload};
		{error, Error} ->
			error(Error)
		end;
		
out(Args, 'GET', [Domain]=Uri, From)->
  Host = get_host(Uri),
  ?DEBUG("Host = ~p", [Host]),
  case mod_pubsub:tree_action(Host, get_subnodes, [Host, <<>>, From ]) of
		[] -> 
		  ?DEBUG("Error getting URI ~p : ~p",[Uri, From]),
		  error(404);
		Collections ->
			{200, [{"Content-Type", "application/atomsvc+xml"}], "<?xml version=\"1.0\" encoding=\"utf-8\"?>" 
				++	xml:element_to_string(service(Args,Domain,  Collections))}
	end;
	
out(Args, 'POST', [Domain]=Uri, {User, UDomain})->
  Host = get_host(Uri),
  case mod_pubsub:create_node(Host, Domain, <<>>, {User, UDomain}, "default") of
    {error, Error} ->
      ?ERROR_MSG("Error create node via HTTP : ~p",[Error]),
      
      error(Error); % will probably detail more
    {result, [Node]} ->
      {200, [{"Content-Type", "application/xml"}], "<?xml version=\"1.0\" encoding=\"utf-8\"?>"
        ++ xml:element_to_string(Node)}
  end;
      
out(Args, 'POST', [Domain]=Uri, undefined) ->
  error(403);
  

	
out(Args, 'GET', [Domain, Node, _Item]=URI, _) -> 
  Failure = fun(Error)->
    ?DEBUG("Error getting URI ~p : ~p",[URI, Error]),
    error(Error)
  end,
	Success = fun(Item)->
		Etag =generate_etag(Item),
		IfNoneMatch=proplists:get_value('If-None-Match', Args#request.headers),
		if IfNoneMatch==Etag
			-> 
				success(304);
			true ->
		    {200, [{"Content-Type",  "application/atom+xml"},{"Etag", Etag}], "<?xml version=\"1.0\" encoding=\"utf-8\"?>" 
				++ xml:element_to_string(item_to_entry(Args, Domain,Node, Item))}
		end
	end,
	get_item(URI, Failure, Success);
	
out(_,Method,Uri,undefined) ->
  ?DEBUG("Error, ~p  not authorized for ~p : ~p",[ Method,Uri]),
  error(403).

get_item(Uri, Failure, Success)->
  ?DEBUG(" mod_pubsub:get_item(~p, ~p,~p)", [get_host(Uri), get_collection(Uri), get_member(Uri)]),
  case mod_pubsub:get_item(get_host(Uri), get_collection(Uri), get_member(Uri)) of
    {error, Reason} ->
			Failure(Reason);
		#pubsub_item{}=Item  ->
			Success(Item)
  end.
		
generate_etag(#pubsub_item{modification={{_, D2, D3}, _JID}})->integer_to_list(D3+D2).
get_host([Domain|_Rest])-> "pubsub."++Domain.
get_collection([_Domain, Node|_Rest])->list_to_binary(Node).
get_member([_Domain, _Node, Member])-> 
	Member.

collection_uri(R, Domain, Node) ->
  base_uri(R, Domain)++ "/" ++ b2l(Node).
 
entry_uri(R,Domain, Node, Id)->
	collection_uri(R,Domain, Node)++"/"++b2l(Id).
	
base_uri(#request{host=Host, port=Port}, Domain)->
	"http://"++Host++":"++i2l(Port)++"/pshb/" ++ Domain.

item_to_entry(Args,Domain, Node,#pubsub_item{itemid={Id,_}, payload=Entry}=Item)->
	[R]=xml:remove_cdata(Entry),
	item_to_entry(Args, Domain, Node, Id, R, Item).

item_to_entry(Args,Domain, Node,  Id,{xmlelement, "entry", Attrs, SubEl}, 
		#pubsub_item{modification={ Secs, JID} }) ->	
	Date = calendar:now_to_local_time(Secs),
	{_User, Domain, _}=jlib:jid_tolower(JID),
	SubEl2=[{xmlelement, "app:edited", [], [{xmlcdata, w3cdtf(Date)}]},
			{xmlelement, "link",[{"rel", "edit"}, 
			{"href", entry_uri(Args,Domain,Node, Id)}],[] }, 
			{xmlelement, "id", [],[{xmlcdata, Id}]}
			| SubEl],
	{xmlelement, "entry", [{"xmlns:app","http://www.w3.org/2007/app"}|Attrs], SubEl2};
   
% Don't do anything except adding xmlns
item_to_entry(_Args,_Domain, Node,  _Id, {xmlelement, Name, Attrs, Subels}=Element, _Item)->
	case proplists:is_defined("xmlns",Attrs) of
		true -> Element;
		false -> {xmlelement, Name, [{"xmlns", Node}|Attrs], Subels}
	end.

collection(Title, Link, Updated, _Id, Entries)->
	{xmlelement, "feed", [{"xmlns", "http://www.w3.org/2005/Atom"}, 
						 {"xmlns:app", "http://www.w3.org/2007/app"}], [
		{xmlelement, "title", [],[{xmlcdata, Title}]},
		{xmlelement, "updated", [],[{xmlcdata, w3cdtf(Updated)}]},
		{xmlelement, "link", [{"href", Link}], []},
		{xmlelement, "title", [],[{xmlcdata, Title}]} | 
		Entries
	]}.
	
service(Args, Domain,Collections)->
	{xmlelement, "service", [{"xmlns", "http://www.w3.org/2007/app"},
							{"xmlns:atom", "http://www.w3.org/2005/Atom"},
							{"xmlns:app", "http://www.w3.org/2007/app"}],[
		{xmlelement, "workspace", [],[
			{xmlelement, "atom:title", [],[{xmlcdata,"Pubsub node Feed for " ++Domain}]} | 
			lists:map(fun(#pubsub_node{nodeid={_Server, Id}, type=_Type})->
				{xmlelement, "collection", [{"href", collection_uri(Args,Domain, Id)}], [
					{xmlelement, "atom:title", [], [{xmlcdata, Id}]}
				]}
				end, Collections)
		]}
	]}.

%% simple output functions
error({xmlelement, "error", Attrs, _}=Error) ->
  Value = list_to_integer(xml:get_attr_s("code", Attrs)),
  {Value, [{"Content-type", "application/xml"}], xml:element_to_string(Error)};
error(404)->
	{404, [], "Not Found"};
error(403)->
	{403, [], "Forbidden"};
error(500)->
	{500, [], "Internal server error"};
error(401)->
	{401, [{"WWW-Authenticate", "basic realm=\"ejabberd\""}],"Unauthorized"};
error(Code)->
	{Code, [], ""}.
error(Code, Error) when is_list(Error) -> {Code, [], Error};
error(Code, _Error) -> {Code, [], "Bad request"}.
success(200)->
	{200, [], ""};
success(Code)->
	{Code, [], ""}.


% Code below is taken (with some modifications) from the yaws webserver, which
% is distributed under the folowing license:
%
% This software (the yaws webserver) is free software.
% Parts of this software is Copyright (c) Claes Wikstrom <klacke@hyber.org>
% Any use or misuse of the source code is hereby freely allowed.
%
% 1. Redistributions of source code must retain the above copyright
%    notice as well as this list of conditions.
%
% 2. Redistributions in binary form must reproduce the above copyright
%    notice as well as this list of conditions.
%%% Create W3CDTF (http://www.w3.org/TR/NOTE-datetime) formatted date
%%% w3cdtf(GregSecs) -> "YYYY-MM-DDThh:mm:ssTZD"
%%%
uniqid(false)->
	{T1, T2, T3} = now(),
    lists:flatten(io_lib:fwrite("~.16B~.16B~.16B", [T1, T2, T3]));
uniqid(Slug) ->
	Slut = string:to_lower(Slug),
	S = string:substr(Slut, 1, 9),
    {_T1, T2, T3} = now(),
    lists:flatten(io_lib:fwrite("~s-~.16B~.16B", [S, T2, T3])).

w3cdtf(Date) -> %1   Date = calendar:gregorian_seconds_to_datetime(GregSecs),
    {{Y, Mo, D},{H, Mi, S}} = Date,
    [UDate|_] = calendar:local_time_to_universal_time_dst(Date),
    {DiffD,{DiffH,DiffMi,_}}=calendar:time_difference(UDate,Date),
    w3cdtf_diff(Y, Mo, D, H, Mi, S, DiffD, DiffH, DiffMi). 

%%%  w3cdtf's helper function
w3cdtf_diff(Y, Mo, D, H, Mi, S, _DiffD, DiffH, DiffMi) when DiffH < 12,  DiffH /= 0 ->
    i2l(Y) ++ "-" ++ add_zero(Mo) ++ "-" ++ add_zero(D) ++ "T" ++
        add_zero(H) ++ ":" ++ add_zero(Mi) ++ ":"  ++
        add_zero(S) ++ "+" ++ add_zero(DiffH) ++ ":"  ++ add_zero(DiffMi);

w3cdtf_diff(Y, Mo, D, H, Mi, S, DiffD, DiffH, DiffMi) when DiffH > 12,  DiffD == 0 ->
    i2l(Y) ++ "-" ++ add_zero(Mo) ++ "-" ++ add_zero(D) ++ "T" ++
        add_zero(H) ++ ":" ++ add_zero(Mi) ++ ":"  ++
        add_zero(S) ++ "+" ++ add_zero(DiffH) ++ ":"  ++
        add_zero(DiffMi);

w3cdtf_diff(Y, Mo, D, H, Mi, S, DiffD, DiffH, DiffMi) when DiffH > 12,  DiffD /= 0, DiffMi /= 0 ->
    i2l(Y) ++ "-" ++ add_zero(Mo) ++ "-" ++ add_zero(D) ++ "T" ++
        add_zero(H) ++ ":" ++ add_zero(Mi) ++ ":"  ++
        add_zero(S) ++ "-" ++ add_zero(23-DiffH) ++
        ":" ++ add_zero(60-DiffMi);

w3cdtf_diff(Y, Mo, D, H, Mi, S, DiffD, DiffH, DiffMi) when DiffH > 12,  DiffD /= 0, DiffMi == 0 ->
   i2l(Y) ++ "-" ++ add_zero(Mo) ++ "-" ++ add_zero(D) ++ "T" ++
        add_zero(H) ++ ":" ++ add_zero(Mi) ++ ":"  ++
        add_zero(S) ++ "-" ++ add_zero(24-DiffH) ++
        ":" ++ add_zero(DiffMi); 

w3cdtf_diff(Y, Mo, D, H, Mi, S, _DiffD, DiffH, _DiffMi) when DiffH == 0 ->
    i2l(Y) ++ "-" ++ add_zero(Mo) ++ "-" ++ add_zero(D) ++ "T" ++
        add_zero(H) ++ ":" ++ add_zero(Mi) ++ ":"  ++
        add_zero(S) ++ "Z".

add_zero(I) when is_integer(I) -> add_zero(i2l(I));
add_zero([A])               -> [$0,A];
add_zero(L) when is_list(L)    -> L. 



i2l(I) when is_integer(I) -> integer_to_list(I);
i2l(L) when is_list(L)    -> L.

b2l(B) when is_binary(B) -> binary_to_list(B);
b2l(L) when is_list(L) -> L.