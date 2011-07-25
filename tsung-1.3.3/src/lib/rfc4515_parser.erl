%% Parsing functions for RFC4515 (String Representation of LDAP Search Filters)
%%
%% TODO: extensibleMatch features not implemented.
%%
%% Author  : Pablo Polvorin <ppolv@yahoo.com.ar>
-module(rfc4515_parser).
-author('ppolv@yahoo.com.ar').


-export([tokenize/1,filter/1,filter_to_string/1]).



%% tokenize/1 
%% Tokenize the input string into a token list. Generated tokens:
%%	'(' , ')'  , '=' , '<=' ,'>=' , '~=' , '*' , '&' , '|' , '*' , {text,Value}
%% Ej:
%% ldap_parser:tokenize("(&(!(prpr=a*s*sse*y))(pr~=sss))").
%%    -->  ['(', '&','(', '!', '(', {text,"prpr"}, '=', {text,"a"}, '*', {text,"s"}, '*', 
%%          {text,"sse"},'*', {text,"y"}, ')', ')', '(', {text,"pr"}, '~=', {text,"sss"}, ')', ')']
%%  
tokenize(L) -> tokenizer(lists:flatten(L),[],[]). %%flatten because &amp; ,etc. in xml attributes ends in deep lists ej:[[38]]
			   

tokenizer([$(|L],Current,Tokens) -> tokenizer(L,[],['('|add_current(Current,Tokens)]);
tokenizer([$)|L],Current,Tokens) -> tokenizer(L,[],[')'|add_current(Current,Tokens)]);
tokenizer([$&|L],Current,Tokens) -> tokenizer(L,[],['&'|add_current(Current,Tokens)]);
tokenizer([$||L],Current,Tokens) -> tokenizer(L,[],['|'|add_current(Current,Tokens)]);
tokenizer([$!|L],Current,Tokens) -> tokenizer(L,[],['!'|add_current(Current,Tokens)]);
tokenizer([$=|L],Current,Tokens) -> tokenizer(L,[],['='|add_current(Current,Tokens)]);
tokenizer([$*|L],Current,Tokens) -> tokenizer(L,[],['*'|add_current(Current,Tokens)]);
tokenizer([$>,$=|L],Current,Tokens) -> tokenizer(L,[],['>='|add_current(Current,Tokens)]);
tokenizer([$<,$=|L],Current,Tokens) -> tokenizer(L,[],['<='|add_current(Current,Tokens)]);
tokenizer([$~,$=|L],Current,Tokens) -> tokenizer(L,[],['~='|add_current(Current,Tokens)]);

%% an encoded valued  start with a backslash '\' character followed by the 
%%two hexadecimal digits representing the ASCII value of the encoded character
tokenizer([92|L],Current,Tokens) -> [H1,H2|L2] = L,  
									tokenizer(L2,[decode([H1,H2])|Current],Tokens);
tokenizer([C|L],Current,Tokens) -> tokenizer(L,[C|Current],Tokens);
tokenizer([],[],Tokens) -> lists:reverse(Tokens).    %%FIXME: accept trailing whitespaces

add_current(Current,Tokens) ->
	case string:strip(Current) of
		[] -> Tokens ; 
		X -> [{text,lists:reverse(X)}|Tokens]
	end.


decode(Hex) ->
	{ok,[C],[]} = io_lib:fread("~#","16#" ++ Hex),
	C.

%% filter/1
%% parse a token list into an AST-like structure. 
%% Ej:
%% ldap_parser:filter(ldap_parser:tokenize("(&(!(prpr=a*s*sse*y))(pr~=sss))")).
%%  --> {{'and',[{'not',{substring,"prpr",
%%                            [{initial,"a"},
%%                             {any,"s"},
%%                             {any,"sse"},
%%                             {final,"y"}]}},
%%          {aprox,"pr","sss"}]},
%%      []}
filter(['('|L]) -> {R, [')'|L2]} =filtercomp(L),
		   {R,L2}.
	
filtercomp(['&'|L]) -> {R,L2} = filterlist(L),
		       {{'and',R},L2};	
filtercomp(['|'|L]) -> {R,L2} = filterlist(L),
		       {{'or',R},L2};	
filtercomp(['!'|L]) -> {R,L2} = filter(L),
		       {{'not',R},L2};	
filtercomp(L) -> item(L).


filterlist(L) -> filterlist(L,[]).

filterlist(L=[')'|_],List) -> {lists:reverse(List),L}; %% ')' marks the end of the filter list
filterlist(L,List) -> {R,L2} = filter(L),
		      filterlist(L2,[R|List]).

item([{text,T}|L]) -> item2(L,T).

item2(['~=',{text,V}|L],Attr) ->  {{aprox,Attr,V},L};
item2(['>=',{text,V}|L],Attr) ->  {{get,Attr,V},L};
item2(['<=',{text,V}|L],Attr) ->  {{'let',Attr,V},L}; 
item2(['='|L],Attr) -> item3(L,Attr). % could be a presence, equality or substring match


item3(L = [')'|_],Attr) -> {{eq,Attr,""},L};    %empty attr  ej: (description=)
item3(['*',')'|L],Attr) -> {{present,Attr},[')'|L]};  %presence ej: (description=*)
item3([{text,V},')'|L],Attr) -> {{eq,Attr,V},[')'|L]}; %eq ej : = (description=some description)
item3(L,Attr) -> {R,L2} = substring(L),
		  {{substring,Attr,R},L2}.

substring([{text,V},'*'|L]) -> any(L,[{initial,V}]);
substring(['*'|L]) -> any(L,[]).

any([{text,V},'*'|L],Subs) ->  any(L,[{'any',V}|Subs]);
any([{text,V},')'|L],Subs) ->  {lists:reverse([{final,V}|Subs]),[')'|L]};
any(L = [')'|_],Subs) ->  {lists:reverse(Subs),L}.







filter_to_string({'and',L}) -> io_lib:format("(& ~s)",[lists:map(fun filter_to_string/1,L)]);
filter_to_string({'or',L}) ->  io_lib:format("(| ~s)",[lists:map(fun filter_to_string/1,L)]);
filter_to_string({'not',I}) -> io_lib:format("(! ~s)",[filter_to_string(I)]);
filter_to_string({'present',Attr}) -> io_lib:format("(~s=*)",[Attr]);
filter_to_string({'substring',Attr,Subs}) -> io_lib:format("(~s=~s)",[Attr,print_substrings(Subs)]);
filter_to_string({'aprox',Attr,Value}) -> io_lib:format("(~s~~=~s)",[Attr,Value]);
filter_to_string({'let',Attr,Value}) -> io_lib:format("(~s<=~s)",[Attr,Value]);
filter_to_string({'get',Attr,Value}) -> io_lib:format("(~s>=~s)",[Attr,Value]);
filter_to_string({'eq',Attr,Value}) -> io_lib:format("(~s=~s)",[Attr,Value]).


print_substrings(Subs) -> lists:map(fun print_substring/1,Subs).
print_substring({initial,V}) -> io_lib:format("~s",[V]);
print_substring({final,V}) -> io_lib:format("*~s",[V]);
print_substring({any,V}) -> io_lib:format("*~s",[V]).

