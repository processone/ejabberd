%% Author: Romuald du Song
%% Created: 9 oct. 2009
%% Description: 
%% Hacked from Joe Armtrong http://www.erlang.org/examples/small_examples/urlget.erl

-module(url_util).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([parse/1]).

%%
%% API Functions
%%

%%----------------------------------------------------------------------
%% parse(URL) -> {http, Site, Port, File} | 
%%               {file, File}             | {error,Why}
%% (primitive)

parse([$h,$t,$t,$p,$:,$/,$/|T]) ->  parse_http(T, http);
parse([$h,$t,$t,$p,$s,$:,$/,$/|T]) ->  parse_http(T, https);
parse([$f,$t,$p,$:,$/,$/|_T])    ->  {error, no_ftp};
parse([$f,$i,$l,$e,$:,$/,$/|F]) ->  {file, F};
parse(_X)                        ->  {error, unknown_url_type}.


%%
%% Local Functions
%%

parse_http(X, Protocol) ->
    case string:chr(X, $/) of
	0 ->
	    %% not terminated by "/" (sigh)
	    %% try again
	    parse_http(X ++ "/", Protocol);
	N ->
	    %% The Host is up to the first "/"
	    %% The file is everything else
	    Host = string:substr(X, 1, N-1),
	    File = string:substr(X, N, length(X)),
	    %% Now check to see if the host name contains a colon
	    %% i.e. there is an explicit port address in the hostname
	    case string:chr(Host, $:) of
		0 ->
		    %% no colon
		    Port = 80,
		    {Protocol, Host, Port, File};
		M ->
		    Site = string:substr(Host,1,M-1),
		    case (catch list_to_integer(
				  string:substr(Host, M+1, length(Host)))) of
			{'EXIT', _} ->
			    {Protocol, Site, 80, File};
			Port ->
			    {Protocol, Site, Port, File}
		    end
	    end
    end.
