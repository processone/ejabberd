%%%----------------------------------------------------------------------
%%% File     : aws_util.erl
%%% Usage    : AWS URL Signing
%%% Author   : Roman Hargrave <roman@hargrave.info>
%%% Purpose  : Signing AWS Requests. Intended for S3-CS use.
%%% Created  : 24 Aug 2022 by Roman Hargrave <roman@hargrave.info>
%%%
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
%%%----------------------------------------------------------------------

%% URL Signing. Documented at
%% https://docs.aws.amazon.com/AmazonS3/latest/API/sig-v4-authenticating-requests.html

-module(aws_util).
-author("roman@hargrave.info").

-include("aws.hrl").

-type verb() :: get | put | post | delete.
-type headers() :: [{unicode:chardata(), unicode:chardata()}].
-type query_list() :: [{unicode:chardata(), unicode:chardata() | true}].
-type ttl() :: 1..604800.

-define(AWS_SIGN_ALGO, <<"AWS4-HMAC-SHA256">>).

-import(crypto, [mac/4]).
-import(uri_string, [compose_query/1,
                     dissect_query/1]).
-import(misc, [crypto_hmac/3]).

-export([signed_url/7]).

%%------------------------------------------------------------------------
%% API
%%------------------------------------------------------------------------

-spec signed_url(
        Auth :: #aws_auth{},
        Verb :: verb(),
        Service :: binary(),
        Url :: binary(),
        ExtraHeaders :: headers(),
        Time :: calendar:datetime(),
        TTL :: ttl()
       ) ->
          SignedUrl :: binary().
% sign a URL given headers, a verb, authentication details, and a time
signed_url(Auth, Verb, Service, URL, ExtraHeaders, Time, TTL) ->
    #{host := Host} = UnauthenticatedUriMap = uri_string:parse(URL),
    Headers = [{<<"host">>, Host} | ExtraHeaders],
    % insert authentication params.
    QueryList = sorted_query_list(uri_query_list(UnauthenticatedUriMap)
                                  ++ base_query_params(Auth, Time, Service, Headers, TTL)),
    UriMap = UnauthenticatedUriMap#{query => compose_query(QueryList)},
    % generate and sign the message
    StringToSign = string_to_sign(Auth, Time, Service, Verb, UriMap, Headers),
    SigningKey = signing_key(Auth, Time, Service),
    Signature = encode_hex(crypto_hmac(sha256, SigningKey, StringToSign)),
    % add signature to the query list and compose URI
    SignedQueryString = compose_query([{<<"X-Amz-Signature">>, Signature}|QueryList]),
    uri_string:recompose(UriMap#{query => SignedQueryString}).

%%------------------------------------------------------------------------
%% Internal
%%------------------------------------------------------------------------

-spec sorted_query_list(
        QueryList :: query_list()
       ) ->
          SortedQueryList :: query_list().
% sort a query paramater list by parameter name, ascending
sorted_query_list(QueryList) ->
    lists:sort(fun ({L, _}, {R, _}) -> L =< R end, QueryList).

-spec uri_query_list(
        UriMap :: uri_string:uri_map()
       ) ->
          QueryList :: query_list().
% extract a query list from a uri_map().
uri_query_list(#{query := QueryString}) ->
    dissect_query(QueryString);
uri_query_list(_) ->
    [].

-spec verb(
        Verb :: verb()
       ) ->
          binary().
% convert a verb atom to a binary list
verb(get) ->
    <<"GET">>;
verb(put) ->
    <<"PUT">>;
verb(post) ->
    <<"POST">>;
verb(delete) ->
    <<"DELETE">>.

-spec encode_hex(
        Data :: binary()
       ) ->
          EncodedData :: binary().
% lowercase binary:encode_hex
encode_hex(Data) ->
    str:to_lower(str:to_hexlist(Data)).

-spec iso8601_timestamp_utc(
        DateTime :: calendar:datetime()
       ) ->
          Timestamp :: binary().
% Generate an ISO8601-like YmdTHMSZ timestamp for X-Amz-Date. Only
% produces UTC ('Z') timestamps. No separators.
iso8601_timestamp_utc({{Y, Mo, D}, {H, M, S}}) ->
    str:format("~B~2..0B~2..0BT~2..0B~2..0B~2..0BZ",
               [Y, Mo, D,
                H, M, S]).

-spec iso8601_date(
        DateTime :: calendar:datetime()
       ) ->
          DateStr :: binary().
% ISO8601 formatted date, no separators.
iso8601_date({{Y, M, D}, _}) ->
    str:format("~B~2..0B~2..0B", [Y, M, D]).

-spec scope(
        Auth :: #aws_auth{},
        Time :: calendar:datetime(),
        Service :: binary()
       ) ->
          Scope :: binary().
% Generate the request scope used in the credential field and signature message
scope(#aws_auth{region = Region},
      Time,
      Service) ->
    str:format("~ts/~ts/~ts/aws4_request",
               [iso8601_date(Time),
                Region,
                Service]).

-spec credential(
        Auth :: #aws_auth{},
        Time :: calendar:datetime(),
        Service :: binary()
       ) ->
          Auth :: binary().
% Generate the value used for X-Amz-Credential
credential(#aws_auth{access_key_id = KeyID} = Auth,
           Time,
           Service) ->
    str:format("~ts/~ts", [KeyID, scope(Auth, Time, Service)]).

-spec base_query_params(
        Auth :: #aws_auth{},
        Time :: calendar:datetime(),
        Service :: binary(),
        Headers :: headers(),
        TTL :: ttl()
       ) ->
          BaseQueryParams :: [{unicode:chardata(), unicode:chardata()}].
% Return the minimum required set of query parameters needed for
% authenticated signed requests.
base_query_params(Auth, Time, Service, Headers, TTL) ->
    [{<<"X-Amz-Algorithm">>,     ?AWS_SIGN_ALGO},
     {<<"X-Amz-Credential">>,    credential(Auth, Time, Service)},
     {<<"X-Amz-Date">>,          iso8601_timestamp_utc(Time)},
     {<<"X-Amz-Expires">>,       erlang:integer_to_binary(TTL)},
     {<<"X-Amz-SignedHeaders">>, signed_headers(Headers)}].

-spec canonical_headers(
        Headers :: headers()
       ) ->
          CanonicalHeaders :: unicode:chardata().
% generate the header list for canonical_request
canonical_headers(Headers) ->
    str:join(lists:map(fun ({Name, Value}) ->
                               str:format("~ts:~ts~n", [Name, Value])
                       end, Headers),
             <<>>).

-spec signed_headers(
        SignedHeaders :: headers()
       ) ->
          SignedHeaders :: unicode:chardata().
% generate a semicolon-delimited list of headers, used to enumerate
% signed headers in the AWSv4 canonical request
signed_headers(SignedHeaders) ->
    str:join(lists:map(fun ({Name, _}) ->
                               Name
                       end, SignedHeaders),
             <<";">>).

-spec canonical_request(
        Verb :: verb(),
        UriMap :: uri_string:uri_map(),
        Headers :: headers()
       ) ->
          CanonicalRequest :: unicode:chardata().
% Generate the canonical request used to compute the signature
canonical_request(Verb,
                  #{query := Query,
                    path  := Path},
                  Headers) ->
    <<(verb(Verb))/binary, "\n",
      Path/binary, "\n",
      Query/binary, "\n",
      (canonical_headers(Headers))/binary, "\n",
      (signed_headers(Headers))/binary, "\n",
      "UNSIGNED-PAYLOAD">>.

-spec string_to_sign(
        Auth :: #aws_auth{},
        Time :: calendar:datetime(),
        Service :: binary(),
        Verb :: verb(),
        UriMap :: uri_string:uri_map(),
        Headers :: headers()
       ) ->
          StringToSign :: unicode:chardata().
% generate the "string to sign", as per AWS specs
string_to_sign(Auth, Time, Service, Verb, UriMap, Headers) ->
    RequestHash = crypto:hash(sha256, canonical_request(Verb, UriMap, Headers)),
    <<?AWS_SIGN_ALGO/binary, "\n",
      (iso8601_timestamp_utc(Time))/binary, "\n",
      (scope(Auth, Time, Service))/binary, "\n",
      (encode_hex(RequestHash))/binary>>.

-spec signing_key(
        Auth :: #aws_auth{},
        Time :: calendar:datetime(),
        Service :: binary()
       ) ->
          SigningKey :: binary().
% generate the signing key used in the final HMAC-SHA256 round for
% request signing.
signing_key(#aws_auth{access_key = AccessKey,
                     region     = Region},
            Time,
            Service) ->
    DateKey = crypto_hmac(sha256, <<"AWS4", AccessKey/binary>>, iso8601_date(Time)),
    DateRegionKey = crypto_hmac(sha256, DateKey, Region),
    DateRegionServiceKey = crypto_hmac(sha256, DateRegionKey, Service),
    crypto_hmac(sha256, DateRegionServiceKey, <<"aws4_request">>).
