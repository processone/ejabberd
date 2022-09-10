%%%----------------------------------------------------------------------
%%% File     : aws_util_SUITE.erl
%%% Usage    : AWS URL Signing Tests
%%% Author   : Roman Hargrave <roman@hargrave.info>
%%% Created  : 08 Sept 2022 by Roman Hargrave <roman@hargrave.info>
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

-module(aws_util_SUITE).
-author("roman@hargrave.info").

-include("aws.hrl").

-behaviour(ct_suite).

-export([all/0,
         signs_url/1]).

all() ->
    [signs_url].

-define(EXPECT_SIGNED_URL, 
        <<"https://examplebucket.s3.amazonaws.com/test.txt?X-Amz-Signature=aeeed9bbccd4d02ee5c0109b86d86835f995330da4c265957d157751f604d404&X-Amz-Algorithm=AWS4-HMAC-SHA256&X-Amz-Credential=AKIAIOSFODNN7EXAMPLE%2F20130524%2Fus-east-1%2Fs3%2Faws4_request&X-Amz-Date=20130524T000000Z&X-Amz-Expires=86400&X-Amz-SignedHeaders=host">>).

% Amazon effectively declares that any valid signing implementation
% should generate the signature
% aeeed9bbccd4d02ee5c0109b86d86835f995330da4c265957d157751f604d404 for
% the values given below
signs_url(_Config) ->
    Auth = #aws_auth{access_key_id = <<"AKIAIOSFODNN7EXAMPLE">>,
                     access_key    = <<"wJalrXUtnFEMI/K7MDENG/bPxRfiCYEXAMPLEKEY">>,
                     region        = <<"us-east-1">>},
    Time = {{2013, 05, 24}, {00, 00, 00}},
    TTL = 86400,
    URL = <<"https://examplebucket.s3.amazonaws.com/test.txt">>,
    case aws_util:signed_url(Auth, get, ?AWS_SERVICE_S3, URL, [], Time, TTL) of
        ?EXPECT_SIGNED_URL ->
            ok;
        UnexpectedUrl ->
            {failed, {{expected, ?EXPECT_SIGNED_URL}, 
                      {received, UnexpectedUrl}}}
    end.
