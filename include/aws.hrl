%%%----------------------------------------------------------------------
%%% File     : s3_util.erl
%%% Usage    : S3 URL Generation and Signing
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

-record(aws_auth, {access_key_id :: binary(),
                   access_key    :: binary(),
                   region        :: binary()}).

-define(AWS_SERVICE_S3, <<"s3">>).
