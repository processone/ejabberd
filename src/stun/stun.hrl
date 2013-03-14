%%%-------------------------------------------------------------------
%%% File    : stun.hrl
%%% Author  : Evgeniy Khramtsov <ekhramtsov@process-one.net>
%%% Description : STUN values
%%% Created :  8 Aug 2009 by Evgeniy Khramtsov <ekhramtsov@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2013   ProcessOne
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
%%%-------------------------------------------------------------------
-define(STUN_MAGIC, 16#2112a442).

%% I know, this is terrible. Refer to 'STUN Message Structure' of
%% RFC5389 to understand this.
-define(STUN_METHOD(Type),
	Type band 15872 bsr 2 bor (Type band 224 bsr 1) bor
	  Type band 15).

-define(STUN_CLASS(Type),
	Type band 256 bsr 7 bor (Type band 16 bsr 4)).

-define(STUN_TYPE(C, M),
%% Comprehension-required range (0x0000-0x7FFF)
%% Comprehension-optional range (0x8000-0xFFFF)
	M band 3968 bsl 2 bor (M band 112 bsl 1) bor M band 15
	  bor (C band 2 bsl 7 bor (C band 1 bsl 4))).

-define(is_required(A), A =< 32767).

-define(STUN_METHOD_BINDING, 1).

-define(STUN_ATTR_MAPPED_ADDRESS, 1).

-define(STUN_ATTR_USERNAME, 6).

-define(STUN_ATTR_MESSAGE_INTEGRITY, 8).

-define(STUN_ATTR_ERROR_CODE, 9).

-define(STUN_ATTR_UNKNOWN_ATTRIBUTES, 10).

-define(STUN_ATTR_REALM, 20).

-define(STUN_ATTR_NONCE, 21).

-define(STUN_ATTR_XOR_MAPPED_ADDRESS, 32).

-define(STUN_ATTR_SOFTWARE, 32802).

-define(STUN_ATTR_ALTERNATE_SERVER, 32803).

-define(STUN_ATTR_FINGERPRINT, 32808).

-record(stun,
	{class = request :: request | response | error | indication,
         method = ?STUN_METHOD_BINDING :: non_neg_integer(),
         magic = ?STUN_MAGIC :: non_neg_integer(),
         trid = 0 :: non_neg_integer() ,
	 unsupported = [] :: [non_neg_integer()],
         'SOFTWARE',
         'ALTERNATE-SERVER',
	 'MAPPED-ADDRESS',
         'XOR-MAPPED-ADDRESS',
         'USERNAME',
	 'REALM',
         'NONCE',
         'MESSAGE-INTEGRITY',
         'ERROR-CODE',
	 'UNKNOWN-ATTRIBUTES' = []}).

%% Workarounds.
%%-define(NO_PADDING, true).

