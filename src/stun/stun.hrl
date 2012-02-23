%%%-------------------------------------------------------------------
%%% File    : stun.hrl
%%% Author  : Evgeniy Khramtsov <ekhramtsov@process-one.net>
%%% Description : STUN values
%%% Created :  8 Aug 2009 by Evgeniy Khramtsov <ekhramtsov@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2012   ProcessOne
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
	((Type band 16#3e00) bsr 2) bor
	((Type band 16#e0) bsr 1) bor (Type band 16#f)).
-define(STUN_CLASS(Type),
	((Type band 16#100) bsr 7) bor
	((Type band 16#10) bsr 4)).
-define(STUN_TYPE(C, M),
	(((M band 16#f80) bsl 2)
	 bor ((M band 16#70) bsl 1)
	 bor (M band 16#f) )
	bor (((C band 16#2) bsl 7) bor ((C band 16#1) bsl 4))).

-define(is_required(A), (A =< 16#7fff)).

-define(STUN_METHOD_BINDING, 16#001).

%% Comprehension-required range (0x0000-0x7FFF)
-define(STUN_ATTR_MAPPED_ADDRESS, 16#0001).
-define(STUN_ATTR_USERNAME, 16#0006).
-define(STUN_ATTR_MESSAGE_INTEGRITY, 16#0008).
-define(STUN_ATTR_ERROR_CODE, 16#0009).
-define(STUN_ATTR_UNKNOWN_ATTRIBUTES, 16#000a).
-define(STUN_ATTR_REALM, 16#0014).
-define(STUN_ATTR_NONCE, 16#0015).
-define(STUN_ATTR_XOR_MAPPED_ADDRESS, 16#0020).

%% Comprehension-optional range (0x8000-0xFFFF)
-define(STUN_ATTR_SOFTWARE, 16#8022).
-define(STUN_ATTR_ALTERNATE_SERVER, 16#8023).
-define(STUN_ATTR_FINGERPRINT, 16#8028).

-record(stun, {class,
	       method,
	       magic = ?STUN_MAGIC,
	       trid,
	       unsupported = [],
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
