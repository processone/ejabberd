%%%----------------------------------------------------------------------
%%%
%%% ejabberd, Copyright (C) 2002-2015   ProcessOne
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
%%%
%%%----------------------------------------------------------------------

-define(CT_XML,
	{<<"Content-Type">>, <<"text/xml; charset=utf-8">>}).

-define(CT_PLAIN,
	{<<"Content-Type">>, <<"text/plain">>}).

-define(AC_ALLOW_ORIGIN,
	{<<"Access-Control-Allow-Origin">>, <<"*">>}).

-define(AC_ALLOW_METHODS,
	{<<"Access-Control-Allow-Methods">>,
	 <<"GET, POST, OPTIONS">>}).

-define(AC_ALLOW_HEADERS,
	{<<"Access-Control-Allow-Headers">>,
	 <<"Content-Type">>}).

-define(AC_MAX_AGE,
	{<<"Access-Control-Max-Age">>, <<"86400">>}).

-define(OPTIONS_HEADER,
	[?CT_PLAIN, ?AC_ALLOW_ORIGIN, ?AC_ALLOW_METHODS,
	 ?AC_ALLOW_HEADERS, ?AC_MAX_AGE]).

-define(HEADER,
	[?CT_XML, ?AC_ALLOW_ORIGIN, ?AC_ALLOW_HEADERS]).
