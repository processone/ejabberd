%%%-------------------------------------------------------------------
%%% File    : mod_admin_extra_private.erl
%%% Author  : Badlop <badlop@process-one.net>, Piotr Nosek <piotr.nosek@erlang-solutions.com>
%%% Purpose : Contributed administrative functions and commands
%%% Created : 10 Aug 2008 by Badlop <badlop@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2008   ProcessOne
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

-module(mod_admin_extra_private).
-author('badlop@process-one.net').

-export([
    commands/0,

    private_get/4,
    private_set/3
    ]).

-include("ejabberd.hrl").
-include("ejabberd_commands.hrl").
-include("jlib.hrl").
-include_lib("exml/include/exml.hrl").

%%%
%%% Register commands
%%%

commands() ->
    [
        #ejabberd_commands{name = private_get, tags = [private],
                           desc = "Get some information from a user private storage",
                           module = ?MODULE, function = private_get,
                           args = [{user, string}, {host, string}, {element, string}, {ns, string}],
                           result = {res, string}},
        #ejabberd_commands{name = private_set, tags = [private],
                           desc = "Set to the user private storage",
                           module = ?MODULE, function = private_set,
                           args = [{user, string}, {host, string}, {element, string}],
                           result = {res, rescode}}
        ].

%%%
%%% Private Storage
%%%

%% Example usage:
%% $ ejabberdctl private_set badlop localhost "\<aa\ xmlns=\'bb\'\>Cluth\</aa\>"
%% $ ejabberdctl private_get badlop localhost aa bb
%% <aa xmlns='bb'>Cluth</aa>

private_get(Username, Host, Element, Ns) ->
    From = jlib:make_jid(Username, Host, ""),
    To = jlib:make_jid(Username, Host, ""),
    IQ = {iq, "", get, ?NS_PRIVATE, "",
          #xmlel{ name = <<"query">>,
                 attrs = [{<<"xmlns">>,?NS_PRIVATE}],
                 children = [#xmlel{ name = Element, attrs = [{<<"xmlns">>, Ns}]}] } },
    ResIq = mod_private:process_sm_iq(From, To, IQ),
    [#xmlel{ name = <<"query">>,
            attrs = [{<<"xmlns">>,<<"jabber:iq:private">>}],
            children = [SubEl] }] = ResIq#iq.sub_el,
    xml:element_to_string(SubEl).

private_set(Username, Host, ElementString) ->
    case xml_stream:parse_element(ElementString) of
        {error, Error} ->
            io:format("Error found parsing the element:~n  ~p~nError: ~p~n",
                      [ElementString, Error]),
            error;
        Xml ->
            private_set2(Username, Host, Xml)
    end.

private_set2(Username, Host, Xml) ->
    From = jlib:make_jid(Username, Host, ""),
    To = jlib:make_jid(Username, Host, ""),
    IQ = {iq, "", set, ?NS_PRIVATE, "",
          #xmlel{ name = <<"query">>,
                 attrs = [{<<"xmlns">>,?NS_PRIVATE}],
                 children = [Xml]}},
    mod_private:process_sm_iq(From, To, IQ),
    ok.

