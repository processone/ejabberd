%%%  Created: July 2008 by Grégoire Reboul <gregoire.reboul@laposte.net>
%%%  From   : ts_config_pgsql.erl by Nicolas Niclausse <nicolas.niclausse@niclux.org>
%%%
%%%  This program is free software; you can redistribute it and/or modify
%%%  it under the terms of the GNU General Public License as published by
%%%  the Free Software Foundation; either version 2 of the License, or
%%%  (at your option) any later version.
%%%
%%%  This program is distributed in the hope that it will be useful,
%%%  but WITHOUT ANY WARRANTY; without even the implied warranty of
%%%  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%%  GNU General Public License for more details.
%%%
%%%  You should have received a copy of the GNU General Public License
%%%  along with this program; if not, write to the Free Software
%%%  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307, USA.
%%%

%%% In addition, as a special exception, you have the permission to
%%% link the code of this program with any library released under
%%% the EPL license and distribute linked combinations including
%%% the two.

-module(ts_config_mysql).
-author('gregoire.reboul@laposte.net').

-export([parse_config/2]).

-include("ts_profile.hrl").
-include("ts_mysql.hrl").
-include("ts_config.hrl").

-include("xmerl.hrl").

%%----------------------------------------------------------------------
%% Func: parse_config/2
%% Args: Element, Config
%% Returns: List
%% Purpose: parse a request defined in the XML config file
%%----------------------------------------------------------------------
%% Parsing other elements
parse_config(Element = #xmlElement{name=dyn_variable}, Conf = #config{}) ->
    ts_config:parse(Element,Conf);
parse_config(Element = #xmlElement{name=mysql},
             Config=#config{curid = Id, session_tab = Tab,
                            sessions = [CurS | _], dynvar=DynVar,
                            subst    = SubstFlag, match=MatchRegExp}) ->

    Request = case ts_config:getAttr(atom, Element#xmlElement.attributes, type) of
                  sql ->
                      ValRaw = ts_config:getText(Element#xmlElement.content),
                      SQL = ts_utils:clean_str(ValRaw),
                      ?LOGF("Got SQL query: ~p~n",[SQL], ?NOTICE),
                      #mysql_request{sql=SQL, type= sql};
                  close ->
                      ?LOGF("Got Close queryp~n",[], ?NOTICE),
                      #mysql_request{type= close};
                  authenticate ->
                      Database = ts_config:getAttr(Element#xmlElement.attributes, database),
                      User     = ts_config:getAttr(Element#xmlElement.attributes, username),
                      Passwd   = ts_config:getAttr(Element#xmlElement.attributes, password),
                      ?LOGF("Got Auth datas: database->~p user->~p password->~p~n",[Database,User,Passwd], ?NOTICE),
                      #mysql_request{username=User, database=Database, passwd=Passwd,  type=authenticate};
                  connect ->
                      ?LOGF("Got Connect ~n",[], ?NOTICE),
                      #mysql_request{type=connect}
              end,
    Msg= #ts_request{ack     = parse,
                     endpage = true,
                     dynvar_specs = DynVar,
                     subst   = SubstFlag,
                     match   = MatchRegExp,
                     param   = Request},

    ts_config:mark_prev_req(Id-1, Tab, CurS),
    ets:insert(Tab,{{CurS#session.id, Id}, Msg }),
    lists:foldl( fun(A,B)->ts_config:parse(A,B) end,
                 Config#config{dynvar=[]},
                 Element#xmlElement.content);

%% Parsing other elements
parse_config(Element = #xmlElement{}, Conf = #config{}) ->
    ts_config:parse(Element,Conf);
%% Parsing non #xmlElement elements
parse_config(_, Conf = #config{}) ->
    Conf.

