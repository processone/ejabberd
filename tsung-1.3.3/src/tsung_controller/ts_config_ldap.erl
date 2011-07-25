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

%%% File    : ts_ldap.erl
%%% Author  : Pablo Polvorin <ppolv@yahoo.com.ar>
%%% Purpose : LDAP plugin

-module(ts_config_ldap).

-export([ parse_config/2 ]).

-include("ts_profile.hrl").
-include("ts_config.hrl").
-include("xmerl.hrl").
-include("ts_ldap.hrl").


%%----------------------------------------------------------------
%%-----Configuration parsing
%%----------------------------------------------------------------
parse_config(Element = #xmlElement{name=dyn_variable}, Conf = #config{}) ->
    ts_config:parse(Element,Conf);

parse_config(Element = #xmlElement{name=ldap},
             Config=#config{curid = Id, session_tab = Tab,
                            sessions = [CurS | _], dynvar=DynVar,
                            subst    = SubstFlag, match=MatchRegExp}) ->

    Request = case ts_config:getAttr(atom, Element#xmlElement.attributes, type) of
                  start_tls ->
                      Cacert = ts_config:getAttr(string,Element#xmlElement.attributes,cacertfile),
                      KeyFile = ts_config:getAttr(string,Element#xmlElement.attributes,keyfile),
                      CertFile = ts_config:getAttr(string,Element#xmlElement.attributes,certfile),
                      #ts_request{ack = parse,
                                  endpage = true,
                                  dynvar_specs= DynVar,
                                  subst = SubstFlag,
                                  match= MatchRegExp,
                                  param = #ldap_request{type=start_tls,cacertfile=Cacert,keyfile=KeyFile,certfile=CertFile}};

                  bind ->
                      User = ts_config:getAttr(string,Element#xmlElement.attributes,user),
                      Password = ts_config:getAttr(string,Element#xmlElement.attributes,password),
                      #ts_request{ack     = parse,
                                  endpage = true,
                                  dynvar_specs  = DynVar,
                                  subst   = SubstFlag,
                                  match   = MatchRegExp,
                                  param   = #ldap_request{type=bind,user=User,password=Password}};

                  unbind ->
                      #ts_request{ack     = no_ack,
                                  endpage = true,
                                  dynvar_specs  = DynVar,
                                  subst   = SubstFlag,
                                  match   = MatchRegExp,
                                  param   = #ldap_request{type=unbind}};
                  search ->
                      Base = ts_config:getAttr(string,Element#xmlElement.attributes,base),
                      Scope = ts_config:getAttr(atom,Element#xmlElement.attributes,scope),
                      Filter = ts_config:getAttr(string,Element#xmlElement.attributes,filter),
                      ResultVar = case ts_config:getAttr(string,Element#xmlElement.attributes,result_var,none) of
                                        none -> none;
                                        VarName -> {ok,list_to_atom(VarName)}
                                 end,

                      Attributes=[],
                      {ParsedFilter,[]} = rfc4515_parser:filter(rfc4515_parser:tokenize(Filter)),
                      #ts_request{ack     = parse,
                                  endpage = true,
                                  dynvar_specs  = DynVar,
                                  subst   = SubstFlag,
                                  match   = MatchRegExp,
                                  param   = #ldap_request{type=search,
                                                          result_var = ResultVar,
                                                          base=Base,
                                                          scope=Scope,
                                                          filter=ParsedFilter,
                                                          attributes=Attributes}};
                  add ->
                      DN = ts_config:getAttr(string,Element#xmlElement.attributes,dn),
                      XMLAttrs = [El || El <- Element#xmlElement.content, is_record(El,xmlElement)],
                      Attrs = lists:map(fun(#xmlElement{name=attr,attributes=Attr,content=Content}) ->
                                                Vals = lists:foldl(fun(#xmlElement{name=value,content=[#xmlText{value=Value}]},Values) ->
                                                                           [binary_to_list(iolist_to_binary(Value))|Values]
                                                                   end,[] ,[E || E=#xmlElement{name=value} <- Content]),
                                                {ts_config:getAttr(string,Attr,type),Vals}
                                        end, XMLAttrs),
                      #ts_request{ack     = parse,
                                  endpage = true,
                                  dynvar_specs  = DynVar,
                                  subst   = SubstFlag,
                                  match   = MatchRegExp,
                                  param   = #ldap_request{type=add,
                                                          dn=DN,
                                                          attrs=Attrs
                                                         }};
                  modify ->
                      DN = ts_config:getAttr(string,Element#xmlElement.attributes,dn),
                      Modifications = [{list_to_atom(ts_config:getAttr(string,Attrs,type)),parse_xml_attr_type_and_value(Content)} ||
                                          #xmlElement{name=modification,attributes=Attrs,content=Content} <- Element#xmlElement.content],

                      ExpandedModifications =  lists:foldl( fun({Operation,Attrs},L) ->
                                                                    lists:foldl(fun({Type,Values},L2) ->
                                                                                        [{Operation,Type,Values}|L2]
                                                                                end,L,Attrs)
                                                            end,[],Modifications),
                      #ts_request{ack     = parse,
                                  endpage = true,
                                  dynvar_specs  = DynVar,
                                  subst   = SubstFlag,
                                  match   = MatchRegExp,
                                  param   = #ldap_request{type=modify,
                                                          modifications = ExpandedModifications,
                                                          dn=DN
                                                         }}
              end,

    ts_config:mark_prev_req(Id-1, Tab, CurS),
    ets:insert(Tab,{{CurS#session.id, Id}, Request }),
    lists:foldl( fun(A,B)->ts_config:parse(A,B) end,
                 Config#config{dynvar=[]},
                 Element#xmlElement.content);

%% Parsing other elements
parse_config(Element = #xmlElement{}, Conf = #config{}) ->
    ts_config:parse(Element,Conf);
%% Parsing non #xmlElement elements
parse_config(_, Conf = #config{}) ->
    Conf.

parse_xml_attr_values(Elements) ->
    [binary_to_list(iolist_to_binary(Value))
     || #xmlElement{name=value,content=[#xmlText{value=Value}]} <- Elements].

parse_xml_attr_type_and_value(Elements) ->
    [ {ts_config:getAttr(string,Attr,type),parse_xml_attr_values(Content)}
      || #xmlElement{name=attr,attributes=Attr,content=Content} <-Elements].


