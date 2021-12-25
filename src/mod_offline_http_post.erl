%% name of module must match file name
%% Update: info@ph-f.nl
-module(mod_offline_http_post).
-author("hiren@xrstudio.in").

-behaviour(gen_mod).

-export([start/2, stop/1, create_message/1, mod_options/1,depends/2, mod_opt_type/1, create_message/3, muc_filter_message/3,  on_muc_filter_message/3, on_create_message/3, on_create_message/1]).

%% -include("xmpp/include/scram.hrl").
-include_lib("xmpp/include/xmpp.hrl").
-include("logger.hrl").

start(_Host, _Opt) ->
  ?INFO_MSG("mod_offline_http_post loading", []),
  inets:start(),
  ?INFO_MSG("HTTP client started", []),
  ejabberd_hooks:add(muc_filter_message, _Host, ?MODULE, on_muc_filter_message, 10),
  ejabberd_hooks:add(offline_message_hook, _Host, ?MODULE, on_create_message, 1).


stop (_Host) ->
  ?INFO_MSG("stopping mod_offline_http_post", []),
  ejabberd_hooks:delete(muc_filter_message, _Host, ?MODULE, on_muc_filter_message, 10),
  ejabberd_hooks:delete(offline_message_hook, _Host, ?MODULE, on_create_message, 1).


depends(_Host, _Opts) ->
  [].


mod_options(_Host) ->
    [{post_url,"https://qa.aapoon.com/api/v1/notify_redirect/"},{auth_token, <<"secret">>}].


mod_opt_type(post_url) -> fun iolist_to_binary/1;
mod_opt_type(auth_token) -> fun iolist_to_binary/1.

on_muc_filter_message(Packet, State, FromNick)->
  spawn(?MODULE, muc_filter_message, [Packet, State, FromNick]),
  Packet.

on_create_message(Acc)->
  spawn(?MODULE, create_message, [Acc]),
  Acc.

on_create_message(From, To, Packet)->
  spawn(?MODULE, create_message, [From, To, Packet]),
  Packet.


create_message({Action, Packet} = Acc) when (Packet#message.type == chat) and (Packet#message.body /= []) ->
        [{text, _, Body}] = Packet#message.body,
        MessageXml = fxml:element_to_binary(xmpp:encode(Packet)),
        ?INFO_MSG("packet log: ~s", [MessageXml]),
        post_offline_message(Packet#message.from, Packet#message.to, Body, Packet#message.id,MessageXml),
  Acc;

create_message(Acc) ->
  Acc.

create_message(_From, _To, Packet) when (Packet#message.type == chat) and (Packet#message.body /= []) ->
  Body = fxml:get_path_s(Packet, [{elem, list_to_binary("body")}, cdata]),
  MessageId = fxml:get_tag_attr_s(list_to_binary("id"), Packet),
  MessageXml = fxml:element_to_binary(xmpp:encode(Packet)),
  ?INFO_MSG("packet log: ~s", [MessageXml]),
  post_offline_message(_From, _To, Body, MessageId, MessageXml),
  ok.

muc_filter_message(Packet, State, FromNick) ->
  if
    Packet#message.body /= [] ->
      [{text, _, Body}] = Packet#message.body,
      MessageXml = fxml:element_to_binary(xmpp:encode(Packet)),
      ?INFO_MSG("packet log: ~s", [MessageXml]),
      post_offline_message(Packet#message.from, Packet#message.to, Body, Packet#message.id, MessageXml);
    true ->
      ?INFO_MSG("skipping chat state message", [])
  end,
  Packet.

post_offline_message(From, To, Body, MessageId, MessageXml) ->
  ?INFO_MSG("Posting domain ~p From ~p To ~p Body ~p ID ~p Packet ~p ~n",[ To#jid.lserver,From, To, Body, MessageId, MessageXml]),
  PostUrl_config = gen_mod:get_module_opt(From#jid.lserver, ?MODULE, post_url),
  PostUrl = binary_to_list(PostUrl_config),
  Token_config = gen_mod:get_module_opt(From#jid.lserver, ?MODULE, auth_token),
  Token = binary_to_list(Token_config),
  ?INFO_MSG("Posting From ~p To ~p Body ~p ID ~p PostUrl ~p Token ~p ~n",[From, To, Body, MessageId, PostUrl,Token]),
  ?DEBUG("URL is ~p~n",[PostUrl]),
  ToUser = To#jid.luser,
  FromUser = From#jid.luser,
  Vhost = To#jid.lserver,
  EncodeString = http_uri:encode(MessageXml),
  Data = string:join(["to=", binary_to_list(ToUser), "&xml=", binary_to_list(EncodeString), "&from=", binary_to_list(FromUser), "&vhost=", binary_to_list(Vhost), "&body=", binary_to_list(Body), "&messageId=", binary_to_list(MessageId)], ""),
  ?INFO_MSG("Posting PostUrl ~p Token ~p Data ~p~n",[PostUrl,  Token, Data]),
  Request = {PostUrl, [{"Authorization", Token}], "application/x-www-form-urlencoded", Data},
  httpc:request(post, Request,[],[]),
  ?INFO_MSG("post request sent", []).
