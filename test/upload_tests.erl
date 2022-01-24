%%%-------------------------------------------------------------------
%%% Author  : Evgeny Khramtsov <ekhramtsov@process-one.net>
%%% Created : 17 May 2018 by Evgeny Khramtsov <ekhramtsov@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2021   ProcessOne
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
%%%-------------------------------------------------------------------
-module(upload_tests).

%% API
-compile(export_all).
-import(suite, [disconnect/1, is_feature_advertised/3, upload_jid/1,
		my_jid/1, wait_for_slave/1, wait_for_master/1,
		send_recv/2, put_event/2, get_event/1]).

-include("suite.hrl").
-define(CONTENT_TYPE, "image/png").

%%%===================================================================
%%% API
%%%===================================================================
%%%===================================================================
%%% Single user tests
%%%===================================================================
single_cases() ->
    {upload_single, [sequence],
     [single_test(feature_enabled),
      single_test(service_vcard),
      single_test(get_max_size),
      single_test(slot_request),
      single_test(put_get_request),
      single_test(max_size_exceed)]}.

feature_enabled(Config) ->
    lists:foreach(
      fun(NS) ->
	      true = is_feature_advertised(Config, NS, upload_jid(Config))
      end, namespaces()),
    disconnect(Config).

service_vcard(Config) ->
    Upload = upload_jid(Config),
    ct:comment("Retrieving vCard from ~s", [jid:encode(Upload)]),
    VCard = mod_http_upload_opt:vcard(?config(server, Config)),
    #iq{type = result, sub_els = [VCard]} =
	send_recv(Config, #iq{type = get, to = Upload, sub_els = [#vcard_temp{}]}),
    disconnect(Config).

get_max_size(Config) ->
    Xs = get_disco_info_xdata(Config),
    lists:foreach(
      fun(NS) ->
	      get_max_size(Config, Xs, NS)
      end, namespaces()),
    disconnect(Config).

get_max_size(_, _, ?NS_HTTP_UPLOAD_OLD) ->
    %% This old spec didn't specify 'max-file-size' attribute
    ok;
get_max_size(Config, Xs, NS) ->
    Xs = get_disco_info_xdata(Config),
    get_size(NS, Config, Xs).

slot_request(Config) ->
    lists:foreach(
      fun(NS) ->
	      slot_request(Config, NS)
      end, namespaces()),
    disconnect(Config).

put_get_request(Config) ->
    lists:foreach(
      fun(NS) ->
	      {GetURL, PutURL, _Filename, Size} = slot_request(Config, NS),
	      Data = p1_rand:bytes(Size),
	      put_request(Config, PutURL, Data),
	      get_request(Config, GetURL, Data)
      end, namespaces()),
    disconnect(Config).

max_size_exceed(Config) ->
    lists:foreach(
      fun(NS) ->
	      max_size_exceed(Config, NS)
      end, namespaces()),
    disconnect(Config).

%%%===================================================================
%%% Internal functions
%%%===================================================================
single_test(T) ->
    list_to_atom("upload_" ++ atom_to_list(T)).

get_disco_info_xdata(Config) ->
    To = upload_jid(Config),
    #iq{type = result, sub_els = [#disco_info{xdata = Xs}]} =
	send_recv(Config,
		  #iq{type = get, sub_els = [#disco_info{}], to = To}),
    Xs.

get_size(NS, Config, [X|Xs]) ->
    case xmpp_util:get_xdata_values(<<"FORM_TYPE">>, X) of
	[NS] ->
	    [Size] = xmpp_util:get_xdata_values(<<"max-file-size">>, X),
	    true = erlang:binary_to_integer(Size) > 0,
	    Size;
	_ ->
	    get_size(NS, Config, Xs)
    end;
get_size(NS, _Config, []) ->
    ct:fail({disco_info_xdata_failed, NS}).

slot_request(Config, NS) ->
    To = upload_jid(Config),
    Filename = filename(),
    Size = p1_rand:uniform(1, 1024),
    case NS of
	?NS_HTTP_UPLOAD_0 ->
	    #iq{type = result,
		sub_els = [#upload_slot_0{get = GetURL,
					  put = PutURL,
					  xmlns = NS}]} =
		send_recv(Config,
			  #iq{type = get, to = To,
			      sub_els = [#upload_request_0{
					    filename = Filename,
					    size = Size,
					    'content-type' = <<?CONTENT_TYPE>>,
					    xmlns = NS}]}),
	    {GetURL, PutURL, Filename, Size};
	_ ->
	    #iq{type = result,
		sub_els = [#upload_slot{get = GetURL,
					put = PutURL,
					xmlns = NS}]} =
		send_recv(Config,
			  #iq{type = get, to = To,
			      sub_els = [#upload_request{
					    filename = Filename,
					    size = Size,
					    'content-type' = <<?CONTENT_TYPE>>,
					    xmlns = NS}]}),
	    {GetURL, PutURL, Filename, Size}
    end.

put_request(_Config, URL0, Data) ->
    ct:comment("Putting ~B bytes to ~s", [size(Data), URL0]),
    URL = binary_to_list(URL0),
    {ok, {{"HTTP/1.1", 201, _}, _, _}} =
	httpc:request(put, {URL, [], ?CONTENT_TYPE, Data}, [], []).

get_request(_Config, URL0, Data) ->
    ct:comment("Getting ~B bytes from ~s", [size(Data), URL0]),
    URL = binary_to_list(URL0),
    {ok, {{"HTTP/1.1", 200, _}, _, Body}} =
	httpc:request(get, {URL, []}, [], [{body_format, binary}]),
    ct:comment("Checking returned body"),
    Body = Data.

max_size_exceed(Config, NS) ->
    To = upload_jid(Config),
    Filename = filename(),
    Size = 1000000000,
    IQErr =
	case NS of
	    ?NS_HTTP_UPLOAD_0 ->
		#iq{type = error} =
		    send_recv(Config,
			      #iq{type = get, to = To,
				  sub_els = [#upload_request_0{
						filename = Filename,
						size = Size,
						'content-type' = <<?CONTENT_TYPE>>,
						xmlns = NS}]});
	    _ ->
		#iq{type = error} =
		    send_recv(Config,
			      #iq{type = get, to = To,
				  sub_els = [#upload_request{
						filename = Filename,
						size = Size,
						'content-type' = <<?CONTENT_TYPE>>,
						xmlns = NS}]})
	end,
    check_size_error(IQErr, Size, NS).

check_size_error(IQErr, Size, NS) ->
    Err = xmpp:get_error(IQErr),
    FileTooLarge = xmpp:get_subtag(Err, #upload_file_too_large{xmlns = NS}),
    #stanza_error{reason = 'not-acceptable'} = Err,
    #upload_file_too_large{'max-file-size' = MaxSize} = FileTooLarge,
    true = Size > MaxSize.

namespaces() ->
    [?NS_HTTP_UPLOAD_0, ?NS_HTTP_UPLOAD, ?NS_HTTP_UPLOAD_OLD].

filename() ->
    <<(p1_rand:get_string())/binary, ".png">>.
