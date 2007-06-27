%%%----------------------------------------------------------------------
%%% File    : mod_proxy65_http
%%% Author  : Jérôme Sautret <jerome.sautret@process-one.net>
%%% Purpose : HTTP bytestreams proxy for oneteam file transfert
%%% Created : 28 may 2007
%%% Id      : $Id: mod_last.erl 370 2005-06-20 03:18:13Z alexey $
%%%----------------------------------------------------------------------

-module(mod_proxy65_http).
-author('jerome.sautret@process-one.net').

-behaviour(gen_mod).

%% gen_mod callbacks.
-export([
    start/2,
    stop/1
    ]).
%% mod_proxy65 api
-export([
	 activate/2
	]).
%% ejabberd_http callback.
-export([
    process/2
    ]).

-include("mod_proxy65.hrl").
-include("../ejabberd.hrl").
-include("../jlib.hrl").
-include("../web/ejabberd_http.hrl").

%%%----------------------------------------------------------------------
%%% gen_mod Callbacks
%%%----------------------------------------------------------------------

start(_Host, _Opts) ->
    ok.

stop(_Host) ->
    ok.

activate({_IPid, _IJid}, {_TPid, _TJid}) ->
    ok.

%%%----------------------------------------------------------------------
%%% ejabberd_http Callbacks
%%%----------------------------------------------------------------------

% Receive File
% XXX TODO: limit file size
process(["upload"], #request{method='POST',
		     content_type = "multipart/form-data;"++_Boundary = ContentType,
		     data=Data} = _Request) ->
    io:format("POST~n", []),
    DataParts = ejabberd_http:parse_data(Data, ContentType),
    {SHA1, {Filename, FileContentType, FileContent}} = parse_upload_data(DataParts),
    case mnesia:dirty_read(bytestream, SHA1) of
	[#bytestream{jid_t = TargetJID, file = BaseURL, myhost = MyHost}] when is_list(BaseURL) ->
	    Path = store_file(Filename, FileContent),
	    F = fun() ->
			mnesia:write(#bytestream{sha1 = SHA1,
						 file = {path, Path},
						 target = FileContentType})
		   end,
	    mnesia:transaction(F),
	    URL = BaseURL ++ "/" ++ filename:join(Path),
	    send_activated(TargetJID, MyHost, SHA1, URL),
	    Result = "ok",
	    ejabberd_web:make_xhtml([{xmlcdata, Result}]);
	_Other ->
	    ?ERROR_MSG("Upload ~p not activated~n", [SHA1]),
	    ejabberd_web:error(bad_request)
    end;

process([_UID, _Filename] = Path, #request{method='GET'} = _Request) ->
    io:format("GET~n~p~n", [Path]),
    case mnesia:dirty_index_read(bytestream, {path, Path}, #bytestream.file) of
	[#bytestream{sha1=SHA1, target=ContentType}|_Tail] ->
	    mnesia:dirty_delete({bytestream, SHA1}),
	    serve_file(Path, ContentType);
	_ ->
	    ?ERROR_MSG("Bad request, GET ~p~n", [Path]),
	    ejabberd_web:error(bad_request)
    end.


% store the data transfered in a file and return the URL of the file
store_file(Filename, FileContent) ->
    HASH = sha:sha(FileContent),
    % TODO store dir from a parameter
    Path = ?DEFAULT_STORE_PATH ++ "/" ++ HASH ++ "/",
    io:format("Path ~p~n", [Path]),
    ok = filelib:ensure_dir(Path),
    FilePath = Path ++ "/" ++ Filename,
    %io:format("FilePath ~p~n~p", [FilePath, FileContent]),
    ok = file:write_file(FilePath, list_to_binary(FileContent)),
    [HASH, Filename].

% send the <activated> connexion to the target.
send_activated(TargetJID, MyJID, SHA1, URL) ->
    IQ = #iq{type=set,
	     sub_el=[{xmlelement, "activated",
		      [{"xmlns", ?NS_HTTP_BYTESTREAMS}, {"sidhash", SHA1}, {"url", URL}], []}]},
    ejabberd_router:route(jlib:string_to_jid(MyJID), TargetJID, jlib:iq_to_xml(IQ)).


parse_upload_data(Data) ->
    parse_upload_data(Data, {undefined, undefined, undefined}).
parse_upload_data([], Result) ->
    Result;
parse_upload_data([#http_data{content_disposition="form-data",
			      content_type=ContentType, args=Args, data=Data} | Tail],
		  Result) ->
    Result2 = case lists:keysearch("name", 1, Args) of
		 {value, {"name", "SIDHASH"}} ->
		     {remove_cr(Data), element(2, Result)};
		 {value, {"name", "FILE"}} ->
		      case lists:keysearch("filename", 1, Args) of
			  {value, {"filename", Filename}} ->
			      {element(1, Result), {Filename, ContentType, remove_cr(Data)}}
		      end;
		  _ ->
		      Result
	      end,
    parse_upload_data(Tail, Result2).


% remove last trailling carriage return
remove_cr(String) ->
    lists:reverse(remove_leading_cr(lists:reverse(String))).
% remove fisrt leading inversed carriage return
remove_leading_cr([$\n, $\r|String]) ->
    String;
remove_leading_cr(String) ->
    String.


serve_file(Path, ContentType) ->
    FileName = filename:join([?DEFAULT_STORE_PATH | Path]),
    case file:read_file(FileName) of
        {ok, FileContents} ->
            ?DEBUG("Delivering content.", []),
            {200,
             [{"Server", "ejabberd"},
              {"Content-type", ContentType},
	      {"Content-disposition", "attachment; filename="++filename:basename(FileName)}],
             FileContents};
        {error, Error} ->
            ?DEBUG("Delivering error: ~p", [Error]),
            case Error of
                eacces -> {403, [], "Forbidden"};
                enoent -> {404, [], "Not found"};
                _Else -> {500, [], atom_to_list(Error)}
            end
    end.
