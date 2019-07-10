%%%----------------------------------------------------------------------
%%% File    : econf.erl
%%% Purpose : Validator for ejabberd configuration options
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2019   ProcessOne
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
-module(econf).

%% API
-export([parse/3, validate/2, fail/1, format_error/2, replace_macros/1]).
%% Simple types
-export([pos_int/0, pos_int/1, non_neg_int/0, non_neg_int/1]).
-export([int/0, int/2, number/1, octal/0]).
-export([binary/0, binary/1]).
-export([string/0, string/1]).
-export([enum/1, bool/0, atom/0, any/0]).
%% Complex types
-export([url/0, url/1]).
-export([file/0, file/1]).
-export([directory/0, directory/1]).
-export([ip/0, ipv4/0, ipv6/0, ip_mask/0, port/0]).
-export([re/0, glob/0]).
-export([path/0, binary_sep/1]).
-export([beam/0, beam/1]).
-export([timeout/1, timeout/2]).
%% Composite types
-export([list/1, list/2]).
-export([list_or_single/1, list_or_single/2]).
-export([map/2, map/3]).
-export([either/2, and_then/2, non_empty/1]).
-export([options/1, options/2]).
%% Custom types
-export([acl/0, shaper/0, url_or_file/0, lang/0]).
-export([pem/0, queue_type/0]).
-export([jid/0, user/0, domain/0, resource/0]).
-export([db_type/1, ldap_filter/0]).
-export([host/0, hosts/0]).
-ifdef(SIP).
-export([sip_uri/0]).
-endif.

-type error_reason() :: term().
-type error_return() :: {error, error_reason(), yconf:ctx()}.
-type validator() :: yconf:validator().
-type validator(T) :: yconf:validator(T).
-type validators() :: yconf:validators().
-export_type([validator/0, validator/1, validators/0]).
-export_type([error_reason/0, error_return/0]).

%%%===================================================================
%%% API
%%%===================================================================
parse(File, Validators, Options) ->
    try yconf:parse(File, Validators, Options)
    catch _:{?MODULE, Reason, Ctx} ->
	    {error, Reason, Ctx}
    end.

validate(Validator, Y) ->
    try yconf:validate(Validator, Y)
    catch _:{?MODULE, Reason, Ctx} ->
	    {error, Reason, Ctx}
    end.

replace_macros(Y) ->
    yconf:replace_macros(Y).

-spec fail(error_reason()) -> no_return().
fail(Reason) ->
    yconf:fail(?MODULE, Reason).

format_error({bad_module, Mod}, Ctx)
  when Ctx == [listen, module];
       Ctx == [listen, request_handlers] ->
    Mods = ejabberd_config:beams(all),
    format("~s: unknown ~s: ~s. Did you mean ~s?",
	   [yconf:format_ctx(Ctx),
	    format_module_type(Ctx),
	    format_module(Mod),
	    format_module(misc:best_match(Mod, Mods))]);
format_error({bad_module, Mod}, Ctx)
  when Ctx == [modules] ->
    Mods = lists:filter(
	     fun(M) ->
		     case atom_to_list(M) of
			 "mod_" ++ _ -> true;
			 "Elixir.Mod" ++ _ -> true;
			 _ -> false
		     end
	     end, ejabberd_config:beams(all)),
    format("~s: unknown ~s: ~s. Did you mean ~s?",
	   [yconf:format_ctx(Ctx),
	    format_module_type(Ctx),
	    format_module(Mod),
	    format_module(misc:best_match(Mod, Mods))]);
format_error({bad_export, {F, A}, Mod}, Ctx)
  when Ctx == [listen, module];
       Ctx == [listen, request_handlers];
       Ctx == [modules] ->
    Type = format_module_type(Ctx),
    Slogan = yconf:format_ctx(Ctx),
    case lists:member(Mod, ejabberd_config:beams(local)) of
	true ->
	    format("~s: '~s' is not a ~s",
		   [Slogan, format_module(Mod), Type]);
	false ->
	    case lists:member(Mod, ejabberd_config:beams(external)) of
		true ->
		    format("~s: third-party ~s '~s' doesn't export "
			   "function ~s/~B. If it's really a ~s, "
			   "consider to upgrade it",
			   [Slogan, Type, format_module(Mod),F, A, Type]);
		false ->
		    format("~s: '~s' doesn't match any known ~s",
			   [Slogan, format_module(Mod), Type])
	    end
    end;
format_error({unknown_option, [], _} = Why, Ctx) ->
    format("~s. There are no available options",
	   [yconf:format_error(Why, Ctx)]);
format_error({unknown_option, Known, Opt} = Why, Ctx) ->
    format("~s. Did you mean ~s? ~s",
	   [yconf:format_error(Why, Ctx),
	    misc:best_match(Opt, Known),
	    format_known("Available options", Known)]);
format_error({bad_enum, Known, Bad} = Why, Ctx) ->
    format("~s. Did you mean ~s? ~s",
	   [yconf:format_error(Why, Ctx),
	    misc:best_match(Bad, Known),
	    format_known("Possible values", Known)]);
format_error({bad_yaml, _, _} = Why, _) ->
    format_error(Why);
format_error(Reason, Ctx) ->
    [H|T] = format_error(Reason),
    yconf:format_ctx(Ctx) ++ ": " ++ [string:to_lower(H)|T].

format_error({bad_db_type, _, Atom}) ->
    format("unsupported database: ~s", [Atom]);
format_error({bad_lang, Lang}) ->
    format("Invalid language tag: ~s", [Lang]);
format_error({bad_pem, Why, Path}) ->
    format("Failed to read PEM file '~s': ~s",
	   [Path, pkix:format_error(Why)]);
format_error({bad_cert, Why, Path}) ->
    format_error({bad_pem, Why, Path});
format_error({bad_jid, Bad}) ->
    format("Invalid XMPP address: ~s", [Bad]);
format_error({bad_user, Bad}) ->
    format("Invalid user part: ~s", [Bad]);
format_error({bad_domain, Bad}) ->
    format("Invalid domain: ~s", [Bad]);
format_error({bad_resource, Bad}) ->
    format("Invalid resource part: ~s", [Bad]);
format_error({bad_ldap_filter, Bad}) ->
    format("Invalid LDAP filter: ~s", [Bad]);
format_error({bad_sip_uri, Bad}) ->
    format("Invalid SIP URI: ~s", [Bad]);
format_error({route_conflict, R}) ->
    format("Failed to reuse route '~s' because it's "
	   "already registered on a virtual host",
	   [R]);
format_error({listener_dup, AddrPort}) ->
    format("Overlapping listeners found at ~s",
	   [format_addr_port(AddrPort)]);
format_error({listener_conflict, AddrPort1, AddrPort2}) ->
    format("Overlapping listeners found at ~s and ~s",
	   [format_addr_port(AddrPort1),
	    format_addr_port(AddrPort2)]);
format_error({invalid_syntax, Reason}) ->
    format("~s", [Reason]);
format_error({missing_module_dep, Mod, DepMod}) ->
    format("module ~s depends on module ~s, "
	   "which is not found in the config",
	   [Mod, DepMod]);
format_error(eimp_error) ->
    format("ejabberd is built without image converter support", []);
format_error({mqtt_codec, Reason}) ->
    mqtt_codec:format_error(Reason);
format_error(Reason) ->
    yconf:format_error(Reason).

-spec format_module(atom()) -> string().
format_module(Mod) ->
    case atom_to_list(Mod) of
	"Elixir." ++ M -> M;
	M -> M
    end.

format_module_type([listen, module]) ->
    "listening module";
format_module_type([listen, request_handlers]) ->
    "HTTP request handler";
format_module_type([modules]) ->
    "ejabberd module".

format_known(_, Known) when length(Known) > 20 ->
    "";
format_known(Prefix, Known) ->
    [Prefix, " are: ", format_join(Known)].

format_join([]) ->
    "(empty)";
format_join([H|_] = L) when is_atom(H) ->
    format_join([atom_to_binary(A, utf8) || A <- L]);
format_join(L) ->
    str:join(lists:sort(L), <<", ">>).

%%%===================================================================
%%% Validators from yconf
%%%===================================================================
pos_int() ->
    yconf:pos_int().

pos_int(Inf) ->
    yconf:pos_int(Inf).

non_neg_int() ->
    yconf:non_neg_int().

non_neg_int(Inf) ->
    yconf:non_neg_int(Inf).

int() ->
    yconf:int().

int(Min, Max) ->
    yconf:int(Min, Max).

number(Min) ->
    yconf:number(Min).

octal() ->
    yconf:octal().

binary() ->
    yconf:binary().

binary(Re) ->
    yconf:binary(Re).

enum(L) ->
    yconf:enum(L).

bool() ->
    yconf:bool().

atom() ->
    yconf:atom().

string() ->
    yconf:string().

string(Re) ->
    yconf:string(Re).

any() ->
    yconf:any().

url() ->
    yconf:url().

url(Schemes) ->
    yconf:url(Schemes).

file() ->
    yconf:file().

file(Type) ->
    yconf:file(Type).

directory() ->
    yconf:directory().

directory(Type) ->
    yconf:directory(Type).

ip() ->
    yconf:ip().

ipv4() ->
    yconf:ipv4().

ipv6() ->
    yconf:ipv6().

ip_mask() ->
    yconf:ip_mask().

port() ->
    yconf:port().

re() ->
    yconf:re().

glob() ->
    yconf:glob().

path() ->
    yconf:path().

binary_sep(Sep) ->
    yconf:binary_sep(Sep).

timeout(Units) ->
    yconf:timeout(Units).

timeout(Units, Inf) ->
    yconf:timeout(Units, Inf).

non_empty(F) ->
    yconf:non_empty(F).

list(F) ->
    yconf:list(F).

list(F, Opts) ->
    yconf:list(F, Opts).

list_or_single(F) ->
    yconf:list_or_single(F).

list_or_single(F, Opts) ->
    yconf:list_or_single(F, Opts).

map(F1, F2) ->
    yconf:map(F1, F2).

map(F1, F2, Opts) ->
    yconf:map(F1, F2, Opts).

either(F1, F2) ->
    yconf:either(F1, F2).

and_then(F1, F2) ->
    yconf:and_then(F1, F2).

options(V) ->
    yconf:options(V).

options(V, O) ->
    yconf:options(V, O).

%%%===================================================================
%%% Custom validators
%%%===================================================================
beam() ->
    beam([]).

beam(Exports) ->
    and_then(
      non_empty(binary()),
      fun(<<"Elixir.", _/binary>> = Val) ->
	      (yconf:beam(Exports))(Val);
	 (<<C, _/binary>> = Val) when C >= $A, C =< $Z ->
	      (yconf:beam(Exports))(<<"Elixir.", Val/binary>>);
	 (Val) ->
	      (yconf:beam(Exports))(Val)
      end).

acl() ->
    either(
      atom(),
      acl:access_rules_validator()).

shaper() ->
    either(
      atom(),
      ejabberd_shaper:shaper_rules_validator()).

-spec url_or_file() -> yconf:validator({file | url, binary()}).
url_or_file() ->
    either(
      and_then(url(), fun(URL) -> {url, URL} end),
      and_then(file(), fun(File) -> {file, File} end)).

-spec lang() -> yconf:validator(binary()).
lang() ->
    and_then(
      binary(),
      fun(Lang) ->
	      try xmpp_lang:check(Lang)
	      catch _:_ -> fail({bad_lang, Lang})
	      end
      end).

-spec pem() -> yconf:validator(binary()).
pem() ->
    and_then(
      path(),
      fun(Path) ->
	      case pkix:is_pem_file(Path) of
		  true -> Path;
		  {false, Reason} ->
		      fail({bad_pem, Reason, Path})
	      end
      end).

-spec jid() -> yconf:validator(jid:jid()).
jid() ->
    and_then(
      binary(),
      fun(Val) ->
	      try jid:decode(Val)
	      catch _:{bad_jid, _} = Reason -> fail(Reason)
	      end
      end).

-spec user() -> yconf:validator(binary()).
user() ->
    and_then(
      binary(),
      fun(Val) ->
	      case jid:nodeprep(Val) of
		  error -> fail({bad_user, Val});
		  U -> U
	      end
      end).

-spec domain() -> yconf:validator(binary()).
domain() ->
    and_then(
      non_empty(binary()),
      fun(Val) ->
	      try jid:tolower(jid:decode(Val)) of
		  {<<"">>, Domain, <<"">>} -> Domain;
		  _ -> fail({bad_domain, Val})
	      catch _:{bad_jid, _} ->
		      fail({bad_domain, Val})
	      end
      end).

-spec resource() -> yconf:validator(binary()).
resource() ->
    and_then(
      binary(),
      fun(Val) ->
	      case jid:resourceprep(Val) of
		  error -> fail({bad_resource, Val});
		  R -> R
	      end
      end).

-spec db_type(module()) -> yconf:validator(atom()).
db_type(M) ->
    and_then(
      atom(),
      fun(T) ->
	      case code:ensure_loaded(db_module(M, T)) of
		  {module, _} -> T;
		  {error, _} -> fail({bad_db_type, M, T})
	      end
      end).

-spec queue_type() -> yconf:validator(ram | file).
queue_type() ->
    enum([ram, file]).

-spec ldap_filter() -> yconf:validator(binary()).
ldap_filter() ->
    and_then(
      binary(),
      fun(Val) ->
	      case eldap_filter:parse(Val) of
		  {ok, _} -> Val;
		  _ -> fail({bad_ldap_filter, Val})
	      end
      end).

-ifdef(SIP).
sip_uri() ->
    and_then(
      binary(),
      fun(Val) ->
	      case esip:decode_uri(Val) of
		  error -> fail({bad_sip_uri, Val});
		  URI -> URI
	      end
      end).
-endif.

-spec host() -> yconf:validator(binary()).
host() ->
    fun(Domain) ->
	    Host = ejabberd_config:get_myname(),
	    Hosts = ejabberd_config:get_option(hosts),
	    Domain1 = (binary())(Domain),
	    Domain2 = misc:expand_keyword(<<"@HOST@">>, Domain1, Host),
	    Domain3 = (domain())(Domain2),
	    case lists:member(Domain3, Hosts) of
		true -> fail({route_conflict, Domain3});
		false -> Domain3
	    end
    end.

-spec hosts() -> yconf:validator([binary()]).
hosts() ->
    list(host(), [unique]).

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec db_module(module(), atom()) -> module().
db_module(M, Type) ->
    try list_to_atom(atom_to_list(M) ++ "_" ++ atom_to_list(Type))
    catch _:system_limit ->
	    fail({bad_length, 255})
    end.

format_addr_port({IP, Port}) ->
    IPStr = case tuple_size(IP) of
		4 -> inet:ntoa(IP);
		8 -> "[" ++ inet:ntoa(IP) ++ "]"
	    end,
    IPStr ++ ":" ++ integer_to_list(Port).

-spec format(iolist(), list()) -> string().
format(Fmt, Args) ->
    lists:flatten(io_lib:format(Fmt, Args)).
