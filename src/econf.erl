%%%----------------------------------------------------------------------
%%% File    : econf.erl
%%% Purpose : Validator for ejabberd configuration options
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2025   ProcessOne
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
-export([group_dups/1]).
%% Simple types
-export([pos_int/0, pos_int/1, non_neg_int/0, non_neg_int/1]).
-export([int/0, int/2, number/1, octal/0]).
-export([binary/0, binary/1, binary/2]).
-export([string/0, string/1, string/2]).
-export([enum/1, bool/0, atom/0, any/0]).
%% Complex types
-export([url/0, url/1]).
-export([file/0, file/1]).
-export([directory/0, directory/1]).
-export([ip/0, ipv4/0, ipv6/0, ip_mask/0, port/0]).
-export([re/0, re/1, glob/0, glob/1]).
-export([path/0, binary_sep/1]).
-export([beam/0, beam/1, base64/0]).
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
-export([vcard_temp/0]).
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
    format("~ts: unknown ~ts: ~ts. Did you mean ~ts?",
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
    format("~ts: unknown ~ts: ~ts. Did you mean ~ts?",
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
	    format("~ts: '~ts' is not a ~ts",
		   [Slogan, format_module(Mod), Type]);
	false ->
	    case lists:member(Mod, ejabberd_config:beams(external)) of
		true ->
		    format("~ts: third-party ~ts '~ts' doesn't export "
			   "function ~ts/~B. If it's really a ~ts, "
			   "consider to upgrade it",
			   [Slogan, Type, format_module(Mod),F, A, Type]);
		false ->
		    format("~ts: '~ts' doesn't match any known ~ts",
			   [Slogan, format_module(Mod), Type])
	    end
    end;
format_error({unknown_option, [], _} = Why, Ctx) ->
    format("~ts. There are no available options",
	   [yconf:format_error(Why, Ctx)]);
format_error({unknown_option, Known, Opt} = Why, Ctx) ->
    format("~ts. Did you mean ~ts? ~ts",
	   [yconf:format_error(Why, Ctx),
	    misc:best_match(Opt, Known),
	    format_known("Available options", Known)]);
format_error({bad_enum, Known, Bad} = Why, Ctx) ->
    format("~ts. Did you mean ~ts? ~ts",
	   [yconf:format_error(Why, Ctx),
	    misc:best_match(Bad, Known),
	    format_known("Possible values", Known)]);
format_error({bad_yaml, _, _} = Why, _) ->
    format_error(Why);
format_error(Reason, Ctx) ->
    yconf:format_ctx(Ctx) ++ ": " ++ format_error(Reason).

format_error({bad_db_type, _, Atom}) ->
    format("unsupported database: ~ts", [Atom]);
format_error({bad_lang, Lang}) ->
    format("Invalid language tag: ~ts", [Lang]);
format_error({bad_pem, Why, Path}) ->
    format("Failed to read PEM file '~ts': ~ts",
	   [Path, pkix:format_error(Why)]);
format_error({bad_cert, Why, Path}) ->
    format_error({bad_pem, Why, Path});
format_error({bad_jwt_key, Path}) ->
    format("No valid JWT key found in file: ~ts", [Path]);
format_error({bad_jwt_key_set, Path}) ->
    format("JWK set contains multiple JWT keys in file: ~ts", [Path]);
format_error({bad_jid, Bad}) ->
    format("Invalid XMPP address: ~ts", [Bad]);
format_error({bad_user, Bad}) ->
    format("Invalid user part: ~ts", [Bad]);
format_error({bad_domain, Bad}) ->
    format("Invalid domain: ~ts", [Bad]);
format_error({bad_resource, Bad}) ->
    format("Invalid resource part: ~ts", [Bad]);
format_error({bad_ldap_filter, Bad}) ->
    format("Invalid LDAP filter: ~ts", [Bad]);
format_error({bad_sip_uri, Bad}) ->
    format("Invalid SIP URI: ~ts", [Bad]);
format_error({route_conflict, R}) ->
    format("Failed to reuse route '~ts' because it's "
	   "already registered on a virtual host",
	   [R]);
format_error({listener_dup, AddrPort}) ->
    format("Overlapping listeners found at ~ts",
	   [format_addr_port(AddrPort)]);
format_error({listener_conflict, AddrPort1, AddrPort2}) ->
    format("Overlapping listeners found at ~ts and ~ts",
	   [format_addr_port(AddrPort1),
	    format_addr_port(AddrPort2)]);
format_error({invalid_syntax, Reason}) ->
    format("~ts", [Reason]);
format_error({missing_module_dep, Mod, DepMod}) ->
    format("module ~ts depends on module ~ts, "
	   "which is not found in the config",
	   [Mod, DepMod]);
format_error(eimp_error) ->
    format("ejabberd is built without image converter support", []);
format_error({mqtt_codec, Reason}) ->
    mqtt_codec:format_error(Reason);
format_error({external_module_error, Module, Error}) ->
    try Module:format_error(Error)
    catch _:_ ->
	format("Invalid value", [])
    end;
format_error(Reason) ->
    yconf:format_error(Reason).

-spec format_module(atom() | string()) -> string().
format_module(Mod) when is_atom(Mod) ->
    format_module(atom_to_list(Mod));
format_module(Mod) ->
    case Mod of
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

%% All duplicated options having list-values are grouped
%% into a single option with all list-values being concatenated
-spec group_dups(list(T)) -> list(T).
group_dups(Y1) ->
    lists:reverse(
      lists:foldl(
        fun({Option, Values}, Acc) when is_list(Values) ->
                case lists:keyfind(Option, 1, Acc) of
                    {Option, Vals} when is_list(Vals) ->
                        lists:keyreplace(Option, 1, Acc, {Option, Vals ++ Values});
                    _ ->
                        [{Option, Values}|Acc]
                end;
           (Other, Acc) ->
                [Other|Acc]
        end, [], Y1)).

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

binary(Re, Opts) ->
    yconf:binary(Re, Opts).

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

string(Re, Opts) ->
    yconf:string(Re, Opts).

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

re(Opts) ->
    yconf:re(Opts).

glob() ->
    yconf:glob().

glob(Opts) ->
    yconf:glob(Opts).

path() ->
    yconf:path().

binary_sep(Sep) ->
    yconf:binary_sep(Sep).

timeout(Units) ->
    yconf:timeout(Units).

timeout(Units, Inf) ->
    yconf:timeout(Units, Inf).

base64() ->
    yconf:base64().

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
          {error, _} ->
            ElixirModule = "Elixir." ++ atom_to_list(T),
            case code:ensure_loaded(list_to_atom(ElixirModule)) of
              {module, _} -> list_to_atom(ElixirModule);
              {error, _} -> fail({bad_db_type, M, T})
            end
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
	    Hosts = ejabberd_config:get_option(hosts),
	    Domain3 = (domain())(Domain),
	    case lists:member(Domain3, Hosts) of
		true -> fail({route_conflict, Domain});
		false -> Domain3
	    end
    end.

-spec hosts() -> yconf:validator([binary()]).
hosts() ->
    list(host(), [unique]).

-spec vcard_temp() -> yconf:validator().
vcard_temp() ->
    and_then(
	vcard_validator(
	    vcard_temp, undefined,
	    [{version, undefined, binary()},
	     {fn, undefined, binary()},
	     {n, undefined, vcard_name()},
	     {nickname, undefined, binary()},
	     {photo, undefined, vcard_photo()},
	     {bday, undefined, binary()},
	     {adr, [], list(vcard_adr())},
	     {label, [], list(vcard_label())},
	     {tel, [], list(vcard_tel())},
	     {email, [], list(vcard_email())},
	     {jabberid, undefined, binary()},
	     {mailer, undefined, binary()},
	     {tz, undefined, binary()},
	     {geo, undefined, vcard_geo()},
	     {title, undefined, binary()},
	     {role, undefined, binary()},
	     {logo, undefined, vcard_logo()},
	     {org, undefined, vcard_org()},
	     {categories, [], list(binary())},
	     {note, undefined, binary()},
	     {prodid, undefined, binary()},
	     {rev, undefined, binary()},
	     {sort_string, undefined, binary()},
	     {sound, undefined, vcard_sound()},
	     {uid, undefined, binary()},
	     {url, undefined, binary()},
	     {class, undefined, enum([confidential, private, public])},
	     {key, undefined, vcard_key()},
	     {desc, undefined, binary()}]),
	fun(Tuple) ->
	    list_to_tuple(tuple_to_list(Tuple) ++ [[]])
	end).


-spec vcard_name() -> yconf:validator().
vcard_name() ->
    vcard_validator(
      vcard_name, undefined,
      [{family, undefined, binary()},
       {given, undefined, binary()},
       {middle, undefined, binary()},
       {prefix, undefined, binary()},
       {suffix, undefined, binary()}]).

-spec vcard_photo() -> yconf:validator().
vcard_photo() ->
    vcard_validator(
      vcard_photo, undefined,
      [{type, undefined, binary()},
       {binval, undefined, base64()},
       {extval, undefined, binary()}]).

-spec vcard_adr() -> yconf:validator().
vcard_adr() ->
    vcard_validator(
      vcard_adr, [],
      [{home, false, bool()},
       {work, false, bool()},
       {postal, false, bool()},
       {parcel, false, bool()},
       {dom, false, bool()},
       {intl, false, bool()},
       {pref, false, bool()},
       {pobox, undefined, binary()},
       {extadd, undefined, binary()},
       {street, undefined, binary()},
       {locality, undefined, binary()},
       {region, undefined, binary()},
       {pcode, undefined, binary()},
       {ctry, undefined, binary()}]).

-spec vcard_label() -> yconf:validator().
vcard_label() ->
    vcard_validator(
      vcard_label, [],
      [{home, false, bool()},
       {work, false, bool()},
       {postal, false, bool()},
       {parcel, false, bool()},
       {dom, false, bool()},
       {intl, false, bool()},
       {pref, false, bool()},
       {line, [], list(binary())}]).

-spec vcard_tel() -> yconf:validator().
vcard_tel() ->
    vcard_validator(
      vcard_tel, [],
      [{home, false, bool()},
       {work, false, bool()},
       {voice, false, bool()},
       {fax, false, bool()},
       {pager, false, bool()},
       {msg, false, bool()},
       {cell, false, bool()},
       {video, false, bool()},
       {bbs, false, bool()},
       {modem, false, bool()},
       {isdn, false, bool()},
       {pcs, false, bool()},
       {pref, false, bool()},
       {number, undefined, binary()}]).

-spec vcard_email() -> yconf:validator().
vcard_email() ->
    vcard_validator(
      vcard_email, [],
      [{home, false, bool()},
       {work, false, bool()},
       {internet, false, bool()},
       {pref, false, bool()},
       {x400, false, bool()},
       {userid, undefined, binary()}]).

-spec vcard_geo() -> yconf:validator().
vcard_geo() ->
    vcard_validator(
      vcard_geo, undefined,
      [{lat, undefined, binary()},
       {lon, undefined, binary()}]).

-spec vcard_logo() -> yconf:validator().
vcard_logo() ->
    vcard_validator(
      vcard_logo, undefined,
      [{type, undefined, binary()},
       {binval, undefined, base64()},
       {extval, undefined, binary()}]).

-spec vcard_org() -> yconf:validator().
vcard_org() ->
    vcard_validator(
      vcard_org, undefined,
      [{name, undefined, binary()},
       {units, [], list(binary())}]).

-spec vcard_sound() -> yconf:validator().
vcard_sound() ->
    vcard_validator(
      vcard_sound, undefined,
      [{phonetic, undefined, binary()},
       {binval, undefined, base64()},
       {extval, undefined, binary()}]).

-spec vcard_key() -> yconf:validator().
vcard_key() ->
    vcard_validator(
      vcard_key, undefined,
      [{type, undefined, binary()},
       {cred, undefined, binary()}]).

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

-spec vcard_validator(atom(), term(), [{atom(), term(), validator()}]) -> validator().
vcard_validator(Name, Default, Schema) ->
    Defaults = [{Key, Val} || {Key, Val, _} <- Schema],
    and_then(
      options(
	maps:from_list([{Key, Fun} || {Key, _, Fun} <- Schema]),
	[{return, map}, {unique, true}]),
      fun(Options) ->
	      merge(Defaults, Options, Name, Default)
      end).

-spec merge([{atom(), term()}], #{atom() => term()}, atom(), T) -> tuple() | T.
merge(_, Options, _, Default) when Options == #{} ->
    Default;
merge(Defaults, Options, Name, _) ->
    list_to_tuple([Name|[maps:get(Key, Options, Val) || {Key, Val} <- Defaults]]).
