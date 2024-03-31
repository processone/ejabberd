%%%----------------------------------------------------------------------
%%% File    : ejabberd_doc.erl
%%% Purpose : Options documentation generator
%%%
%%% ejabberd, Copyright (C) 2002-2024   ProcessOne
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
-module(ejabberd_doc).

%% API
-export([man/0, man/1, have_a2x/0]).

-include("translate.hrl").

%%%===================================================================
%%% API
%%%===================================================================
man() ->
    man(<<"en">>).

man(Lang) when is_list(Lang) ->
    man(list_to_binary(Lang));
man(Lang) ->
    {ModDoc, SubModDoc} =
        lists:foldl(
          fun(M, {Mods, SubMods} = Acc) ->
                  case lists:prefix("mod_", atom_to_list(M)) orelse
                      lists:prefix("Elixir.Mod", atom_to_list(M)) of
                      true ->
                          try M:mod_doc() of
                              #{desc := Descr} = Map ->
                                  DocOpts = maps:get(opts, Map, []),
                                  Example = maps:get(example, Map, []),
                                  Note = maps:get(note, Map, []),
                                  {[{M, Descr, DocOpts, #{example => Example, note => Note}}|Mods], SubMods};
                              #{opts := DocOpts} ->
                                  {ParentMod, Backend} = strip_backend_suffix(M),
                                  {Mods, dict:append(ParentMod, {M, Backend, DocOpts}, SubMods)};
                              #{} ->
                                  warn("module ~s is not properly documented", [M]),
                                  Acc
                          catch _:undef ->
                                  case erlang:function_exported(
                                         M, mod_options, 1) of
                                           true ->
                                          warn("module ~s is not documented", [M]);
                                      false ->
                                          ok
                                  end,
                                  Acc
                          end;
                      false ->
                          Acc
                  end
          end, {[], dict:new()}, ejabberd_config:beams(all)),
    Doc = lists:flatmap(
            fun(M) ->
                    try M:doc()
                    catch _:undef -> []
                    end
            end, ejabberd_config:callback_modules(all)),
    Version = binary_to_list(ejabberd_config:version()),
    Options =
        ["TOP LEVEL OPTIONS",
         "-----------------",
         "This section describes top level options of ejabberd " ++ Version ++ ".",
         "The options that changed in this version are marked with 🟤.",
         io_lib:nl()] ++
        lists:flatmap(
          fun(Opt) ->
                  opt_to_man(Lang, Opt, 1)
          end, lists:keysort(1, Doc)),
    ModDoc1 = lists:map(
                fun({M, Descr, DocOpts, Ex}) ->
                        case dict:find(M, SubModDoc) of
                            {ok, Backends} ->
                                {M, Descr, DocOpts, Backends, Ex};
                            error ->
                                {M, Descr, DocOpts, [], Ex}
                        end
                end, ModDoc),
    ModOptions =
        [io_lib:nl(),
         "MODULES",
         "-------",
         "[[modules]]",
         "This section describes modules options of ejabberd " ++ Version ++ ".",
         "The modules that changed in this version are marked with 🟤.",
         io_lib:nl()] ++
        lists:flatmap(
          fun({M, Descr, DocOpts, Backends, Example}) ->
                  ModName = atom_to_list(M),
                  VersionMark = get_version_mark(Example),
                  [io_lib:nl(),
                   lists:flatten([ModName, VersionMark]),
                   lists:duplicate(length(atom_to_list(M)), $~),
                   "[[" ++ ModName ++ "]]",
                   io_lib:nl()] ++
                      format_versions(Lang, Example) ++ [io_lib:nl()] ++
                      tr_multi(Lang, Descr) ++ [io_lib:nl()] ++
                      opts_to_man(Lang, [{M, '', DocOpts}|Backends]) ++
                      format_example(0, Lang, Example)
          end, lists:keysort(1, ModDoc1)),
    ListenOptions =
        [io_lib:nl(),
         "LISTENERS",
         "-------",
         "[[listeners]]",
         "This section describes listeners options of ejabberd " ++ Version ++ ".",
         io_lib:nl(),
         "TODO"],
    AsciiData =
         [[unicode:characters_to_binary(Line), io_lib:nl()]
          || Line <- man_header(Lang) ++ Options ++ [io_lib:nl()]
                 ++ ModOptions ++ ListenOptions ++ man_footer(Lang)],
    warn_undocumented_modules(ModDoc1),
    warn_undocumented_options(Doc),
    write_man(AsciiData).

%%%===================================================================
%%% Internal functions
%%%===================================================================
opts_to_man(Lang, [{_, _, []}]) ->
    Text = tr(Lang, ?T("The module has no options.")),
    [Text, io_lib:nl()];
opts_to_man(Lang, Backends) ->
    lists:flatmap(
      fun({_, Backend, DocOpts}) when DocOpts /= [] ->
              Text = if Backend == '' ->
                             tr(Lang, ?T("Available options"));
                        true ->
                             lists:flatten(
                               io_lib:format(
                                 tr(Lang, ?T("Available options for '~s' backend")),
                                 [Backend]))
                     end,
              [Text ++ ":", lists:duplicate(length(Text)+1, $^)|
               lists:flatmap(
                 fun(Opt) -> opt_to_man(Lang, Opt, 1) end,
                 lists:keysort(1, DocOpts))] ++ [io_lib:nl()];
         (_) ->
              []
      end, Backends).

opt_to_man(Lang, {Option, Options}, Level) ->
    [format_option(Lang, Option, Options)|format_versions(Lang, Options)++format_desc(Lang, Options)] ++
        format_example(Level, Lang, Options);
opt_to_man(Lang, {Option, Options, Children}, Level) ->
    [format_option(Lang, Option, Options)|format_desc(Lang, Options)] ++
        lists:append(
          [[H ++ ":"|T]
           || [H|T] <- lists:map(
                         fun(Opt) -> opt_to_man(Lang, Opt, Level+1) end,
                         lists:keysort(1, Children))]) ++
        [io_lib:nl()|format_example(Level, Lang, Options)].

get_version_mark(#{note := Note}) ->
    [XX, YY | _] = string:tokens(binary_to_list(ejabberd_option:version()), "."),
    XXYY = string:join([XX, YY], "."),
    case string:find(Note, XXYY) of
        nomatch -> "";
        _ -> " 🟤"
    end;
get_version_mark(_) ->
    "".

format_option(Lang, Option, #{value := Val} = Options) ->
    VersionMark = get_version_mark(Options),
    "*" ++ atom_to_list(Option) ++ VersionMark ++ "*: 'pass:[" ++
        tr(Lang, Val) ++ "]'::";
format_option(_Lang, Option, #{}) ->
    "*" ++ atom_to_list(Option) ++ "*::".

format_versions(_Lang, #{note := Note}) when Note /= [] ->
    ["_Note_ about this option: " ++ Note ++ ". "];
format_versions(_, _) ->
    [].

format_desc(Lang, #{desc := Desc}) ->
    tr_multi(Lang, Desc).

format_example(Level, Lang, #{example := [_|_] = Example}) ->
    case lists:all(fun is_list/1, Example) of
        true ->
            if Level == 0 ->
                    ["*Example*:",
                     "^^^^^^^^^^"];
               true ->
                    ["+", "*Example*:", "+"]
            end ++ format_yaml(Example);
        false when Level == 0 ->
            ["Examples:",
             "^^^^^^^^^"] ++
                lists:flatmap(
                    fun({Text, Lines}) ->
                            [tr(Lang, Text)] ++ format_yaml(Lines)
                    end, Example);
        false ->
            lists:flatmap(
              fun(Block) ->
                      ["+", "*Examples*:", "+"|Block]
              end,
              lists:map(
                fun({Text, Lines}) ->
                        [tr(Lang, Text), "+"] ++ format_yaml(Lines)
                end, Example))
    end;
format_example(_, _, _) ->
    [].

format_yaml(Lines) ->
    ["==========================",
     "[source,yaml]",
     "----"|Lines] ++
        ["----",
         "=========================="].

man_header(Lang) ->
    ["ejabberd.yml(5)",
     "===============",
     ":doctype: manpage",
     ":version: " ++ binary_to_list(ejabberd_config:version()),
     io_lib:nl(),
     "NAME",
     "----",
     "ejabberd.yml - " ++ tr(Lang, ?T("main configuration file for ejabberd.")),
     io_lib:nl(),
     "SYNOPSIS",
     "--------",
     "ejabberd.yml",
     io_lib:nl(),
     "DESCRIPTION",
     "-----------",
     tr(Lang, ?T("The configuration file is written in "
                 "https://en.wikipedia.org/wiki/YAML[YAML] language.")),
     io_lib:nl(),
     tr(Lang, ?T("WARNING: YAML is indentation sensitive, so make sure you respect "
                 "indentation, or otherwise you will get pretty cryptic "
                 "configuration errors.")),
     io_lib:nl(),
     tr(Lang, ?T("Logically, configuration options are split into 3 main categories: "
                 "'Modules', 'Listeners' and everything else called 'Top Level' options. "
                 "Thus this document is split into 3 main chapters describing each "
                 "category separately. So, the contents of ejabberd.yml will typically "
                 "look like this:")),
     io_lib:nl(),
     "==========================",
     "[source,yaml]",
     "----",
     "hosts:",
     "  - example.com",
     "  - domain.tld",
     "loglevel: info",
     "...",
     "listen:",
     "  -",
     "    port: 5222",
     "    module: ejabberd_c2s",
     "  ...",
     "modules:",
     "  mod_roster: {}",
     "  ...",
     "----",
     "==========================",
     io_lib:nl(),
     tr(Lang, ?T("Any configuration error (such as syntax error, unknown option "
                 "or invalid option value) is fatal in the sense that ejabberd will "
                 "refuse to load the whole configuration file and will not start or will "
                 "abort configuration reload.")),
     io_lib:nl(),
     tr(Lang, ?T("All options can be changed in runtime by running 'ejabberdctl "
                 "reload-config' command. Configuration reload is atomic: either all options "
                 "are accepted and applied simultaneously or the new configuration is "
                 "refused without any impact on currently running configuration.")),
     io_lib:nl(),
     tr(Lang, ?T("Some options can be specified for particular virtual host(s) only "
                 "using 'host_config' or 'append_host_config' options. Such options "
                 "are called 'local'. Examples are 'modules', 'auth_method' and 'default_db'. "
                 "The options that cannot be defined per virtual host are called 'global'. "
                 "Examples are 'loglevel', 'certfiles' and 'listen'. It is a configuration "
                 "mistake to put 'global' options under 'host_config' or 'append_host_config' "
                 "section - ejabberd will refuse to load such configuration.")),
     io_lib:nl(),
     str:format(
       tr(Lang, ?T("It is not recommended to write ejabberd.yml from scratch. Instead it is "
                   "better to start from \"default\" configuration file available at ~s. "
                   "Once you get ejabberd running you can start changing configuration "
                   "options to meet your requirements.")),
       [default_config_url()]),
     io_lib:nl(),
     str:format(
       tr(Lang, ?T("Note that this document is intended to provide comprehensive description of "
                   "all configuration options that can be consulted to understand the meaning "
                   "of a particular option, its format and possible values. It will be quite "
                   "hard to understand how to configure ejabberd by reading this document only "
                   "- for this purpose the reader is recommended to read online Configuration "
                   "Guide available at ~s.")),
       [configuration_guide_url()]),
     io_lib:nl()].

man_footer(Lang) ->
    {Year, _, _} = date(),
    [io_lib:nl(),
     "AUTHOR",
     "------",
     "https://www.process-one.net[ProcessOne].",
     io_lib:nl(),
     "VERSION",
     "-------",
     str:format(
       tr(Lang, ?T("This document describes the configuration file of ejabberd ~ts. "
                   "Configuration options of other ejabberd versions "
                   "may differ significantly.")),
       [ejabberd_config:version()]),
     io_lib:nl(),
     "REPORTING BUGS",
     "--------------",
     tr(Lang, ?T("Report bugs to <https://github.com/processone/ejabberd/issues>")),
     io_lib:nl(),
     "SEE ALSO",
     "---------",
     tr(Lang, ?T("Default configuration file")) ++ ": " ++ default_config_url(),
     io_lib:nl(),
     tr(Lang, ?T("Main site")) ++ ": <https://ejabberd.im>",
     io_lib:nl(),
     tr(Lang, ?T("Documentation")) ++ ": <https://docs.ejabberd.im>",
     io_lib:nl(),
     tr(Lang, ?T("Configuration Guide")) ++ ": " ++ configuration_guide_url(),
     io_lib:nl(),
     tr(Lang, ?T("Source code")) ++ ": <https://github.com/processone/ejabberd>",
     io_lib:nl(),
     "COPYING",
     "-------",
     "Copyright (c) 2002-" ++ integer_to_list(Year) ++
         " https://www.process-one.net[ProcessOne]."].

tr(Lang, {Format, Args}) ->
    unicode:characters_to_list(
      str:format(
        translate:translate(Lang, iolist_to_binary(Format)),
        Args));
tr(Lang, Txt) ->
    unicode:characters_to_list(translate:translate(Lang, iolist_to_binary(Txt))).

tr_multi(Lang, Txt) when is_binary(Txt) ->
    tr_multi(Lang, [Txt]);
tr_multi(Lang, {Format, Args}) ->
    tr_multi(Lang, [{Format, Args}]);
tr_multi(Lang, Lines) when is_list(Lines) ->
    [tr(Lang, Txt) || Txt <- Lines].

write_man(AsciiData) ->
    case file:get_cwd() of
        {ok, Cwd} ->
            AsciiDocFile = filename:join(Cwd, "ejabberd.yml.5.txt"),
            ManPage = filename:join(Cwd, "ejabberd.yml.5"),
            case file:write_file(AsciiDocFile, AsciiData) of
                ok ->
                    Ret = run_a2x(Cwd, AsciiDocFile),
                    %%file:delete(AsciiDocFile),
                    case Ret of
                        ok ->
                            {ok, lists:flatten(
                                   io_lib:format(
                                     "The manpage saved as ~ts", [ManPage]))};
                        {error, Error} ->
                            {error, lists:flatten(
                                      io_lib:format(
                                        "Failed to generate manpage: ~ts", [Error]))}
                    end;
                {error, Reason} ->
                    {error, lists:flatten(
                              io_lib:format(
                                "Failed to write to ~ts: ~s",
                                [AsciiDocFile, file:format_error(Reason)]))}
            end;
        {error, Reason} ->
            {error, lists:flatten(
                      io_lib:format("Failed to get current directory: ~s",
                                    [file:format_error(Reason)]))}
    end.

have_a2x() ->
    case os:find_executable("a2x") of
        false -> false;
        Path -> {true, Path}
    end.

run_a2x(Cwd, AsciiDocFile) ->
    case have_a2x() of
        false ->
            {error, "a2x was not found: do you have 'asciidoc' installed?"};
        {true, Path} ->
            Cmd = lists:flatten(
                    io_lib:format("~ts -f manpage ~ts -D ~ts",
                                  [Path, AsciiDocFile, Cwd])),
            case os:cmd(Cmd) of
                "" -> ok;
                Ret -> {error, Ret}
            end
    end.

warn_undocumented_modules(Docs) ->
    lists:foreach(
      fun({M, _, DocOpts, Backends, _}) ->
              warn_undocumented_module(M, DocOpts),
              lists:foreach(
                fun({SubM, _, SubOpts}) ->
                        warn_undocumented_module(SubM, SubOpts)
                end, Backends)
      end, Docs).

warn_undocumented_module(M, DocOpts) ->
    try M:mod_options(ejabberd_config:get_myname()) of
        Defaults ->
            lists:foreach(
              fun(OptDefault) ->
                      Opt = case OptDefault of
                                O when is_atom(O) -> O;
                                {O, _} -> O
                            end,
                      case lists:keymember(Opt, 1, DocOpts) of
                          false ->
                              warn("~s: option ~s is not documented",
                                   [M, Opt]);
                          true ->
                              ok
                      end
              end, Defaults)
    catch _:undef ->
            ok
    end.

warn_undocumented_options(Docs) ->
    Opts = lists:flatmap(
             fun(M) ->
                     try M:options() of
                         Defaults ->
                             lists:map(
                               fun({O, _}) -> O;
                                  (O) when is_atom(O) -> O
                               end, Defaults)
                     catch _:undef ->
                             []
                     end
             end, ejabberd_config:callback_modules(all)),
    lists:foreach(
      fun(Opt) ->
              case lists:keymember(Opt, 1, Docs) of
                  false ->
                      warn("option ~s is not documented", [Opt]);
                  true ->
                      ok
              end
      end, Opts).

warn(Format, Args) ->
    io:format(standard_error, "Warning: " ++ Format ++ "~n", Args).

strip_backend_suffix(M) ->
    [H|T] = lists:reverse(string:tokens(atom_to_list(M), "_")),
    {list_to_atom(string:join(lists:reverse(T), "_")), list_to_atom(H)}.

default_config_url() ->
    "<https://github.com/processone/ejabberd/blob/" ++
        binary_to_list(binary:part(ejabberd_config:version(), {0,5})) ++
        "/ejabberd.yml.example>".

configuration_guide_url() ->
    "<https://docs.ejabberd.im/admin/configuration>".
