dnl erlang-extra.m4

AC_DEFUN([ERLANG_SUBST_LIB_VER],
[AC_ERLANG_CHECK_LIB([$1])
ERLANG_LIB_VER_SUBST="$ERLANG_LIB_VER_SUBST -e 's,[@]ERLANG_LIB_VER_$1[@],\$(ERLANG_LIB_VER_$1),g'"
AC_SUBST([ERLANG_LIB_VER_SUBST])
]) # ERLANG_SUBST_LIB_VER

AC_DEFUN([ERLANG_VERSION_CHECK],
[		AC_MSG_CHECKING([Erlang/OTP version])
		cat > conftest.erl <<EOF
-module(conftest).
-export([[start/0]]).

start() ->
    ERTS = erlang:system_info(version),
    RequiredMin = "$1",
    RequiredMax = "$2",
    Status =
        case {string:tokens(RequiredMin, " "),
              string:tokens(RequiredMax, " ")} of
	    {[[MinStr | _]], [[MaxStr | _]]} ->
                case check(ERTS, {MinStr, MaxStr}) of
                    less ->
                         list_to_binary([[ERTS, " found, ", RequiredMin, " required"]]);
                    greater ->
                         list_to_binary([[ERTS, " found, ", RequiredMax, " or earlier required"]]);
                    ok ->
	                 <<"ok">>
                end;
	    _ ->
	        list_to_binary([[ERTS, " found, ", RequiredMin, " required"]])
	end,
    file:write_file("conftest.out", Status),
    halt().

check(CurStr, {MinStr, MaxStr}) ->
    Cur = parse(CurStr),
    Min = parse(MinStr),
    Max = parse(MaxStr),
    case {less_or_equal(Min, Cur), less_or_equal(Cur, Max)} of
        {false, true} -> less;
        {true, true} -> ok;
        {true, false} -> greater
    end.

parse(Version) ->
    lists:map(fun(A) -> {Int,[[]]} = string:to_integer(A), Int end,
              string:tokens(Version, ".")).

less_or_equal([[]], [[]]) ->
    true;
less_or_equal([[]], _Any) ->
    true;
less_or_equal(_Any, [[]]) ->
    false;
less_or_equal([[Left| Rl]], [[Right| Rr]]) ->
    case {Left < Right, Left == Right} of
        {true, _}  ->
            true;
        {false, false} ->
            false;
        {false, true} ->
            less_or_equal(Rl, Rr)
    end.

EOF

	$ERLC conftest.erl || AC_MSG_ERROR(["Could not compile Erlang/OTP version check program using '$ERLC'"])

	if ! $ERL -s conftest -noshell -o ! -f conftest.out ; then
	   AC_MSG_ERROR(["Could not run Erlang/OTP version check program using '$ERL'"])
	fi

	if test "x`cat conftest.out`" != "xok"; then
	   AC_MSG_RESULT([failed])
	   X="`cat conftest.out`"
	   if test "[$3]" == "warn"; then
	      AC_MSG_WARN([$X])
	   else
	      AC_MSG_FAILURE([$X])
	   fi
	else
	   AC_MSG_RESULT([ok])
	fi
]) dnl ERLANG_VERSION_CHECK

AC_DEFUN([ERLANG_DEPRECATED_TYPES_CHECK],
[		AC_MSG_CHECKING([whether Erlang is using deprecated types])
		cat > conftest.erl <<EOF
-module(conftest).

-record(state, {host = dict:new() :: dict:dict()}).
EOF

	if $ERLC conftest.erl > /dev/null 2>&1; then
	   AC_MSG_RESULT([no])
	   AC_SUBST(erlang_deprecated_types, false)
	else
	   AC_MSG_RESULT([yes])
	   AC_SUBST(erlang_deprecated_types, true)
	fi
])
