%%%----------------------------------------------------------------------
%%% File    : mod_private.erl
%%% Author  : Alexey Shchepin <alexey@sevcom.net>
%%% Purpose : 
%%% Created : 16 Jan 2003 by Alexey Shchepin <alexey@sevcom.net>
%%% Id      : $Id$
%%%----------------------------------------------------------------------

-module(mod_private).
-author('alexey@sevcom.net').
-vsn('$Revision$ ').

-behaviour(gen_mod).

-export([start/1,
	 stop/0,
	 process_local_iq/3,
	 remove_user/1]).

-include("ejabberd.hrl").
-include("jlib.hrl").

-record(private_storage, {userns, xml}).

start(Opts) ->
    IQDisc = gen_mod:get_opt(iqdisc, Opts, one_queue),
    mnesia:create_table(private_storage,
			[{disc_only_copies, [node()]},
			 {attributes, record_info(fields, private_storage)}]),
    gen_iq_handler:add_iq_handler(ejabberd_sm, ?NS_PRIVATE,
				  ?MODULE, process_local_iq, IQDisc).

stop() ->
    gen_iq_handler:remove_iq_handler(ejabberd_local, ?NS_PRIVATE).


process_local_iq(From, _To, #iq{type = Type, sub_el = SubEl} = IQ) ->
    #jid{luser = LUser, lserver = LServer} = From,
    case ?MYNAME of
	LServer ->
	    {xmlelement, Name, Attrs, Els} = SubEl,
	    case Type of
		set ->
		    F = fun() ->
				lists:foreach(
				  fun(El) ->
					  set_data(LUser, El)
				  end, Els)
			end,
		    mnesia:transaction(F),
		    IQ#iq{type = result,
			  sub_el = [{xmlelement, Name, Attrs, []}]};
		get ->
		    case catch get_data(LUser, Els) of
			{'EXIT', _Reason} ->
			    IQ#iq{type = error,
				  sub_el = [SubEl,
					    ?ERR_INTERNAL_SERVER_ERROR]};
			Res ->
			    IQ#iq{type = result,
				  sub_el = [{xmlelement, Name, Attrs, Res}]}
		    end
	    end;
	_ ->
	    IQ#iq{type = error, sub_el = [SubEl, ?ERR_NOT_ALLOWED]}
    end.

set_data(LUser, El) ->
    case El of
	{xmlelement, _Name, Attrs, _Els} ->
	    XMLNS = xml:get_attr_s("xmlns", Attrs),
	    case XMLNS of
		"" ->
		    ignore;
		_ ->
		    mnesia:write(#private_storage{userns = {LUser, XMLNS},
						  xml = El})
	    end;
	_ ->
	    ignore
    end.

get_data(LUser, Els) ->
    get_data(LUser, Els, []).

get_data(_LUser, [], Res) ->
    lists:reverse(Res);
get_data(LUser, [El | Els], Res) ->
    case El of
	{xmlelement, _Name, Attrs, _} ->
	    XMLNS = xml:get_attr_s("xmlns", Attrs),
	    case mnesia:dirty_read(private_storage, {LUser, XMLNS}) of
		[R] ->
		    get_data(LUser, Els, [R#private_storage.xml | Res]);
		[] ->
		    get_data(LUser, Els, [El | Res])
	    end;
	_ ->
	    get_data(LUser, Els, Res)
    end.


remove_user(User) ->
    LUser = jlib:nodeprep(User),
    F = fun() ->
		lists:foreach(
		  fun({U, _} = Key) ->
			  if
			      U == LUser ->
				  mnesia:delete({private_storage, Key});
			      true ->
				  ok
			  end
		  end, mnesia:all_keys(private_storage))
        end,
    mnesia:transaction(F).

