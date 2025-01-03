%%
% File    : ejabberd_websocket_codec.erl
% Author  : Paweł Chmielowski <pawel@process-one.net>
% Purpose : Coder/Encoder of websocket frames
% Created : 9 sty 2023 by Paweł Chmielowski <pawel@process-one.net>
%
%
% ejabberd, Copyright (C) 2002-2025  ProcessOne
%
% This program is free software; you can redistribute it and/or
% modify it under the terms of the GNU General Public License as
% published by the Free Software Foundation; either version 2 of the
% License, or (at your option) any later version.
%
% This program is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
% General Public License for more details.
%
% You should have received a copy of the GNU General Public License along
% with this program; if not, write to the Free Software Foundation, Inc.,
% 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
%
%
-module(ejabberd_websocket_codec).
-author("pawel@process-one.net").

%% API
-export([new_server/0, new_client/0, decode/2, encode/3]).

-record(codec_state, {
    our_mask = none :: none | binary(),
    partial = none :: none | {non_neg_integer(), binary()},
    opcode = 0 :: non_neg_integer(),
    is_fin = false :: boolean(),
    mask = none :: none | binary(),
    mask_offset = 0 :: non_neg_integer(),
    required = -1 :: integer(),
    data = <<>> :: binary()
}).

-opaque codec_state() :: #codec_state{}.
-export_type([codec_state/0]).

-spec new_server() -> codec_state().
new_server() ->
    #codec_state{}.

new_client() ->
    #codec_state{our_mask = p1_rand:bytes(4)}.

-spec decode(codec_state(), binary()) -> {ok, codec_state(), [binary()]} | {error, atom(), [binary()]}.
decode(#codec_state{required = -1, data = PrevData, partial = Partial} = S, Data) ->
    Data2 = <<PrevData/binary, Data/binary>>,
    case parse_header(Data2) of
	none ->
	    {ok, S#codec_state{data = Data2}, []};
	{_, _, Opcode, _, _} when (Opcode > 2 andalso Opcode < 8) orelse (Opcode > 10) ->
	    {error, unknown_opcode, []};
	{_, 0, Opcode, _, _} when Opcode > 7 ->
	    {error, partial_control_frame, []};
	{_, _, Opcode, _, _} when Opcode > 0 andalso Opcode < 8 andalso Partial /= none ->
	    {error, partial_frame_non_finished, []};
	{Len, Final, Opcode, Mask, Payload} ->
	    decode(S#codec_state{opcode = Opcode, is_fin = Final == 1,
				 mask = Mask, mask_offset = 0,
				 required = Len, data = <<>>}, Payload)
    end;
decode(#codec_state{required = Req, data = PrevData,
		    mask = Mask, mask_offset = Offset} = S, Data)
    when byte_size(PrevData) + byte_size(Data) < Req ->
    {Unmasked, NewOffset} = apply_mask(Offset, Mask, Data, PrevData),
    {ok, S#codec_state{data = Unmasked, mask_offset = NewOffset}, []};
decode(#codec_state{required = Req, data = PrevData,
		    mask = Mask, mask_offset = Offset,
		    is_fin = IsFin, opcode = Opcode,
		    partial = Partial} = S, Data) ->
    Left = Req - byte_size(PrevData),
    <<CurrentPayload:Left/binary, NextPacketData/binary>> = Data,
    {Unmasked, _} = apply_mask(Offset, Mask, CurrentPayload, PrevData),
    {NS, Packets} =
    case {IsFin, Partial} of
	{false, none} ->
	    {S#codec_state{partial = {Opcode, Unmasked},
			   data = <<>>, required = -1}, []};
	{false, {PartOp, PartData}} ->
	    {S#codec_state{partial = {PartOp, <<PartData/binary, Unmasked/binary>>},
			   data = <<>>, required = -1}, []};
	{true, none} ->
	    {S#codec_state{data = <<>>, required = -1}, [{Opcode, Unmasked}]};
	{true, {PartOp, PartData}} ->
	    {S#codec_state{partial = none, data = <<>>, required = -1},
	     [{PartOp, <<PartData/binary, Unmasked/binary>>}]}
    end,
    case NextPacketData of
	<<>> ->
	    {ok, NS, Packets};
	_ ->
	    case decode(NS, NextPacketData) of
		{T1, T2, Packets2} ->
		    {T1, T2, Packets ++ Packets2}
	    end
    end.

-spec encode(codec_state(), non_neg_integer(), binary()) -> binary().
encode(#codec_state{our_mask = none}, Opcode, Data) ->
    case byte_size(Data) of
	S1 when S1 < 126 ->
	    <<1:1, 0:3, Opcode:4, 0:1, S1:7, Data/binary>>;
	S2 when S2 < 65536 ->
	    <<1:1, 0:3, Opcode:4, 0:1, 126:7, S2:16, Data/binary>>;
	S3 ->
	    <<1:1, 0:3, Opcode:4, 0:1, 127:7, S3:64, Data/binary>>
    end;
encode(#codec_state{our_mask = Mask}, Opcode, Data) ->
    {MaskedData, _} = apply_mask(0, Mask, Data, <<>>),
    case byte_size(Data) of
	S1 when S1 < 126 ->
	    <<1:1, 0:3, Opcode:4, 1:1, S1:7, Mask/binary, MaskedData/binary>>;
	S2 when S2 < 65536 ->
	    <<1:1, 0:3, Opcode:4, 1:1, 126:7, S2:16, Mask/binary, MaskedData/binary>>;
	S3 ->
	    <<1:1, 0:3, Opcode:4, 1:1, 127:7, S3:64, Mask/binary, MaskedData/binary>>
    end.


-spec parse_header(binary()) -> none | {integer(), integer(), integer(), none | binary(), binary()}.
parse_header(<<Final:1, _:3, Opcode:4, 0:1,
	       Len:7, Data/binary>>)
    when Len < 126 ->
    {Len, Final, Opcode, none, Data};
parse_header(<<Final:1, _:3, Opcode:4, 0:1,
	       126:7, Len:16/integer, Data/binary>>) ->
    {Len, Final, Opcode, none, Data};
parse_header(<<Final:1, _:3, Opcode:4, 0:1,
	       127:7, Len:64/integer, Data/binary>>) ->
    {Len, Final, Opcode, none, Data};
parse_header(<<Final:1, _:3, Opcode:4, 1:1,
	       Len:7, Mask:4/binary, Data/binary>>)
    when Len < 126 ->
    {Len, Final, Opcode, Mask, Data};
parse_header(<<Final:1, _:3, Opcode:4, 1:1,
	       126:7, Len:16/integer, Mask:4/binary, Data/binary>>) ->
    {Len, Final, Opcode, Mask, Data};
parse_header(<<Final:1, _:3, Opcode:4, 1:1,
	       127:7, Len:64/integer, Mask:4/binary, Data/binary>>) ->
    {Len, Final, Opcode, Mask, Data};
parse_header(_) ->
    none.

-spec apply_mask(integer(), none | binary(), binary(), binary()) -> {binary(), non_neg_integer()}.
apply_mask(_, none, Data, _) ->
    {Data, 0};
apply_mask(Offset, _, <<>>, Acc) ->
    {Acc, Offset};
apply_mask(0, <<M:32>> = Mask,
	   <<N:32, Rest/binary>>, Acc) ->
    apply_mask(0, Mask, Rest,
	       <<Acc/binary, (M bxor N):32>>);
apply_mask(0, <<M:8, _/binary>> = Mask,
	   <<N:8, Rest/binary>>, Acc) ->
    apply_mask(1, Mask, Rest,
	       <<Acc/binary, (M bxor N):8>>);
apply_mask(1, <<_:8, M:8, _/binary>> = Mask,
	   <<N:8, Rest/binary>>, Acc) ->
    apply_mask(2, Mask, Rest,
	       <<Acc/binary, (M bxor N):8>>);
apply_mask(2, <<_:16, M:8, _/binary>> = Mask,
	   <<N:8, Rest/binary>>, Acc) ->
    apply_mask(3, Mask, Rest,
	       <<Acc/binary, (M bxor N):8>>);
apply_mask(3, <<_:24, M:8>> = Mask,
	   <<N:8, Rest/binary>>, Acc) ->
    apply_mask(0, Mask, Rest,
	       <<Acc/binary, (M bxor N):8>>).
