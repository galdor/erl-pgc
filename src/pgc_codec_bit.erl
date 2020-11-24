%% Copyright (c) 2020 Nicolas Martyanoff <khaelin@gmail.com>.
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH
%% REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY
%% AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT,
%% INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM
%% LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR
%% OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
%% PERFORMANCE OF THIS SOFTWARE.

-module(pgc_codec_bit).

-behaviour(pgc_codec).

-export([encode/4, decode/4]).

-spec encode(bitstring(), pgc_types:type(), pgc_types:type_set(), []) -> iodata().
encode(Bin, _, _, []) ->
  NbBits = bit_size(Bin),
  NbBytes = (NbBits + 7) div 8,
  NbPaddingBits = NbBytes * 8 - NbBits,
  <<NbBits:32, Bin:NbBits/binary-unit:1, 0:NbPaddingBits>>.

-spec decode(binary(), pgc_types:type(), pgc_types:type_set(), []) -> bitstring().
decode(<<NbBits:32, Data/binary>>, _, _, []) ->
  NbBytes = (NbBits + 7) div 8,
  byte_size(Data) == NbBytes orelse error({invalid_data, Data}),
  <<Bits:NbBits/binary-unit:1, _/binary-unit:1>> = Data,
  Bits;
decode(Data, _, _, []) ->
  error({invalid_data, Data}).
