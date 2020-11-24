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

-module(pgc_codec_box).

-behaviour(pgc_codec).

-export([encode/4, decode/4]).

-spec encode(pgc:box(), pgc_types:type(), pgc_types:type_set(), []) -> iodata().
encode({UpperRight, LowerLeft}, _, Types, []) ->
  {UpperRightData, _} = pgc_types:encode_value({point, UpperRight}, Types),
  {LowerLeftData, _} = pgc_types:encode_value({point, LowerLeft}, Types),
  [UpperRightData, LowerLeftData].

-spec decode(binary(), pgc_types:type(), pgc_types:type_set(), []) -> pgc:box().
decode(<<UpperRightData:16/binary, LowerLeftData:16/binary>>, _, Types, []) ->
  {pgc_types:decode_value(UpperRightData, point, Types),
   pgc_types:decode_value(LowerLeftData, point, Types)};
decode(Data, _, _, []) ->
  error({invalid_data, Data}).
