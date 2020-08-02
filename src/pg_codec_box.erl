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

-module(pg_codec_box).

-behaviour(pg_codec).

-export([encode/4, decode/4]).

-spec encode(pg:box(), pg_types:type(), pg_types:type_set(), []) -> iodata().
encode({UpperRight, LowerLeft}, _, Types, []) ->
  {UpperRightData, _} = pg_types:encode_value({point, UpperRight}, Types),
  {LowerLeftData, _} = pg_types:encode_value({point, LowerLeft}, Types),
  [UpperRightData, LowerLeftData].

-spec decode(binary(), pg_types:type(), pg_types:type_set(), []) -> pg:box().
decode(<<UpperRightData:16/binary, LowerLeftData:16/binary>>, _, Types, []) ->
  {pg_types:decode_value(UpperRightData, point, Types),
   pg_types:decode_value(LowerLeftData, point, Types)};
decode(Data, _, _, []) ->
  error({invalid_data, Data}).
