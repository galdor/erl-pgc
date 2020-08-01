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

-module(pg_codec_line).

-export([encode/4, decode/4]).

-spec encode(pg:line(), pg_types:type(), pg_types:type_db(), []) -> iodata().
encode({A, B, C}, _, TypeDb, []) ->
  {AData, _} = pg_types:encode_value({float8, A}, TypeDb),
  {BData, _} = pg_types:encode_value({float8, B}, TypeDb),
  {CData, _} = pg_types:encode_value({float8, C}, TypeDb),
  [AData, BData, CData].

-spec decode(binary(), pg_types:type(), pg_types:type_db(), []) -> pg:line().
decode(<<AData:8/binary, BData:8/binary, CData:8/binary>>, _, TypeDb, []) ->
  {pg_types:decode_value(AData, float8, TypeDb),
   pg_types:decode_value(BData, float8, TypeDb),
   pg_types:decode_value(CData, float8, TypeDb)};
decode(Data, _, _, []) ->
  error({invalid_data, Data}).
