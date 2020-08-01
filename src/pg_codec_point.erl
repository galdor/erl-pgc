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

-module(pg_codec_point).

-export([encode/4, decode/4]).

-spec encode(pg:point(), pg_types:type(), pg_types:type_db(), []) -> iodata().
encode({X, Y}, _, TypeDb, []) ->
  {XData, _} = pg_types:encode_value({float8, X}, TypeDb),
  {YData, _} = pg_types:encode_value({float8, Y}, TypeDb),
  [XData, YData].

-spec decode(binary(), pg_types:type(), pg_types:type_db(), []) -> pg:point().
decode(<<XData:8/binary, YData:8/binary>>, _, TypeDb, []) ->
  {pg_types:decode_value(XData, float8, TypeDb),
   pg_types:decode_value(YData, float8, TypeDb)};
decode(Data, _, _, []) ->
  error({invalid_data, Data}).
