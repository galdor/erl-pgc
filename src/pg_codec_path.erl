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

-module(pg_codec_path).

-behaviour(pg_codec).

-export([encode/4, decode/4]).

-spec encode(pg:path(), pg_types:type(), pg_types:type_set(), []) -> iodata().
encode({Type, Points}, _, Types, []) ->
  TypeValue = case Type of
               open -> 0;
               closed -> 1
             end,
  NbPoints = length(Points),
  PointData = lists:map(fun (Point) ->
                            {Data, _} = pg_types:encode_value({point, Point},
                                                              Types),
                            Data
                        end, Points),
  [<<TypeValue:8, NbPoints:32>>, PointData].

-spec decode(binary(), pg_types:type(), pg_types:type_set(), []) -> pg:path().
decode(<<TypeValue:8, NbPoints:32, Data/binary>>, _, Types, []) ->
  Type = case TypeValue of
           0 -> open;
           1 -> closed;
           Value -> error({invalid_path_type, Value})
         end,
  F = fun (_, <<PointData:16/binary, Rest/binary>>) ->
          Point = pg_types:decode_value(PointData, point, Types),
          {Point, Rest}
      end,
  {Points, _} = lists:mapfoldl(F, Data, lists:seq(1, NbPoints)),
  {Type, Points};
decode(Data, _, _, []) ->
  error({invalid_data, Data}).
