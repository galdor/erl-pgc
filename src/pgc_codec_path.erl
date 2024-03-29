%% Copyright (c) 2020-2021 Exograd SAS.
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
%% SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR
%% IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

-module(pgc_codec_path).

-behaviour(pgc_codec).

-export([encode/4, decode/4]).

-spec encode(pgc:path(), pgc_types:type(), pgc_types:type_set(), []) -> iodata().
encode({Type, Points}, _, Types, []) ->
  TypeValue = case Type of
               open -> 0;
               closed -> 1
             end,
  NbPoints = length(Points),
  PointData = lists:map(fun (Point) ->
                            {Data, _} = pgc_types:encode_value({point, Point},
                                                              Types),
                            Data
                        end, Points),
  [<<TypeValue:8, NbPoints:32>>, PointData].

-spec decode(binary(), pgc_types:type(), pgc_types:type_set(), []) -> pgc:path().
decode(<<TypeValue:8, NbPoints:32, Data/binary>>, _, Types, []) ->
  Type = case TypeValue of
           0 -> open;
           1 -> closed;
           Value -> throw({error, {invalid_path_type, Value}})
         end,
  F = fun (_, <<PointData:16/binary, Rest/binary>>) ->
          Point = pgc_types:decode_value(PointData, point, Types),
          {Point, Rest}
      end,
  {Points, _} = lists:mapfoldl(F, Data, lists:seq(1, NbPoints)),
  {Type, Points};
decode(Data, _, _, []) ->
  throw({error, {invalid_data, Data}}).
