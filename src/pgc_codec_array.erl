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

-module(pgc_codec_array).

-behaviour(pgc_codec).

-export([encode/4, decode/4]).
-export([array_dimension_lengths/1, array_contains_null/1]).

-spec encode(list(), pgc_types:type(), pgc_types:type_set(), list()) -> iodata().
encode(_, _, _, [ValueTypeName = {array, _}]) ->
  error({invalid_value_type, ValueTypeName});
encode(Array, _, Types, [ValueTypeName]) ->
  Lengths = array_dimension_lengths(Array),
  NbDimensions = length(Lengths),
  Flags = case array_contains_null(Array) of
            true -> 1;
            false -> 0
          end,
  {_, ValueTypeOid, _} = pgc_types:find_type_by_name(ValueTypeName, Types),
  Encode = fun (V) ->
               {Data, _} = pgc_types:encode_value({ValueTypeName, V}, Types),
               Data
           end,
  Header = <<NbDimensions:32, Flags:32, ValueTypeOid:32>>,
  DimensionData = [<<Length:32, 1:32>> || Length <- Lengths],
  ContentData = encode_array_content(Array, Encode),
  [Header, DimensionData, ContentData].

-spec decode(binary(), pgc_types:type(), pgc_types:type_set(), list()) -> list().
decode(<<NbDimensions:32, _Flags:32, _ValueTypeOid:32, Data/binary>>,
       _, Types, [ValueTypeName]) ->
  DimensionDataSize = NbDimensions*8,
  {DimensionData, ContentData} = split_binary(Data, DimensionDataSize),
  Lengths = [Length || <<Length:32, _:32>> <= DimensionData],
  Decode = fun (Bin) -> pgc_types:decode_value(Bin, ValueTypeName, Types) end,
  {Content, _} = decode_dimension(ContentData, Lengths, Decode),
  Content;
decode(Data, _, _, [_]) ->
  error({invalid_data, Data}).

-spec array_contains_null(list()) -> boolean().
array_contains_null([]) ->
  false;
array_contains_null([null | _]) ->
  true;
array_contains_null([Head | Tail]) when is_list(Head) ->
  array_contains_null(Head) orelse array_contains_null(Tail);
array_contains_null([_ | Tail]) ->
  array_contains_null(Tail).

-spec array_dimension_lengths(list()) -> [integer()].
array_dimension_lengths(Array) ->
  array_dimension_lengths(Array, []).

-spec array_dimension_lengths(list(), Acc :: [integer()]) -> [integer()].
array_dimension_lengths([], _Acc) ->
  [];
array_dimension_lengths(Array = [Head | _], Acc) when is_list(Head) ->
  array_dimension_lengths(Head, [length(Array) | Acc]);
array_dimension_lengths(Array, Acc) ->
  lists:reverse([length(Array) | Acc]).

-spec encode_array_content(list(), Encode :: fun((term()) -> iolist())) ->
        iodata().
encode_array_content([], _) ->
  [];
encode_array_content(Array = [Head | _], Encode) when is_list(Head) ->
  EncodeSubArray = fun (SubArray) ->
                       encode_array_content(SubArray, Encode)
                   end,
  lists:map(EncodeSubArray, Array);
encode_array_content(Array, Encode) ->
  EncodeValue = fun (null) ->
                    <<-1:32/signed-integer>>;
                    (Value) ->
                    ValueData = Encode(Value),
                    ValueDataSize = iolist_size(ValueData),
                    [<<ValueDataSize:32>>, ValueData]
                end,
  lists:map(EncodeValue, Array).

-spec decode_dimension(Data :: binary(), Length :: [integer()],
                       Decode :: fun((binary()) -> term())) ->
        {list(), Rest :: binary()}.
decode_dimension(Data, [], _) ->
  {[], Data};
decode_dimension(Data, [Length], Decode) ->
  decode_elements(Data, Length, Decode, []);
decode_dimension(Data, [Length | NextLengths], Decode) ->
  F = fun(_N, Data2) -> decode_dimension(Data2, NextLengths, Decode) end,
  lists:mapfoldl(F, Data, lists:seq(1, Length)).

-spec decode_elements(Data :: binary(), N :: non_neg_integer(),
                      Decode :: fun((binary()) -> term()), Acc :: list()) ->
        {list(), Rest :: binary()}.
decode_elements(Data, 0, _Decode, Acc) ->
  {lists:reverse(Acc), Data};
decode_elements(<<-1:32/signed-integer, Rest/binary>>, N, Decode, Acc) ->
  decode_elements(Rest, N-1, Decode, [null | Acc]);
decode_elements(<<Len:32, Data:Len/binary, Rest/binary>>, N, Decode, Acc) ->
  decode_elements(Rest, N-1, Decode, [Decode(Data) | Acc]).
