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

-module(pg_codec_float).

-behaviour(pg_codec).

-export([encode/4, decode/4]).

-spec encode(float(), pg_types:type(), pg_types:type_set(), list()) -> iodata().
encode(positive_infinity, _, _, [8]) ->
  <<0:1, 2047:11, 0:52>>;
encode(negative_infinity, _, _, [8]) ->
  <<1:1, 2047:11, 0:52>>;
encode(nan, _, _, [8]) ->
  <<0:1, 2047:11, 2#1000000000000000000000000000000000000000000000000001:52>>;
encode(F, _, _, [8]) ->
  <<F:64/float>>;
encode(positive_infinity, _, _, [4]) ->
  <<0:1, 255:8, 0:23>>;
encode(negative_infinity, _, _, [4]) ->
  <<1:1, 255:8, 0:23>>;
encode(nan, _, _, [4]) ->
  <<0:1, 255:8, 2#10000000000000000000001:23>>;
encode(F, _, _, [4]) ->
  <<F:32/float>>.

-spec decode(binary(), pg_types:type(), pg_types:type_set(), list()) ->
        pg:float_value().
decode(<<0:1, 2047:11, 0:52>>, _, _, [8]) ->
  positive_infinity;
decode(<<1:1, 2047:11, 0:52>>, _, _, [8]) ->
  negative_infinity;
decode(<<0:1, 2047:11, _:52>>, _, _, [8]) ->
  nan;
decode(<<F:64/float>>, _, _, [8]) ->
  F;
decode(<<0:1, 255:8, 0:23>>, _, _, [4]) ->
  positive_infinity;
decode(<<1:1, 255:8, 0:23>>, _, _, [4]) ->
  negative_infinity;
decode(<<0:1, 255:8, _:23>>, _, _, [4]) ->
  nan;
decode(<<F:32/float>>, _, _, [4]) ->
  F;
decode(Data, _, _, [_]) ->
  error({invalid_data, Data}).
