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

-module(pg_codec_inet).

-behaviour(pg_codec).

-export([encode/4, decode/4]).

-spec encode(pg:inet_address(), pg_types:type(), pg_types:type_set(), []) ->
        iodata().
encode({{A, B, C, D}, NetMask}, {TypeName, _, _}, _, []) ->
  <<2:8, NetMask:8, (cidr_flag(TypeName)):8, 4:8, A:8, B:8, C:8, D:8>>;
encode({{A, B, C, D, E, F, G, H}, NetMask}, {TypeName, _, _}, _, []) ->
  <<3:8, NetMask:8, (cidr_flag(TypeName)):8, 16:8,
    A:16, B:16, C:16, D:16, E:16, F:16, G:16, H:16>>.

-spec decode(binary(), pg_types:type(), pg_types:type_set(), []) ->
        pg:inet_address().
decode(<<2:8, NetMask:8, _:8, 4:8, A:8, B:8, C:8, D:8>>,
       _, _, []) ->
  {{A, B, C, D}, NetMask};
decode(<<3:8, NetMask:8, _:8, 16:8,
         A:16, B:16, C:16, D:16, E:16, F:16, G:16, H:16>>,
       _, _, []) ->
  {{A, B, C, D, E, F, G, H}, NetMask};
decode(Data, _, _, [_]) ->
  error({invalid_data, Data}).

-spec cidr_flag(inet | cidr) -> 0..1.
cidr_flag(inet) -> 0;
cidr_flag(cidr) -> 1.
