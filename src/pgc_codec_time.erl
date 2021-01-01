%% Copyright (c) 2020-2021 Nicolas Martyanoff <khaelin@gmail.com>.
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

-module(pgc_codec_time).

-behaviour(pgc_codec).

-export([encode/4, decode/4]).

-spec encode(pgc:time(), pgc_types:type(), pgc_types:type_set(), []) -> iodata().
encode({Hours, Minutes, Seconds, Microseconds}, _, _, []) ->
  TotalSeconds = Hours*3600 + Minutes*60 + Seconds,
  TotalMicroseconds = TotalSeconds * 1_000_000 + Microseconds,
  <<TotalMicroseconds:64/signed-integer>>.

-spec decode(binary(), pgc_types:type(), pgc_types:type_set(), []) -> pgc:time().
decode(<<TotalMicroseconds:64/signed-integer>>, _, _, []) when
    TotalMicroseconds =< 86_400_000_000 ->
  US0 = TotalMicroseconds,
  Hours = US0 div 3600_000_000,
  US1 = US0 - Hours * 3600_000_000,
  Minutes = US1 div 60_000_000,
  US2 = US1 - Minutes * 60_000_000,
  Seconds = US2 div 1_000_000,
  Microseconds = US2 rem 1_000_000,
  {Hours, Minutes, Seconds, Microseconds};
decode(Data, _, _, [_]) ->
  throw({error, {invalid_data, Data}}).
