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

-module(pg_codec_timestamp).

-export([encode/4, decode/4]).

-define(REF_MICROSECONDS, 63_113_904_000_000_000). % 2000-01-01T00:00:00.0

-spec encode(pg:timestamp(), pg_types:type(), pg_types:type_set(), []) ->
        iodata().
encode(negative_infinity, _, _, []) ->
  <<-9223372036854775808:64/signed-integer>>;
encode(positive_infinity, _, _, []) ->
  <<9223372036854775807:64/signed-integer>>;
encode({Date, {Hours, Minutes, Seconds, Microseconds}}, _, _, []) ->
  DT = {Date, {Hours, Minutes, Seconds}},
  TotalSeconds = calendar:datetime_to_gregorian_seconds(DT),
  TotalMicroseconds = TotalSeconds*1_000_000 + Microseconds - ?REF_MICROSECONDS,
  <<TotalMicroseconds:64/signed-integer>>.

-spec decode(binary(), pg_types:type(), pg_types:type_set(), []) ->
        pg:timestamp().
decode(<<-9223372036854775808:64/signed-integer>>, _, _, []) ->
  negative_infinity;
decode(<<9223372036854775807:64/signed-integer>>, _, _, []) ->
  positive_infinity;
decode(<<MicrosecondOffset:64/signed-integer>>, _, _, []) ->
  TotalMicroseconds = MicrosecondOffset + ?REF_MICROSECONDS,
  if
    TotalMicroseconds >= 0 ->
      TotalSeconds = TotalMicroseconds div 1_000_000,
      Microseconds = TotalMicroseconds rem 1_000_000,
      {Date, {Hours, Minutes, Seconds}} =
        calendar:gregorian_seconds_to_datetime(TotalSeconds),
      {Date, {Hours, Minutes, Seconds, Microseconds}};
    true ->
      error({unsupported_bc_timestamp, MicrosecondOffset})
  end;
decode(Data, _, _, [_]) ->
  error({invalid_data, Data}).
