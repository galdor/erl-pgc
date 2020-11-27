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

-module(pgc_codec_date).

-behaviour(pgc_codec).

-export([encode/4, decode/4]).

-define(REF_DAYS, 730_485). % 2000-01-01

-spec encode(pgc:date(), pgc_types:type(), pgc_types:type_set(), []) -> iodata().
encode(negative_infinity, _, _, []) ->
  <<-2147483648:32/signed-integer>>;
encode(positive_infinity, _, _, []) ->
  <<2147483647:32/signed-integer>>;
encode(Date, _, _, []) ->
  Days = calendar:date_to_gregorian_days(Date) - ?REF_DAYS,
  <<Days:32/signed-integer>>.

-spec decode(binary(), pgc_types:type(), pgc_types:type_set(), []) -> pgc:date().
decode(<<-2147483648:32/signed-integer>>, _, _, []) ->
  negative_infinity;
decode(<<2147483647:32/signed-integer>>, _, _, []) ->
  positive_infinity;
decode(<<DayOffset:32/signed-integer>>, _, _, []) ->
  Days = DayOffset + ?REF_DAYS,
  if
    Days >= 0 ->
      calendar:gregorian_days_to_date(Days);
    true ->
      throw({error, {unsupported_bc_date, DayOffset}})
  end;
decode(Data, _, _, [_]) ->
  throw({error, {invalid_data, Data}}).
