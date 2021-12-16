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

-module(pgc_utils).

-export([timestamp/1, timestamp_to_erlang_datetime/1,
         time/1, time_to_erlang_time/1,
         quote_identifier/1]).

-spec timestamp(calendar:datetime()) -> pgc:timestamp().
timestamp({Date, Time}) ->
  {Date, time(Time)}.

-spec timestamp_to_erlang_datetime(pgc:timestamp()) -> calendar:datetime().
timestamp_to_erlang_datetime({Date, Time}) ->
  {Date, time_to_erlang_time(Time)}.

-spec time(calendar:time()) -> pgc:time().
time({H, M, S}) ->
  {H, M, S, 0}.

-spec time_to_erlang_time(pgc:time()) -> calendar:time().
time_to_erlang_time({H, M, S, _}) ->
  {H, M, S}.

-spec quote_identifier(unicode:chardata()) -> binary().
quote_identifier(Id) when is_binary(Id) ->
  case re:run(Id, "^[a-zA-Z_][a-zA-Z0-9_$]*$") of
    {match, _} ->
      Id;
    nomatch ->
      quote_identifier(Id, <<>>)
  end;
quote_identifier(Id) ->
  quote_identifier(unicode:characters_to_binary(Id)).

-spec quote_identifier(binary(), binary()) -> binary().
quote_identifier(<<>>, Acc) ->
  <<$", Acc/binary, $">>;
quote_identifier(<<$", Rest/binary>>, Acc) ->
  quote_identifier(Rest, <<Acc/binary, $", $">>);
quote_identifier(<<C, Rest/binary>>, Acc) ->
  quote_identifier(Rest, <<Acc/binary, C>>).
