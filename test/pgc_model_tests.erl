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

-module(pgc_model_tests).

-include_lib("eunit/include/eunit.hrl").

-spec test_model() -> pgc_model:model().
test_model() ->
  #{a => int4,
    b => #{type => text},
    c => #{type => boolean, column => hello},
    d => #{type => date, column => 'été'},
    e => #{type => timestamp, column => '"foo"'}}.

encode_entity_test_() ->
  Model = test_model(),
  Encode = fun pgc_model:encode_entity/3,
  [?_assertEqual([],
                 Encode(#{a => 1}, Model, [])),
   ?_assertEqual([{int4, 42}],
                 Encode(#{a => 42, b => <<"foo">>}, Model, [a])),
   ?_assertEqual([{int4, 42}, {text, <<"foo">>}],
                 Encode(#{a => 42, b => <<"foo">>, c => true}, Model, [a, b])),
   ?_assertEqual([{int4, 42}, {text, null}, {boolean, null}],
                 Encode(#{a => 42}, Model, [a, b, c])),
   ?_assertEqual([{date, {2020, 10, 5}},
                  {timestamp, {{2020, 10, 10}, {10, 20, 30, 0}}}],
                 Encode(#{d => {2020, 10, 5},
                          e => {{2020, 10, 10}, {10, 20, 30}}},
                        Model, [d, e]))].

decode_entity_test_() ->
  Model = test_model(),
  Decode = fun pgc_model:decode_entity/3,
  [?_assertEqual(#{},
                 Decode([], Model, [])),
   ?_assertEqual(#{a => 42, b => <<"foo">>},
                 Decode([42, <<"foo">>], Model, [a, b])),
   ?_assertEqual(#{b => <<"foo">>},
                 Decode([null, <<"foo">>, null], Model, [a, b, c])),
   ?_assertEqual(#{d => {2020, 10, 5}, e => {{2020, 10, 10}, {10, 20, 30}}},
                 Decode([{2020, 10, 5}, {{2020, 10, 10}, {10, 20, 30, 0}}],
                        Model, [d, e]))].

column_name_test_() ->
  Model = test_model(),
  ColumnName = fun (M, K) ->
                   unicode:characters_to_binary(pgc_model:column_name(M, K))
               end,
  [?_assertEqual(<<"a">>, ColumnName(Model, a)),
   ?_assertEqual(<<"b">>, ColumnName(Model, b)),
   ?_assertEqual(<<"hello">>, ColumnName(Model, c)),
   ?_assertEqual(<<"été"/utf8>>, ColumnName(Model, d)),
   ?_assertEqual(<<"\"\"\"foo\"\"\"">>, ColumnName(Model, e))].

column_name_csv_test_() ->
  Model = test_model(),
  ColumnNameCSV = fun (M, Ks) ->
                      Data = pgc_model:column_name_csv(M, Ks),
                      unicode:characters_to_binary(Data)
                  end,
  [?_assertEqual(<<"">>, ColumnNameCSV(Model, [])),
   ?_assertEqual(<<"a">>, ColumnNameCSV(Model, [a])),
   ?_assertEqual(<<"a,été,\"\"\"foo\"\"\""/utf8>>,
                 ColumnNameCSV(Model, [a, d, e]))].

column_name_tuple_test_() ->
  Model = test_model(),
  ColumnNameTuple = fun (M, Ks) ->
                        Data = pgc_model:column_name_tuple(M, Ks),
                        unicode:characters_to_binary(Data)
                    end,
  [?_assertEqual(<<"()">>, ColumnNameTuple(Model, [])),
   ?_assertEqual(<<"(a)">>, ColumnNameTuple(Model, [a])),
   ?_assertEqual(<<"(a,été,\"\"\"foo\"\"\")"/utf8>>,
                 ColumnNameTuple(Model, [a, d, e]))].
