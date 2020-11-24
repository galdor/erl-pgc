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

-module(pgc_types_tests).

-include_lib("eunit/include/eunit.hrl").

types_test_() ->
  {foreach,
   fun pgc_tests:start_client/0,
   fun pgc_tests:stop_client/1,
   [fun type_boolean/1,
    fun type_bytea/1,
    fun type_char/1,
    fun type_name/1,
    fun type_integer/1,
    fun type_oid/1,
    fun type_text/1,
    fun type_float/1,
    fun type_uuid/1,
    fun type_jsonb/1,
    fun type_bit/1,

    fun type_point/1,
    fun type_lseg/1,
    fun type_path/1,
    fun type_box/1,
    fun type_polygon/1,
    fun type_line/1,
    fun type_circle/1,

    fun type_cidr/1,
    fun type_macaddr/1,
    fun type_inet/1,

    fun type_date/1,
    fun type_time/1,
    fun type_timetz/1,
    fun type_timestamp/1,
    fun type_timestamptz/1,
    fun type_interval/1,

    fun type_array/1,
    fun type_null/1]}.

type_boolean(C) ->
  [?_assertEqual([true, false],
                 query_row(C, "SELECT true, false", [])),
   ?_assertEqual([true, false],
                 query_row(C, "SELECT $1, $2", [true, false]))].

type_bytea(C) ->
  [?_assertEqual([<<>>, <<1, 2, 3, 171>>],
                 query_row(C, "SELECT ''::bytea, '\\x010203ab'::bytea")),
   ?_assertEqual([<<>>, <<0>>, <<1, 0, 1>>],
                 query_row(C, "SELECT $1, $2, $3",
                           [{bytea, <<>>},
                            {bytea, <<0>>},
                            {bytea, <<1, 0, 1>>}]))].

type_char(C) ->
  %% As surprising as it is, the type "char" is different from the type char
  %% (which is actually char(1)). The PostgreSQL name for "char" is char, and
  %% bpchar for char (i.e. blank padded character string).
  [?_assertEqual([$a, $\0],
                 query_row(C, "SELECT 'a'::\"char\", 0::\"char\"", [])),
   ?_assertEqual([$a, $\0],
                 query_row(C, "SELECT $1, $2", [{char, $a}, {char, $\0}]))].

type_name(C) ->
  [?_assertEqual([<<>>, <<"foo">>],
                 query_row(C, "SELECT ''::name, 'foo'::name")),
   ?_assertEqual([<<>>, <<"foo">>],
                 query_row(C, "SELECT $1, $2",
                           [{name, <<>>}, {name, <<"foo">>}]))].

type_integer(C) ->
  %% Note that '::' has a higher precedence than '-', so '-value::type' is
  %% actually '-(value::type)' which is important for numeric values at the
  %% boundary of their range.
  [?_assertEqual([0, 42, -1, -9223372036854775808, 9223372036854775807],
                 query_row(C, "SELECT 0::int8, 42::int8, -1::int8, " ++
                             "(-9223372036854775808)::int8, " ++
                             "9223372036854775807::int8")),
   ?_assertEqual([0, 42, -1, -9223372036854775808, 9223372036854775807],
                 query_row(C, "SELECT $1, $2, $3, $4, $5",
                           [{int8, 0}, {int8, 42}, {int8, -1},
                            {int8, -9223372036854775808},
                            {int8, 9223372036854775807}])),
   ?_assertEqual([0, 42, -1, -2147483648, 2147483647],
                 query_row(C, "SELECT 0::int4, 42::int4, -1::int4, " ++
                             "(-2147483648)::int4, 2147483647::int4")),
   ?_assertEqual([0, 42, -1, -2147483648, 2147483647],
                 query_row(C, "SELECT $1, $2, $3, $4, $5",
                           [{int4, 0}, {int4, 42}, {int4, -1},
                            {int4, -2147483648}, {int4, 2147483647}])),
   ?_assertEqual([0, 42, -1, -32768, 32767],
                 query_row(C, "SELECT 0::int2, 42::int2, -1::int2, " ++
                             "(-32768)::int2, 32767::int2")),
   ?_assertEqual([0, 42, -1, -32768, 32767],
                 query_row(C, "SELECT $1, $2, $3, $4, $5",
                           [{int2, 0}, {int2, 42}, {int2, -1},
                            {int2, -32768}, {int2, 32767}])),
   ?_assertEqual([20, 50000, 4000000000, -20, -50000, -4000000000],
                 query_row(C, "SELECT $1, $2, $3, $4, $5, $6",
                           [20, 50000, 4000000000,
                            -20, -50000, -4000000000]))].

type_oid(C) ->
  [?_assertEqual([0, 42, 4294967295],
                 query_row(C, "SELECT 0::oid, 42::oid, 4294967295::oid")),
   ?_assertEqual([0, 42, 4294967295],
                 query_row(C, "SELECT $1, $2, $3",
                           [{oid, 0}, {oid, 42}, {oid, 4294967295}]))].

type_text(C) ->
  [?_assertEqual([<<"">>, <<"foo">>, <<"'foo'">>, <<"été"/utf8>>],
                 query_row(C, "SELECT ''::text, 'foo'::text, " ++
                             "'''foo'''::text, 'été'::text")),
   ?_assertEqual([<<"">>, <<"foo">>, <<"'foo'">>, <<"été"/utf8>>],
                 query_row(C, "SELECT $1, $2, $3, $4",
                           [{text, <<"">>}, {text, <<"foo">>},
                            {text, <<"'foo'">>}, {text, <<"été"/utf8>>}])),
   ?_assertEqual([<<"">>, <<"foo">>, <<"'foo'">>, <<"été"/utf8>>],
                 query_row(C, "SELECT $1, $2, $3, $4",
                           [<<"">>, <<"foo">>, <<"'foo'">>, <<"été"/utf8>>]))].

type_float(C) ->
  [?_assertEqual([0.0, -42.5, 0.00006103515625],
                 query_row(C, "SELECT 0.0::float4, -42.5::float4, " ++
                             "0.00006103515625::float4")),
   ?_assertEqual([0.0, -42.5, 0.00006103515625],
                 query_row(C, "SELECT $1, $2, $3",
                           [{float4, 0.0}, {float4, -42.5},
                            {float4, 0.00006103515625}])),
   ?_assertEqual([1.1, 1.0e+300, -4.1],
                 query_row(C, "SELECT 1.1::float8, 1.0e300::float8, " ++
                             "-4.1::float8")),
   ?_assertEqual([1.1, 1.0e+300, -4.1],
                 query_row(C, "SELECT $1, $2, $3",
                           [{float8, 1.1}, {float8, 1.0e300},
                            {float8, -4.1}])),
   ?_assertEqual([0.0, -42.5, 0.00006103515625, 1.1, 1.0e+300, -4.1],
                 query_row(C, "SELECT $1, $2, $3, $4, $5, $6",
                           [0.0, -42.5, 0.00006103515625,
                            1.1, 1.0e300, -4.1])),
   ?_assertEqual([nan, positive_infinity, negative_infinity],
                 query_row(C, "SELECT 'NaN'::float8, " ++
                             "'Infinity'::float8, '-Infinity'::float8", [])),
   ?_assertEqual([nan, positive_infinity, negative_infinity],
                 query_row(C, "SELECT $1, $2, $3",
                           [{float8, nan}, {float8, positive_infinity},
                            {float8, negative_infinity}])),
   ?_assertEqual([nan, positive_infinity, negative_infinity],
                 query_row(C, "SELECT 'NaN'::float4, " ++
                             "'Infinity'::float4, '-Infinity'::float4", [])),
   ?_assertEqual([nan, positive_infinity, negative_infinity],
                 query_row(C, "SELECT $1, $2, $3",
                           [{float4, nan}, {float4, positive_infinity},
                            {float4, negative_infinity}]))].

type_uuid(C) ->
  [?_assertEqual([<<3,172,86,36,126,103,79,211,178,40,23,231,189,76,180,179>>],
                 query_row(C, "SELECT '03ac5624-7e67-4fd3-b228-17e7bd4cb4b3'" ++
                             "::uuid")),
   ?_assertEqual([<<3,172,86,36,126,103,79,211,178,40,23,231,189,76,180,179>>],
                 query_row(C, "SELECT $1",
                           [{uuid, <<3,172,86,36,126,103,79,211,
                                     178,40,23,231,189,76,180,179>>}]))].

type_jsonb(C) ->
  [?_assertEqual([<<"[1, 2, 3]">>],
                 query_row(C, "SELECT '[1, 2, 3]'::jsonb")),
   ?_assertEqual([<<"[1, 2, 3]">>],
                 query_row(C, "SELECT $1", [{jsonb, <<"[1, 2, 3]">>}]))].

type_bit(C) ->
  [?_assertEqual([<<1:1>>, <<0:2>>, <<12:4>>, <<326:9>>,
                  <<2#10000000_00101010:16>>],
                 query_row(C, "SELECT 1::bit, ''::bit(2), '11'::bit(4), " ++
                             "'101000110'::bit(9), " ++
                             "'1000000000101010'::bit(16)")),
   ?_assertEqual([<<1:1>>, <<0:2>>, <<12:4>>, <<326:9>>,
                  <<2#10000000_00101010:16>>],
                 query_row(C, "SELECT $1, $2, $3, $4, $5",
                           [{bit, <<1:1>>}, {bit, <<0:2>>},
                            {bit, <<12:4>>}, {bit, <<326:9>>},
                            {bit, <<2#10000000_00101010:16>>}]))].

type_point(C) ->
  [?_assertEqual([{2.5, -1.3}],
                 query_row(C, "SELECT '(2.5, -1.3)'::point")),
   ?_assertEqual([{2.5, -1.3}],
                 query_row(C, "SELECT $1", [{point, {2.5, -1.3}}]))].

type_lseg(C) ->
  [?_assertEqual([{{-0.5, 2.0}, {2.5, -1.3}}],
                 query_row(C, "SELECT '((-0.5, 2.0), (2.5, -1.3))'::lseg")),
   ?_assertEqual([{{-0.5, 2.0}, {2.5, -1.3}}],
                 query_row(C, "SELECT $1",
                           [{lseg, {{-0.5, 2.0}, {2.5, -1.3}}}]))].

type_path(C) ->
  [?_assertEqual([{closed, [{-1.0, 1.5}, {0.0, 0.3}]}],
                 query_row(C, "SELECT '((-1.0, 1.5), (0.0, 0.3))'::path")),
   ?_assertEqual([{open, [{-1.0, 1.5}, {0.0, 0.3}, {2.1, -1.5}]}],
                 query_row(C, "SELECT '[(-1.0, 1.5), (0.0, 0.3), " ++
                             "(2.1, -1.5)]'::path")),
   ?_assertEqual([{closed, [{-1.0, 1.5}, {0.0, 0.3}]}],
                 query_row(C, "SELECT $1",
                           [{path, {closed, [{-1.0, 1.5}, {0.0, 0.3}]}}])),
   ?_assertEqual([{open, [{-1.0, 1.5}, {0.0, 0.3}, {2.1, -1.5}]}],
                 query_row(C, "SELECT $1",
                           [{path, {open, [{-1.0, 1.5}, {0.0, 0.3},
                                           {2.1, -1.5}]}}]))].

type_box(C) ->
  [?_assertEqual([{{2.5, 2.0}, {-2.5, -1.3}}],
                 query_row(C, "SELECT '((2.5, 2.0), (-2.5, -1.3))'::box")),
   ?_assertEqual([{{2.5, 2.0}, {-2.5, -1.3}}],
                 query_row(C, "SELECT $1",
                           [{box, {{2.5, 2.0}, {-2.5, -1.3}}}]))].

type_polygon(C) ->
  [?_assertEqual([[{-1.0, 1.5}, {0.0, 0.3}, {2.1, -1.5}]],
                 query_row(C, "SELECT '((-1.0, 1.5), (0.0, 0.3), " ++
                             "(2.1, -1.5))'::polygon")),
   ?_assertEqual([[{-1.0, 1.5}, {0.0, 0.3}, {2.1, -1.5}]],
                 query_row(C, "SELECT $1",
                           [{polygon, [{-1.0, 1.5}, {0.0, 0.3},
                                       {2.1, -1.5}]}]))].

type_line(C) ->
  [?_assertEqual([{2.0, -3.5, 0.4}],
                 query_row(C, "SELECT '{2.0, -3.5, 0.4}'::line")),
   ?_assertEqual([{2.0, -3.5, 0.4}],
                 query_row(C, "SELECT $1",
                           [{line, {2.0, -3.5, 0.4}}]))].

type_circle(C) ->
  [?_assertEqual([{{-0.5, 2.0}, 1.3}],
                 query_row(C, "SELECT '((-0.5, 2.0), 1.3)'::circle")),
   ?_assertEqual([{{-0.5, 2.0}, 1.3}],
                 query_row(C, "SELECT $1",
                           [{circle, {{-0.5, 2.0}, 1.3}}]))].

type_cidr(C) ->
  [?_assertEqual([{{192, 168, 1, 0}, 32}],
                 query_row(C, "SELECT '192.168.1.0'::cidr")),
   ?_assertEqual([{{16#2001, 16#4f8, 16#3, 16#ba, 16#0, 16#0, 16#0, 16#0}, 64}],
                 query_row(C, "SELECT '2001:4f8:3:ba::/64'::cidr")),
   ?_assertEqual([{{192, 168, 1, 0}, 24}],
                 query_row(C, "SELECT $1",
                           [{cidr, {{192, 168, 1, 0}, 24}}])),
   ?_assertEqual([{{16#2001, 16#4f8, 16#3, 16#ba, 16#0, 16#0, 16#0, 16#0}, 64}],
                 query_row(C, "SELECT $1",
                           [{cidr, {{16#2001, 16#4f8, 16#3, 16#ba,
                                     16#0, 16#0, 16#0, 16#0}, 64}}]))].

type_macaddr(C) ->
  [?_assertEqual([<<16#08, 16#00, 16#2b, 16#01, 16#02, 16#03>>],
                 query_row(C, "SELECT '08:00:2b:01:02:03'::macaddr")),
   ?_assertEqual([<<16#08, 16#00, 16#2b, 16#01, 16#02, 16#03>>],
                 query_row(C, "SELECT $1",
                           [{macaddr,
                             <<16#08, 16#00, 16#2b, 16#01, 16#02, 16#03>>}])),
   ?_assertEqual([<<16#08, 16#00, 16#2b, 16#01, 16#02, 16#03, 16#04, 16#05>>],
                 query_row(C, "SELECT '08:00:2b:01:02:03:04:05'::macaddr8")),
   ?_assertEqual([<<16#08, 16#00, 16#2b, 16#01, 16#02, 16#03, 16#04, 16#05>>],
                 query_row(C, "SELECT $1",
                           [{macaddr8, <<16#08, 16#00, 16#2b, 16#01,
                                         16#02, 16#03, 16#04, 16#05>>}]))].

type_inet(C) ->
  [?_assertEqual([{{192, 168, 1, 1}, 32}],
                 query_row(C, "SELECT '192.168.1.1'::inet")),
   ?_assertEqual([{{16#2001, 16#4f8, 16#3, 16#ba, 16#0, 16#0, 16#0, 16#1}, 64}],
                 query_row(C, "SELECT '2001:4f8:3:ba::1/64'::inet")),
   ?_assertEqual([{{192, 168, 1, 1}, 24}],
                 query_row(C, "SELECT $1",
                           [{inet, {{192, 168, 1, 1}, 24}}])),
   ?_assertEqual([{{16#2001, 16#4f8, 16#3, 16#ba, 16#0, 16#0, 16#0, 16#1}, 64}],
                 query_row(C, "SELECT $1",
                           [{inet, {{16#2001, 16#4f8, 16#3, 16#ba,
                                     16#0, 16#0, 16#0, 16#1}, 64}}]))].

type_date(C) ->
  [?_assertEqual([{2020, 3, 1}],
                 query_row(C, "SELECT '2020-03-01'::date")),
   ?_assertEqual([{1950, 12, 31}],
                 query_row(C, "SELECT '1950-12-31'::date")),
   ?_assertEqual([{1, 1, 1}],
                 query_row(C, "SELECT '0001-01-01'::date")),
   ?_assertEqual([negative_infinity, positive_infinity],
                 query_row(C, "SELECT '-infinity'::date, 'infinity'::date")),
   ?_assertEqual([{2020, 3, 1}],
                 query_row(C, "SELECT $1", [{date, {2020, 3, 1}}])),
   ?_assertEqual([{1950, 12, 31}],
                 query_row(C, "SELECT $1", [{date, {1950, 12, 31}}])),
   ?_assertEqual([{1, 1, 1}],
                 query_row(C, "SELECT $1", [{date, {1, 1, 1}}])),
   ?_assertEqual([negative_infinity, positive_infinity],
                 query_row(C, "SELECT $1, $2",
                           [{date, negative_infinity},
                            {date, positive_infinity}]))].

type_time(C) ->
  [?_assertEqual([{14, 10, 5, 4500}],
                 query_row(C, "SELECT '14:10:05.0045'::time")),
   ?_assertEqual([{0, 0, 0, 0}],
                 query_row(C, "SELECT '00:00:00'::time")),
   ?_assertEqual([{23, 59, 59, 999999}],
                 query_row(C, "SELECT '23:59:59.999999'::time")),
   ?_assertEqual([{24, 0, 0, 0}],
                 query_row(C, "SELECT '24:00:00'::time")),
   ?_assertEqual([{14, 10, 5, 4500}],
                 query_row(C, "SELECT $1", [{time, {14, 10, 5, 4500}}])),
   ?_assertEqual([{0, 0, 0, 0}],
                 query_row(C, "SELECT $1", [{time, {0, 0, 0, 0}}])),
   ?_assertEqual([{23, 59, 59, 999999}],
                 query_row(C, "SELECT $1", [{time, {23, 59, 59, 999999}}])),
   ?_assertEqual([{24, 0, 0, 0}],
                 query_row(C, "SELECT $1", [{time, {24, 0, 0, 0}}]))].

type_timetz(C) ->
  {setup,
   fun() ->
       {ok, _} = pgc:exec(C, "SET timezone='GMT'")
   end,
   [?_assertEqual([{14, 10, 5, 4500, 7200}],
                  query_row(C, "SELECT '14:10:05.0045+02:00'::timetz")),
    ?_assertEqual([{0, 0, 0, 0, 0}],
                  query_row(C, "SELECT '00:00:00'::timetz")),
    ?_assertEqual([{23, 59, 59, 999999, 0}],
                  query_row(C, "SELECT '23:59:59.999999-00:00'::timetz")),
    ?_assertEqual([{24, 0, 0, 0, 0}],
                  query_row(C, "SELECT '24:00:00'::timetz")),
    ?_assertEqual([{14, 10, 5, 4500, 7200}],
                  query_row(C, "SELECT $1",
                            [{timetz, {14, 10, 5, 4500, 7200}}])),
    ?_assertEqual([{0, 0, 0, 0, 0}],
                  query_row(C, "SELECT $1",
                            [{timetz, {0, 0, 0, 0, 0}}])),
    ?_assertEqual([{23, 59, 59, 999999, 0}],
                  query_row(C, "SELECT $1",
                            [{timetz, {23, 59, 59, 999999, 0}}])),
    ?_assertEqual([{24, 0, 0, 0, 0}],
                  query_row(C, "SELECT $1",
                            [{timetz, {24, 0, 0, 0, 0}}]))]}.

type_timestamp(C) ->
  [?_assertEqual([{{2020, 3, 1}, {15, 20, 30, 3500}}],
                 query_row(C, "SELECT '2020-03-01T15:20:30.0035'::timestamp")),
   ?_assertEqual([{{1950, 12, 31}, {23, 59, 59, 999999}}],
                 query_row(C, "SELECT '1950-12-31T23:59:59.999999'" ++
                             "::timestamp")),
   ?_assertEqual([{{1, 1, 1}, {0, 0, 0, 0}}],
                 query_row(C, "SELECT '0001-01-01T00:00:00'::timestamp")),
   ?_assertEqual([negative_infinity, positive_infinity],
                 query_row(C, "SELECT '-infinity'::timestamp, " ++
                             "'infinity'::timestamp")),
   ?_assertEqual([{{2020, 3, 1}, {15, 20, 30, 3500}}],
                 query_row(C, "SELECT $1",
                           [{timestamp, {{2020, 3, 1}, {15, 20, 30, 3500}}}])),
   ?_assertEqual([{{1950, 12, 31}, {23, 59, 59, 999999}}],
                 query_row(C, "SELECT $1",
                           [{timestamp, {{1950, 12, 31},
                                         {23, 59, 59, 999999}}}])),
   ?_assertEqual([{{1, 1, 1}, {0, 0, 0, 0}}],
                 query_row(C, "SELECT $1",
                           [{timestamp, {{1, 1, 1}, {0, 0, 0, 0}}}])),
   ?_assertEqual([negative_infinity, positive_infinity],
                 query_row(C, "SELECT $1, $2",
                           [{timestamp, negative_infinity},
                            {timestamp, positive_infinity}]))].

type_timestamptz(C) ->
  {setup,
   fun() ->
       {ok, _} = pgc:exec(C, "SET timezone='GMT'")
   end,
   [?_assertEqual([{{2020, 3, 1}, {15, 20, 30, 3500}}],
                  query_row(C, "SELECT '2020-03-01T15:20:30.0035'" ++
                              "::timestamptz")),
    ?_assertEqual([{{1950, 12, 31}, {23, 59, 59, 999999}}],
                  query_row(C, "SELECT '1950-12-31T23:59:59.999999'" ++
                              "::timestamptz")),
    ?_assertEqual([{{1, 1, 1}, {0, 0, 0, 0}}],
                  query_row(C, "SELECT '0001-01-01T00:00:00'::timestamptz")),
    ?_assertEqual([negative_infinity, positive_infinity],
                  query_row(C, "SELECT '-infinity'::timestamptz, " ++
                              "'infinity'::timestamptz")),
    ?_assertEqual([{{2020, 3, 1}, {15, 20, 30, 3500}}],
                  query_row(C, "SELECT $1",
                            [{timestamptz, {{2020, 3, 1},
                                            {15, 20, 30, 3500}}}])),
    ?_assertEqual([{{1950, 12, 31}, {23, 59, 59, 999999}}],
                  query_row(C, "SELECT $1",
                            [{timestamptz, {{1950, 12, 31},
                                            {23, 59, 59, 999999}}}])),
    ?_assertEqual([{{1, 1, 1}, {0, 0, 0, 0}}],
                  query_row(C, "SELECT $1",
                            [{timestamptz, {{1, 1, 1}, {0, 0, 0, 0}}}])),
    ?_assertEqual([negative_infinity, positive_infinity],
                  query_row(C, "SELECT $1, $2",
                            [{timestamptz, negative_infinity},
                             {timestamp, positive_infinity}]))]}.

type_interval(C) ->
  [?_assertEqual([{0, 0, 11_430_002_000}],
                 query_row(C, "SELECT 'PT3H10M30.002S'::interval")),
   ?_assertEqual([{0, 4, 600_000_000}],
                 query_row(C, "SELECT 'P4DT10M'::interval")),
   ?_assertEqual([{63, 4, 0}],
                 query_row(C, "SELECT 'P5Y3M4D'::interval")),
   ?_assertEqual([{0, 0, 11_430_002_000}],
                 query_row(C, "SELECT $1",
                           [{interval, {0, 0, 11_430_002_000}}])),
   ?_assertEqual([{0, 4, 600_000_000}],
                 query_row(C, "SELECT $1", [{interval, {0, 4, 600_000_000}}])),
   ?_assertEqual([{63, 4, 0}],
                 query_row(C, "SELECT $1", [{interval, {63, 4, 0}}]))].

type_array(C) ->
  [?_assertEqual([[]],
                 query_row(C, "SELECT ARRAY[]::int4[]")),
   ?_assertEqual([[1, 2, 3]],
                 query_row(C, "SELECT ARRAY[1,2,3]")),
   ?_assertEqual([[<<"foo">>, <<"">>, <<"bar">>]],
                 query_row(C, "SELECT ARRAY['foo', '', 'bar']")),
   ?_assertEqual([[null]],
                 query_row(C, "SELECT ARRAY[NULL]")),
   ?_assertEqual([[[null, null], [null, null]]],
                 query_row(C, "SELECT ARRAY[[NULL, NULL], [NULL, NULL]]")),
   ?_assertEqual([[1, null, 3]],
                 query_row(C, "SELECT ARRAY[1, NULL, 3]")),
   ?_assertEqual([[[1, 2, 3], [4, 5, 6]]],
                 query_row(C, "SELECT ARRAY[[1,2,3], [4,5,6]]")),
   ?_assertEqual([[[[1], [2]], [[3], [4]], [[5], [6]]]],
                 query_row(C, "SELECT " ++
                             "ARRAY[[[1], [2]], [[3], [4]], [[5], [6]]]")),
   ?_assertEqual([[]],
                 query_row(C, "SELECT ARRAY[]::int4[]",
                           [{{array, int4}, []}])),
   ?_assertEqual([[1, 2, 3]],
                 query_row(C, "SELECT $1",
                           [{{array, int4}, [1, 2, 3]}])),
   ?_assertEqual([[<<"foo">>, <<"">>, <<"bar">>]],
                 query_row(C, "SELECT $1",
                           [{{array, text}, [<<"foo">>, <<"">>, <<"bar">>]}])),
   ?_assertEqual([[null]],
                 query_row(C, "SELECT $1",
                           [{{array, int4}, [null]}])),
   ?_assertEqual([[[null, null], [null, null]]],
                 query_row(C, "SELECT $1",
                           [{{array, int4}, [[null, null], [null, null]]}])),
   ?_assertEqual([[1, null, 3]],
                 query_row(C, "SELECT $1",
                           [{{array, int4}, [1, null, 3]}])),
   ?_assertEqual([[[1, 2, 3], [4, 5, 6]]],
                 query_row(C, "SELECT $1",
                           [{{array, int4}, [[1, 2, 3], [4, 5, 6]]}])),
   ?_assertEqual([[[[1], [2]], [[3], [4]], [[5], [6]]]],
                 query_row(C, "SELECT $1",
                           [{{array, int4},
                             [[[1], [2]], [[3], [4]], [[5], [6]]]}]))].

type_null(C) ->
  [?_assertEqual([null, null, [null]],
                 query_row(C, "SELECT NULL, NULL::int4, ARRAY[NULL]::text[]")),
   ?_assertEqual([null, null, [null]],
                 query_row(C, "SELECT $1, $2, $3",
                           [null, {int4, null}, {{array, text}, [null]}]))].

custom_types_test_() ->
  {setup,
   fun () ->
       ReverseTextCodec = {pgc_codec_reverse_text_tests, []},
       Types = [{text, 25, ReverseTextCodec}],
       pgc_tests:start_client(#{types => Types})
   end,
   fun pgc_tests:stop_client/1,
   fun (C) ->
       [?_assertEqual([<<"">>, <<"olleh">>, <<"dlrow">>],
                      query_row(C, "SELECT '', 'hello', 'world'"))]
   end}.

-spec query_row(pgc_client:ref(), unicode:chardata()) -> [term()].
query_row(Client, Query) ->
  query_row(Client, Query, []).

-spec query_row(pgc_client:ref(), unicode:chardata(), [term()]) -> [term()].
query_row(Client, Query, Params) ->
  [Row] = query_rows(Client, Query, Params),
  Row.

%% -spec query_rows(pgc_client:ref(), unicode:chardata()) -> [[term()]].
%% query_rows(Client, Query) ->
%%   query_rows(Client, Query, []).

-spec query_rows(pgc_client:ref(), unicode:chardata(), [term()]) -> [[term()]].
query_rows(Client, Query, Params) ->
  {ok, _, Rows, _} = pgc:query(Client, Query, Params),
  Rows.
