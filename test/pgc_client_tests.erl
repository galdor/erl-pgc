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

-module(pgc_client_tests).

-include_lib("eunit/include/eunit.hrl").

client_test_() ->
  {foreach,
   fun pgc_tests:start_client/0,
   fun pgc_tests:stop_client/1,
   [fun column_names_as_atom/1,
    fun rows_as_hashes/1]}.

column_names_as_atom(C) ->
  [?_assertEqual({ok, [<<"name">>, <<"score">>], [[<<"bob">>, 42]], 1},
                 pgc:query(C, "SELECT 'bob' AS name, 42 AS score")),
   ?_assertEqual({ok, [name, score], [[<<"bob">>, 42]], 1},
                 pgc:query(C, "SELECT 'bob' AS name, 42 AS score", [],
                          #{column_names_as_atoms => true}))].

rows_as_hashes(C) ->
  [?_assertEqual({ok, [n], [[1], [2], [3]], 3},
                 pgc:query(C, "SELECT generate_series(1, 3) AS n", [],
                                 #{column_names_as_atoms => true})),
   ?_assertEqual({ok, [<<"n">>], [#{<<"n">> => 1}, #{<<"n">> => 2},
                                  #{<<"n">> => 3}], 3},
                 pgc:query(C, "SELECT generate_series(1, 3) AS n", [],
                          #{rows_as_hashes => true})),
   ?_assertEqual({ok, [n], [#{n => 1}, #{n => 2}, #{n => 3}], 3},
                 pgc:query(C, "SELECT generate_series(1, 3) AS n", [],
                          #{column_names_as_atoms => true,
                            rows_as_hashes => true}))].
