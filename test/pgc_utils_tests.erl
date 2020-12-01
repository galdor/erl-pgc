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

-module(pgc_utils_tests).

-include_lib("eunit/include/eunit.hrl").

quote_identifier_test_() ->
  [?_assertEqual(<<"">>, pgc_utils:quote_identifier(<<"">>)),
   ?_assertEqual(<<"">>, pgc_utils:quote_identifier("")),
   ?_assertEqual(<<"foo">>, pgc_utils:quote_identifier(<<"foo">>)),
   ?_assertEqual(<<"été"/utf8>>, pgc_utils:quote_identifier(<<"été"/utf8>>)),
   ?_assertEqual(<<"\"foo \"\"bar\"\" baz\"">>,
                 pgc_utils:quote_identifier(<<"foo \"bar\" baz">>)),
   ?_assertEqual(<<"\"\"\"hello\"\"\"">>,
                 pgc_utils:quote_identifier(<<"\"hello\"">>))].
