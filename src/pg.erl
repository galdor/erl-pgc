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

-module(pg).

-export([default_query_options/0, query_options/1]).
-export([start_pool/2]).

-export_type([query_options/0, query_result/0, exec_result/0,
-export_type([pool_id/0,
              query_options/0, query_result/0, exec_result/0,
              column_name/0, row/0,
              oid/0, float_value/0,
              point/0, line_segment/0, path/0, box/0, polygon/0, line/0,
              circle/0,
              inet_address/0, mac_address/0,
              date/0, time/0, time_with_timezone/0, timestamp/0,
              interval/0,
              uuid/0]).

-type pool_id() :: atom().

-type query_options() :: #{column_names_as_atoms => boolean(),
                           rows_as_hashes => boolean()}.

-type query_result() :: {ok,
                         [column_name()],
                         [row()],
                         NbAffectedRows :: non_neg_integer()}
                      | {error,
                         pg_proto:error_and_notice_fields()}.
-type exec_result() :: {ok,
                        NbAffectedRows :: non_neg_integer()}
                     | {error,
                        pg_proto:error_and_notice_fields()}.

-type column_name() :: binary() | atom().

-type row() :: [term()] | #{column_name() := term()}.

-type oid() :: non_neg_integer().

-type float_value() :: float() | nan | positive_infinity | negative_infinity.

-type point() :: {float(), float()}.
-type line_segment() :: {Start :: point(), End :: point()}.
-type path() :: {open | closed, [point()]}.
-type box() :: {UpperRight :: point(), LowerLeft :: point()}.
-type polygon() :: [point()].
-type line() :: {A :: float(), B :: float(), C :: float()}. % Ax + By + C
-type circle() :: {Center :: point(), Radius :: float()}.

-type inet_address() :: {inet:ip4_address(), NetMask :: 0..32}
                      | {inet:ip6_address(), NetMask :: 0..128}.
-type mac_address() :: <<_:48>> | <<_:64>>.

-type time() :: {0..23, 0..59, 0..59, 0..1_000_000} | {24, 0, 0, 0}.
-type time_with_timezone() :: {0..23, 0..59, 0..59, 0..1_000_000,
                               Offset :: integer()}
                            | {24, 0, 0, 0,
                               Offset :: integer()}.
-type date() :: {integer(), 1..12, 1..31}
              | positive_infinity | negative_infinity.
-type timestamp() :: {date(), time()}.

-type interval() :: {Months :: integer(), Days :: integer(),
                     Microseconds :: integer()}.

-type uuid() :: <<_:128>>.

-spec default_query_options() -> query_options().
default_query_options() ->
  #{column_names_as_atoms => false,
    rows_as_hashes => false}.
-spec start_pool(pg:pool_id(), pg_pool:options()) ->
        supervisor:startchild_ret().
start_pool(Id, Options) ->
  pg_sup:start_pool(Id, Options).

-spec query_options(query_options()) -> query_options().
query_options(Options) ->
  maps:merge(default_query_options(), Options).
