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

-module(pg_test).

-export([start_client/0, start_client/1, stop_client/1]).

-spec start_client() -> pg_client:client().
start_client() ->
  Options = #{user => "erl-pg-test", database => "erl-pg-test"},
  start_client(pg_client:options(Options)).

-spec start_client(pg_client:options()) -> pg_client:client().
start_client(Options) ->
  {ok, Client} = pg_client:start_link(Options),
  Client.

-spec stop_client(pg_client:client()) -> ok.
stop_client(Client) ->
  pg_client:stop(Client).
