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

-module(pg_tests).

-export([client_options/0,
         start_client/0, start_client/1, stop_client/1]).

-spec client_options() -> pg_client:options().
client_options() ->
  #{user => "erl-pg-test",
    database => "erl-pg-test"}.

-spec start_client() -> pg_client:ref().
start_client() ->
  start_client(client_options()).

-spec start_client(pg_client:options()) -> pg_client:ref().
start_client(Options) ->
  Options2 = maps:merge(client_options(), Options),
  {ok, Client} = pg_client:start_link(Options2),
  Client.

-spec stop_client(pg_client:ref()) -> ok.
stop_client(Client) ->
  pg_client:stop(Client).
