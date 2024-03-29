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

-module(pgc_pool_sup).

-behaviour(c_sup).

-export([start_link/0, start_pool/2]).
-export([children/0]).

-spec start_link() -> c_sup:start_ret().
start_link() ->
  c_sup:start_link({local, ?MODULE}, ?MODULE, #{}).

-spec start_pool(pgc:pool_id(), pgc_pool:options()) -> c_sup:result(pid()).
start_pool(Id, Options) ->
  Spec = #{start => fun pgc_pool:start_link/2,
           start_args => [Id, Options]},
  c_sup:start_child(?MODULE, Id, Spec).

-spec children() -> c_sup:child_specs().
children() ->
  PoolSpecs = application:get_env(pgc, pools, #{}),
  maps:fold(fun (Id, Options, Acc) ->
                Spec = #{start => fun pgc_pool:start_link/2,
                         start_args => [Id, Options]},
                [{Id, Spec} | Acc]
            end, [], PoolSpecs).
