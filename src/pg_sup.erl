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

-module(pg_sup).

-behaviour(supervisor).

-export([start_link/0, start_pool/2]).
-export([init/1]).

-spec start_link() -> supervisor:startlink_ret().
start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec start_pool(pg:pool_id(), pg_pool:options()) ->
        supervisor:startchild_ret().
start_pool(Id, Options) ->
  supervisor:start_child(?MODULE, pool_child_spec(Id, Options)).

init([]) ->
  Children = pool_child_specs(),
  Flags = #{strategy => one_for_one,
            intensity => 1,
            period => 5},
  {ok, {Flags, Children}}.

-spec pool_child_specs() -> [supervisor:child_spec()].
pool_child_specs() ->
  PoolSpecs = application:get_env(pg, pools, []),
  maps:fold(fun (Id, Options, Acc) ->
                [pool_child_spec(Id, Options) | Acc]
            end,
            [], PoolSpecs).

-spec pool_child_spec(pg:pool_id(), pg_pool:options()) ->
        supervisor:child_spec().
pool_child_spec(ChildId, Options) ->
  Name = pg_pool:process_name(ChildId),
  #{id => ChildId,
    start => {pg_pool, start_link, [{local, Name}, Options]}}.
