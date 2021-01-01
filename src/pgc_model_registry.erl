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

-module(pgc_model_registry).

-include_lib("kernel/include/logger.hrl").

-behaviour(gen_server).

-export([start_link/0, register_model/2, unregister_model/1,
         find_model/1, get_model/1]).
-export([init/1, terminate/2, handle_call/3, handle_cast/2, handle_info/2]).

-type state() :: #{table := ets:tid()}.

-spec start_link() -> Result when
    Result :: {ok, pid()} | ignore | {error, term()}.
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec register_model(pgc_model:model_name(), pgc_model:model()) -> ok.
register_model(Name, Model) ->
  gen_server:call(?MODULE, {register_model, Name, Model}).

-spec unregister_model(pgc_model:model_name()) -> ok.
unregister_model(Name) ->
  gen_server:call(?MODULE, {unregister_model, Name}).

-spec find_model(pgc_model:model_name()) -> {ok, pgc_model:model()} | error.
find_model(Name) ->
  TableName = ?MODULE,
  case ets:lookup(TableName, Name) of
    [{_, Model}] ->
      {ok, Model};
    [] ->
      error
  end.

-spec get_model(pgc_model:model_name()) -> pgc_model:model().
get_model(Name) ->
  case find_model(Name) of
    {ok, Model} ->
      Model;
    error ->
      error({unknown_model, Name})
  end.

-spec init(list()) -> {ok, state()}.
init([]) ->
  logger:update_process_metadata(#{domain => [pgc, model_registry]}),
  TableId = ets:new(?MODULE, [set,
                              named_table,
                              {read_concurrency, true}]),
  State = #{table => TableId},
  {ok, State}.

terminate(_Reason, _State) ->
  ets:delete(?MODULE),
  ok.

handle_call({register_model, Name, Model}, _From, State) ->
  do_register_model(Name, Model, State),
  {reply, ok, State};

handle_call({unregister_model, Name}, _From, State) ->
  do_unregister_model(Name, State),
  {reply, ok, State};

handle_call(Msg, From, State) ->
  ?LOG_WARNING("unhandled call ~p from ~p", [Msg, From]),
  {reply, unhandled, State}.

handle_cast(Msg, State) ->
  ?LOG_WARNING("unhandled cast ~p", [Msg]),
  {noreply, State}.

handle_info(Msg, State) ->
  ?LOG_WARNING("unhandled info ~p", [Msg]),
  {noreply, State}.

-spec do_register_model(pgc_model:model_name(), pgc_model:model(), state()) ->
        ok.
do_register_model(Name, Model, #{table := Table}) ->
  ets:insert(Table, {Name, Model}),
  ok.

-spec do_unregister_model(pgc_model:model_name(), state()) -> ok.
do_unregister_model(Name, #{table := Table}) ->
  ets:delete(Table, Name),
  ok.
