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

-module(pgc_pool).

-include_lib("kernel/include/logger.hrl").

-behaviour(gen_server).

-export([process_name/1, start_link/1, start_link/2, stop/1,
         stats/1, acquire/1, release/2,
         with_client/2, with_transaction/2, with_transaction/3]).
-export([init/1, terminate/2, handle_call/3, handle_cast/2, handle_info/2]).

-export_type([options/0, name/0, ref/0, client_fun/0, stats/0]).

-type options() :: #{client_options => pgc_client:options(),
                     max_nb_clients => pos_integer(),
                     request_timeout => pos_integer()}.

-type name() :: et_gen_server:name().
-type ref() :: et_gen_server:ref().

-type client_fun() :: fun((pgc_client:ref()) ->
                             ok | {ok, term()} | {error, term()}).

-type stats() :: #{nb_clients := non_neg_integer(),
                   max_nb_clients := pos_integer(),
                   nb_free_clients := non_neg_integer(),
                   nb_busy_clients := non_neg_integer(),
                   nb_requests := non_neg_integer()}.

-type state() :: #{options := options(),
                   free_clients := [pgc_client:ref()],
                   busy_clients := [pgc_client:ref()],
                   requests := queue:queue(request()),
                   request_timer => reference()}.

-type request() :: {From :: {pid(), term()}, Time :: integer()}.

-spec process_name(pgc:pool_id()) -> atom().
process_name(Id) ->
  Name = <<"pgc_pool_", (atom_to_binary(Id))/binary>>,
  binary_to_atom(Name).

-spec start_link(name() | options()) -> Result when
    Result :: {ok, pid()} | ignore | {error, term()}.
start_link(Options) when is_map(Options) ->
  gen_server:start_link(?MODULE, [Options], []);
start_link(Name) ->
  start_link(Name, #{}).

-spec start_link(name(), options()) -> Result when
    Result :: {ok, pid()} | ignore | {error, term()}.
start_link(Name, Options) ->
  gen_server:start_link(Name, ?MODULE, [Options], []).

-spec stop(ref()) -> ok.
stop(PoolRef) ->
  gen_server:stop(PoolRef).

-spec stats(ref()) -> stats().
stats(PoolRef) ->
  gen_server:call(PoolRef, stats, infinity).

-spec acquire(ref()) -> {ok, pgc_client:ref()} | {error, term()}.
acquire(PoolRef) ->
  gen_server:call(PoolRef, acquire, infinity).

-spec release(ref(), pgc_client:ref()) -> ok.
release(PoolRef, Client) ->
  gen_server:call(PoolRef, {release, Client}, infinity).

-spec with_client(ref(), client_fun()) -> term() | {error, term()}.
with_client(PoolRef, Fun) ->
  case acquire(PoolRef) of
    {ok, Client} ->
      try
        Fun(Client)
      after
        ok = release(PoolRef, Client)
      end;
    {error, Reason} ->
      {error, Reason}
  end.

-spec with_transaction(ref(), client_fun()) -> term() | {error, term()}.
with_transaction(PoolRef, Fun) ->
  with_transaction(PoolRef, Fun, <<"">>).

-spec with_transaction(ref(), client_fun(), BeginOpts :: iodata()) ->
        term() | {error, term()}.
with_transaction(PoolRef, Fun, BeginOpts) ->
  SendQuery = fun (Client, Query, ErrType) ->
                  case pgc:query(Client, Query) of
                    {ok, _, _, _} ->
                      ok;
                    {error, Reason} ->
                      %% BEGIN/COMMIT/ROLLBACK failing is a big deal.
                      QueryString = binary_to_list(iolist_to_binary(Query)),
                      ?LOG_ERROR("request ~w failed: ~p~n",
                                 [QueryString, Reason]),
                      pgc_client:stop(Client),
                      error({ErrType, Reason})
                  end
              end,
  Fun2 = fun (Client) ->
             SendQuery(Client, [<<"BEGIN ">>, BeginOpts], begin_failure),
             try
               Res = Fun(Client),
               SendQuery(Client, <<"COMMIT">>, commit_failure),
               Res
             of
               {error, Reason} ->
                 SendQuery(Client, <<"ROLLBACK">>, rollback_failure),
                 {error, Reason};
               Result ->
                 Result
             catch
               error:{Type, Reason} when
                   Type =:= commit_failure; Type =:= rollback_failure ->
                 {error, {Type, Reason}};
               Type:Reason:Stack ->
                 %% If something goes wrong here, we do not want to hide the
                 %% original error.
                 try
                   SendQuery(Client, <<"ROLLBACK">>, rollback_failure)
                 catch
                   _:_ -> ok
                 end,
                 erlang:raise(Type, Reason, Stack)
             end
         end,
  with_client(PoolRef, Fun2).

init([Options]) ->
  logger:update_process_metadata(#{domain => [pgc, pool]}),
  process_flag(trap_exit, true),
  State = #{options => Options,
            free_clients => [],
            busy_clients => [],
            requests => queue:new()},
  {ok, State}.

terminate(_Reason, #{free_clients := FreeClients,
                     busy_clients := BusyClients,
                     requests := Requests}) ->
  lists:foreach(fun pgc_client:stop/1, FreeClients),
  lists:foreach(fun pgc_client:stop/1, BusyClients),
  lists:foreach(fun ({From, _}) ->
                    gen_server:reply(From, {error, stopping})
                end, queue:to_list(Requests)),
  ok.

handle_call(stats, _From, State) ->
  #{options := Options,
    free_clients := FreeClients,
    busy_clients := BusyClients,
    requests := Requests} = State,
  MaxNbClients = max_nb_clients(Options),
  NbFreeClients = length(FreeClients),
  NbBusyClients = length(BusyClients),
  Stats = #{nb_clients => NbFreeClients + NbBusyClients,
            max_nb_clients => MaxNbClients,
            nb_free_clients => NbFreeClients,
            nb_busy_clients => NbBusyClients,
            nb_requests => queue:len(Requests)},
  {reply, Stats, State};

handle_call(acquire, _From,
            State = #{free_clients := [Client | FreeClients],
                      busy_clients := BusyClients}) ->
  State2 = State#{free_clients => FreeClients,
                  busy_clients => [Client | BusyClients]},
  {reply, {ok, Client}, State2};
handle_call(acquire, From,
            State = #{options := Options,
                      free_clients := [],
                      busy_clients := BusyClients}) ->
  ClientOptions = maps:get(client_options, Options, #{}),
  MaxNbClients = max_nb_clients(Options),
  case length(BusyClients) < MaxNbClients of
    true ->
      %% The client limit has not been reached, we can create a new one.
      case pgc_client:start_link(ClientOptions) of
        {ok, Client} ->
          State2 = State#{busy_clients => [Client | BusyClients]},
          {reply, {ok, Client}, State2};
        {error, Reason} ->
          {reply, {error, {client_error, Reason}}, State}
      end;
    false ->
      %% There is no available client and we cannot create a new one, so we
      %% enqueue the request.
      Request = {From, erlang:monotonic_time(millisecond)},
      State2 = insert_request(Request, State),
      {noreply, State2}
  end;

handle_call({release, Client}, _From,
            State = #{free_clients := FreeClients,
                      busy_clients := BusyClients}) ->
  Pred = fun (C) -> C =:= Client end,
  {[_], BusyClients2} = lists:partition(Pred, BusyClients),
  %% The process of the client may have exited after being acquired but
  %% before being released.
  case is_process_alive(Client) of
    true ->
      %% If there is a request in the request queue, we can satisfy it
      %% immediately.
      case pop_oldest_request(State) of
        {request, {ReqFrom, _}, State2} ->
          %% The client stays busy, so we update neither the free list nor
          %% the busy list.
          gen_server:reply(ReqFrom, {ok, Client}),
          {reply, ok, State2};
        no_request ->
          State2 = State#{free_clients => [Client | FreeClients],
                          busy_clients => BusyClients2},
          {reply, ok, State2}
      end;
    false ->
      %% If the client exited before being released, it does not go back
      %% to the free list.
      State2 = State#{busy_clients => BusyClients2},
      {reply, ok, State2}
  end;

handle_call(Msg, From, State) ->
  ?LOG_WARNING("unhandled call ~p from ~p~n", [Msg, From]),
  {noreply, State}.

handle_cast(Msg, State) ->
  ?LOG_WARNING("unhandled cast ~p~n", [Msg]),
  {noreply, State}.

handle_info({request_timeout, Request = {From, _}}, State) ->
  %% It is possible in some rare cases to receive a timeout for a request
  %% which has already been satisfied with the following sequence of events:
  %%
  %% 1. The timer of the oldest request expires.
  %% 2. The pool process receives a release message and reassigns the
  %%    client to the oldest request, removing it from the queue.
  %% 3. The timeout message is received by the pool process, but is associated
  %%    with a request which has already been removed.
  %%
  %% To prevent that, the request_timeout message contains the request. If it
  %% does not match the oldest request in the queue, then it has already been
  %% removed.
  case pop_request(Request, State) of
    {ok, State2} ->
      gen_server:reply(From, {error, timeout}),
      {noreply, State2};
    request_mismatch ->
      ?LOG_INFO("ignoring obsolete request_timeout message for request ~p~n",
                [Request]),
      {noreply, State}
  end;

handle_info({'EXIT', Client, _Reason},
            State = #{free_clients := FreeClients}) ->
  Pred = fun (C) -> C == Client end,
  case lists:partition(Pred, FreeClients) of
    {[_W], FreeClients2} ->
      %% The client is free, just remove it
      {noreply, State#{free_clients => FreeClients2}};
    {[], _} ->
      %% The client is busy, it will be removed when released
      {noreply, State}
  end;

handle_info(Msg, State) ->
  ?LOG_WARNING("unhandled info ~p~n", [Msg]),
  {noreply, State}.

-spec insert_request(request(), state()) -> state().
insert_request(Request, State) ->
  #{options := Options,
    requests := Requests} = State,
  Timeout = maps:get(request_timeout, Options, 1000),
  Empty = queue:is_empty(Requests),
  State2 = State#{requests => queue:in(Request, Requests)},
  %% If the request queue was empty before the insertion, schedule a new timer
  %% for the expiration of the request.
  case Empty of
    true ->
      Timer = schedule_request_timeout(Request, Timeout),
      State2#{request_timer => Timer};
    false ->
      State2
  end.

-spec pop_request(request(), state()) -> {ok, state()} | request_mismatch.
pop_request(Request, State) ->
  #{requests := Requests} = State,
  case queue:peek(Requests) of
    {value, Request} ->
      {request, _, State2} = pop_oldest_request(State),
      {ok, State2};
    _ ->
      request_mismatch
  end.

-spec pop_oldest_request(state()) -> {request, request(), state()} | no_request.
pop_oldest_request(State) ->
  #{options := Options,
    requests := Requests} = State,
  Timeout = maps:get(request_timeout, Options, 1000),
  case queue:out(Requests) of
    {{value, Request}, Requests2} ->
      {OldTimer, State2} = maps:take(request_timer, State),
      erlang:cancel_timer(OldTimer),
      State3 = State2#{requests => Requests2},
      case queue:peek(Requests2) of
        empty ->
          {request, Request, State3};
        {value, NextRequest = {_, NextTime}} ->
          %% We need to start a timer to handle the expiration of the next
          %% request in the queue, since it is now the oldest one.
          Age = erlang:monotonic_time(millisecond) - NextTime,
          Timeout2 = case Age > Timeout of
                       true -> 0;
                       false -> Timeout - Age
                     end,
          Timer = schedule_request_timeout(NextRequest, Timeout2),
          {request, Request, State3#{request_timer => Timer}}
      end;
    {empty, _} ->
      no_request
  end.

-spec schedule_request_timeout(request(), Delay ::pos_integer()) -> reference().
schedule_request_timeout(Request, Delay) ->
  erlang:send_after(Delay, self(), {request_timeout, Request}).

-spec max_nb_clients(options()) -> pos_integer().
max_nb_clients(#{max_nb_clients := N}) -> N;
max_nb_clients(_) -> 10.
