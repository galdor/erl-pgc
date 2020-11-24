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

-module(pgc_pool_tests).

-include_lib("eunit/include/eunit.hrl").

pool_test_() ->
  {foreach,
   fun () ->
       error_logger:tty(false)
   end,
   fun (_) ->
       try
         pgc_pool:stop(pgc_pool_test)
       catch
         exit:{normal, _} ->
           ok;
         exit:noproc ->
           ok
       end,
       error_logger:tty(true)
   end,
   [fun start_stop/0,
    fun stop/0,
    fun client_init_error/0,
    fun with_client/0,
    fun with_client_close/0,
    fun with_transaction/0,
    fun with_transaction_rollback/0,
    fun with_transaction_commit_failure/0,
    fun timeout/0,
    fun stats/0,
    fun crash_while_busy/0,
    fun crash_while_free/0,
    fun stress/0]}.

start_stop() ->
  Pool = test_pool(#{}),
  pgc_pool:stop(Pool).

stop() ->
  Pool = test_pool(#{max_nb_clients => 2}),
  {ok, Client1} = pgc_pool:acquire(Pool),
  {ok, Client2} = pgc_pool:acquire(Pool),
  pgc_pool:release(Pool, Client2),
  ?assert(is_process_alive(Client1)),
  ?assert(is_process_alive(Client2)),
  pgc_pool:stop(Pool),
  ?assertNot(is_process_alive(Client1)),
  ?assertNot(is_process_alive(Client2)).

client_init_error() ->
  Pool = test_pool(#{client_options => #{database => "does_not_exist"}}),
  ?assertMatch({error, {client_error, #{code := invalid_catalog_name}}},
               pgc_pool:acquire(Pool)),
  pgc_pool:stop(Pool).

with_client() ->
  Pool = test_pool(#{max_nb_clients => 10}),
  ?assertEqual(ok, pgc_pool:with_client(Pool,
                                           fun (_Client) -> ok end)),
  ?assertThrow(crash, pgc_pool:with_client(Pool,
                                              fun (_Client) -> throw(crash) end)),
  ?assertExit(crash, pgc_pool:with_client(Pool,
                                             fun (_Client) -> exit(crash) end)),
  ?assertError(crash, pgc_pool:with_client(Pool,
                                              fun (_Client) -> error(crash) end)),
  ?assertMatch(#{nb_clients := 1,
                 nb_free_clients := 1,
                 nb_busy_clients := 0},
               pgc_pool:stats(Pool)),
  pgc_pool:stop(Pool).

with_client_close() ->
  Pool = test_pool(#{max_nb_clients => 10}),
  ?assertExit({noproc, {gen_server, call, _}},
              pgc_pool:with_client(Pool,
                                      fun (Client) ->
                                          close_client(Client),
                                          pgc:query(Client, "SELECT 42")
                                      end)),
  ?assertMatch(#{nb_clients := 0,
                 nb_free_clients := 0,
                 nb_busy_clients := 0},
               pgc_pool:stats(Pool)),
  pgc_pool:stop(Pool).

with_transaction() ->
  Pool = test_pool(#{}),
  Fun1 = fun (Client) -> pgc:query(Client, "SELECT 42") end,
  ?assertMatch({ok, [_], [[_]], 1}, pgc_pool:with_transaction(Pool, Fun1)),
  Fun2 = fun (Client) -> pgc:query(Client, "FOO") end,
  ?assertMatch({error, _}, pgc_pool:with_transaction(Pool, Fun2)),
  Fun3 = fun (_Client) -> throw(test) end,
  ?assertThrow(test, pgc_pool:with_transaction(Pool, Fun3)),
  Fun4 = fun (_Client) -> exit(test) end,
  ?assertExit(test, pgc_pool:with_transaction(Pool, Fun4)),
  Fun5 = fun (_Client) -> error(test) end,
  ?assertError(test, pgc_pool:with_transaction(Pool, Fun5)),
  pgc_pool:stop(Pool).

with_transaction_rollback() ->
  Pool = test_pool(#{}),
  Table = atom_to_binary(?FUNCTION_NAME),
  Fun1 = fun (Client) ->
             {ok, _} = pgc:exec(Client, ["CREATE TABLE ", Table, "()"]),
             error(test)
         end,
  ?assertError(test, pgc_pool:with_transaction(Pool, Fun1)),
  Fun2 = fun (Client) ->
             pgc:query(Client, ["DROP TABLE ", Table])
         end,
  ?assertMatch({error, #{code := undefined_table}},
               pgc_pool:with_client(Pool, Fun2)),
  pgc_pool:stop(Pool).

with_transaction_commit_failure() ->
  Pool = test_pool(#{}),
  Table = atom_to_binary(?FUNCTION_NAME),
  Fun1 = fun (Client) ->
             {ok, _} = pgc:exec(Client, ["CREATE TABLE ", Table,
                                        "(i INTEGER UNIQUE DEFERRABLE ",
                                        "INITIALLY DEFERRED)"]),
             {ok, _} = pgc:exec(Client, ["INSERT INTO ", Table, "(i)",
                                        " VALUES (1)"]),
             {ok, _} = pgc:exec(Client, ["INSERT INTO ", Table, "(i)",
                                        " VALUES (1)"])
         end,
  ?assertMatch({error, {commit_failure, #{code := unique_violation}}},
               pgc_pool:with_transaction(Pool, Fun1)),
  pgc_pool:stop(Pool).

timeout() ->
  Pool = test_pool(#{max_nb_clients => 2, request_timeout => 100}),
  {ok, Client1} = pgc_pool:acquire(Pool),
  {ok, Client2} = pgc_pool:acquire(Pool),
  {error, timeout} = pgc_pool:acquire(Pool),
  pgc_pool:release(Pool, Client1),
  {ok, Client3} = pgc_pool:acquire(Pool),
  pgc_pool:release(Pool, Client2),
  pgc_pool:release(Pool, Client3),
  pgc_pool:stop(Pool).

stats() ->
  Pool = test_pool(#{max_nb_clients => 10, request_timeout => 1000}),
  ?assertEqual(#{nb_clients => 0,
                 max_nb_clients => 10,
                 nb_free_clients => 0,
                 nb_busy_clients => 0,
                 nb_requests => 0},
               pgc_pool:stats(Pool)),
  {ok, Client1} = pgc_pool:acquire(Pool),
  ?assertEqual(#{nb_clients => 1,
                 max_nb_clients => 10,
                 nb_free_clients => 0,
                 nb_busy_clients => 1,
                 nb_requests => 0},
               pgc_pool:stats(Pool)),
  {ok, Client2} = pgc_pool:acquire(Pool),
  ?assertEqual(#{nb_clients => 2,
                 max_nb_clients => 10,
                 nb_free_clients => 0,
                 nb_busy_clients => 2,
                 nb_requests => 0},
               pgc_pool:stats(Pool)),
  pgc_pool:release(Pool, Client1),
  ?assertEqual(#{nb_clients => 2,
                 max_nb_clients => 10,
                 nb_free_clients => 1,
                 nb_busy_clients => 1,
                 nb_requests => 0},
               pgc_pool:stats(Pool)),
  pgc_pool:release(Pool, Client2),
  ?assertEqual(#{nb_clients => 2,
                 max_nb_clients => 10,
                 nb_free_clients => 2,
                 nb_busy_clients => 0,
                 nb_requests => 0},
               pgc_pool:stats(Pool)),
  pgc_pool:stop(Pool).

crash_while_busy() ->
  Pool = test_pool(#{max_nb_clients => 10}),
  {ok, Client1} = pgc_pool:acquire(Pool),
  ?assertMatch(#{nb_clients := 1,
                 nb_free_clients := 0,
                 nb_busy_clients := 1},
               pgc_pool:stats(Pool)),
  close_client(Client1),
  pgc_pool:release(Pool, Client1),
  ?assertMatch(#{nb_clients := 0,
                 nb_free_clients := 0,
                 nb_busy_clients := 0},
               pgc_pool:stats(Pool)),
  pgc_pool:stop(Pool).

crash_while_free() ->
  Pool = test_pool(#{}),
  {ok, Client1} = pgc_pool:acquire(Pool),
  pgc_pool:release(Pool, Client1),
  ?assertMatch(#{nb_clients := 1,
                 nb_free_clients := 1,
                 nb_busy_clients := 0},
               pgc_pool:stats(Pool)),
  close_client(Client1),
  ?assertMatch(#{nb_clients := 0,
                 nb_free_clients := 0,
                 nb_busy_clients := 0},
               pgc_pool:stats(Pool)),
  pgc_pool:stop(Pool).

stress() ->
  NbClients = 10,
  NbProcesses = 1000,
  Duration = 1000,
  AcquisitionDuration = 50,
  Pool = test_pool(#{max_nb_clients => NbClients}),
  ProcessFun = fun F() ->
                   case pgc_pool:acquire(Pool) of
                     {ok, Client} ->
                       {ok, _} = pgc:exec(Client, "SELECT 42"),
                       timer:sleep(AcquisitionDuration),
                       pgc_pool:release(Pool, Client),
                       F();
                     {error, timeout} ->
                       F()
                   end
               end,
  Processes = [spawn(ProcessFun) || _ <- lists:seq(1, NbProcesses)],
  timer:sleep(Duration),
  lists:foreach(fun (Process) ->
                    monitor(process, Process),
                    exit(Process, stop),
                    receive
                      {'DOWN', _, process, Process, _} ->
                        ok
                    end
                end, Processes),
  pgc_pool:stop(Pool).

-spec test_pool(pgc_pool:options()) -> pgc_pool:ref().
test_pool(ExtraOptions0) ->
  ExtraClientOptions = maps:get(client_options, ExtraOptions0, #{}),
  ClientOptions = maps:merge(pgc_tests:client_options(), ExtraClientOptions),
  ExtraOptions = ExtraOptions0#{client_options => ClientOptions},
  Options = maps:merge(#{client_options => ClientOptions,
                         max_nb_clients => 10,
                         request_timeout => 1000},
                       ExtraOptions),
  {ok, Pool} = pgc_pool:start_link(test, Options),
  Pool.

-spec close_client(pgc_client:ref()) -> ok.
close_client(Client) ->
  monitor(process, Client),
  pgc_client:stop(Client),
  receive
    {'DOWN', _, process, Client, _} ->
      ok
  end.
