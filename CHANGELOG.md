% erl-pg changelog

# Next Version
## Bugs
- Fix the default value of pool specifications in the configuration of the
  application.
## Misc
- Rename `pg_pool:pool_name/0` into `pg_pool:name/0` and `pg_pool:pool_ref/0`
  into `pg_pool:ref/0`.

# 1.1.0
## Features
- It is now possible to start supervised pools using `pg:start_pool/2`.
- Pool processes are now registered with a name based on the id provided in
  the application configuration (or passed to `pg:start_pool/2`). For example,
  a pool identified as `example` will have a process named `pg_pool_example`.
  It is therefore easier to guarantee name unicity.
- Add `pg:pool_stats/1`, `pg:acquire/1`, `pg:release/2`, `pg:with_client/2`,
  `pg:with_transaction/2`, which takes a pool identifier as argument instead
  of a pool reference.
- Add `pg:exec/2`, `pg:exec/3`, `pg:exec/4`, `pg:query/2`, `pg:query/3`,
  `pg:query/4`. They simply call the equivalent functions from `pg_client`.
## Bugs
- Fix various invalid types.
## Misc
- Log domains are now `[pg, client]` for clients and `[pg, pool]` for pools.
- Query options are now the empty map by default, making
  `pg:default_query_options/0` and `pg:query_options/0` useless. These
  functions have been removed.
- The set of pools in the application configuration file is now a map instead
  of a list of tuples.

# 1.0.0
First public version.
