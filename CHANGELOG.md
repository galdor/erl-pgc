% erl-pg changelog

# Next Version
## Features
- It is now possible to start supervised pools using `pg:start_pool/2`.
- Pool processes are now registered with a name based on the id provided in
  the application configuration (or passed to `pg:start_pool/2`). For example,
  a pool identified as `example` will have a process named `pg_pool_example`.
  It is therefore easier to guarantee name unicity.
## Misc
- Log domains are now `[pg, client]` for clients and `[pg, pool]` for pools.
- Query options are now the empty map by default, making
  `pg:default_query_options/0` and `pg:query_options/0` useless. These
  functions have been removed.
- The set of pools in the application configuration file is now a map instead
  of a list of tuples.

# 1.0.0
First public version.
