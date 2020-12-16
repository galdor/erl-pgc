% erl-pgc changelog

# Next Version
# Features
- Add `pgc_model:decode_rows/2` and `pgc_model:decode_rows/3`.
- Add `pgc_model:decode_row/2` and `pgc_model:decode_row/2` to keep names coherent.

# 1.4.1
## Bugs
- Fix type specification for model values.
## Misc
- Export missing types for models.

# 1.4.0
## Features
- Introduce the `pgc:error/0` and `pgc:notice/0` types to avoid manipulating
  the `pgc_proto:error_and_notice_fields()` type.
- Add support for the `void` pseudo-type.
- Add `pgc:simple_exec/2`.
- Add functions to manipulate timestamp and time values.
- Add a `log_messages` client option to log messages received by the
  client. Default to false to avoid spamming as it was before.
- Add `pgc_utils:quote_identifier/1`.
- Add a `pgc_model` module providing a data model definition system making it
  easier to convert data business entities to database rows and vice versa.
- Add a `log_backend_notices` client option (default: `true`) to control the
  logging of notice messages sent by the server.
## Bugs
- Fix query failure logging in `pgc:with_transaction/2`.
- Fix response handling for empty statements.
- Fix the default application name.
- Fix MD5 password authentication.
## Misc
- Query functions now return type decoding errors instead of signalling them
  with `error/1`.
- Registered names are now listed in the application definition file.

# 1.3.0
## Features
- Annotate connection errors as `{connect, Reason}`.
- Include the pool id in the log domain.
## Bugs
- Fix handling of query responses which do not contain any row.
## Misc
- Use host strings (or binaries) for client connection addresses instead of
  inet values.
- The `pgc_pool:start_link/2` function now accepts a pool identifier and not a
  process name.
- Remove `pgc_pool:start_link/1`.

# 1.2.0
The big change with this new version is the renaming from `pg` to `pgc`, to
avoid conflict with a new module named `pg` introduced in Erlang 23.

Infortunately, there is a single global module namespace, and there is no way
to avoid or resolve conflicts in any way.

## Bugs
- Fix the default value of pool specifications in the configuration of the
  application.
## Misc
- Rename `pg_pool:pool_name/0` into `pgc_pool:name/0` and `pg_pool:pool_ref/0`
  into `pgc_pool:ref/0`.

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
