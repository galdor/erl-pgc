% erl-pg

# Introduction
The erl-pg project is an Erlang client for the
[PostgreSQL](https://www.postgresql.org) database.

# Sending queries
**TODO**

## Query options
The following query options are available:

- `column_names_as_atoms`: return column names as atoms instead of binaries.

# Type and value mapping
## Erlang types to PostgreSQL types
| Erlang type                                  | PostgreSQL type |
|----------------------------------------------|-----------------|
| `boolean()`                                  | `bool`          |
| `-32768..32767`                              | `int2`          |
| `-2147483648..2147483647`                    | `int4`          |
| `-9223372036854775808..-9223372036854775807` | `int8`          |
| `binary()`                                   | `text`          |
| `float`                                      | `float8`        |
| `{PgType, Value}`                            | `PgType`        |

## PostgreSQL types to Erlang types
| PostgreSQL type | Erlang type                                  | Example                                                       |
|-----------------|----------------------------------------------|---------------------------------------------------------------|
| `bool`          | `boolean()`                                  | `true`                                                        |
| `bytea`         | `binary()`                                   | `<<1,2,3>>`                                                   |
| `"char"`        | `0..255`                                     | `$a`                                                          |
| `name`          | `binary()`                                   | `<<"users">>`                                                 |
| `int8`          | `-9223372036854775808..-9223372036854775807` | `42`                                                          |
| `int2`          | `-32768..32767`                              | `42`                                                          |
| `int4`          | `-2147483648..2147483647`                    | `42`                                                          |
| `text`          | `binary()`                                   | `<<"été"/utf8>>`                                              |
| `oid`           | `0..4294967295`                              | `42`                                                          |
| `json`          | `binary()`                                   | `<<"[1,2,3]">>`                                               |
| `xml`           | `binary()`                                   | `<<"<br />">>`                                                |
| `point`         | `pg:point()`                                 | `{0.0,1.0}`                                                   |
| `lseg`          | `pg:line_segment()`                          | `{{0.0,1.0}, {2.5,-1.5}}`                                     |
| `path`          | `pg:path()`                                  | `{open, [{-1.0,1.5}, {0.0,0.3}, {2.1,-1.5}]}`                 |
| `box`           | `pg:box()`                                   | `{{2.5,2.0}, {-2.5,-1.3}}`                                    |
| `line`          | `pg:line()`                                  | `{2.0, -1.5, 3.1}`                                            |
| `polygon`       | `pg:polygon()`                               | `[{-1.0,1.5}, {0.0,0.3}, {2.1,-1.5}]`                         |
| `cidr`          | `pg:inet_address()`                          | `{{192,168,0,0}, 24}`                                         |
| `float4`        | `pg:float_value()`                           | `3.14`                                                        |
| `float8`        | `pg:float_value()`                           | `3.14`                                                        |
| `macaddr8`      | `pg:macaddr()`                               | `{16#08, 16#00, 16#2b, 16#01, 16#02, 16#03, 16#04, 16#05}`    |
| `circle`        | `pg:circle()`                                | `{{0.0,1.0}, 3.0}`                                            |
| `macaddr`       | `pg:mac_address()`                           | `{16#08, 16#00, 16#2b, 16#01, 16#02, 16#03}`                  |
| `inet`          | `pg:inet_address()`                          | `{{192,168,0,1}, 24}`                                         |
| `bpchar`        | `binary()`                                   | `<<"été"/utf8>>`                                              |
| `varchar`       | `binary()`                                   | `<<"été"/utf8>>`                                              |
| `date`          | `pg:date()`                                  | `{2020, 03, 01}`                                              |
| `time`          | `pg:time()`                                  | `{14, 10, 30, 3500}`                                          |
| `timestamp`     | `pg:timestamp()`                             | `{{2020, 03, 01}, {14, 10, 30, 3500}}`                        |
| `timestamptz`   | `pg:timestamp()`                             | `{{2020, 03, 01}, {14, 10, 30, 3500}}`                        |
| `interval`      | `pg:interval()`                              | `{1, 5, 16200000000}`                                         |
| `timetz`        | `pg:time_with_timezone()`                    | `{14, 10, 30, 0, 7200}`                                       |
| `bit`           | `binary()`                                   | `<<1:1,0:1,1:1>>`                                             |
| `varbit`        | `binary()`                                   | `<<1:1,0:1,1:1>>`                                             |
| `uuid`          | `pg:uuid()`                                  | `<<3,172,86,36,126,103,79,211,178,40,23,231,189,76,180,179>>` |
| `jsonb`         | `binary()`                                   | `<<"[1,2,3]">>`                                               |

## NULL
The SQL `NULL` value is represented by the `null` Erlang atom.

## Arrays
Arrays are represented by Erlang lists. Multidimensional arrays are
represented by nested Erlang lists. The type name of an array is `{array,
ElementType}`. For example, an array of 4 byte integers has the type `{array,
int4}`. Note that there is no such things as arrays of arrays.

## Floating point values
Erlang does not support `NaN`, `+Infinity` and `-Infinity`. Therefore we
introduce `pg:float_value()` which extends `float()` with `nan`,
`positive_infinity` and `negative_infinity`. These values are supported for
both decoding and encoding. We use a quiet NaN value.

# Caveats
## Character encoding
We currently only support databases whose character encoding is
UTF-8. Supporting other database character encodings would require to
either:
- stop converting text data with `unicode:characters_to_binary/1` and use
  binaries without any validation;
- detect the database character encoding and use something such as iconv to
  convert from/to UTF-8.

The former would cause interesting errors for any code which relies on textual
data loaded from a non-UTF-8 database.

The later is complex and requires an external library.

Just use UTF-8.

## Inet addresses
The binary protocol uses an integer to identify IPv4 and IPv6 address. The
value is based on the `AF_INET` POSIX constant which is platform
dependant. Fortunately, it has the same value (2) for at least Linux, FreeBSD,
OpenBSD, NetBSD and Windows, but the `inet` and `cidr` types will not be
usable when PostgreSQL runs on a platform where `AF_INET` has a different
value.

## Numeric type
The `numeric` PostgreSQL type represents precise numbers with a user-defined
precision. Since Erlang does not have any data type to handle this kind of
value, we do not implement it.

## Money type
The `money` PostgreSQL type represents numbers with a fixed fractional
precision. Infortunately, it relies on the system environment of the database
(`LC_MONETARY`) making it. Additionally, Erlang does not have any data type to
handle this kind of value, therefore we do not implement it.

## Dates
While PostgreSQL supports BC dates, Erlang calendar functions do not. We
should still be able to represent them, but we currently depend on Erlang
functions to decode and encode dates to number days.

An error is therefore signaled when a BC date is present in query results.

This restriction also applies to timestamps.
