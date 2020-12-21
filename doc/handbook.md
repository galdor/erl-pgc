% erl-pgc

# Introduction
The erl-pgc project is an Erlang client for the
[PostgreSQL](https://www.postgresql.org) database.

# Client
**TODO**

## Client options
**TODO**

# Pool
## Configuration
Pools are created by `pgc_sup` supervisor based on the configuration of the
`pgc` application. Pools are identified by an atom. Each pool process is
registered as `pgc_pool_<id>` where `<id>` is its identifier. For example, the
process of the `example` pool is registered as `pgc_pool_example`.

The following example configures a pool named `example`:

```erlang
[{pgc,
  [{pools,
    #{example => #{client_options => #{user => "test",
                                       database => "test"}}}}]}].
```

Pools can also be created with the `pgc:start_pool/2` function; these pools
are also handled by the `pgc_sup` supervisor.

## Pool options
**TODO**

# Sending queries
**TODO**

## Query options
The following query options are available:

- `column_names_as_atoms`: return column names as atoms instead of binaries.
- `rows_as_hashes`: return rows as hashes associating column name to value
  instead of lists of values.

# Types and values
For encoding, Erlang types are mapped to PostgreSQL types directly when it
makes sense (for example for booleans) or using a `{PgType, Value}` tuple. For
example, `{point, {1.0, -2.0}}` represents the PostgreSQL point `(1.0, -2.0)`.

Array types are represented by a `{array, ElementType}` typle. For example,
`{{array, int4}, [1, 2, 3]}` represents the PostgreSQL integer array `ARRAY[1,
2, 3]::int4`.

For decoding, PostgreSQL types are mapped to a variety of Erlang types. The
type itself is never included in returned value. For example, the PostgreSQL
array `ARRAY[1, 2, 3]::int4[]` will be decoded to `[1, 2, 3]`.

Additional types can be provided to each client for custom encoding and
decoding. Custom types will override internal types defined in erl-pg.

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
| `point`         | `pgc:point()`                                | `{0.0,1.0}`                                                   |
| `lseg`          | `pgc:line_segment()`                         | `{{0.0,1.0}, {2.5,-1.5}}`                                     |
| `path`          | `pgc:path()`                                 | `{open, [{-1.0,1.5}, {0.0,0.3}, {2.1,-1.5}]}`                 |
| `box`           | `pgc:box()`                                  | `{{2.5,2.0}, {-2.5,-1.3}}`                                    |
| `line`          | `pgc:line()`                                 | `{2.0, -1.5, 3.1}`                                            |
| `polygon`       | `pgc:polygon()`                              | `[{-1.0,1.5}, {0.0,0.3}, {2.1,-1.5}]`                         |
| `cidr`          | `pgc:inet_address()`                         | `{{192,168,0,0}, 24}`                                         |
| `float4`        | `pgc:float_value()`                          | `3.14`                                                        |
| `float8`        | `pgc:float_value()`                          | `3.14`                                                        |
| `macaddr8`      | `pgc:macaddr()`                              | `{16#08, 16#00, 16#2b, 16#01, 16#02, 16#03, 16#04, 16#05}`    |
| `circle`        | `pgc:circle()`                               | `{{0.0,1.0}, 3.0}`                                            |
| `macaddr`       | `pgc:mac_address()`                          | `{16#08, 16#00, 16#2b, 16#01, 16#02, 16#03}`                  |
| `inet`          | `pgc:inet_address()`                         | `{{192,168,0,1}, 24}`                                         |
| `bpchar`        | `binary()`                                   | `<<"été"/utf8>>`                                              |
| `varchar`       | `binary()`                                   | `<<"été"/utf8>>`                                              |
| `date`          | `pgc:date()`                                 | `{2020, 03, 01}`                                              |
| `time`          | `pgc:time()`                                 | `{14, 10, 30, 3500}`                                          |
| `timestamp`     | `pgc:timestamp()`                            | `{{2020, 03, 01}, {14, 10, 30, 3500}}`                        |
| `timestamptz`   | `pgc:timestamp()`                            | `{{2020, 03, 01}, {14, 10, 30, 3500}}`                        |
| `interval`      | `pgc:interval()`                             | `{1, 5, 16200000000}`                                         |
| `timetz`        | `pgc:time_with_timezone()`                   | `{14, 10, 30, 0, 7200}`                                       |
| `bit`           | `bitstring()`                                | `<<1:1,0:1,1:1>>`                                             |
| `varbit`        | `bitstring()`                                | `<<1:1,0:1,1:1>>`                                             |
| `uuid`          | `pgc:uuid()`                                 | `<<3,172,86,36,126,103,79,211,178,40,23,231,189,76,180,179>>` |
| `jsonb`         | `binary()`                                   | `<<"[1,2,3]">>`                                               |
| `Type[]`        | `list()`                                     | `[1, 2, 3]`                                                   |

## NULL
The SQL `NULL` value is represented by the `null` Erlang atom.

## Arrays
Arrays are represented by Erlang lists. Multidimensional arrays are
represented by nested Erlang lists. The type name of an array is `{array,
ElementType}`. For example, an array of 4 byte integers has the type `{array,
int4}`. Note that there is no such things as arrays of arrays.

## Floating point values
Erlang does not support `NaN`, `+Infinity` and `-Infinity`. Therefore we
introduce `pgc:float_value()` which extends `float()` with `nan`,
`positive_infinity` and `negative_infinity`. These values are supported for
both decoding and encoding. We use a quiet NaN value.

# Data models
Mapping database relational data to in-memory business data is not always
simple: among other things, data types are different, supported value ranges
vary, and optional values are handled differently.

Developers usually convert data in functions which interact with the database,
which is a repetitive and error-prone task.

The `pgc_model` module introduces the concept of data model. A model describes
how a business entity is represented as a row in the database. Each model
describes the way each value of the business entity is represented in the
database.

With these information, we can then use various functions to simplify
conversions from and to database values.

## Model
A model is a map where each key is an atom being the name of the entity key
and each value is a model value.

A model value is either a database type, or a map containing the following
entries:
- `type`: the database type used to represent the value in the database.
- `column`: the name of the database column as an atom (optional, the default
  value being the key of the entry).
- `default`: an Erlang term used when a database value is null.
- `encode`: a custom encoding function which transforms an Erlang term into a
  typed pgc value of the form `{Type, Value}`.
- `decode`: a custom decoding function which transforms a pgc value into an
  Erlang term.

Example:
```erlang
#{id => int4,
  name => #{type => text, column => user_name},
  group_ids => {array, int4},
  disabled => #{type => boolean, default => false}}
```

## Model registry
Model functions usually accept a model reference, i.e. either a model value
(as defined above) or an atom being an entry in the model registry.

The registry stores all models in a ETS table named `pgc_model_registry`.

Models can be registered using `pgc:register_model/2`.

Example:
```erlang
UserModel = #{id => int4,
              name => #{type => text, column => full_name},
              group_ids => {array, int4}},
pgc_model_registry:register_model(user, UserModel).
```

After this call, model functions can use the `user` atom directly as model
reference. Note that since there is a single global registry, applications
should take care to prefix model names, as they usually do with modules or
registered process names.

A simple way to manage data models is for each OTP application to register the
model it uses during startup.

### Encoding and decoding
We define the notion of encoding as converting a business entity to a list of
typed values to be used in pgc query functions, and the notion of decoding as
converting database rows returned by pgc query functions to a business entity.

#### Encoding
Encoding is based on a list of entity keys: for each key, the associated value
is extracted from the business entity and converted to a pgc typed
value. If the value is missing from the entity, the `null` pgc value is used.

For example:
```erlang
pgc_model:encode(#{id => 42, name => <<"Bob">>}, user,
                 [id, name, group_ids])
```
will return `[{int4, 42}, {text, <<"Bob">>}, null]`.

The `pgc_model:encode/2` will behave similarly, using all keys defined in the
model.

#### Decoding
Decoding also uses a list of entity keys: for each key, a row value is
extracted and used to add the corresponding entity value. When a field is
null, the associated entity key is not added to the entity.

For example:
```erlang
pgc_model:decode([42, null, [1,2,3]], user, [id, name, group_ids])
```
will return `#{id => 42, group_ids => [1,2,3]}`.

The `pgc_model:decode/2` will behave similarly, using all keys defined in the
model.

### Columns
Since data models contain the name of the associated database column, they can
be used to build column name lists, e.g. to simplify SQL queries.

At the most basic level, `pgc_model:column/2` will return the name of de
column for a key in a model.

The `pgc_model:column_csv/2` will return multiple column names separated by a
comma. The `pgc_model:column_tuple/2` will return the same comma separated
list enclosed in parentheses.

For example:
```erlang
pgc_model:column_tuple(user, [id, name])
```
Will return an iodata value equivalent to `(id, user_name)`.

Like encoding and decoding functions, `pgc_model:column_csv/1` and
`pgc_model:column_tuple/1` use all keys of the model.

Note that these functions will correctly quote column names if necessary.

### Usage
Since pgc query functions accept queries of type `unicode:chardata()`, it is
simple to mix literal strings and function calls.

For example, a function loading a user could be implemented as follows:
```erlang
load_user(Id) ->
  Query = ["SELECT ", pgc_model:column_tuple(user),
           "  FROM users",
           "  WHERE ", pgc_model:column(id), " = $1"],
  {ok, _, [Row], _} = pgc:query(Query, [Id]),
  pgc_model:decode(Row).
```

# Caveats
## Character encoding
We currently only support databases whose character encoding is
UTF-8. Supporting other database character encodings would require to either:
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
precision. Infortunately, its behaviour is affected by the system environment
of the database (`LC_MONETARY`) making it unreliable. Additionally, Erlang
does not have any data type to handle this kind of value, therefore we do not
implement it.

## Dates
While PostgreSQL supports BC dates, Erlang calendar functions do not. We
should still be able to represent them, but we currently depend on Erlang
functions to decode and encode dates to number days.

An error is therefore signaled when a BC date is present in query results.

This restriction also applies to timestamps.
