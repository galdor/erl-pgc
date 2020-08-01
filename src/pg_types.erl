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

-module(pg_types).

-export([type_has_array_type/1, type_to_array_type/1, types_to_array_types/1,
         encode_values/2, encode_value/2,
         decode_values/3, decode_value/3,
         empty_type_db/0, add_to_type_db/2, type_db_from_list/1,
         find_type_by_oid/2, find_type_by_name/2,
         default_type_db/0]).

-export_type([type/0, type_db/0]).

-type type_name() :: atom() | {array, atom()}.

-type type() :: #{oid := pg:oid(),
                  name := type_name(),
                  array_oid => pg:oid(),
                  codec => {module(), list()}}.

-type type_db() :: #{by_oid := #{pg:oid() := type()},
                     by_name := #{type_name() := type()}}.

-spec type_has_array_type(type()) -> boolean().
type_has_array_type(Type) ->
  maps:is_key(array_oid, Type).

-spec type_to_array_type(type()) -> type().
type_to_array_type(#{name := Name, array_oid := ArrayOid}) ->
  #{oid => ArrayOid,
    name => {array, Name},
    codec => {pg_codec_array, [Name]}}.

-spec types_to_array_types([type()]) -> [type()].
types_to_array_types(Types) ->
  lists:filtermap(fun (Type) ->
                      case type_has_array_type(Type) of
                        true ->
                          {true, type_to_array_type(Type)};
                        false ->
                          false
                      end
                  end, Types).

-spec encode_values(list(), type_db()) -> {pg_proto:row(), [pg:oid()]}.
encode_values(Values, TypeDb) ->
  {Rows, Oids} = lists:foldl(fun (Value, {EncodedValues, Oids}) ->
                                 {EncodedValue, Oid} =
                                   encode_value(Value, TypeDb),
                                 {[EncodedValue | EncodedValues], [Oid | Oids]}
                             end, {[], []}, Values),
  {lists:reverse(Rows), lists:reverse(Oids)}.

-spec encode_value(term(), type_db()) -> {pg_proto:row_field(), pg:oid()}.
encode_value(null, _TypeDb) ->
  {null, 0};
encode_value(V, TypeDb) when is_boolean(V) ->
  encode_value({boolean, V}, TypeDb);
encode_value(V, TypeDb) when
    is_integer(V), V >= -32768, V =< 32767 ->
  encode_value({int2, V}, TypeDb);
encode_value(V, TypeDb) when
    is_integer(V), V >= -2147483648, V =< 2147483647 ->
  encode_value({int4, V}, TypeDb);
encode_value(V, TypeDb) when
    is_integer(V), V >= -9223372036854775808, V =< 9223372036854775807 ->
  encode_value({int8, V}, TypeDb);
encode_value(V, TypeDb) when is_binary(V) ->
  encode_value({text, V}, TypeDb);
encode_value(V, TypeDb) when is_float(V) ->
  encode_value({float8, V}, TypeDb);
encode_value({TypeName, null}, TypeDb) ->
  case find_type_by_name(TypeName, TypeDb) of
    {type, #{oid := Oid}} ->
      {null, Oid};
    unknown_type ->
      error({unknown_type, TypeName})
  end;
encode_value({TypeName, V}, TypeDb) ->
  case find_type_by_name(TypeName, TypeDb) of
    {type, Type = #{oid := Oid, codec := {Module, Args}}} ->
      {Module:encode(V, Type, TypeDb, Args), Oid};
    unknown_type ->
      error({unknown_type, TypeName})
  end;
encode_value(V, _TypeDb) ->
  error({unencodable_value, V}).

-spec decode_values(pg_proto:row(), [pg:oid()], type_db()) -> list().
decode_values(ValueData, Oids, TypeDb) ->
  lists:zipwith(fun (Data, Oid) ->
                    decode_value(Data, Oid, TypeDb)
                end, ValueData, Oids).

-spec decode_value(pg_proto:row_field(), TypeRef, type_db()) -> term() when
    TypeRef :: type_name() | pg:oid().
decode_value(null, _NameOrOid, _TypeDb) ->
  null;
decode_value(Data, NameOrOid, TypeDb) ->
  Fun = case NameOrOid of
          Oid when is_integer(Oid) ->
            fun find_type_by_oid/2;
          _ ->
            fun find_type_by_name/2
        end,
  case Fun(NameOrOid, TypeDb) of
    {type, Type = #{codec := {Module, Args}}} ->
      Module:decode(Data, Type, TypeDb, Args);
    unknown_type ->
      error({unknown_type, NameOrOid})
  end.

-spec empty_type_db() -> type_db().
empty_type_db() ->
  #{by_oid => #{}, by_name => #{}}.

-spec add_to_type_db(type(), type_db()) -> type_db().
add_to_type_db(Type = #{oid := Oid, name := Name},
               #{by_oid := ByOid, by_name := ByName}) ->
  ByOid2 = maps:put(Oid, Type, ByOid),
  ByName2 = maps:put(Name, Type, ByName),
  #{by_oid => ByOid2, by_name => ByName2}.

-spec type_db_from_list([type()]) -> type_db().
type_db_from_list(Types) ->
  lists:foldl(fun add_to_type_db/2, empty_type_db(), Types).

-spec find_type_by_oid(pg:oid(), type_db()) -> {type, type()} | unknown_type.
find_type_by_oid(Oid, #{by_oid := ByOid}) ->
  case maps:find(Oid, ByOid) of
    {ok, Type} ->
      {type, Type};
    error ->
      unknown_type
  end.

-spec find_type_by_name(type_name(), type_db()) -> {type, type()} | unknown_type.
find_type_by_name(Name, #{by_name := ByName}) ->
  case maps:find(Name, ByName) of
    {ok, Type} ->
      {type, Type};
    error ->
      unknown_type
  end.

-spec default_type_db() -> type_db().
default_type_db() ->
  Types = [#{oid => 16,
             name => boolean,
             array_oid => 1000,
             codec => {pg_codec_boolean, []}},
           #{oid => 17,
             name => bytea,
             array_oid => 1001,
             codec => {pg_codec_bytea, []}},
           #{oid => 18,
             name => char,
             array_oid => 1002,
             codec => {pg_codec_char, []}},
           #{oid => 19,
             name => name,
             array_oid => 1003,
             codec => {pg_codec_name, []}},
           #{oid => 20,
             name => int8,
             array_oid => 1016,
             codec => {pg_codec_integer, [8]}},
           #{oid => 21,
             name => int2,
             array_oid => 1005,
             codec => {pg_codec_integer, [2]}},
           #{oid => 23,
             name => int4,
             array_oid => 1007,
             codec => {pg_codec_integer, [4]}},
           #{oid => 25,
             name => text,
             array_oid => 1009,
             codec => {pg_codec_text, []}},
           #{oid => 26,
             name => oid,
             array_oid => 1028,
             codec => {pg_codec_oid, []}},
           #{oid => 114,
             name => json,
             array_oid => 199,
             codec => {pg_codec_bytea, []}},
           #{oid => 142,
             name => xml,
             array_oid => 143,
             codec => {pg_codec_bytea, []}},
           #{oid => 600,
             name => point,
             array_oid => 1017,
             codec => {pg_codec_point, []}},
           #{oid => 601,
             name => lseg,
             array_oid => 1018,
             codec => {pg_codec_lseg, []}},
           #{oid => 602,
             name => path,
             array_oid => 1019,
             codec => {pg_codec_path, []}},
           #{oid => 603,
             name => box,
             array_oid => 1020,
             codec => {pg_codec_box, []}},
           #{oid => 604,
             name => polygon,
             array_oid => 1027,
             codec => {pg_codec_polygon, []}},
           #{oid => 628,
             name => line,
             array_oid => 629,
             codec => {pg_codec_line, []}},
           #{oid => 650,
             name => cidr,
             array_oid => 651,
             codec => {pg_codec_inet, []}},
           #{oid => 700,
             name => float4,
             array_oid => 1021,
             codec => {pg_codec_float, [4]}},
           #{oid => 701,
             name => float8,
             array_oid => 1022,
             codec => {pg_codec_float, [8]}},
           #{oid => 718,
             name => circle,
             array_oid => 719,
             codec => {pg_codec_circle, []}},
           #{oid => 774,
             name => macaddr8,
             array_oid => 775,
             codec => {pg_codec_macaddr, [8]}},
           #{oid => 829,
             name => macaddr,
             array_oid => 1040,
             codec => {pg_codec_macaddr, [6]}},
           #{oid => 869,
             name => inet,
             array_oid => 1041,
             codec => {pg_codec_inet, []}},
           #{oid => 1042,
             name => bpchar,
             array_oid => 1014,
             codec => {pg_codec_text, []}},
           #{oid => 1043,
             name => varchar,
             array_oid => 1015,
             codec => {pg_codec_text, []}},
           #{oid => 1082,
             name => date,
             array_oid => 1182,
             codec => {pg_codec_date, []}},
           #{oid => 1083,
             name => time,
             array_oid => 1183,
             codec => {pg_codec_time, []}},
           #{oid => 1114,
             name => timestamp,
             array_oid => 1115,
             codec => {pg_codec_timestamp, []}},
           #{oid => 1184,
             name => timestamptz,
             array_oid => 1185,
             codec => {pg_codec_timestamp, []}},
           #{oid => 1186,
             name => interval,
             array_oid => 1187,
             codec => {pg_codec_interval, []}},
           #{oid => 1266,
             name => timetz,
             array_oid => 1270,
             codec => {pg_codec_timetz, []}},
           #{oid => 1560,
             name => bit,
             array_oid => 1561,
             codec => {pg_codec_bit, []}},
           #{oid => 1562,
             name => varbit,
             array_oid => 1563,
             codec => {pg_codec_bit, []}},
           #{oid => 2950,
             name => uuid,
             array_oid => 2951,
             codec => {pg_codec_uuid, []}},
           #{oid => 3802,
             name => jsonb,
             array_oid => 3807,
             codec => {pg_codec_jsonb, []}}],
  Types2 = Types ++ types_to_array_types(Types),
  type_db_from_list(Types2).
