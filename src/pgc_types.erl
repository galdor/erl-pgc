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

-module(pgc_types).

-export([empty_type_set/0,
         encode_values/2, encode_value/2,
         decode_values/3, decode_value/3,
         find_type_by_name/2, find_type_by_oid/2]).

-export_type([type/0, type_set/0, type_name/0, codec/0]).

-type type() :: {type_name(), pgc:oid(), codec()}.
-type type_set() :: [type()].

-type type_name() :: atomic_type_name()
                   | {array, atomic_type_name()}.
-type atomic_type_name() :: atom()
                          | {enum, atom()}
                          | {domain, atom()}.

-type codec() :: {module(), list()}.

-spec empty_type_set() -> type_set().
empty_type_set() ->
  [].

-spec encode_values(list(), type_set()) -> {pgc_proto:row(), [pgc:oid()]}.
encode_values(Values, Types) ->
  F = fun (Value, {EncodedValues, Oids}) ->
          {EncodedValue, Oid} = encode_value(Value, Types),
          {[EncodedValue | EncodedValues], [Oid | Oids]}
      end,
  {Rows, Oids} = lists:foldl(F, {[], []}, Values),
  {lists:reverse(Rows), lists:reverse(Oids)}.

-spec encode_value(term(), type_set()) -> {pgc_proto:row_field(), pgc:oid()}.
encode_value(null, _Types) ->
  {null, 0};
encode_value(void, Types) ->
  encode_value({void, void}, Types);
encode_value(V, Types) when is_boolean(V) ->
  encode_value({boolean, V}, Types);
encode_value(V, Types) when
    is_integer(V), V >= -32768, V =< 32767 ->
  encode_value({int2, V}, Types);
encode_value(V, Types) when
    is_integer(V), V >= -2147483648, V =< 2147483647 ->
  encode_value({int4, V}, Types);
encode_value(V, Types) when
    is_integer(V), V >= -9223372036854775808, V =< 9223372036854775807 ->
  encode_value({int8, V}, Types);
encode_value(V, Types) when is_binary(V) ->
  encode_value({text, V}, Types);
encode_value(V, Types) when is_float(V) ->
  encode_value({float8, V}, Types);
encode_value({TypeName, null}, Types) ->
  case find_type_by_name(TypeName, Types) of
    {_, Oid, _} ->
      {null, Oid};
    unknown_type ->
      error({unknown_type, TypeName})
  end;
encode_value({TypeName, V}, Types) ->
  case find_type_by_name(TypeName, Types) of
    {_, Oid, {Module, Args}} = Type ->
      {Module:encode(V, Type, Types, Args), Oid};
    unknown_type ->
      error({unknown_type, TypeName})
  end;
encode_value(V, _Types) ->
  error({unencodable_value, V}).

-spec decode_values(pgc_proto:row(), [pgc:oid()], type_set()) ->
        {ok, list()} | {error, pgc:error_reason()}.
decode_values(Values, Oids, Types) ->
  try
    DecodedValues = lists:zipwith(fun (Value, Oid) ->
                                      decode_value(Value, Oid, Types)
                                  end, Values, Oids),
    {ok, DecodedValues}
  catch
    throw:{error, Reason} ->
      {error, Reason}
  end.

-spec decode_value(pgc_proto:row_field(), TypeRef, type_set()) -> term() when
    TypeRef :: type_name() | pgc:oid().
decode_value(null, _NameOrOid, _Types) ->
  null;
decode_value(Data, NameOrOid, Types) ->
  Fun = case NameOrOid of
          Oid when is_integer(Oid) ->
            fun find_type_by_oid/2;
          _ ->
            fun find_type_by_name/2
        end,
  case Fun(NameOrOid, Types) of
    {_, _, {Module, Args}} = Type ->
      Module:decode(Data, Type, Types, Args);
    unknown_type ->
      throw({error, {unknown_type, NameOrOid}})
  end.

-spec find_type_by_name(type_name(), type_set()) -> type() | unknown_type.
find_type_by_name(Name, Types) ->
  Pred = fun ({Name2, _, _}) -> Name2 == Name end,
  case lists:search(Pred, Types) of
    {value, Type} ->
      Type;
    false ->
      case name_to_oid(Name) of
        unknown_name ->
          unknown_type;
        Oid ->
          find_type_by_oid(Oid, Types)
      end
  end.

-spec find_type_by_oid(pgc:oid(), type_set()) -> type() | unknown_type.
find_type_by_oid(Oid, Types) ->
  Pred = fun ({_, Oid2, _}) -> Oid2 == Oid end,
  case lists:search(Pred, Types) of
    {value, Type} ->
      Type;
    false ->
      case oid_to_type(Oid) of
        unknown_oid ->
          unknown_type;
        {Name, Codec} ->
          {Name, Oid, Codec}
      end
  end.

-spec oid_to_type(pgc:oid()) -> {type_name(), codec()} | unknown_oid.
oid_to_type(16) -> {boolean, {pgc_codec_boolean, []}};
oid_to_type(17) -> {bytea, {pgc_codec_bytea, []}};
oid_to_type(18) -> {char, {pgc_codec_char, []}};
oid_to_type(19) -> {name, {pgc_codec_name, []}};
oid_to_type(20) -> {int8, {pgc_codec_integer, [8]}};
oid_to_type(21) -> {int2, {pgc_codec_integer, [2]}};
oid_to_type(23) -> {int4, {pgc_codec_integer, [4]}};
oid_to_type(25) -> {text, {pgc_codec_text, []}};
oid_to_type(26) -> {oid, {pgc_codec_oid, []}};
oid_to_type(114) -> {json, {pgc_codec_bytea, []}};
oid_to_type(142) -> {xml, {pgc_codec_bytea, []}};
oid_to_type(143) -> {{array, xml}, {pgc_codec_array, [xml]}};
oid_to_type(199) -> {{array, json}, {pgc_codec_array, [json]}};
oid_to_type(600) -> {point, {pgc_codec_point, []}};
oid_to_type(601) -> {lseg, {pgc_codec_lseg, []}};
oid_to_type(602) -> {path, {pgc_codec_path, []}};
oid_to_type(603) -> {box, {pgc_codec_box, []}};
oid_to_type(604) -> {polygon, {pgc_codec_polygon, []}};
oid_to_type(628) -> {line, {pgc_codec_line, []}};
oid_to_type(629) -> {{array, line}, {pgc_codec_array, [line]}};
oid_to_type(650) -> {cidr, {pgc_codec_inet, []}};
oid_to_type(651) -> {{array, cidr}, {pgc_codec_array, [cidr]}};
oid_to_type(700) -> {float4, {pgc_codec_float, [4]}};
oid_to_type(701) -> {float8, {pgc_codec_float, [8]}};
oid_to_type(718) -> {circle, {pgc_codec_circle, []}};
oid_to_type(719) -> {{array, circle}, {pgc_codec_array, [circle]}};
oid_to_type(774) -> {macaddr8, {pgc_codec_macaddr, [8]}};
oid_to_type(775) -> {{array, macaddr8}, {pgc_codec_array, [macaddr8]}};
oid_to_type(829) -> {macaddr, {pgc_codec_macaddr, [6]}};
oid_to_type(869) -> {inet, {pgc_codec_inet, []}};
oid_to_type(1000) -> {{array, boolean}, {pgc_codec_array, [boolean]}};
oid_to_type(1001) -> {{array, bytea}, {pgc_codec_array, [bytea]}};
oid_to_type(1002) -> {{array, char}, {pgc_codec_array, [char]}};
oid_to_type(1003) -> {{array, name}, {pgc_codec_array, [name]}};
oid_to_type(1005) -> {{array, int2}, {pgc_codec_array, [int2]}};
oid_to_type(1007) -> {{array, int4}, {pgc_codec_array, [int4]}};
oid_to_type(1009) -> {{array, text}, {pgc_codec_array, [text]}};
oid_to_type(1014) -> {{array, bpchar}, {pgc_codec_array, [bpchar]}};
oid_to_type(1015) -> {{array, varchar}, {pgc_codec_array, [varchar]}};
oid_to_type(1016) -> {{array, int8}, {pgc_codec_array, [int8]}};
oid_to_type(1017) -> {{array, point}, {pgc_codec_array, [point]}};
oid_to_type(1018) -> {{array, lseg}, {pgc_codec_array, [lseg]}};
oid_to_type(1019) -> {{array, path}, {pgc_codec_array, [path]}};
oid_to_type(1020) -> {{array, box}, {pgc_codec_array, [box]}};
oid_to_type(1021) -> {{array, float4}, {pgc_codec_array, [float4]}};
oid_to_type(1022) -> {{array, float8}, {pgc_codec_array, [float8]}};
oid_to_type(1027) -> {{array, polygon}, {pgc_codec_array, [polygon]}};
oid_to_type(1028) -> {{array, oid}, {pgc_codec_array, [oid]}};
oid_to_type(1040) -> {{array, macaddr}, {pgc_codec_array, [macaddr]}};
oid_to_type(1041) -> {{array, inet}, {pgc_codec_array, [inet]}};
oid_to_type(1042) -> {bpchar, {pgc_codec_text, []}};
oid_to_type(1043) -> {varchar, {pgc_codec_text, []}};
oid_to_type(1082) -> {date, {pgc_codec_date, []}};
oid_to_type(1083) -> {time, {pgc_codec_time, []}};
oid_to_type(1114) -> {timestamp, {pgc_codec_timestamp, []}};
oid_to_type(1115) -> {{array, timestamp}, {pgc_codec_array, [timestamp]}};
oid_to_type(1182) -> {{array, date}, {pgc_codec_array, [date]}};
oid_to_type(1183) -> {{array, time}, {pgc_codec_array, [time]}};
oid_to_type(1184) -> {timestamptz, {pgc_codec_timestamp, []}};
oid_to_type(1185) -> {{array, timestamptz}, {pgc_codec_array, [timestamptz]}};
oid_to_type(1186) -> {interval, {pgc_codec_interval, []}};
oid_to_type(1187) -> {{array, interval}, {pgc_codec_array, [interval]}};
oid_to_type(1266) -> {timetz, {pgc_codec_timetz, []}};
oid_to_type(1270) -> {{array, timetz}, {pgc_codec_array, [timetz]}};
oid_to_type(1560) -> {bit, {pgc_codec_bit, []}};
oid_to_type(1561) -> {{array, bit}, {pgc_codec_array, [bit]}};
oid_to_type(1562) -> {varbit, {pgc_codec_bit, []}};
oid_to_type(1563) -> {{array, varbit}, {pgc_codec_array, [varbit]}};
oid_to_type(2278) -> {void, {pgc_codec_void, []}};
oid_to_type(2950) -> {uuid, {pgc_codec_uuid, []}};
oid_to_type(2951) -> {{array, uuid}, {pgc_codec_array, [uuid]}};
oid_to_type(3734) -> {regconfig, {pgc_codec_oid, []}};
oid_to_type(3735) -> {{array, regconfig}, {pgc_codec_oid, []}};
oid_to_type(3802) -> {jsonb, {pgc_codec_jsonb, []}};
oid_to_type(3807) -> {{array, jsonb}, {pgc_codec_array, [jsonb]}};
oid_to_type(_) -> unknown_oid.

-spec name_to_oid(type_name()) -> pgc:oid() | unknown_name.
name_to_oid(bit) -> 1560;
name_to_oid(boolean) -> 16;
name_to_oid(box) -> 603;
name_to_oid(bpchar) -> 1042;
name_to_oid(bytea) -> 17;
name_to_oid(char) -> 18;
name_to_oid(cidr) -> 650;
name_to_oid(circle) -> 718;
name_to_oid(date) -> 1082;
name_to_oid(float4) -> 700;
name_to_oid(float8) -> 701;
name_to_oid(inet) -> 869;
name_to_oid(int2) -> 21;
name_to_oid(int4) -> 23;
name_to_oid(int8) -> 20;
name_to_oid(interval) -> 1186;
name_to_oid(json) -> 114;
name_to_oid(jsonb) -> 3802;
name_to_oid(line) -> 628;
name_to_oid(lseg) -> 601;
name_to_oid(macaddr) -> 829;
name_to_oid(macaddr8) -> 774;
name_to_oid(name) -> 19;
name_to_oid(oid) -> 26;
name_to_oid(path) -> 602;
name_to_oid(point) -> 600;
name_to_oid(polygon) -> 604;
name_to_oid(regconfig) -> 3734;
name_to_oid(text) -> 25;
name_to_oid(time) -> 1083;
name_to_oid(timestamp) -> 1114;
name_to_oid(timestamptz) -> 1184;
name_to_oid(timetz) -> 1266;
name_to_oid(uuid) -> 2950;
name_to_oid(varbit) -> 1562;
name_to_oid(varchar) -> 1043;
name_to_oid(void) -> 2278;
name_to_oid(xml) -> 142;
name_to_oid({array, bit}) -> 1561;
name_to_oid({array, boolean}) -> 1000;
name_to_oid({array, box}) -> 1020;
name_to_oid({array, bpchar}) -> 1014;
name_to_oid({array, bytea}) -> 1001;
name_to_oid({array, char}) -> 1002;
name_to_oid({array, cidr}) -> 651;
name_to_oid({array, circle}) -> 719;
name_to_oid({array, date}) -> 1182;
name_to_oid({array, float4}) -> 1021;
name_to_oid({array, float8}) -> 1022;
name_to_oid({array, inet}) -> 1041;
name_to_oid({array, int2}) -> 1005;
name_to_oid({array, int4}) -> 1007;
name_to_oid({array, int8}) -> 1016;
name_to_oid({array, interval}) -> 1187;
name_to_oid({array, jsonb}) -> 3807;
name_to_oid({array, json}) -> 199;
name_to_oid({array, line}) -> 629;
name_to_oid({array, lseg}) -> 1018;
name_to_oid({array, macaddr8}) -> 775;
name_to_oid({array, macaddr}) -> 1040;
name_to_oid({array, name}) -> 1003;
name_to_oid({array, oid}) -> 1028;
name_to_oid({array, path}) -> 1019;
name_to_oid({array, point}) -> 1017;
name_to_oid({array, polygon}) -> 1027;
name_to_oid({array, regconfig}) -> 3735;
name_to_oid({array, text}) -> 1009;
name_to_oid({array, timestamptz}) -> 1185;
name_to_oid({array, timestamp}) -> 1115;
name_to_oid({array, timetz}) -> 1270;
name_to_oid({array, time}) -> 1183;
name_to_oid({array, uuid}) -> 2951;
name_to_oid({array, varbit}) -> 1563;
name_to_oid({array, varchar}) -> 1015;
name_to_oid({array, xml}) -> 143;
name_to_oid(_) -> unknown_name.
