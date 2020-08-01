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

-module(pg_proto).

-export([encode_msg/1, encode_msg/2,
         encode_bind_msg/3, encode_describe_msg/2, encode_execute_msg/2,
         encode_parse_msg/3, encode_password_msg/1, encode_query_msg/1,
         encode_startup_msg/3, encode_sync_msg/0, encode_terminate_msg/0,
         decode_msg/2,
         query_response/0, add_query_response_row/2,
         finalize_query_response/1, query_response_to_query_result/2]).

-export_type([msg_type/0, msg/0, error_and_notice_fields/0, severity/0,
              query_response/0, transaction_status/0]).

-type msg_type() :: byte().

-type msg() :: authentication_ok
             | authentication_cleartext_password
             | authentication_gss
             | authentication_kerberos_v5
             | {authentication_md5_password, Salt :: binary()}
             | authentication_scm_credential
             | authentication_sspi
             | {authentication_sasl, Mechanisms :: [binary()]}
             | {backend_key_data, Pid :: integer(), Key :: integer()}
             | bind_complete
             | {command_complete, pg_proto:command_tag()}
             | {data_row, pg_query:row()}
             | empty_query_response
             | {error_response, error_and_notice_fields()}
             | no_data
             | {notice_response, error_and_notice_fields()}
             | {parameter_status, Name :: binary(), Value :: binary()}
             | parse_complete
             | {ready_for_query, transaction_status()}
             | {row_description, [pg_queries:column()]}.

-type error_and_notice_fields() :: #{l10n_severity := severity(),
                                     severity := severity(),
                                     code := binary(),
                                     message := binary(),
                                     detail => binary(),
                                     position => pos_integer(),
                                     internal_position => pos_integer(),
                                     internal_query => binary(),
                                     where => binary(),
                                     schema => binary(),
                                     table => binary(),
                                     column => binary(),
                                     data_type => binary(),
                                     constraint => binary(),
                                     file => binary(),
                                     line => pos_integer(),
                                     routine => binary(),
                                     byte() => binary()}.

-type severity() :: log | debug | info | notice | warning
                  | error | fatal | panic
                  | binary().

-type column() :: #{name => binary(),
                    table_oid => pg:oid(),
                    column_attribute => integer(),
                    type_oid => pg:oid(),
                    type_size => integer(),
                    type_modifier => integer(),
                    format => column_format()}.

-type column_format() :: binary | text.

-type row_field() ::  null | binary().
-type row() :: [row_field()].

-type query_response() :: #{transaction_status => transaction_status(),
                            columns => [column()],
                            rows => [row()],
                            command_tag => pg_proto:command_tag(),
                            error => error_and_notice_fields()}.

-type transaction_status() :: idle | in_transaction | in_failed_transaction.

-type command_tag() :: {insert, non_neg_integer()}
                     | {delete, non_neg_integer()}
                     | {update, non_neg_integer()}
                     | {select, non_neg_integer()}
                     | {move, non_neg_integer()}
                     | {fetch, non_neg_integer()}
                     | {copy, non_neg_integer()}
                     | set.

-spec encode_msg(Payload :: iodata()) -> iodata().
encode_msg(Payload) ->
  Length = 4 + iolist_size(Payload),
  [<<Length:32>>, Payload].

-spec encode_msg(msg_type(), Payload :: iodata()) -> iodata().
encode_msg(Type, Payload) ->
  Length = 4 + iolist_size(Payload),
  [<<Type:8, Length:32>>, Payload].

-spec encode_string(unicode:chardata()) -> iodata().
encode_string(S) ->
  [unicode:characters_to_binary(S), 0].

-spec encode_bind_msg(Portal :: binary(),
                      PreparedStmt :: binary(),
                      Params :: row()) -> iodata().
encode_bind_msg(Portal, PreparedStmt, Params) ->
  EncodeParam = fun (null) ->
                    <<-1:32/signed-integer>>;
                    (Param) ->
                    Size = iolist_size(Param), [<<Size:32>>, Param]
                end,
  NbParams = length(Params),
  encode_msg($B, [encode_string(Portal),
                  encode_string(PreparedStmt),
                  <<1:16, 1:16>>, % use binary for all parameters
                  <<NbParams:16>>,
                  [EncodeParam(P) || P <- Params],
                  <<1:16, 1:16>>]). % use binary for all results

-spec encode_describe_msg(Type :: prepared_statement | portal,
                          Name :: binary()) -> iodata().
encode_describe_msg(prepared_statement, Name) ->
  encode_msg($D, [$S, encode_string(Name)]);
encode_describe_msg(portal, Name) ->
  encode_msg($D, [$P, encode_string(Name)]).

-spec encode_execute_msg(Portal :: binary(),
                         MaxNbRows :: non_neg_integer()) -> iodata().
encode_execute_msg(Portal, MaxNbRows) ->
  encode_msg($E, [encode_string(Portal), <<MaxNbRows:32>>]).

-spec encode_parse_msg(PreparedStmt :: binary(), Query :: iodata(),
                       TypeOids :: [pg:oid()]) -> iodata().
encode_parse_msg(PreparedStmt, Query, TypeOids) ->
  NbTypeOids = length(TypeOids),
  encode_msg($P, [encode_string(PreparedStmt),
                  encode_string(Query),
                  <<NbTypeOids:16>>,
                  [<<Oid:32>> || Oid <- TypeOids]]).

-spec encode_password_msg(Password :: unicode:chardata()) -> iodata().
encode_password_msg(Password) ->
  encode_msg($p, encode_string(Password)).

-spec encode_query_msg(Query :: unicode:chardata()) -> iodata().
encode_query_msg(Query) ->
  encode_msg($Q, encode_string(Query)).

-spec encode_startup_msg(MajorVersion :: Version,
                         MinorVersion :: Version,
                         Parameters) -> iodata() when
    Version :: non_neg_integer(),
    Parameters :: #{atom() := unicode:chardata()}.
encode_startup_msg(MajorVersion, MinorVersion, Parameters) ->
  VersionData = <<MajorVersion:16, MinorVersion:16>>,
  ParametersData = maps:fold(fun (Name, Value, Acc) ->
                                 Data = [encode_string(atom_to_binary(Name)),
                                         encode_string(Value)],
                                 [Data | Acc]
                             end, [], Parameters),
  encode_msg([VersionData, ParametersData, 0]).

-spec encode_sync_msg() -> iodata().
encode_sync_msg() ->
  encode_msg($S, []).

-spec encode_terminate_msg() -> iodata().
encode_terminate_msg() ->
  encode_msg($X, []).

-spec decode_msg(msg_type(), Payload :: binary()) -> msg().
decode_msg($R, Data) -> decode_authentication_msg(Data);
decode_msg($K, Data) -> decode_backend_key_data_msg(Data);
decode_msg($2, Data) -> decode_bind_complete_msg(Data);
decode_msg($C, Data) -> decode_command_complete_msg(Data);
decode_msg($D, Data) -> decode_data_row_msg(Data);
decode_msg($I, Data) -> decode_empty_query_response_msg(Data);
decode_msg($E, Data) -> decode_error_response_msg(Data);
decode_msg($n, Data) -> decode_no_data_msg(Data);
decode_msg($N, Data) -> decode_notice_response_msg(Data);
decode_msg($S, Data) -> decode_parameter_status_msg(Data);
decode_msg($1, Data) -> decode_parse_complete_msg(Data);
decode_msg($Z, Data) -> decode_ready_for_query_msg(Data);
decode_msg($T, Data) -> decode_row_description_msg(Data);
decode_msg(Type, _) ->
  error({unknown_msg_type, Type}).

-spec decode_authentication_msg(binary()) -> msg().
decode_authentication_msg(<<0:32>>) ->
  authentication_ok;
decode_authentication_msg(<<2:32>>) ->
  authentication_kerberos_v5;
decode_authentication_msg(<<3:32>>) ->
  authentication_cleartext_password;
decode_authentication_msg(<<5:32, Salt:4/binary>>) ->
  {authentication_md5_password, Salt};
decode_authentication_msg(<<6:32>>) ->
  authentication_scm_credential;
decode_authentication_msg(<<7:32>>) ->
  authentication_gss;
decode_authentication_msg(<<9:32>>) ->
  authentication_sspi;
decode_authentication_msg(<<10:32, Data/binary>>) ->
  Mechanisms = decode_strings(Data),
  {authentication_sasl, Mechanisms};
decode_authentication_msg(Data) ->
  error({invalid_data, Data}).

-spec decode_backend_key_data_msg(binary()) -> msg().
decode_backend_key_data_msg(<<Pid:32, Key:32>>) ->
  {backend_key_data, Pid, Key};
decode_backend_key_data_msg(Data) ->
  error({invalid_data, Data}).

-spec decode_bind_complete_msg(binary()) -> msg().
decode_bind_complete_msg(_Data) ->
  bind_complete.

-spec decode_command_complete_msg(binary()) -> msg().
decode_command_complete_msg(Data) ->
  {Value, _} = decode_string(Data),
  Tag = decode_command_tag(Value),
  {command_complete, Tag}.

-spec decode_data_row_msg(binary()) -> msg().
decode_data_row_msg(<<NbColumns:16, Data/binary>>) ->
  Columns = decode_data_row_msg_fields(NbColumns, Data, []),
  {data_row, Columns};
decode_data_row_msg(Data) ->
  error({invalid_data, Data}).

-spec decode_data_row_msg_fields(N :: integer(), Data :: binary(),
                                 Acc :: row()) -> row().
decode_data_row_msg_fields(0, _, Acc) ->
  lists:reverse(Acc);
decode_data_row_msg_fields(_, <<Length:32/signed-integer, _/binary>>, _) when
    Length < -1 ->
  error({invalid_field_length, Length});
decode_data_row_msg_fields(N, <<-1:32/signed-integer, Rest/binary>>, Acc) ->
  decode_data_row_msg_fields(N-1, Rest, [null | Acc]);
decode_data_row_msg_fields(N, <<Length:32/signed-integer,
                                Field:Length/binary,
                                Rest/binary>>, Acc) ->
  decode_data_row_msg_fields(N-1, Rest, [Field | Acc]);
decode_data_row_msg_fields(_N, Data, _Acc) ->
  {error, {invalid_data, Data}}.

-spec decode_empty_query_response_msg(binary()) -> msg().
decode_empty_query_response_msg(_) ->
  empty_query_response.

-spec decode_error_response_msg(binary()) -> msg().
decode_error_response_msg(Data) ->
  Fields = decode_error_and_notice_fields(Data),
  {error_response, Fields}.

-spec decode_no_data_msg(binary()) -> msg().
decode_no_data_msg(_Data) ->
  no_data.

-spec decode_notice_response_msg(binary()) -> msg().
decode_notice_response_msg(Data) ->
  Fields = decode_error_and_notice_fields(Data),
  {notice_response, Fields}.

-spec decode_parameter_status_msg(binary()) -> msg().
decode_parameter_status_msg(Data) ->
  {Name, Rest} = decode_string(Data),
  {Value, _} = decode_string(Rest),
  {parameter_status, Name, Value}.

-spec decode_parse_complete_msg(binary()) -> msg().
decode_parse_complete_msg(_Data) ->
  parse_complete.

-spec decode_ready_for_query_msg(binary()) -> msg().
decode_ready_for_query_msg(<<$I:8>>) ->
  {ready_for_query, idle};
decode_ready_for_query_msg(<<$T:8>>) ->
  {ready_for_query, in_transaction};
decode_ready_for_query_msg(<<$E:8>>) ->
  {ready_for_query, in_failed_transaction};
decode_ready_for_query_msg(Data) ->
  error({invalid_data, Data}).

-spec decode_row_description_msg(binary()) -> msg().
decode_row_description_msg(<<NbColumns:16, Data/binary>>) ->
  Columns = decode_row_description_msg_fields(NbColumns, Data, []),
  {row_description, Columns};
decode_row_description_msg(Data) ->
  error({invalid_data, Data}).

-spec decode_row_description_msg_fields(N :: integer(), Data :: binary(),
                                        Acc :: [column()]) -> [column()].
decode_row_description_msg_fields(0, _, Acc) ->
  lists:reverse(Acc);
decode_row_description_msg_fields(N, Data, Acc) ->
  case decode_string(Data) of
    {Name, <<TableOid:32, ColumnAttr:16, TypeOid:32,
             TypeSize:16/signed-integer, TypeMod:32/signed-integer,
             FormatInt:16, Rest/binary>>} ->
      Format = case FormatInt of
                 0 -> text;
                 1 -> binary;
                 _ -> error({invalid_column_format, FormatInt})
               end,
      Column = #{name => Name,
                 table_oid => TableOid,
                 column_attribute => ColumnAttr,
                 type_oid => TypeOid,
                 type_size => TypeSize,
                 type_modifier => TypeMod,
                 format => Format},
      decode_row_description_msg_fields(N-1, Rest, [Column | Acc]);
    _ ->
      error({invalid_data, Data})
  end.

-spec decode_error_and_notice_fields(binary()) -> error_and_notice_fields().
decode_error_and_notice_fields(Data) ->
  decode_error_and_notice_fields(Data, #{}).

decode_error_and_notice_fields(<<>>, _) ->
  error(truncated_data);
decode_error_and_notice_fields(<<0>>, Acc) ->
  Acc;
decode_error_and_notice_fields(Data, Acc) ->
  {TypeByte, ValueBin, Rest} = decode_error_and_notice_field(Data),
  {Type, Value} = decode_error_and_notice_field_data(TypeByte, ValueBin),
  Acc2 = maps:put(Type, Value, Acc),
  decode_error_and_notice_fields(Rest, Acc2).

-spec decode_error_and_notice_field_data(TypeByte :: byte(), binary()) ->
        {Type :: atom() | byte(), Value :: term()}.
decode_error_and_notice_field_data($S, Value) ->
  {l10n_severity, decode_severity(Value)};
decode_error_and_notice_field_data($V, Value) ->
  {severity, decode_severity(Value)};
decode_error_and_notice_field_data($C, Value) ->
  {code, pg_error_codes:decode(Value)};
decode_error_and_notice_field_data($M, Value) ->
  {message, Value};
decode_error_and_notice_field_data($D, Value) ->
  {detail, Value};
decode_error_and_notice_field_data($H, Value) ->
  {hint, Value};
decode_error_and_notice_field_data($P, Value) ->
  {position, binary_to_integer(Value)};
decode_error_and_notice_field_data($p, Value) ->
  {internal_position, binary_to_integer(Value)};
decode_error_and_notice_field_data($q, Value) ->
  {internal_query, Value};
decode_error_and_notice_field_data($W, Value) ->
  {where, Value};
decode_error_and_notice_field_data($s, Value) ->
  {schema, Value};
decode_error_and_notice_field_data($t, Value) ->
  {table, Value};
decode_error_and_notice_field_data($c, Value) ->
  {column, Value};
decode_error_and_notice_field_data($d, Value) ->
  {data_type, Value};
decode_error_and_notice_field_data($n, Value) ->
  {constraint, Value};
decode_error_and_notice_field_data($F, Value) ->
  {file, Value};
decode_error_and_notice_field_data($L, Value) ->
  {internal_line, binary_to_integer(Value)};
decode_error_and_notice_field_data($R, Value) ->
  {routine, Value};
decode_error_and_notice_field_data(TypeByte, Value) ->
  {TypeByte, Value}.

-spec decode_error_and_notice_field(binary()) ->
        {byte(), unicode:chardata(), Rest :: binary()}.
decode_error_and_notice_field(<<Type:8, Data/binary>>) ->
  {Value, Rest} = decode_string(Data),
  {Type, Value, Rest};
decode_error_and_notice_field(Data) ->
  error({invalid_data, Data}).

-spec decode_severity(binary()) -> severity().
decode_severity(<<"LOG">>) ->
  log;
decode_severity(<<"DEBUG">>) ->
  debug;
decode_severity(<<"INFO">>) ->
  info;
decode_severity(<<"NOTICE">>) ->
  notice;
decode_severity(<<"WARNING">>) ->
  warning;
decode_severity(<<"ERROR">>) ->
  error;
decode_severity(<<"FATAL">>) ->
  fatal;
decode_severity(<<"PANIC">>) ->
  panic;
decode_severity(Bin) ->
  Bin.

-spec decode_command_tag(binary()) -> command_tag().
decode_command_tag(<<"INSERT ", Data/binary>>) ->
  case binary:split(Data, <<" ">>) of
    [_, Data2] ->
      decode_command_tag_value(insert, Data2);
    _ ->
      error({invalid_data, Data})
  end;
decode_command_tag(<<"DELETE ", Data/binary>>) ->
  decode_command_tag_value(delete, Data);
decode_command_tag(<<"UPDATE ", Data/binary>>) ->
  decode_command_tag_value(update, Data);
decode_command_tag(<<"SELECT ", Data/binary>>) ->
  decode_command_tag_value(select, Data);
decode_command_tag(<<"MOVE ", Data/binary>>) ->
  decode_command_tag_value(move, Data);
decode_command_tag(<<"FETCH ", Data/binary>>) ->
  decode_command_tag_value(fetch, Data);
decode_command_tag(<<"COPY ", Data/binary>>) ->
  decode_command_tag_value(copy, Data);
decode_command_tag(<<"SET">>) ->
  set;
decode_command_tag(Data) ->
  error({invalid_data, Data}).

-spec decode_command_tag_value(Type :: atom(), Data :: binary()) ->
        command_tag().
decode_command_tag_value(Type, Value) ->
  {Type, binary_to_integer(Value)}.

-spec decode_string(binary()) -> {String :: binary(), Rest :: binary()}.
decode_string(Data) ->
  case binary:split(Data, <<0>>) of
    [String, Rest] ->
      {String, Rest};
    _ ->
      error({truncated_string, Data})
  end.

-spec decode_strings(binary()) -> [binary()].
decode_strings(Data) ->
  decode_strings(Data, []).

-spec decode_strings(binary(), [binary()]) -> [binary()].
decode_strings(<<>>, _Acc) ->
  error(truncated_data);
decode_strings(<<0>>, Acc) ->
  lists:reverse(Acc);
decode_strings(Data, Acc) ->
  {String, Rest} = decode_string(Data),
  decode_strings(Rest, [String | Acc]).

-spec query_response() -> query_response().
query_response() ->
  #{}.

-spec add_query_response_row(row(), query_response()) -> query_response().
add_query_response_row(Row, Response) ->
  Rows = maps:get(rows, Response, []),
  maps:put(rows, [Row | Rows], Response).

-spec finalize_query_response(query_response()) -> query_response().
finalize_query_response(Response = #{rows := Rows}) ->
  Response#{rows => lists:reverse(Rows)};
finalize_query_response(Response) ->
  Response.

-spec query_response_to_query_result(query_response(), pg_types:type_db()) ->
        pg:query_result().
query_response_to_query_result(#{error := Error}, _TypeDb) ->
  {error, Error};
query_response_to_query_result(#{columns := ResponseColumns,
                                 rows := ResponseRows,
                                 command_tag := CommandTag},
                               TypeDb) ->
  NbAffectedRows = case CommandTag of
                     {_, Nb} -> Nb;
                     _ -> 0
                   end,
  Columns = [Name || #{name := Name} <- ResponseColumns],
  Oids = [Oid || #{type_oid := Oid} <- ResponseColumns],
  Rows = lists:map(fun (Row) ->
                       pg_types:decode_values(Row, Oids, TypeDb)
                   end, ResponseRows),
  {ok, Columns, Rows, NbAffectedRows}.
