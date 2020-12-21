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

-module(pgc_model).

-export([model/1,
         encode/2, encode/3,
         decode/2, decode/3,
         decode_row/2, decode_row/3,
         decode_rows/2, decode_rows/3,
         column_tuple/1, column_tuple/2, column_tuple/3,
         column_csv/1, column_csv/2, column_csv/3,
         placeholder_tuple/1, placeholder_tuple/2,
         placeholder_csv/1, placeholder_csv/2,
         placeholder_list/2,
         columns/1, columns/2, columns/3,
         column/2, column/3]).

-export_type([model_ref/0, model_name/0, model_table_name/0,
              model/0, model_key/0, model_keys/0, model_value/0,
              encode_fun/0, decode_fun/0, encoded_value/0,
              row/0, column/0, entity/0]).

-type model_ref() :: model_name() | model().

-type model_name() :: atom().
-type model_table_name() :: atom().

-type model() :: #{model_key() := model_value()}.
-type model_key() :: atom().
-type model_keys() :: [model_key()] | all.
-type model_value() :: pgc_types:type_name()
                     | #{type := pgc_types:type_name(),
                         column => column(),
                         default => term(),
                         encode => encode_fun(),
                         decode => decode_fun()}.
-type encode_fun() :: fun((term()) -> encoded_value()).
-type decode_fun() :: fun((term()) -> term()).

-type encoded_value() :: {pgc_types:type_name(), term()}.

-type row() :: [term()].
-type column() :: atom().

-type entity() :: #{atom() := term()}.

-spec model(model_ref()) -> model().
model(Ref) when is_atom(Ref) ->
  pgc_model_registry:get_model(Ref);
model(Model) ->
  Model.

-spec model_keys(model(), model_keys()) -> [model_key()].
model_keys(Model, all) ->
  maps:keys(Model);
model_keys(_Model, Keys) ->
  Keys.

-spec encode(entity(), model_ref()) -> [encoded_value()].
encode(Entity, ModelRef) ->
  encode(Entity, model(ModelRef), all).

-spec encode(entity(), model_ref(), model_keys()) ->
        [encoded_value()].
encode(Entity, ModelRef, Keys) ->
  Model = model(ModelRef),
  F = fun (Key) ->
          Value = case maps:find(Key, Entity) of
                    {ok, V} ->
                      V;
                    error ->
                      default(Model, Key)
                  end,
          encode_value(Value, Model, Key)
      end,
  lists:map(F, model_keys(Model, Keys)).

-spec encode_value(term(), model(), model_key()) -> encoded_value().
encode_value(null, Model, Key) ->
  {type(Model, Key), null};
encode_value(Value, Model, Key) ->
  case encode_fun(Model, Key) of
    {ok, Encode} ->
      Encode(Value);
    error ->
      case type(Model, Key) of
        time ->
          encode_time(Value);
        timestamp ->
          encode_timestamp(Value);
        Type ->
          {Type, Value}
      end
  end.

-spec encode_time(calendar:time()) -> encoded_value().
encode_time(Time) ->
  {time, pgc_utils:time(Time)}.

-spec encode_timestamp(calendar:datetime()) -> encoded_value().
encode_timestamp(Datetime) ->
  {timestamp, pgc_utils:timestamp(Datetime)}.

-spec decode(row(), model_ref()) -> entity().
decode(Row, ModelRef) ->
  decode_row(Row, ModelRef).

-spec decode(row(), model_ref(), model_keys()) -> entity().
decode(Row, ModelRef, Keys) ->
  decode_row(Row, ModelRef, Keys).

-spec decode_rows([row()], model_ref()) -> [entity()].
decode_rows(Rows, ModelRef) ->
  decode_rows(Rows, model(ModelRef), all).

-spec decode_rows([row()], model_ref(), model_keys()) -> [entity()].
decode_rows(Rows, ModelRef, Keys) ->
  Model = model(ModelRef),
  lists:map(fun (Row) -> decode_row(Row, Model, Keys) end, Rows).

-spec decode_row(row(), model_ref()) -> entity().
decode_row(Row, ModelRef) ->
  decode_row(Row, model(ModelRef), all).

-spec decode_row(row(), model_ref(), model_keys()) -> entity().
decode_row(Row, ModelRef, Keys) ->
  Model = model(ModelRef),
  F = fun ({Value0, Key}, Entity) ->
          Value = case Value0 of
                    null ->
                      default(Model, Key);
                    _ ->
                      Value0
                  end,
          case {Value, type(Model, Key)} of
            {null, _} ->
              Entity;
            {_, TypeName} ->
              case decode_fun(Model, Key) of
                {ok, Decode} ->
                  Entity#{Key => Decode(Value)};
                error ->
                  DecodedValue = decode_field(Value, TypeName),
                  Entity#{Key => DecodedValue}
              end
          end
      end,
  lists:foldl(F, #{}, lists:zip(Row, model_keys(Model, Keys))).

-spec decode_field(term(), pgc_types:type_name()) -> term().
decode_field(Value, time) ->
  pgc_utils:time_to_erlang_time(Value);
decode_field(Value, timestamp) ->
  pgc_utils:timestamp_to_erlang_datetime(Value);
decode_field(Value, _) ->
  Value.

-spec column_tuple(model_ref()) -> unicode:chardata().
column_tuple(ModelRef) ->
  column_tuple(model(ModelRef), all).

-spec column_tuple(model_ref(), model_keys()) -> unicode:chardata().
column_tuple(ModelRef, Keys) ->
  Model = model(ModelRef),
  [$(, column_csv(Model, Keys), $)].

-spec column_tuple(model_ref(), string(), model_keys()) -> unicode:chardata().
column_tuple(ModelRef, Correlation, Keys) ->
  Model = model(ModelRef),
  [$(, column_csv(Model, Correlation, Keys), $)].

-spec column_csv(model_ref()) -> unicode:chardata().
column_csv(ModelRef) ->
  column_csv(model(ModelRef), all).

-spec column_csv(model_ref(), model_keys()) -> unicode:chardata().
column_csv(ModelRef, Keys) ->
  Model = model(ModelRef),
  Names = lists:map(fun (Key) -> column(Model, Key) end,
                    model_keys(Model, Keys)),
  lists:join($,, Names).

-spec column_csv(model_ref(), string(), model_keys()) -> unicode:chardata().
column_csv(ModelRef, Correlation, Keys) ->
  Model = model(ModelRef),
  Names = lists:map(fun (Key) -> column(Model, Correlation, Key) end,
                    model_keys(Model, Keys)),
  lists:join($,, Names).

-spec columns(model_ref()) -> [unicode:chardata()].
columns(ModelRef) ->
  columns(model(ModelRef), all).

-spec columns(model_ref(), model_keys()) -> [unicode:chardata()].
columns(ModelRef, Keys) ->
  Model = model(ModelRef),
  lists:map(fun (Key) -> column(Model, Key) end,
            model_keys(Model, Keys)).

-spec columns(model_ref(), string(), model_keys()) -> [unicode:chardata()].
columns(ModelRef, Correlation, Keys) ->
  Model = model(ModelRef),
  lists:map(fun (Key) -> column(Model, Correlation, Key) end,
            model_keys(Model, Keys)).

-spec column(model_ref(), model_key()) -> unicode:chardata().
column(ModelRef, Key) ->
  column(ModelRef, "", Key).

-spec column(model_ref(), string(), model_key()) ->
        unicode:chardata().
column(ModelRef, Correlation, Key) ->
  Model = model(ModelRef),
  Name0 = case maps:find(Key, Model) of
            {ok, #{column := Column}} ->
              Column;
            {ok, _} ->
              Key;
            error ->
              error({unknown_model_key, Key, Model})
          end,
  case iolist_size(Correlation) of
    0 ->
      quote_identifier(Name0);
    _ ->
      [quote_identifier(Correlation), $., quote_identifier(Name0)]
  end.

-spec placeholder_tuple(model_ref()) -> unicode:chardata().
placeholder_tuple(ModelRef) ->
  placeholder_tuple(model(ModelRef), all).

-spec placeholder_tuple(model_ref(), model_keys()) -> unicode:chardata().
placeholder_tuple(ModelRef, Keys) ->
  Model = model(ModelRef),
  [$(, placeholder_csv(Model, Keys), $)].

-spec placeholder_csv(model_ref()) -> unicode:chardata().
placeholder_csv(ModelRef) ->
  placeholder_csv(model(ModelRef), all).

-spec placeholder_csv(model_ref(), model_keys()) -> unicode:chardata().
placeholder_csv(Model, Keys) ->
  lists:join($,, placeholder_list(1, length(model_keys(Model, Keys)))).

-spec placeholder_list(Min :: pos_integer(), Max :: pos_integer()) ->
        [binary()].
placeholder_list(Min, Max) ->
  [<<$$, (integer_to_binary(N))/binary>> || N <- lists:seq(Min, Max)].

-spec type(model(), model_key()) -> pgc_types:type_name().
type(Model, Key) ->
  case maps:find(Key, Model) of
    {ok, #{type := Type}} ->
      Type;
    {ok, Type} when is_atom(Type); is_tuple(Type) ->
      Type;
    error ->
      error({unknown_model_key, Key, Model})
  end.

-spec default(model(), model_key()) -> term().
default(Model, Key) ->
  case maps:find(Key, Model) of
    {ok, #{default := Default}} ->
      Default;
    {ok, _} ->
      null;
    error ->
      error({unknown_model_key, Key, Model})
  end.

-spec encode_fun(model(), model_key()) -> {ok, encode_fun()} | error.
encode_fun(Model, Key) ->
  case maps:find(Key, Model) of
    {ok, #{encode := Encode}} ->
      {ok, Encode};
    {ok, _} ->
      error;
    error ->
      error({unknown_model_key, Key, Model})
  end.

-spec decode_fun(model(), model_key()) -> {ok, decode_fun()} | error.
decode_fun(Model, Key) ->
  case maps:find(Key, Model) of
    {ok, #{decode := Decode}} ->
      {ok, Decode};
    {ok, _} ->
      error;
    error ->
      error({unknown_model_key, Key, Model})
  end.

-spec quote_identifier(unicode:chardata() | atom()) -> binary().
quote_identifier(Id) when is_atom(Id) ->
  quote_identifier(atom_to_binary(Id));
quote_identifier(Id) when is_binary(Id) ->
  pgc_utils:quote_identifier(Id);
quote_identifier(Id) ->
  case unicode:characters_to_binary(Id) of
    Bin when is_binary(Bin) ->
      quote_identifier(Bin);
    _ ->
      error({invalid_identifier, Id})
  end.
