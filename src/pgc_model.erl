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

-export([encode_entity/2, encode_entity/3,
         decode_entity/2, decode_entity/3,
         column_name_tuple/1, column_name_tuple/2,
         column_name_csv/1, column_name_csv/2,
         column_name/2]).

-export_type([model_name/0, model_table_name/0,
              model/0, model_key/0, model_value/0, row/0, column/0, entity/0]).

-type model_ref() :: model_name() | model().

-type model_name() :: atom().
-type model_table_name() :: atom().

-type model() :: #{model_key() := model_value()}.
-type model_key() :: atom().
-type model_value() :: pgc_types:type_name()
                     | #{type := pgc_types:type_name(),
                         column => column()}.

-type encoded_value() :: {pgc_types:type_name(), term()}.

-type row() :: [term()].
-type column() :: atom().

-type entity() :: #{atom() := term()}.

-spec encode_entity(entity(), model_ref()) -> [encoded_value()].
encode_entity(Entity, Model) when is_atom(Model) ->
  encode_entity(Entity, pgc_model_registry:get_model(Model));
encode_entity(Entity, Model) ->
  encode_entity(Entity, Model, maps:keys(Model)).

-spec encode_entity(entity(), model_ref(), [model_key()]) ->
        [encoded_value()].
encode_entity(Entity, Model, Keys) when is_atom(Model) ->
  encode_entity(Entity, pgc_model_registry:get_model(Model), Keys);
encode_entity(Entity, Model, Keys) ->
  lists:map(fun (Key) ->
                Value = case maps:find(Key, Entity) of
                          {ok, V} -> V;
                          error -> null
                        end,
                encode_value(Value, Model, Key)
            end, Keys).

-spec encode_value(term(), model(), model_key()) -> encoded_value().
encode_value(Value, Model, Key) ->
  case type(Model, Key) of
    time ->
      encode_time(Value);
    timestamp ->
      encode_timestamp(Value);
    Type ->
      {Type, Value}
  end.

-spec encode_time(calendar:time()) -> encoded_value().
encode_time(Time) ->
  {time, pgc_utils:time(Time)}.

-spec encode_timestamp(calendar:datetime()) -> encoded_value().
encode_timestamp(Datetime) ->
  {timestamp, pgc_utils:timestamp(Datetime)}.

-spec decode_entity(row(), model_ref()) -> entity().
decode_entity(Row, Model) when is_atom(Model) ->
  decode_entity(Row, pgc_model_registry:get_model(Model));
decode_entity(Row, Model) ->
  decode_entity(Row, Model, maps:keys(Model)).

-spec decode_entity(row(), model_ref(), [model_key()]) -> entity().
decode_entity(Row, Model, Keys) when is_atom(Model) ->
  decode_entity(Row, pgc_model_registry:get_model(Model), Keys);
decode_entity(Row, Model, Keys) ->
  lists:foldl(fun ({Field, Key}, Entity) ->
                  case {Field, type(Model, Key)} of
                    {null, _} ->
                      Entity;
                    {_, time} ->
                      Entity#{Key => decode_time(Field)};
                    {_, timestamp} ->
                      Entity#{Key => decode_timestamp(Field)};
                    {_, _} ->
                      Entity#{Key => Field}
                  end
              end, #{}, lists:zip(Row, Keys)).

-spec decode_time(pgc:time()) -> calendar:time().
decode_time(Time) ->
  pgc_utils:time_to_erlang_time(Time).

-spec decode_timestamp(pgc:timestamp()) -> calendar:datetime().
decode_timestamp(Timestamp) ->
  pgc_utils:timestamp_to_erlang_datetime(Timestamp).

-spec column_name_tuple(model_ref()) -> unicode:chardata().
column_name_tuple(Model) when is_atom(Model) ->
  column_name_tuple(pgc_model_registry:get_model(Model));
column_name_tuple(Model) ->
  column_name_tuple(Model, maps:keys(Model)).

-spec column_name_tuple(model_ref(), [model_key()]) -> unicode:chardata().
column_name_tuple(Model, Keys) when is_atom(Model) ->
  column_name_tuple(pgc_model_registry:get_model(Model), Keys);
column_name_tuple(Model, Keys) ->
  [$(, column_name_csv(Model, Keys), $)].

-spec column_name_csv(model_ref()) -> unicode:chardata().
column_name_csv(Model) when is_atom(Model) ->
  column_name_csv(pgc_model_registry:get_model(Model));
column_name_csv(Model) ->
  column_name_csv(Model, maps:keys(Model)).

-spec column_name_csv(model_ref(), [model_key()]) -> unicode:chardata().
column_name_csv(Model, Keys) when is_atom(Model) ->
  column_name_csv(pgc_model_registry:get_model(Model), Keys);
column_name_csv(Model, Keys) ->
  Names = lists:map(fun (Key) -> column_name(Model, Key) end, Keys),
  lists:join($,, Names).

-spec column_name(model_ref(), model_key()) -> unicode:chardata().
column_name(Model, Key) when is_atom(Model) ->
  column_name(pgc_model_registry:get_model(Model), Key);
column_name(Model, Key) ->
  Column = column(Model, Key),
  pgc_utils:quote_identifier(atom_to_binary(Column)).

-spec column(model(), model_key()) -> column().
column(Model, Key) ->
  case maps:find(Key, Model) of
    {ok, #{column := Column}} ->
      Column;
    {ok, _} ->
      Key;
    error ->
      error({unknown_model_key, Key, Model})
  end.

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
