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

-export([encode/2, encode/3,
         decode/2, decode/3,
         column_tuple/1, column_tuple/2, column_csv/1, column_csv/2,
         placeholder_tuple/1, placeholder_tuple/2,
         placeholder_csv/1, placeholder_csv/2,
         placeholder_list/2,
         columns/1, columns/2, column/2]).

-export_type([model_name/0, model_table_name/0,
              model/0, model_key/0, model_value/0, row/0, column/0, entity/0]).

-type model_ref() :: model_name() | model().

-type model_name() :: atom().
-type model_table_name() :: atom().

-type model() :: #{model_key() := model_value()}.
-type model_key() :: atom().
-type model_value() :: pgc_types:type_name()
                     | #{type := pgc_types:type_name(),
                         column => column(),
                         encode => encode_fun(),
                         decode => decode_fun}.
-type encode_fun() :: fun((term()) -> encoded_value()).
-type decode_fun() :: fun((encoded_value()) -> term()).

-type encoded_value() :: {pgc_types:type_name(), term()}.

-type row() :: [term()].
-type column() :: atom().

-type entity() :: #{atom() := term()}.

-spec encode(entity(), model_ref()) -> [encoded_value()].
encode(Entity, Model) when is_atom(Model) ->
  encode(Entity, pgc_model_registry:get_model(Model));
encode(Entity, Model) ->
  encode(Entity, Model, maps:keys(Model)).

-spec encode(entity(), model_ref(), [model_key()]) ->
        [encoded_value()].
encode(Entity, Model, Keys) when is_atom(Model) ->
  encode(Entity, pgc_model_registry:get_model(Model), Keys);
encode(Entity, Model, Keys) ->
  lists:map(fun (Key) ->
                Value = case maps:find(Key, Entity) of
                          {ok, V} -> V;
                          error -> null
                        end,
                encode_value(Value, Model, Key)
            end, Keys).

-spec encode_value(term(), model(), model_key()) -> encoded_value().
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
decode(Row, Model) when is_atom(Model) ->
  decode(Row, pgc_model_registry:get_model(Model));
decode(Row, Model) ->
  decode(Row, Model, maps:keys(Model)).

-spec decode(row(), model_ref(), [model_key()]) -> entity().
decode(Row, Model, Keys) when is_atom(Model) ->
  decode(Row, pgc_model_registry:get_model(Model), Keys);
decode(Row, Model, Keys) ->
  lists:foldl(fun ({Field, Key}, Entity) ->
                  case decode_fun(Model, Key) of
                    {ok, Decode} ->
                      Entity#{Key => Decode(Field)};
                    error ->
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
                  end
              end, #{}, lists:zip(Row, Keys)).

-spec decode_time(pgc:time()) -> calendar:time().
decode_time(Time) ->
  pgc_utils:time_to_erlang_time(Time).

-spec decode_timestamp(pgc:timestamp()) -> calendar:datetime().
decode_timestamp(Timestamp) ->
  pgc_utils:timestamp_to_erlang_datetime(Timestamp).

-spec column_tuple(model_ref()) -> unicode:chardata().
column_tuple(Model) when is_atom(Model) ->
  column_tuple(pgc_model_registry:get_model(Model));
column_tuple(Model) ->
  column_tuple(Model, maps:keys(Model)).

-spec column_tuple(model_ref(), [model_key()]) -> unicode:chardata().
column_tuple(Model, Keys) when is_atom(Model) ->
  column_tuple(pgc_model_registry:get_model(Model), Keys);
column_tuple(Model, Keys) ->
  [$(, column_csv(Model, Keys), $)].

-spec column_csv(model_ref()) -> unicode:chardata().
column_csv(Model) when is_atom(Model) ->
  column_csv(pgc_model_registry:get_model(Model));
column_csv(Model) ->
  column_csv(Model, maps:keys(Model)).

-spec column_csv(model_ref(), [model_key()]) -> unicode:chardata().
column_csv(Model, Keys) when is_atom(Model) ->
  column_csv(pgc_model_registry:get_model(Model), Keys);
column_csv(Model, Keys) ->
  Names = lists:map(fun (Key) -> column(Model, Key) end, Keys),
  lists:join($,, Names).

-spec columns(model_ref()) -> [unicode:chardata()].
columns(Model) when is_atom(Model) ->
  columns(pgc_model_registry:get_model(Model));
columns(Model) ->
  columns(Model, maps:keys(Model)).

-spec columns(model_ref(), [model_key()]) -> [unicode:chardata()].
columns(Model, Keys) when is_atom(Model) ->
  columns(pgc_model_registry:get_model(Model), Keys);
columns(Model, Keys) ->
  lists:map(fun (Key) -> column(Model, Key) end, Keys).

-spec column(model_ref(), model_key()) -> unicode:chardata().
column(Model, Key) when is_atom(Model) ->
  column(pgc_model_registry:get_model(Model), Key);
column(Model, Key) ->
  Name = case maps:find(Key, Model) of
           {ok, #{column := Column}} ->
             Column;
           {ok, _} ->
             Key;
           error ->
             error({unknown_model_key, Key, Model})
         end,
  pgc_utils:quote_identifier(atom_to_binary(Name)).

-spec placeholder_tuple(model_ref()) -> unicode:chardata().
placeholder_tuple(Model) when is_atom(Model) ->
  placeholder_tuple(pgc_model_registry:get_model(Model));
placeholder_tuple(Model) ->
  placeholder_tuple(Model, maps:keys(Model)).

-spec placeholder_tuple(model_ref(), [model_key()]) -> unicode:chardata().
placeholder_tuple(Model, Keys) when is_atom(Model) ->
  placeholder_tuple(pgc_model_registry:get_model(Model), Keys);
placeholder_tuple(Model, Keys) ->
  [$(, placeholder_csv(Model, Keys), $)].

-spec placeholder_csv(model_ref()) -> unicode:chardata().
placeholder_csv(Model) when is_atom(Model) ->
  placeholder_csv(pgc_model_registry:get_model(Model));
placeholder_csv(Model) ->
  placeholder_csv(Model, maps:keys(Model)).

-spec placeholder_csv(model_ref(), [model_key()]) -> unicode:chardata().
placeholder_csv(Model, Keys) when is_atom(Model) ->
  placeholder_csv(pgc_model_registry:get_model(Model), Keys);
placeholder_csv(_, Keys) ->
  lists:join($,, placeholder_list(1, length(Keys))).

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
