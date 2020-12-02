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
         column_name_list/1, column_name_list/2,
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

-type query_parameter() :: {pgc_types:type_name(), term()}.

-type row() :: [term()].
-type column() :: atom().

-type entity() :: #{atom() := term()}.

-spec encode_entity(entity(), model_ref()) -> [query_parameter()].
encode_entity(Entity, Model) when is_atom(Model) ->
  encode_entity(Entity, pgc_model_registry:get_model(Model));
encode_entity(Entity, Model) ->
  encode_entity(Entity, Model, maps:keys(Model)).

-spec encode_entity(entity(), model_ref(), [model_key()]) ->
        [query_parameter()].
encode_entity(Entity, Model, Keys) when is_atom(Model) ->
  encode_entity(Entity, pgc_model_registry:get_model(Model), Keys);
encode_entity(Entity, Model, Keys) ->
  lists:map(fun (Key) ->
                Value = case maps:find(Key, Entity) of
                          {ok, V} -> V;
                          error -> null
                        end,
                query_parameter(Value, Model, Key)
            end, Keys).

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
                  case maps:is_key(Key, Model) of
                    true ->
                      case Field of
                        null -> Entity;
                        _ -> Entity#{Key => Field}
                      end;
                    false ->
                      error({unknown_model_key, Key, Model})
                  end
              end, #{}, lists:zip(Row, Keys)).

-spec column_name_list(model_ref()) -> unicode:chardata().
column_name_list(Model) when is_atom(Model) ->
  column_name_list(pgc_model_registry:get_model(Model));
column_name_list(Model) ->
  column_name_list(Model, maps:keys(Model)).

-spec column_name_list(model_ref(), [model_key()]) -> unicode:chardata().
column_name_list(Model, Keys) when is_atom(Model) ->
  column_name_list(pgc_model_registry:get_model(Model), Keys);
column_name_list(Model, Keys) ->
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

-spec columns(model(), [model_key()]) -> [column()].
columns(Model, Keys) ->
  lists:map(fun (Key) -> column(Model, Key) end, Keys).

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

-spec query_parameter(term(), model(), model_key()) -> query_parameter().
query_parameter(Value, Model, Key) ->
  case maps:find(Key, Model) of
    {ok, #{type := Type}} ->
      {Type, Value};
    {ok, Type} when is_atom(Type); is_tuple(Type) ->
      {Type, Value};
    error ->
      error({unknown_model_key, Key, Model})
  end.
