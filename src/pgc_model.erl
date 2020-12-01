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

-export([entity_to_row/2, entity_to_row/3,
         entity_from_row/2, entity_from_row/3]).

-export_type([model/0, model_key/0, model_value/0, row/0, column/0, entity/0]).

-type model() :: #{model_key() := model_value()}.
-type model_key() :: atom().
-type model_value() :: #{type := pgc_types:type_name(),
                         column => column()}.

-type query_parameter() :: {pgc_types:type_name(), term()}.

-type row() :: [term()].
-type column() :: atom().

-type entity() :: #{atom() := term()}.

-spec entity_to_row(entity(), model()) -> [query_parameter()].
entity_to_row(Entity, Model) ->
  entity_to_row(Entity, Model, maps:keys(Model)).

-spec entity_to_row(entity(), model(), [model_key()]) -> [query_parameter()].
entity_to_row(Entity, Model, Keys) ->
  lists:map(fun (Key) ->
                Value = case maps:find(Key, Entity) of
                          {ok, V} -> V;
                          error -> null
                        end,
                query_parameter(Value, Model, Key)
            end, Keys).

-spec entity_from_row(row(), model()) -> entity().
entity_from_row(Row, Model) ->
  entity_from_row(Row, Model, maps:keys(Model)).

-spec entity_from_row(row(), model(), [model_key()]) -> entity().
entity_from_row(Row, Model, Keys) ->
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

-spec query_parameter(term(), model(), model_key()) -> query_parameter().
query_parameter(Value, Model, Key) ->
  case maps:find(Key, Model) of
    {ok, #{type := Type}} ->
      {Type, Value};
    error ->
      error({unknown_model_key, Key, Model})
  end.
